/*
* Copyright 2023 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the ActiveOberon parser/navigator project.
*
* The following is the license that applies to this copy of the
* file. For a license to use the file under conditions
* other than those described here, please email to me@rochus-keller.ch.
*
* GNU General Public License Usage
* This file may be used under the terms of the GNU General Public
* License (GPL) versions 2.0 or 3.0 as published by the Free Software
* Foundation and appearing in the file LICENSE.GPL included in
* the packaging of this file. Please review the following information
* to ensure GNU General Public Licensing requirements will be met:
* http://www.fsf.org/licensing/licenses/info/GPLv2.html and
* http://www.gnu.org/copyleft/gpl.html.
*/


// Adopted from the Lisa Pascal Navigator

#include "AoFileSystem.h"
#include "AoLexer.h"
#include <QFile>
#include <QDir>
#include <QtDebug>
using namespace Ao;

FileSystem::FileSystem(QObject *parent) : QObject(parent)
{

}

static QString oneUp(const QString& path)
{
    QStringList segs = path.split('/');
    if( segs.size() > 2 )
    {
        segs.pop_back();
        return segs.join('/');
    }else
        return path;
}

static QString replaceLast(const QString& path, const QString& newLast )
{
    QStringList segs = path.split('/');
    segs.pop_back();
    segs.push_back(newLast);
    return segs.join('/');
}

static QStringList collectFiles( const QDir& dir, const QStringList& suffix )
{
    QStringList res;
    QStringList files = dir.entryList( QDir::Dirs | QDir::NoDotAndDotDot, QDir::Name );

    foreach( const QString& f, files )
        res += collectFiles( QDir( dir.absoluteFilePath(f) ), suffix );

    files = dir.entryList( suffix, QDir::Files, QDir::Name );
    foreach( const QString& f, files )
    {
        res.append(dir.absoluteFilePath(f));
    }
    return res;
}

bool FileSystem::load(const QString& rootDir)
{
    QFileInfo dirInfo(rootDir.endsWith('/') ? rootDir : rootDir + "/");
    if( !dirInfo.isDir() )
        return error("not a directory");

    d_root.clear();
    d_fileMap.clear();
    d_moduleMap.clear();
    d_rootDir = rootDir;

    const QStringList files = collectFiles(dirInfo.absolutePath(),QStringList() << "*.Mod");
    const int off = dirInfo.absolutePath().size();

    foreach( const QString& f, files )
    {
        QFile in(f);
        if( !in.open(QIODevice::ReadOnly) )
            return error(tr("cannot open file for reading: %1").arg(f));
        QByteArray moduleName;
        FileType fileType = detectType(&in,&moduleName);
        if( fileType == UnknownFile )
            continue;
        in.close();

        QFileInfo info(f);

        const QString relDirPath = info.absolutePath().mid(off+1);

        QString name = info.completeBaseName();

        File* file = new File();
        file->d_type = fileType;
        file->d_name = name;
        file->d_moduleName = moduleName;
        if( !file->d_moduleName.isEmpty() )
        {
            File*& slot = d_moduleMap[file->d_moduleName];
            if( slot == 0 )
                slot = file;
            else
            {
                if( ( slot->d_moduleName != slot->d_name && file->d_moduleName == file->d_name)
                      // prefer modules where name corresponds to file name
                        || ( slot->level() < file->level() )
                             // prefer modules deeper in the hierarchy
                        )
                {
                    slot->d_doublette = true;
                    slot = file;
                }else
                    file->d_doublette = true;
            }
        }
        file->d_realPath = f;
        d_fileMap[f] = file;
        // file with no dir
        Dir* dir = getDir(relDirPath);
        file->d_dir = dir;
        dir->d_files.append(file);
    }

#if 0
    // print stats
    QMap<int,int> count;
    QHash<QByteArray,QList<File*> >::const_iterator i;
    for( i = d_moduleMap.begin(); i != d_moduleMap.end(); ++i )
    {
        const QList<File*>& hits = i.value();
        count[hits.size()]++;
        if( hits.size() > 1 )
        {
            for(int j = 0; j < hits.size(); j++ )
                qDebug() << i.key() << "\t" << hits.size() << "\t" << hits[j]->getVirtualPath();
        }
    }
    qDebug() << count;
#endif
    return true;
}

bool FileSystem::addToRoot(const QStringList& files)
{
    // used for testing purpose

    d_root.clear();
    d_fileMap.clear();
    d_moduleMap.clear();

    foreach( const QString& f, files )
    {
        QFile in(f);
        if( !in.open(QIODevice::ReadOnly) )
            return error(tr("cannot open file for reading: %1").arg(f));
        in.close();

        QFileInfo info(f);

        File* file = new File();
        file->d_type = UnknownFile;
        file->d_name = info.completeBaseName();
        file->d_moduleName = info.completeBaseName().toUtf8();
        file->d_realPath = f;
        d_fileMap[f] = file;
        file->d_dir = &d_root;
        d_root.d_files.append(file);
    }
    return true;
}

static void walkForPas(const FileSystem::Dir* d, QList<const FileSystem::File*>& res )
{
    for( int i = 0; i < d->d_subdirs.size(); i++ )
        walkForPas( d->d_subdirs[i], res );
    for( int i = 0; i < d->d_files.size(); i++ )
        if( d->d_files[i]->d_type == FileSystem::OberonModule )
            res.append(d->d_files[i]);
}

QList<const FileSystem::File*> FileSystem::getAllPas() const
{
    QList<const FileSystem::File*> res;
    walkForPas(&d_root,res);
    return res;
}

const FileSystem::File*FileSystem::findFile(const QString& realPath) const
{
    return d_fileMap.value(realPath);
}

const FileSystem::File*FileSystem::findFile(const Dir* startFrom, const QString& dir, QString name) const
{
    Q_ASSERT(startFrom);
    const File* res = 0;
    const Dir* d = startFrom;
    name = QFileInfo(name).completeBaseName();
    while( res == 0 && d )
    {
        if( dir.isEmpty() || d->d_name == dir )
            res = d->file(name);
        if( res == 0 && !dir.isEmpty() )
        {
            for( int i = 0; i < d->d_subdirs.size(); i++ )
            {
                if( d->d_subdirs[i]->d_name == dir )
                    res = d->d_subdirs[i]->file(name);
                if( res )
                    break;
            }
        }
        d = d->d_dir;
    }
    return res;
}

const FileSystem::File*FileSystem::findModule(const FileSystem::Dir* startFrom, const QByteArray& nameLc) const
{
    Q_ASSERT(startFrom);
    const File* res = 0;
    const Dir* d = startFrom;
    if( res == 0 && d )
    {
        res = d->module(nameLc);
        d = d->d_dir;
    }
    if( res == 0 )
    {
        res = d_moduleMap.value(nameLc);
#if 0
        // print stats; only works when d_moduleMap -> QList<File*>
        static QSet<QByteArray> seen;
        if( !seen.contains(nameLc) && tmp.size() != 1 )
        {
            if( tmp.size() == 0 )
                qDebug() << "not_found" << nameLc;
            else
            {
                for( int i = 0; i < tmp.size(); i++ )
                    qDebug() << "ambiguous" << nameLc << tmp[i]->getVirtualPath();
            }
            seen.insert(nameLc);
        }
#endif
    }else if( res->d_doublette )
        const_cast<File*>(res)->d_forceParse = true;
    return res;
}

FileSystem::FileType FileSystem::detectType(QIODevice* in, QByteArray* name)
{
    Q_ASSERT(in);
    in->reset();
    Lexer lex;
    lex.setStream(in, QString());
    lex.setIgnoreComments(false);
    Token t = lex.nextToken();
    FileType res = UnknownFile;
    int n = 0;
    while( t.isValid() && n++ < 20 )
    {
        switch(t.d_type)
        {
        case Tok_MODULE:
            if( name )
            {
                while( t.d_type != Tok_ident && t.isValid() )
                    t = lex.nextToken();
                if( t.d_type == Tok_ident )
                    *name = t.d_val;
            }
            return OberonModule;
        default:
            res = OberonFragment;
            break;
        }

        t = lex.nextToken();
    }

    return res;
}

bool FileSystem::error(const QString& msg)
{
    d_error = msg;
    return false;
}

FileSystem::Dir*FileSystem::getDir(const QString& relPath)
{
    const QStringList segs = relPath.split('/');
    Dir* cur = &d_root;
    Dir* res = 0;
    for( int i = 0; i < segs.size(); i++ )
    {
        res = cur->subdir(segs[i]);
        if( res == 0 )
        {
            res = new Dir();
            res->d_name = segs[i];
            cur->d_subdirs.append(res);
            res->d_dir = cur;
        }
        cur = res;
    }
    return res;
}

void FileSystem::Dir::clear()
{
    for( int i = 0; i < d_subdirs.size(); i++ )
        d_subdirs[i]->clear();
    d_subdirs.clear();
    for( int i = 0; i < d_files.size(); i++ )
        delete d_files[i];
    d_files.clear();
}

static const char* typeName(int t)
{
    switch( t )
    {
    case FileSystem::OberonModule:
        return ".Mod";
    case FileSystem::OberonFragment:
        return ".Inc";
    default:
        return "";
    }
}

void FileSystem::Dir::dump(int level) const
{
    QTextStream out(stdout);
    QByteArray indent;
    for( int i = 0; i < level; i++ )
        indent += "|  ";
    out << indent.constData() << ( d_name.isEmpty() ? "<root>" : d_name ) << endl;
    for( int i = 0; i < d_files.size(); i++ )
        out << indent.constData() << "|  " << d_files[i]->d_name << typeName(d_files[i]->d_type) << endl;
    for( int i = 0; i < d_subdirs.size(); i++ )
        d_subdirs[i]->dump(level+1);
}

FileSystem::Dir*FileSystem::Dir::subdir(const QString& name) const
{
    for(int i = 0; i < d_subdirs.size(); i++ )
        if( d_subdirs[i]->d_name == name )
            return d_subdirs[i];
    return 0;
}

const FileSystem::File*FileSystem::Dir::file(const QString& name) const
{
    for(int i = 0; i < d_files.size(); i++ )
        if( d_files[i]->d_name == name )
            return d_files[i];
    return 0;
}

const FileSystem::File*FileSystem::Dir::module(const QByteArray& name) const
{
    for(int i = 0; i < d_files.size(); i++ )
        if( d_files[i]->d_moduleName == name )
            return d_files[i];
    return 0;
}

QString FileSystem::File::getVirtualPath(bool suffix) const
{
    const Dir* d = d_dir;
    QString res;
    while( d && !d->d_name.isEmpty() )
    {
        res = d->d_name + "/" + res;
        d = d->d_dir;
    }
    res += d_name;
    if( suffix )
        res += typeName(d_type);
    return res;
}

int FileSystem::File::level() const
{
    int res = 0;
    const Dir* d = d_dir;
    while( d && !d->d_name.isEmpty() )
    {
        res++;
        d = d->d_dir;
    }
    return res;
}
