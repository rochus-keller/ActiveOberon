#ifndef FILESYSTEM_H
#define FILESYSTEM_H

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

#include <QHash>
#include <QObject>

class QIODevice;

namespace Ao
{
class FileSystem : public QObject
{
public:
    struct File;

    struct Dir
    {
        QList<Dir*> d_subdirs;
        QList<File*> d_files;
        QString d_name;
        Dir* d_dir;

        void clear();
        void dump(int level = 0) const;
        Dir* subdir(const QString& name) const;
        const File* file(const QString& name) const;
        const File* module(const QByteArray& name) const;
        Dir():d_dir(0) {}
        ~Dir() { clear(); }
    };

    enum FileType { UnknownFile, OberonModule, OberonFragment };
    struct File
    {
        quint8 d_type;
        bool d_doublette;
        bool d_forceParse;
        bool d_parsed;
        QString d_realPath;
        QString d_name; // fileName
        QByteArray d_moduleName;
        Dir* d_dir;
        QString getVirtualPath(bool suffix = true) const;
        int level() const;

        File():d_doublette(false),d_type(UnknownFile),d_dir(0),d_forceParse(false),d_parsed(false){}
    };

    explicit FileSystem(QObject *parent = 0);
    bool load( const QString& rootDir );
    bool addToRoot( const QStringList& files );
    const QString& getError() const { return d_error; }
    const Dir& getRoot() const { return d_root; }
    const QString& getRootPath() const { return d_rootDir; }
    QList<const File*> getAllPas() const;
    const File* findFile(const QString& realPath) const;
    const File* findFile(const Dir* startFrom, const QString& dir, QString name) const;
    const File* findModule(const Dir* startFrom, const QByteArray& nameLc) const;

    static FileType detectType(QIODevice* in, QByteArray* = 0);
protected:
    bool error( const QString& );
    Dir* getDir( const QString& relPath );

private:
    QString d_rootDir;
    QString d_error;
    Dir d_root;
    QHash<QString,File*> d_fileMap;
    QHash<QByteArray,File*> d_moduleMap; // module to File* is ambig, but besides "prmgr" (nearly) identical
};
}

#endif // FILESYSTEM_H
