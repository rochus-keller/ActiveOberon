/*
* Copyright 2026 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the Active Oberon IDE application.
*
* The following is the license that applies to this copy of the
* library. For a license to use the library under conditions
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

// Adopted from the Luon IDE

#include "AoProject2.h"
#include "AoParser2.h"
#include "AoLexer.h"
#include "AoValidator2.h"
#include "AoCeeGen.h"
#include <QBuffer>
#include <QDir>
#include <QtDebug>
#include <QSettings>
#include <QCoreApplication>
#include <qdatetime.h>
using namespace Ao;
using namespace Ast;

struct HitTest
{
    quint32 line, col;
    QList<Declaration*> scopes;
    Declaration* scopeHit;

    HitTest():scopeHit(0){}

    QVariant findHit( Declaration* module, int row, int col )
    {
        this->col = row;
        this->line = line;
        try
        {
            visitDecl(module);
        }catch( Expression* e )
        {
            Q_ASSERT( !scopes.isEmpty() );
            scopeHit = scopes.back();
            return QVariant::fromValue(e);
        }catch( Declaration* d )
        {
            Q_ASSERT( !scopes.isEmpty() );
            scopeHit = scopes.back();
            return QVariant::fromValue(d);
        }catch(...)
        {

        }
        return 0;
    }

    void test( Declaration* d )
    {
        if( line == d->pos.d_row && col >= d->pos.d_col && col <= d->pos.d_col + d->name.size() )
            throw d;
    }

    void test(Expression* e)
    {
        if( e == 0 )
            return;
        if( e->pos.d_row > line )
            return;
        Declaration* n = e->val.value<Declaration*>();
        if( n == 0 )
            return;
        if( line == e->pos.d_row && col >= e->pos.d_col && col <= e->pos.d_col + n->name.size() )
            throw e;
    }

    void visitDecl(Declaration* d)
    {
        test(d);
    }
};

Project2::Project2(QObject *parent) : QObject(parent),d_dirty(false),d_useBuiltInOakwood(false)
{
    d_suffixes << ".Mod" << ".mod";
    d_groups.append( FileGroup() );    // root
}

Project2::~Project2()
{
    clearModules();
}

void Project2::clear()
{
    clearModules();
    d_groups.clear();
    d_groups.append( FileGroup() );    // root
    d_filePath.clear();
    d_files.clear();
    d_byName.clear();
}

void Project2::createNew()
{
    clear();
    d_filePath.clear();
    d_dirty = false;
    emit sigModified(d_dirty);
    emit sigRenamed();
}

bool Project2::initializeFromDir(const QDir& dir, bool recursive)
{
    clear();
    d_dirty = false;

    QStringList files = findFiles(dir, recursive);
    foreach( const QString& filePath, files )
        addFile(filePath);
    emit sigRenamed();
    return true;
}

void Project2::setSuffixes(const QStringList& s)
{
    d_suffixes = s;
    touch();
}

void Project2::setMain(const Project2::ModProc& mp)
{
    d_main = mp;
    touch();
}

QString Project2::renderMain() const
{
    QString res = d_main.first;
    if( !d_main.second.isEmpty() )
        res += "." + d_main.second;
    return res;
}

void Project2::setUseBuiltInOakwood(bool on)
{
    d_useBuiltInOakwood = on;
    touch();
}

bool Project2::addFile(const QString& filePath, const QByteArrayList& package)
{
    if( d_files.contains(filePath) )
        return false;
    int pos = findPackage(package);
    if( pos == -1 )
    {
        pos = d_groups.size();
        d_groups.append( FileGroup() );
        d_groups.back().d_package = package;
    }
    FileGroup& fg = d_groups[pos];
    FileRef ref( new File() );
    fg.d_files.append(ref.data());
    ref->d_group = &fg;
    ref->d_filePath = filePath;
    ref->d_name = QFileInfo(filePath).baseName().toUtf8();
    d_files.insert(filePath,ref);
    touch();
    return true;
}

bool Project2::addPackagePath(const QByteArrayList& path)
{
    int pos = findPackage(path);
    if( pos == -1 )
    {
        pos = d_groups.size();
        d_groups.append( FileGroup() );
        d_groups.back().d_package = path;
        touch();
        return true;
    }else
        return false;
}

bool Project2::removeFile(const QString& filePath)
{
    FileHash::iterator i = d_files.find(filePath);
    if( i == d_files.end() )
        return false;
    const int pos = findPackage( i.value()->d_group->d_package );
    Q_ASSERT( pos != -1 );
    d_groups[pos].d_files.removeAll(i.value().data());
    d_files.erase(i);
    touch();
    return true;
}

bool Project2::removePackagePath(const QByteArrayList& path)
{
    if( path.isEmpty() )
        return false;
    int pos = findPackage(path);
    if( pos == -1 )
        return false;
    if( !d_groups[pos].d_files.isEmpty() )
        return false;
    d_groups.removeAt(pos);
    touch();
    return true;
}

const Project2::FileGroup* Project2::getRootFileGroup() const
{
    return findFileGroup(QByteArrayList());
}

const Project2::FileGroup* Project2::findFileGroup(const QByteArrayList& package) const
{
    for( int i = 0; i < d_groups.size(); i++ )
    {
        if( d_groups[i].d_package == package )
            return &d_groups[i];
    }
    return 0;
}

Symbol* Project2::findSymbolBySourcePos(const QString& file, quint32 line, quint16 col, Declaration** scopePtr) const
{
    File* f = findFile(file);
    if( f == 0 || f->d_mod == 0 )
        return 0;

    return findSymbolByModule(f->d_mod,line,col, scopePtr);
}

Symbol* Project2::findSymbolByModule(Declaration* m, quint32 line, quint16 col, Declaration** scopePtr) const
{
    Q_ASSERT(m && m->kind == Declaration::Module);
    const ModuleSlot* module = findModule(m);
    if( module == 0 || module->xref.syms == 0 )
        return 0;
    Symbol* s = module->xref.syms;
    do
    {
        if( line == s->pos.d_row && col >= s->pos.d_col && col <= s->pos.d_col + s->len )
            return s;
        s = s->next;
    }while( s && s != module->xref.syms );
    return 0;
}

Symbol*Project2::findSymbolByModuleName(const QByteArray& fullName, quint32 line, quint16 col, Declaration** scopePtr) const
{
    Declaration* module = findModule(fullName);
    if( module )
        return findSymbolByModule(module, line, col, scopePtr);
    else
        return 0;
}

Project2::File* Project2::findFile(const QString& file) const
{
    File* f = d_files.value(file).data();
    if( f == 0 )
        f = d_byName.value(file.toUtf8()).first; // includes also generic instances
    return f;
}

Declaration*Project2::findModule(const QByteArray& fullName) const
{
    return d_byName.value(fullName).second;
}

void Project2::addPreload(const QByteArray& name, const QByteArray& code)
{
    for( int i = 0; i < preloads.size(); i++ )
    {
        if( preloads[i]->d_name == name )
        {
            preloads[i]->d_cache = code;
            return;
        }
    }
    // else
    File* f = new File();
    f->d_name = name;
    f->d_cache = code;
    preloads.append(FileRef(f));
}

Project2::UsageByMod Project2::getUsage(Declaration* n) const
{
    UsageByMod res;
    for( int i = 0; i < modules.size(); i++ )
    {
        const SymList& syms = modules[i].xref.uses.value(n);
        if( !syms.isEmpty() )
            res << qMakePair(modules[i].decl, syms);
    }

    return res;
}

Symbol*Project2::getSymbolsOfModule(Declaration* module) const
{
    for( int i = 0; i < modules.size(); i++ )
    {
        if( modules[i].decl == module )
            return modules[i].xref.syms;
    }
    return 0;
}

DeclList Project2::getSubs(Declaration* d) const
{
    return subs.value(d);
}

QString Project2::getWorkingDir(bool resolved) const
{
    if( d_workingDir.isEmpty() )
    {
        if( !d_filePath.isEmpty() )
            return QFileInfo(d_filePath).dir().path();
        else
            return QCoreApplication::applicationDirPath();
    }
    else if( !resolved )
        return d_workingDir;
    // else
    QString wd = d_workingDir;
    wd.replace("%PRODIR%", QFileInfo(d_filePath).dir().path() );
    QString path;
#ifdef Q_OS_MAC
    QDir cur = QCoreApplication::applicationDirPath();
    if( cur.path().endsWith("/Contents/MacOS") )
    {
        // we're in a bundle
        cur.cdUp();
        cur.cdUp();
        cur.cdUp();
    }
    path = cur.path();
#else
    path = QCoreApplication::applicationDirPath();
#endif
    wd.replace("%APPDIR%", path );
    return wd;
}

void Project2::setWorkingDir(const QString& wd)
{
    d_workingDir = wd;
    touch();
}

QString Project2::getBuildDir(bool resolved) const
{
    if( d_buildDir.isEmpty() )
    {
        if( !d_filePath.isEmpty() )
            return QFileInfo(d_filePath).dir().absoluteFilePath("build");
        else
            return QCoreApplication::applicationDirPath() + "/build";
    }
    else if( !resolved )
        return d_buildDir;
    // else
    QString bd = d_buildDir;
    bd.replace("%PRODIR%", QFileInfo(d_filePath).dir().path() );
    QString path;
#ifdef Q_OS_MAC
    QDir cur = QCoreApplication::applicationDirPath();
    if( cur.path().endsWith("/Contents/MacOS") )
    {
        // we're in a bundle
        cur.cdUp();
        cur.cdUp();
        cur.cdUp();
    }
    path = cur.path();
#else
    path = QCoreApplication::applicationDirPath();
#endif
    bd.replace("%APPDIR%", path );
    return bd;
}

void Project2::setBuildDir(const QString& bd)
{
    d_buildDir = bd;
    touch();
}

void Project2::setOptions(const QByteArrayList& o)
{
    d_options = o;
    touch();
}

void Project2::setArguments(const QStringList& args)
{
    d_arguments = args;
    touch();
}

bool Project2::printTreeShaken(const QString& module, const QString& fileName)
{
#if 0
    // TODO
    FileRef f = d_files.value(module);
    if( f->d_mod.isNull() )
        return false;
    Declaration* m = d_mdl->treeShaken(f->d_mod.data());
    QFile out(fileName);
    if( !out.open(QIODevice::WriteOnly) )
        return false;


    ObxModuleDump dump;
    dump.out.setDevice( &out );
    m->accept(&dump);
#endif
    return true;
}

static inline QByteArray escapeDot(QByteArray name)
{
    return "\"" + name + "\"";
}

#if 0
static void removeRedundantImports(Declaration* cur, QSet<Declaration*>& imports )
{
    foreach( Import* i, cur->d_imports )
    {
        imports.remove( i->d_mod.data() );
        removeRedundantImports( i->d_mod.data(), imports );
    }
}

static QList<Declaration*> removeRedundantImports(Declaration* m)
{
    QSet<Declaration*> imports;
    foreach( Import* i, m->d_imports )
    {
        if( !i->d_mod->isFullyInstantiated() || i->d_mod->d_synthetic )
            continue;
        imports << i->d_mod.data();
    }
    foreach( Import* i, m->d_imports )
        removeRedundantImports(i->d_mod.data(), imports);
    return imports.toList();
}
#endif

bool Project2::printImportDependencies(const QString& fileName, bool pruned)
{
    QFile f(fileName);
    if( !f.open(QIODevice::WriteOnly) )
        return false;
    QTextStream s(&f);

    s << "digraph \"Import Dependency Tree\" {" << endl;
    if( !pruned )
        s << "    graph [splines=ortho]" << endl;
    s << "    node [shape=box]" << endl;

#if 0
    // TODO
    QList<Declaration*> mods = getModulesToGenerate(true);
    foreach( Declaration* m, mods )
    {
        if( !m->isFullyInstantiated() || m->d_synthetic )
            continue;
        QSet<QByteArray> names;
        if( pruned )
        {
            QList<Declaration*> imports = removeRedundantImports(m);
            foreach( Declaration* i, imports )
                names << escapeDot(i->getFullName());
        }else
        {
            foreach( Import* i, m->d_imports )
            {
                if( i->d_mod.isNull() || i->d_mod->d_synthetic )
                    continue;
                names << escapeDot(i->d_mod->getFullName());
            }
        }
        const QByteArray name = escapeDot(m->getFullName());
        // s << "    " << name << " [shape=box];" << endl;
        s << "    " << name << " -> {";
        bool first = true;
        foreach( const QByteArray& to, names )
        {
            if( !first )
                s << ", ";
            first = false;
            s << to;
        }
        s << "};" << endl;
    }
#endif

    s << "}";
    return true;
}

void Project2::addError(const QString& file, const RowCol& pos, const QString& msg)
{
    errors << Error(msg, pos, file);
}

QStringList Project2::findFiles(const QDir& dir, bool recursive)
{
    QStringList res;
    QStringList files;

    if( recursive )
    {
        files = dir.entryList( QDir::Dirs | QDir::NoDotAndDotDot, QDir::Name );

        foreach( const QString& f, files )
            res += findFiles( QDir( dir.absoluteFilePath(f) ), recursive );
    }

    QStringList suff = d_suffixes;
    for(int i = 0; i < suff.size(); i++ )
        suff[i] = "*" + suff[i];
    files = dir.entryList( suff, QDir::Files, QDir::Name );
    foreach( const QString& f, files )
    {
        res.append( dir.absoluteFilePath(f) );
    }
    return res;
}

void Project2::touch()
{
    if( !d_dirty )
    {
        d_dirty = true;
        emit sigModified(d_dirty);
    }
}

int Project2::findPackage(const QByteArrayList& path) const
{
    int pos = -1;
    for( int i = 0; i < d_groups.size(); i++ )
    {
        if( d_groups[i].d_package == path )
        {
            pos = i;
            break;
        }
    }
    return pos;
}

QByteArray Project2::modulePath(const QByteArrayList& path)
{
    return path.join('$');
}

static inline QByteArray failWhen(const Import& imp)
{
    QByteArray str;
    QTextStream out(&str);
    out << "when importing " << imp.moduleName << " from ";
    if( imp.importer )
    {
        out << imp.importer->data.value<ModuleData>().fullName
            << " at " << imp.importedAt.d_row << ":" << imp.importedAt.d_col;
    }else
        out << "top level";
    out.flush();
    return str;
}

Declaration*Project2::loadModule(const Import& imp)
{
    if( imp.moduleName == "SYSTEM" )
        return AstModel::getSystem();

    // toFile also finds modules with incomplete path, such as Interface instead of som.Interface
    // we must thus look for the file first, so that we can complete the Import spec if necessary
    File* file = toFile(imp);
    if( file == 0 )
    {
        QString importer;
        if( imp.importer )
            importer = imp.importer->data.value<ModuleData>().sourcePath;
        errors << Error("cannot find source file of imported module", imp.importedAt, importer);
        modules.append(ModuleSlot(imp,QString(),0));
        return 0;
    }

    ModuleSlot* ms = find(imp);
    if( ms != 0 )
        return ms->decl;

    // immediately add it so that circular module refs lead to an error
    modules.append(ModuleSlot(imp,file->d_filePath,0));
    ms = &modules.back();

    class Lex2 : public Scanner2
    {
    public:
        QString sourcePath;
        Lexer lex;
        Token next()
        {
            return lex.nextToken();
        }
        Token peek(int offset)
        {
            return lex.peekToken(offset);
        }
        QString source() const { return sourcePath; }
    };

    Lex2 lex;
    lex.sourcePath = file->d_filePath; // to keep file name if invalid
    QBuffer buf;
    if( !file->d_cache.isEmpty() )
    {
        buf.setData(file->d_cache);
        buf.open(QIODevice::ReadOnly);
        lex.lex.setStream(&buf, file->d_filePath);
    }else
        lex.lex.setStream(file->d_filePath);
    //lex.lex.reset(d_options);
    AstModel mdl;
    Parser2 p(&mdl,&lex);
    p.RunParser();
    Declaration* module = 0;
    if( !p.errors.isEmpty() )
    {
        foreach( const Parser2::Error& e, p.errors )
            errors << Error(e.msg, e.pos, e.path);
        qDebug() << "### parser failed" << failWhen(imp).constData();
    }else
    {
        module = p.takeResult();
        Validator2 v(&mdl, this, true);
        if( !v.validate(module, imp) )
        {
            foreach( const Validator2::Error& e, v.errors )
                errors << Error(e.msg, e.pos, e.path);
            module->hasErrors = true;
        }
        file->d_mod = module; // in case of generic modules, file->d_mod points to the non-instantiated version
        ms->xref = v.takeXref();
        QHash<Declaration*,DeclList>::const_iterator i;
        for( i = ms->xref.subs.begin(); i != ms->xref.subs.end(); ++i )
            subs[i.key()] += i.value();
        ModuleData md = module->data.value<ModuleData>();
        dependencyOrder << module;
        d_byName.insert(md.fullName,qMakePair(file,module));
    }

    ms->decl = module;

    return module;
}

Project2::ModuleSlot*Project2::find(const Import& imp)
{
    for(int i = 0; i < modules.size(); i++ )
    {
        if( modules[i].imp.equals(imp) )
            return &modules[i];
    }
    return 0;
}

Project2::File* Project2::toFile(const Import& imp)
{
    FileHash::const_iterator j;
    for( j = d_files.begin(); j != d_files.end(); ++j )
    {
        if( j.value()->d_name == imp.moduleName )
            return j.value().data();
    }
    for( int i = 0; i < preloads.size(); i++ )
    {
        if( preloads[i]->d_name == imp.moduleName )
            return preloads[i].data();
    }
    return 0;
}

void Project2::clearModules()
{
    errors.clear();
    d_byName.clear();
    dependencyOrder.clear();
    Modules::const_iterator i;
    for( i = modules.begin(); i != modules.end(); ++i )
    {
        Symbol::deleteAll((*i).xref.syms);
        Declaration::deleteAll((*i).decl);
    }
    modules.clear();
    subs.clear();
    FileHash::const_iterator j;
    for( j = d_files.begin(); j != d_files.end(); ++j )
        j.value()->d_mod = 0;
}

const Project2::ModuleSlot*Project2::findModule(Declaration* m) const
{
    for( int i = 0; i < modules.size(); i++ )
    {
        if( modules[i].decl == m )
            return &modules[i];
    }
    return 0;
}

bool Project2::parse()
{
    clearModules();

    int all = 0, ok = 0;

    for( int i = 0; i < d_groups.size(); i++ )
    {
        for( int j = 0; j < d_groups[i].d_files.size(); j++ )
        {
            QFileInfo info(d_groups[i].d_files[j]->d_filePath);
            Import imp;
            imp.moduleName = Token::getSymbol(info.baseName().toUtf8());
            Declaration* module = loadModule(imp); // recursively compiles all imported files
            all++;
            if( module && !module->hasErrors )
                ok++;
        }
    }

    emit sigReparsed();
    return all == ok;
}

bool Project2::generateC(const QString &outDir, bool genMain)
{
    QDir dir(outDir);
    // TODO: check if files can be created and written
    QList<Declaration*> tops;
    foreach( const ModuleSlot& module, modules )
    {
        if( module.decl == 0 || !module.decl->validated )
            continue;
        CeeGen cg;
        QFile header( dir.absoluteFilePath(module.decl->name + ".h"));
        if( !header.open(QFile::WriteOnly) )
        {
            errors << Error("cannot open file for writing", RowCol(), header.fileName());
            continue;
        }
        QFile body( dir.absoluteFilePath(module.decl->name + ".c"));
        if( !module.decl->extern_ )
        {
            if( !body.open(QFile::WriteOnly) )
            {
                errors << Error("cannot open file for writing", RowCol(), body.fileName());
                continue;
            }
        }

        // only generate bodies if there are implementations, generate main which calls the top levels
        if( !cg.generate(module.decl, &header, module.decl->extern_ ? 0 : &body, false) )
        {
            foreach( const CeeGen::Error& e, cg.errors )
                errors << Error(e.msg,e.pos,e.path);
        }

        if( !module.decl->imported )
            tops << module.decl;
    }

    if( !genMain )
        return true;
    QFile main(dir.absoluteFilePath("main+.c"));
    main.open(QFile::WriteOnly);
    QTextStream out(&main);
    out << "// main+.c" << endl;
    out << CeeGen::genDedication() << endl << endl;

    out << "#include <stdio.h>" << endl;
    out << "#include <gc.h>" << endl;

    foreach( Declaration* module, tops )
    {
        // if a module is not in "used", it is never imported and thus a root module
        if( !module->extern_ )
            out << "#include \"" <<  module->name << ".h\"" << endl;
    }

    out << endl;

    out << "int main(int argc, char** argv) {" << endl;
    out << "    setvbuf(stdout, NULL, _IONBF, 0);" << endl;
    out << "    GC_INIT();" << endl;

    foreach( Declaration* module, tops )
    {
        // if a module is not in "used", it is never imported and thus a root module
        if( !module->extern_  )
            out << "    " <<  module->name << "$init$();" << endl;
    }
    out << "    return 0;" << endl;
    out << "}" << endl;
    return true;
}

bool Project2::save()
{
    if( d_filePath.isEmpty() )
        return false;

    QDir dir = QFileInfo(d_filePath).dir();

    QSettings out(d_filePath,QSettings::IniFormat);
    if( !out.isWritable() )
        return false;

    out.setValue("Suffixes", d_suffixes );
    out.setValue("BuiltInOakwood", d_useBuiltInOakwood );
    out.setValue("MainModule", d_main.first );
    out.setValue("MainProc", d_main.second );
    out.setValue("WorkingDir", d_workingDir );
    out.setValue("BuildDir", d_buildDir );
    out.setValue("Options", d_options.join(' ') );
    out.setValue("Arguments", d_arguments );

    const FileGroup* root = getRootFileGroup();
    if( root )
    {
        out.beginWriteArray("Modules", root->d_files.size() ); // nested arrays don't work
        for( int i = 0; i < root->d_files.size(); i++ )
        {
            const QString absPath = root->d_files[i]->d_filePath;
            const QString relPath = dir.relativeFilePath( absPath );
            out.setArrayIndex(i);
            out.setValue("AbsPath", absPath );
            out.setValue("RelPath", relPath );
        }
        out.endArray();
    }

    out.beginWriteArray("Packages", d_groups.size() );
    for( int i = 0; i < d_groups.size(); i++ )
    {
        out.setArrayIndex(i);
        out.setValue("Name", d_groups[i].d_package.join('.') ); // '/' in key gives strange effects
    }
    out.endArray();

    for( int i = 0; i < d_groups.size(); i++ )
    {
        if(d_groups[i].d_package.isEmpty())
            continue;
        out.beginWriteArray(QString::fromUtf8("." + d_groups[i].d_package.join('.')), d_groups[i].d_files.size() );
        for( int j = 0; j < d_groups[i].d_files.size(); j++ )
        {
            const QString absPath = d_groups[i].d_files[j]->d_filePath;
            const QString relPath = dir.relativeFilePath( absPath );
            out.setArrayIndex(j);
            out.setValue("AbsPath", absPath );
            out.setValue("RelPath", relPath );
        }
        out.endArray();
    }

    d_dirty = false;
    emit sigModified(d_dirty);
    return true;
}

bool Project2::loadFrom(const QString& filePath)
{
    clear();

    d_filePath = filePath;

    QDir dir = QFileInfo(d_filePath).dir();

    QSettings in(d_filePath,QSettings::IniFormat);

    d_suffixes = in.value("Suffixes").toStringList();
    d_useBuiltInOakwood = in.value("BuiltInOakwood").toBool();
    d_main.first = in.value("MainModule").toByteArray();
    d_main.second = in.value("MainProc").toByteArray();
    d_workingDir = in.value("WorkingDir").toString();
    d_buildDir = in.value("BuildDir").toString();
    d_options = in.value("Options").toByteArray().split(' ');
    d_arguments = in.value("Arguments").toStringList();

    int count = in.beginReadArray("Modules");
    for( int i = 0; i < count; i++ )
    {
        in.setArrayIndex(i);
        QString absPath = in.value("AbsPath").toString();
        const QString relPath = in.value("RelPath").toString();
        if( QFileInfo(absPath).exists() )
            addFile(absPath);
        else
        {
            absPath = dir.absoluteFilePath(relPath);
            QFileInfo info(absPath);
            if( info.exists() && info.isFile() )
                addFile(absPath);
            else
                qCritical() << "Could not open module" << relPath;
        }
    }
    in.endArray();

    QList<QByteArrayList> paths;
    count = in.beginReadArray("Packages");
    for( int i = 0; i < count; i++ )
    {
        in.setArrayIndex(i);
        QString name = in.value("Name").toString();
        paths << name.toLatin1().split('.');
        addPackagePath( paths.back() );
    }
    in.endArray();

    for( int j = 0; j < paths.size(); j++ )
    {
        count = in.beginReadArray(QString::fromUtf8("." + paths[j].join('.')));
        for( int i = 0; i < count; i++ )
        {
            in.setArrayIndex(i);
            QString absPath = in.value("AbsPath").toString();
            const QString relPath = in.value("RelPath").toString();
            if( QFileInfo(absPath).exists() )
                addFile(absPath, paths[j]);
            else
            {
                absPath = dir.absoluteFilePath(relPath);
                if( QFileInfo(absPath).exists() )
                    addFile(absPath, paths[j]);
                else
                    qCritical() << "Could not open module" << relPath;
            }
        }
        in.endArray();
    }

    d_dirty = false;
    emit sigModified(d_dirty);
    emit sigRenamed();
    return true;
}

bool Project2::saveTo(const QString& filePath)
{
    d_filePath = filePath;
    const bool res = save();
    emit sigRenamed();
    return res;
}

#ifdef QT_NO_QOBJECT
void Project2::sigModified(bool){}
void Project2::sigRenamed(){}
void Project2::sigReparsed(){}
#endif

