#ifndef OBXPROJECT_H
#define OBXPROJECT_H

/*
* Copyright 2025 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the ActiveOberon parser/compiler library.
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

// Adopted from the Oberon+ IDE

#include <QObject>
#include <QStringList>
#include <QExplicitlySharedDataPointer>
#include "AoValidator.h"

class QDir;
class QBuffer;

namespace Ao
{
    class Project : public QObject, public Ast::Importer
    {
#ifndef QT_NO_QOBJECT
        Q_OBJECT
#endif
    public:
        struct Error {
            QString msg;
            RowCol pos;
            QString path;
            Error( const QString& m, const RowCol& pos, const QString& p):msg(m),pos(pos),path(p){}
        };

        struct File : public QSharedData
        {
            QString d_filePath;
            QByteArray d_moduleName;
            QByteArray d_cache;
            Ast::Declaration* d_mod;
            bool d_isLib;
            File():d_isLib(false),d_mod(0){}
        };
        typedef QExplicitlySharedDataPointer<File> FileRef;

        typedef QHash<QString,FileRef> FileHash; // FilePath -> File
        typedef QList<FileRef> FileList;
        typedef QPair<QByteArray,QByteArray> ModProc; // module.procedure or just module
        typedef QHash<QByteArray,QPair<File*,Ast::Declaration*> > FileByModuleName; // Module.fullName -> File, intance

        explicit Project(QObject *parent = 0);
        ~Project();

        void clear();

        void createNew();
        bool loadFrom( const QString& filePath );
        bool save();
        bool saveTo(const QString& filePath );
        void setSuffixes( const QStringList& ); // Form: ".suffix"
        const QStringList& getSuffixes() const { return d_suffixes; }
        const QString& getProjectPath() const { return d_filePath; }
        bool isDirty() const { return d_dirty; }

        void setMain( const ModProc& );
        const ModProc& getMain() const { return d_main; }
        QString renderMain() const;
        void setUseBuiltInOakwood(bool);
        bool useBuiltInOakwood() const { return d_useBuiltInOakwood; }
        QString getWorkingDir(bool resolved = false) const;
        void setWorkingDir( const QString& );
        QString getBuildDir(bool resolved = false) const;
        void setBuildDir( const QString& );
        QByteArrayList getOptions() const { return d_options; }
        void setOptions( const QByteArrayList& );
        QStringList getArguments() const { return d_arguments; }
        void setArguments( const QStringList& );

        bool addFile(const QString& filePath, const QByteArray& moduleName);
        bool removeFile( const QString& filePath );
        bool initializeFromDir(const QDir& dir, bool recursive = true);

        bool parse();
        bool generateC(const QString& outDir);

        const FileHash& getFiles() const { return d_files; }
        File* findFile( const QString& file ) const;
        Ast::Declaration* findModule( const QByteArray& fullName ) const;
        Ast::Declaration* findModuleByPath( const QString& path ) const;
        void addPreload( const QByteArray& name, const QByteArray& code );

        Ast::Symbol* findSymbolBySourcePos(const QString& file, quint32 line, quint16 col, Ast::Declaration** = 0 ) const;
        Ast::Symbol* findSymbolByModule(Ast::Declaration*, quint32 line, quint16 col, Ast::Declaration** scopePtr = 0) const;
        Ast::Symbol* findSymbolByModuleName(const QByteArray& fullName, quint32 line, quint16 col, Ast::Declaration** scopePtr = 0) const;
        typedef QList<QPair<Ast::Declaration*, Ast::SymList> > UsageByMod;
        UsageByMod getUsage( Ast::Declaration* ) const;
        Ast::Symbol* getSymbolsOfModule(Ast::Declaration*) const;
        Ast::DeclList getSubs(Ast::Declaration*) const;
        Ast::DeclList getDependencyOrder() const { return dependencyOrder; }

        bool printTreeShaken( const QString& module, const QString& fileName );
        bool printImportDependencies(const QString& fileName , bool pruned);

        const QList<Error>& getErrors() const {return errors; }
        void addError(const QString& file, const RowCol& pos, const QString& msg);
        quint32 getSloc()const { return d_sloc; }

    signals:
        void sigModified(bool);
        void sigRenamed();
        void sigReparsed();
    protected:
        QStringList findFiles(const QDir& , bool recursive = false);
        void touch();

        Ast::Declaration* loadModule( const Ast::Import& imp );

        struct ModuleSlot
        {
            Ast::Import imp;
            QString file;
            Ast::Declaration* decl;
            Ast::Xref xref;
            ModuleSlot():decl(0) {}
            ModuleSlot( const Ast::Import& i, const QString& f, Ast::Declaration* d):imp(i),file(f),decl(d){}
        };
        ModuleSlot* find(const Ast::Import& imp);
        File* toFile(const Ast::Import& imp);
        void clearModules();
        const ModuleSlot* findModule(Ast::Declaration*) const;
    private:
        typedef QList<ModuleSlot> Modules;
        Modules modules;
        Ast::DeclList dependencyOrder;
        QHash<Ast::Declaration*, Ast::DeclList> subs;
        FileList preloads;
        QList<Error> errors;
        FileHash d_files;
        FileByModuleName d_byName;
        QString d_filePath; // path where the project file was loaded from or saved to
        QStringList d_suffixes;
        QByteArrayList d_options;
        QStringList d_arguments;
        QString d_workingDir, d_buildDir;
        ModProc d_main;
        quint32 d_sloc;
        QHash<int,int> uses;
        bool d_dirty;
        bool d_useBuiltInOakwood;
    };
}

Q_DECLARE_METATYPE(Ao::Project::File*)

#endif // OBXPROJECT_H
