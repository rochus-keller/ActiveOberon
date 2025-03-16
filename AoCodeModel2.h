#ifndef LISACODEMODEL_H
#define LISACODEMODEL_H

/*
* Copyright 2025 Rochus Keller <mailto:me@rochus-keller.ch>
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

#include <QAbstractItemModel>
#include <QHash>
#include <AoFileSystem.h>
#include <QSharedData>
#include "AoAst.h"

namespace Ao
{
class ModuleFile;

class Thing
{
public:
    enum Kind { Undefined,
       /* Declaration: */ Const, TypeDecl, Var, Proc, Attribute, Param, Field,
                            Module, Self, Import,
       /* Scope: */ Body, Members,
       /* UnitFile: */ Unit,
       /* CodeFolder: */ Folder
    };
    quint8 d_kind;

    enum Visi { NA, ReadOnly, Public };
    quint8 d_visi;
    bool d_active;
    quint8 d_exclusive; // count exclusive attrs in proc body

    virtual FilePos getLoc() const { return FilePos(); }
    virtual quint16 getLen() const { return 0; }
    virtual QString getName() const;
    virtual const FileSystem::File* getFile() const { return 0; }
    bool isDeclaration() const { return d_kind >= Const && d_kind <= Import; }
    const char* typeName() const;
    Thing():d_kind(Undefined),d_visi(0),d_active(false),d_exclusive(0){}
    virtual ~Thing() {}
};

class Package;

class ModuleFile : public Thing
{
public:
    const FileSystem::File* d_file;
    Package* d_folder;

    Ast::Declaration* d_module;

    QString getName() const;
    const FileSystem::File* getFile() const { return d_file; }

    QByteArrayList findUses() const;
    ModuleFile():d_file(0), d_folder(0), d_module(0) { d_kind = Unit; }
    ~ModuleFile();
};

class Package : public Thing
{
public:
    FileSystem::Dir* d_dir;
    QList<Package*> d_subs; // owns
    QList<ModuleFile*> d_files; // owns

    QString getName() const;
    void clear();
    Package():d_dir(0){ d_kind = Folder; }
    ~Package() { clear(); }
};

struct ModelItem
{
    union {
    Thing* d_thing;
    Ast::Declaration* d_decl;
    };
    QList<ModelItem*> d_children;
    ModelItem* d_parent;
    ModelItem(ModelItem* p = 0, Thing* t = 0):d_parent(p),d_thing(t){ if( p ) p->d_children.append(this); }
    ModelItem(ModelItem* p, Ast::Declaration* t):d_parent(p),d_decl(t){ if( p ) p->d_children.append(this); }
    ~ModelItem() { foreach( ModelItem* s, d_children ) delete s; }
    static bool lessThan( const ModelItem* lhs, const ModelItem* rhs);
};

class ItemModel : public QAbstractItemModel
{
    Q_OBJECT
public:
    explicit ItemModel(QObject *parent = 0);
    const Thing* getThing(const QModelIndex& index) const;
    QModelIndex findThing(const Thing* nt) const;

    // overrides
    int columnCount ( const QModelIndex & parent = QModelIndex() ) const { return 1; }
    QVariant data ( const QModelIndex & index, int role = Qt::DisplayRole ) const;
    QModelIndex index ( int row, int column, const QModelIndex & parent = QModelIndex() ) const;
    QModelIndex parent ( const QModelIndex & index ) const;
    int rowCount ( const QModelIndex & parent = QModelIndex() ) const;
    Qt::ItemFlags flags ( const QModelIndex & index ) const { return Qt::ItemIsEnabled | Qt::ItemIsSelectable; }
protected:
    QModelIndex findThing(const ModelItem* slot,const Thing* nt) const;
    ModelItem d_root;
};

class CodeModel : public ItemModel
{
    Q_OBJECT
public:
    explicit CodeModel(QObject *parent = 0);

    bool load( const QString& rootDir );
    FileSystem* getFs() const { return d_fs; }
    quint32 getSloc() const { return d_sloc; }
    ModuleFile* getFile(const QString& path) const;
    Ranges getMutes( const QString& path );
    int getErrCount() const { return d_errCount; }

    // overrides
    QVariant data ( const QModelIndex & index, int role = Qt::DisplayRole ) const;

private:
    void fillFolders(ModelItem* root, const FileSystem::Dir* super, Package* top, QList<ModelItem*>& fileSlots);
    FileSystem* d_fs;
    Package d_top;
    QHash<const FileSystem::File*,ModuleFile*> d_map1;
    QHash<QString,ModuleFile*> d_map2; // real path -> file
    quint32 d_sloc; // number of lines of code without empty or comment lines
    QHash<QString,Ranges> d_mutes;
    int d_errCount;
};

class ModuleDetailMdl : public ItemModel
{
    Q_OBJECT
public:
    explicit ModuleDetailMdl(QObject *parent = 0);

    void load(Ast::Declaration* impl);

    // overrides
    QVariant data ( const QModelIndex & index, int role = Qt::DisplayRole ) const;
private:
    void fillItems(ModelItem* parentItem, Ast::Declaration* scope);
    void fillSubs(ModelItem* parentItem, const QList<Ast::Declaration*>& subs);
    Ast::Declaration* d_impl;
};

}

#endif // LISACODEMODEL_H
