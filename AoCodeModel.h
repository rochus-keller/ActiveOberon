#ifndef LISACODEMODEL_H
#define LISACODEMODEL_H

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

#include <QAbstractItemModel>
#include <QHash>
#include <AoFileSystem.h>
#include <QSharedData>
#include "AoRowCol.h"

namespace Ao
{
class Scope;
class UnitFile;
class Symbol;

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
    virtual ~Thing();
};

class Type : public QSharedData
{
public:
    typedef QExplicitlySharedDataPointer<Type> Ref;
    enum Kind { Undefined, Pointer, Array, Record, Class, Procedural
              };
    Type::Ref d_type;
    Scope* d_members; // owns
    quint8 d_kind;

    Type():d_type(0), d_members(0),d_kind(Undefined) {}
    ~Type();
};

class Declaration : public Thing
{
public:
    Scope* d_body; // owns, unless Import; used for Proc, Module and Import
    Type::Ref d_type;
    QByteArray d_name;

    FilePos d_loc; // place in unit or any include file where the decl is actually located
    Scope* d_owner;

    typedef QList<Symbol*> SymList;
    typedef QHash<QString,SymList> Refs;
    Refs d_refs; // file path -> Symbols in it, not owned
    Symbol* d_me; // this is the symbol by which the decl itself is represented in the file

    FilePos getLoc() const { return d_loc; }
    quint16 getLen() const { return d_name.size(); }
    QString getName() const;

    UnitFile* getUnitFile() const; // only for ownership, not for actual file position
    Declaration():d_body(0),d_owner(0),d_me(0),d_type(0){}
    ~Declaration();
};

class Scope : public Thing
{
public:
    QList<Declaration*> d_order; // owns
    Thing* d_owner; // either declaration or unit file or class or 0
    Scope* d_outer; // surrounding scope for blocks, inherited scope for record/object
    Scope* d_altOuter; // local scope of the record/object declaration
    Type* d_type; // for object member scopes pointing to the object

    UnitFile* getUnitFile() const;
    Declaration* findDecl(const char* id, bool useAlt = false) const;
    void clear();
    Scope():d_owner(0),d_outer(0),d_altOuter(0),d_type(0){}
    ~Scope();
};

class Symbol
{
public:
    Declaration* d_decl;
    RowCol d_loc; // the position of the symbol in the file (declaration has other position, but same length)
    Symbol():d_decl(0){}
};

class UnitFile;
class CodeFolder;

class CodeFile : public Thing
{
public:
    const FileSystem::File* d_file;
    CodeFolder* d_folder;

    UnitFile* toUnit();

    QString getName() const;
    const FileSystem::File* getFile() const { return d_file; }
    CodeFile():d_file(0),d_folder(0) {}
    ~CodeFile();
};

class UnitFile : public CodeFile
{
public:
    Scope* d_body; // owns (technical scope with only one module declaration member)
    Declaration* d_module; // points to the corresponding declaration in d_body
    QList<UnitFile*> d_import;
    typedef QList<Symbol*> SymList;
    QHash<QString,SymList> d_syms; // owns, all things we can click on in a code file ordered by row/col

    QByteArrayList findUses() const;
    UnitFile():d_body(0),d_module(0) { d_kind = Unit; }
    ~UnitFile();
};

class CodeFolder : public Thing
{
public:
    FileSystem::Dir* d_dir;
    QList<CodeFolder*> d_subs; // owns
    QList<CodeFile*> d_files; // owns

    QString getName() const;
    void clear();
    CodeFolder():d_dir(0){ d_kind = Folder; }
    ~CodeFolder() { clear(); }
};

struct ModelItem
{
    Thing* d_thing;
    QList<ModelItem*> d_children;
    ModelItem* d_parent;
    ModelItem(ModelItem* p = 0, Thing* t = 0):d_parent(p),d_thing(t){ if( p ) p->d_children.append(this); }
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
    Symbol* findSymbolBySourcePos(const QString& path, int line, int col) const;
    FileSystem* getFs() const { return d_fs; }
    quint32 getSloc() const { return d_sloc; }
    CodeFile* getCodeFile(const QString& path) const;
    UnitFile* getUnitFile(const QString& path) const;
    Ranges getMutes( const QString& path );
    int getErrCount() const { return d_errCount; }
    Declaration* getSystem() { return &d_system; }
    Scope* getAttrs() { return &d_attrs; }
    Scope* getPredecls() { return &d_predecls; }

    // overrides
    QVariant data ( const QModelIndex & index, int role = Qt::DisplayRole ) const;
protected:
    void parseAndResolve(UnitFile*);

private:
    void fillFolders(ModelItem* root, const FileSystem::Dir* super, CodeFolder* top, QList<ModelItem*>& fileSlots);
    FileSystem* d_fs;
    CodeFolder d_top;
    Declaration d_system;
    Scope d_attrs, d_predecls;
    QHash<const FileSystem::File*,UnitFile*> d_map1;
    QHash<QString,CodeFile*> d_map2; // real path -> file
    quint32 d_sloc; // number of lines of code without empty or comment lines
    QHash<QString,Ranges> d_mutes;
    int d_errCount;
};

class ModuleDetailMdl : public ItemModel
{
    Q_OBJECT
public:
    explicit ModuleDetailMdl(QObject *parent = 0);

    void load(Scope* impl);

    // overrides
    QVariant data ( const QModelIndex & index, int role = Qt::DisplayRole ) const;
private:
    void fillItems(ModelItem* parentItem, Scope* scope);
    void fillSubs(ModelItem* parentItem, Scope* scope);
    Scope* d_impl;
};

}

#endif // LISACODEMODEL_H
