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

#include "AoCodeModel2.h"
#include "AoLexer.h"
#include "AoParser.h"
#include "AoParser2.h"
#include <QFile>
#include <QPixmap>
#include <QtDebug>
#include <QCoreApplication>
using namespace Ao;
using namespace Ast;

CodeModel::CodeModel(QObject *parent) : ItemModel(parent),d_sloc(0),d_errCount(0)
{
    d_fs = new FileSystem(this);
}

bool CodeModel::load(const QString& rootDir)
{
    beginResetModel();
    d_root = ModelItem();
    d_top.clear();
    d_map1.clear();
    d_map2.clear();
    d_sloc = 0;
    d_errCount = 0;
    d_mutes.clear();
    d_fs->load(rootDir);
    QList<ModelItem*> fileSlots;
    fillFolders(&d_root,&d_fs->getRoot(), &d_top, fileSlots);
    foreach( ModelItem* s, fileSlots )
    {
        Q_ASSERT( s->d_thing );
        if( s->d_thing->d_kind != Thing::Unit )
            continue;
        ModuleFile* f = static_cast<ModuleFile*>(s->d_thing);
        Q_ASSERT( f->d_file );
        // TODO parseAndResolve(f);
    }
    endResetModel();
    return true;
}

ItemModel::ItemModel(QObject* parent):QAbstractItemModel(parent)
{

}

const Thing* ItemModel::getThing(const QModelIndex& index) const
{
    if( !index.isValid() )
        return 0;
    ModelItem* s = static_cast<ModelItem*>( index.internalPointer() );
    Q_ASSERT( s != 0 );
    return s->d_thing;
}

QModelIndex ItemModel::findThing(const Thing* nt) const
{
    return findThing( &d_root, nt );
}

QVariant ItemModel::data(const QModelIndex& index, int role) const
{
    return QVariant();
}

ModuleFile*CodeModel::getFile(const QString& path) const
{
    return d_map2.value(path);
}

Ranges CodeModel::getMutes(const QString& path)
{
    return d_mutes.value(path);
}

QVariant CodeModel::data(const QModelIndex& index, int role) const
{
    ModelItem* s = static_cast<ModelItem*>( index.internalPointer() );
    Q_ASSERT( s != 0 );
    switch( role )
    {
    case Qt::DisplayRole:
        switch( s->d_thing->d_kind )
        {
        case Thing::Unit:
            return static_cast<ModuleFile*>(s->d_thing)->d_file->d_name;
        case Thing::Folder:
            {
                Package* cf = static_cast<Package*>(s->d_thing);
                if( cf->d_dir->d_name.isEmpty() && ( s->d_parent == 0 || s->d_parent->d_parent == 0 ) )
                    return "<root>";
                return cf->d_dir->d_name;
            }
        }
        break;
    case Qt::DecorationRole:
        switch( s->d_thing->d_kind )
        {
        case Thing::Unit:
            return QPixmap(":/images/module.png");
        case Thing::Folder:
            return QPixmap(":/images/folder.png");
        }
        break;
    case Qt::ToolTipRole:
        switch( s->d_thing->d_kind )
        {
        case Thing::Unit:
            {
                ModuleFile* cf = static_cast<ModuleFile*>(s->d_thing);
                return QString("<html><b>%1 %2</b><br>"
                               "<p>Logical path: %3</p>"
                               "<p>Real path: %4</p></html>")
                        .arg("Module")
                        .arg(cf->d_file->d_moduleName.constData())
                        .arg(cf->d_file->getVirtualPath())
                        .arg(cf->d_file->d_realPath);
            }
        }
        break;
    case Qt::FontRole:
        break;
    case Qt::ForegroundRole:
        break;
    }
    return QVariant();
}

QModelIndex ItemModel::index(int row, int column, const QModelIndex& parent) const
{
    const ModelItem* s = &d_root;
    if( parent.isValid() )
    {
        s = static_cast<ModelItem*>( parent.internalPointer() );
        Q_ASSERT( s != 0 );
    }
    if( row < s->d_children.size() && column < columnCount( parent ) )
        return createIndex( row, column, s->d_children[row] );
    else
        return QModelIndex();
}

QModelIndex ItemModel::parent(const QModelIndex& index) const
{
    if( index.isValid() )
    {
        ModelItem* s = static_cast<ModelItem*>( index.internalPointer() );
        Q_ASSERT( s != 0 );
        if( s->d_parent == &d_root )
            return QModelIndex();
        // else
        Q_ASSERT( s->d_parent != 0 );
        Q_ASSERT( s->d_parent->d_parent != 0 );
        return createIndex( s->d_parent->d_parent->d_children.indexOf( s->d_parent ), 0, s->d_parent );
    }else
        return QModelIndex();
}

int ItemModel::rowCount(const QModelIndex& parent) const
{
    if( parent.isValid() )
    {
        ModelItem* s = static_cast<ModelItem*>( parent.internalPointer() );
        Q_ASSERT( s != 0 );
        return s->d_children.size();
    }else
        return d_root.d_children.size();
}

QModelIndex ItemModel::findThing(const ModelItem* slot, const Thing* nt) const
{
    for( int i = 0; i < slot->d_children.size(); i++ )
    {
        ModelItem* s = slot->d_children[i];
        if( s->d_thing == nt )
            return createIndex( i, 0, s );
        QModelIndex index = findThing( s, nt );
        if( index.isValid() )
            return index;
    }
    return QModelIndex();
}

bool ModelItem::lessThan(const ModelItem* lhs, const ModelItem* rhs)
{
    if( lhs->d_thing == 0 || rhs->d_thing == 0 )
        return false;
    return lhs->d_thing->getName().compare(rhs->d_thing->getName(),Qt::CaseInsensitive) < 0;
}

void CodeModel::fillFolders(ModelItem* root, const FileSystem::Dir* super, Package* top, QList<ModelItem*>& fileSlots)
{
    for( int i = 0; i < super->d_subdirs.size(); i++ )
    {
        Package* f = new Package();
        f->d_dir = super->d_subdirs[i];
        top->d_subs.append(f);
        ModelItem* s = new ModelItem(root,f);
        fillFolders(s,super->d_subdirs[i],f,fileSlots);
    }
    for( int i = 0; i < super->d_files.size(); i++ )
    {
        const int t = super->d_files[i]->d_type;
        if( t == FileSystem::OberonModule )
        {
            ModuleFile* f = new ModuleFile();
            f->d_file = super->d_files[i];
            f->d_folder = top;
            d_map1[f->d_file] = f;
            d_map2[f->d_file->d_realPath] = f;
            top->d_files.append(f);
            ModelItem* s = new ModelItem(root,f);
            fileSlots.append(s);
        }
    }
    std::sort( root->d_children.begin(), root->d_children.end(), ModelItem::lessThan );
}

QString ModuleFile::getName() const
{
    return d_file->d_name;
}

QByteArrayList ModuleFile::findUses() const
{
    QByteArrayList res;
    if( d_file == 0 || d_file->d_type != FileSystem::OberonModule )
        return res;
    QFile f(d_file->d_realPath);
    f.open(QIODevice::ReadOnly);
    Lexer lex;
    lex.setStream(&f,f.fileName());
    Token t = lex.nextToken();
    while( t.isValid() )
    {
        switch( t.d_type )
        {
        case Tok_IMPORT:
            t = lex.nextToken();
            // TODO
            while( t.isValid() && t.d_type != Tok_Semi )
            {
                if( t.d_type == Tok_Comma )
                {
                    t = lex.nextToken();
                    continue;
                }
                if( t.d_type == Tok_ident )
                {
                    const QByteArray id = t.d_val;
                    t = lex.nextToken();
                    if( t.d_type == Tok_ColonEq )
                    {
                        t = lex.nextToken();
                        if( t.d_type == Tok_ident ) // just to make sure
                        {
                            res.append( t.d_val );
                            t = lex.nextToken();
                        }
                    }else
                        res.append(id);
                }else
                    t = lex.nextToken();
            }
            return res;
        }
        t = lex.nextToken();
    }
    return res;
}

ModuleFile::~ModuleFile()
{
}

QString Package::getName() const
{
    return d_dir->d_name;
}

void Package::clear()
{
    for( int i = 0; i < d_subs.size(); i++ )
    {
        d_subs[i]->clear();
        delete d_subs[i];
    }
    d_subs.clear();
    for( int i = 0; i < d_files.size(); i++ )
        delete d_files[i];
    d_files.clear();
}

QString Thing::getName() const
{
    return QString();
}

const char*Thing::typeName() const
{
    switch( d_kind )
    {
    case Const:
        return "Const";
    case TypeDecl:
        return "Type";
    case Var:
        return "Var";
    case Proc:
        return "Procedure";
    case Module:
        return "Module";
    case Param:
        return "Parameter";
    case Field:
        return "Field";
    case Attribute:
        return "Attribute";
    case Import:
        return "Import";
    case Self:
        return "SELF";
    default:
        return "";
    }
}

ModuleDetailMdl::ModuleDetailMdl(QObject* parent)
{

}

void ModuleDetailMdl::load(Declaration* impl)
{
    if( d_impl == impl )
        return;

    beginResetModel();
    d_root = ModelItem();
    d_impl = impl;

    if( impl )
        fillItems(&d_root, impl);

    endResetModel();
}

QVariant ModuleDetailMdl::data(const QModelIndex& index, int role) const
{
    ModelItem* s = static_cast<ModelItem*>( index.internalPointer() );
    Q_ASSERT( s != 0 );
    switch( role )
    {
    case Qt::DisplayRole:
        {
            QString str;
            if( s->d_thing->d_active || s->d_thing->d_exclusive )
                str += "! ";
            str += s->d_thing->getName();
            return str;
        }
        break;
    case Qt::DecorationRole:
        switch( s->d_decl->kind )
        {
        case Declaration::TypeDecl:
            {
                Declaration* d = s->d_decl;
                if( d->type )
                {
                    switch( d->type->kind )
                    {
                    case Type::Pointer:
                        switch( s->d_decl->visi )
                        {
                        case Declaration::Private:
                        case Declaration::NA:
                            return QPixmap(":/images/pointer_priv.png");
                        case Declaration::ReadWrite:
                            return QPixmap(":/images/pointer.png");
                        }
                        break;
                    case Type::Array:
                        switch( s->d_thing->d_visi )
                        {
                        case Thing::NA:
                            return QPixmap(":/images/array_priv.png");
                        case Thing::Public:
                            return QPixmap(":/images/array.png");
                        }
                        break;
                    case Type::Record:
                        switch( s->d_thing->d_visi )
                        {
                        case Thing::NA:
                            return QPixmap(":/images/struct_priv.png");
                        case Thing::Public:
                            return QPixmap(":/images/struct.png");
                        }
                        break;
                    case Type::Object:
                        switch( s->d_thing->d_visi )
                        {
                        case Thing::NA:
                            return QPixmap(":/images/class_priv.png");
                        case Thing::Public:
                            return QPixmap(":/images/class.png");
                        }
                        break;
                    case Type::Procedure:
                        switch( s->d_thing->d_visi )
                        {
                        case Thing::NA:
                            return QPixmap(":/images/alias_priv.png");
                        case Thing::Public:
                            return QPixmap(":/images/alias.png");
                        }
                        break;
                    }
                }
            }
            break;
        case Declaration::VarDecl:
        case Declaration::Field:
            switch( s->d_thing->d_visi )
            {
            case Thing::NA:
                return QPixmap(":/images/var_priv.png");
            case Thing::ReadOnly:
                return QPixmap(":/images/var_prot.png");
            case Thing::Public:
                return QPixmap(":/images/var.png");
            }
            break;
        case Declaration::Procedure:
            switch( s->d_thing->d_visi )
            {
            case Thing::NA:
                return QPixmap(":/images/func_priv.png");
            case Thing::Public:
                return QPixmap(":/images/func.png");
            }
            break;
        case Declaration::Import:
            return QPixmap(":/images/module.png");
        }
        break;
    }
    return QVariant();
}

static bool DeclLessThan(Declaration* lhs, Declaration* rhs)
{
    return lhs->name < rhs->name;
}

void ModuleDetailMdl::fillItems(ModelItem* parentItem, Declaration* scope)
{
    QList<Declaration*> decls;
    Declaration* d = scope->link;
    while(d)
    {
        decls.append(d);
        d = d->getNext();
    }
    std::sort( decls.begin(), decls.end(), DeclLessThan );
    for( int j = 0; j < decls.size(); j++ )
    {
        Declaration* dd = decls[j];
        ModelItem* item = new ModelItem(parentItem,dd);
        if( dd->kind == Declaration::TypeDecl && dd->type && dd->type->kind == Type::Object )
            fillSubs(item,dd->type->subs);
    }
}

void ModuleDetailMdl::fillSubs(ModelItem* parentItem, const QList<Declaration*>& subs)
{
    QList<Declaration*> funcs;
    foreach( Declaration* d, subs )
    {
        if( d->kind == Declaration::Procedure )
            funcs.append(d);
    }
    std::sort( funcs.begin(), funcs.end(), DeclLessThan );
    for( int j = 0; j < funcs.size(); j++ )
        new ModelItem(parentItem,funcs[j]);
}

