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

#include "AoCodeNavigator2.h"
#include "AoHighlighter2.h"
#include "AoCodeModel2.h"
#include "AoProject.h"
#include <QApplication>
#include <QFileInfo>
#include <QtDebug>
#include <QDir>
#include <QVBoxLayout>
#include <QLabel>
#include <QPlainTextEdit>
#include <QSettings>
#include <QDockWidget>
#include <QShortcut>
#include <QTreeView>
#include <QTreeWidget>
#include <QInputDialog>
#include <QFileDialog>
#include <QTimer>
#include <QElapsedTimer>
#include <QScrollBar>
using namespace Ao;
using namespace Ast;

Q_DECLARE_METATYPE(FilePos)

static CodeNavigator* s_this = 0;
static void report(QtMsgType type, const QString& message )
{
    if( s_this )
    {
        switch(type)
        {
        case QtDebugMsg:
            s_this->logMessage(QLatin1String("INF: ") + message);
            break;
        case QtWarningMsg:
            s_this->logMessage(QLatin1String("WRN: ") + message);
            break;
        case QtCriticalMsg:
        case QtFatalMsg:
            s_this->logMessage(QLatin1String("ERR: ") + message);
            break;
        }
    }
}

static QtMessageHandler s_oldHandler = 0;
void messageHander(QtMsgType type, const QMessageLogContext& ctx, const QString& message)
{
    if( s_oldHandler )
        s_oldHandler(type, ctx, message );
    report(type,message);
}

class CodeNavigator::Viewer : public QPlainTextEdit
{
public:
    QString d_path;
    typedef QList<QTextEdit::ExtraSelection> ESL;
    ESL d_link, d_nonTerms, d_mutes, d_missing;
    CodeNavigator* d_that;
    Symbol* d_goto;
    OberonPainter2* d_hl1;
    QString d_find;

    Viewer(CodeNavigator* p):QPlainTextEdit(p),d_that(p),d_goto(0)
    {
        setReadOnly(true);
        setLineWrapMode( QPlainTextEdit::NoWrap );
        setTabStopWidth( 30 );
        setTabChangesFocus(true);
        setMouseTracking(true);
        d_hl1 = new OberonPainter2( this );
        d_hl1->addBuiltIns();
        d_hl1->setDocument(document());


#if defined(Q_OS_WIN32)
        QFont monospace("Consolas");
#elif defined(Q_OS_MAC)
        QFont monospace("SF Mono");
#else
        QFont monospace("Monospace");
#endif
        if( !monospace.exactMatch() )
            monospace = QFont("DejaVu Sans Mono");
        setFont(monospace);
    }

    bool loadFile( const QString& path )
    {
        if( d_path == path )
            return true;
        d_path = path;

        QFile in(d_path);
        if( !in.open(QIODevice::ReadOnly) )
            return false;
        QByteArray buf = in.readAll();
        buf.chop(1);
        setPlainText( QString::fromUtf8(buf) );
        markMissing();
        that()->syncModuleList();
        return true;
    }

    CodeNavigator* that() { return d_that; }

    void mouseMoveEvent(QMouseEvent* e)
    {
        QPlainTextEdit::mouseMoveEvent(e);
        if( QApplication::keyboardModifiers() == Qt::ControlModifier )
        {
            QTextCursor cur = cursorForPosition(e->pos());
            Symbol* e = that()->d_pro->findSymbolBySourcePos(d_path,cur.blockNumber() + 1,
                                                                          cur.positionInBlock() + 1);
            const bool alreadyArrow = !d_link.isEmpty();
            d_link.clear();
            if( e && e->decl )
            {
                Declaration* sym = e->decl;
                Q_ASSERT(sym);
                const int off = cur.positionInBlock() + 1 - e->pos.d_col;
                cur.setPosition(cur.position() - off);
                cur.setPosition( cur.position() + sym->name.size(), QTextCursor::KeepAnchor );

                QTextEdit::ExtraSelection sel;
                sel.cursor = cur;
                sel.format.setFontUnderline(true);
                d_link << sel;
                d_goto = e;
                if( !alreadyArrow )
                    QApplication::setOverrideCursor(Qt::ArrowCursor);
            }
            if( alreadyArrow && d_link.isEmpty() )
                QApplication::restoreOverrideCursor();
            updateExtraSelections();
        }else if( !d_link.isEmpty() )
        {
            QApplication::restoreOverrideCursor();
            d_link.clear();
            updateExtraSelections();
        }
    }

    void mousePressEvent(QMouseEvent* e)
    {
        QPlainTextEdit::mousePressEvent(e);
        QTextCursor cur = cursorForPosition(e->pos());
        d_that->pushLocation( CodeNavigator::Place(
                                  FilePos(RowCol(cur.blockNumber()+1, cur.positionInBlock()+1),d_path),
                                                    verticalScrollBar()->value() ) );
        if( !d_link.isEmpty() )
        {
            QApplication::restoreOverrideCursor();
            d_link.clear();
            Q_ASSERT( d_goto );
            setPosition( d_goto, false, true );
        }else if( QApplication::keyboardModifiers() == Qt::ControlModifier )
        {
            QTextCursor cur = cursorForPosition(e->pos());
            Symbol* e = that()->d_pro->findSymbolBySourcePos(
                        d_path,cur.blockNumber() + 1,cur.positionInBlock() + 1);
            if( e )
            {
                Declaration* decl = e->decl;
                if( decl->kind == Declaration::Import && e->pos == decl->pos )
                {
                    Import imp = decl->data.value<Import>();
                    decl = imp.resolved;
                }else if( decl->kind == Declaration::Procedure )
                {
                    if( decl->receiver && decl->super )
                        decl = decl->super;
                }
                QString path = decl->getSourcePath();
                if( path.isEmpty() )
                    path = d_path;
                setPosition( FilePos(e->pos, path), false, true );
            }
            updateExtraSelections();
        }else
            updateExtraSelections();
    }

    void updateExtraSelections()
    {
        ESL sum;

        sum << d_mutes;
        sum << d_missing;

        QTextEdit::ExtraSelection line;
        line.format.setBackground(QColor(Qt::yellow).lighter(150));
        line.format.setProperty(QTextFormat::FullWidthSelection, true);
        line.cursor = textCursor();
        line.cursor.clearSelection();
        sum << line;

        sum << d_nonTerms;
        sum << d_link;

        setExtraSelections(sum);
    }

    void setPosition(Symbol* sym, bool center, bool pushPosition )
    {
        if( sym == 0 || sym->decl == 0 )
            return;
        FilePos pos(sym->decl->pos, sym->decl->getSourcePath());
        if( sym->pos == pos.d_pos )
        {
            // position on itself
            if( sym->decl->kind == Declaration::Import )
            {
                // make that a ctrl-click on an import decl jumps to the module name
                Import import = sym->decl->data.value<Import>();
                Declaration* module = that()->d_pro->findModule(import.moduleName);
                if( module )
                    pos = FilePos(module->pos, module->getSourcePath());
            }
        }
        setPosition( pos, center, pushPosition );
    }
    void setPosition(const FilePos& pos, bool center, bool pushPosition )
    {
        if( pos.d_filePath.isEmpty() )
            return;
        const int line = pos.d_pos.d_row - 1;
        const int col = pos.d_pos.d_col - 1;
        loadFile( pos.d_filePath );
        // Qt-Koordinaten
        if( line >= 0 && line < document()->blockCount() )
        {
            QTextBlock block = document()->findBlockByNumber(line);
            QTextCursor cur = textCursor();
            cur.setPosition( block.position() + col );
            setTextCursor( cur );
            if( center )
                centerCursor();
            else
                ensureCursorVisible();
            updateExtraSelections();
            if( pushPosition )
                that()->pushLocation(Place(pos,verticalScrollBar()->value()));
            setFocus();
        }
    }

    void setCursorPosition(int line, int col, bool center, int sel )
    {
        // Qt-Koordinaten
        if( line >= 0 && line < document()->blockCount() )
        {
            QTextBlock block = document()->findBlockByNumber(line);
            QTextCursor cur = textCursor();
            cur.setPosition( block.position() + col );
            if( sel > 0 )
                cur.setPosition( block.position() + col + sel, QTextCursor::KeepAnchor );
            setTextCursor( cur );
            if( center )
                centerCursor();
            else
                ensureCursorVisible();
            updateExtraSelections();
        }
    }

    void markMissing()
    {
        d_missing.clear();
        QTextCharFormat missing;
#if 0
        missing.setBackground( QColor(Qt::red).lighter(170));
#else
        missing.setUnderlineColor(QColor(Qt::red));
        missing.setFontUnderline(true);
        missing.setUnderlineStyle(QTextCharFormat::WaveUnderline);
#endif
        ModuleFile* uf = 0; // TODO that()->d_mdl->getFile(d_path);
        if( uf == 0 )
            return;
        Symbol* syms = that()->d_pro->getSymbolsOfModule(that()->d_pro->findModuleByPath(d_path));
        Symbol* s = syms;
        while( s )
        {
            if( s->decl != 0 )
                continue; // we're not interested in Symbols pointing to a Declaration here

            RowCol loc = s->pos;
            QTextCursor c( document()->findBlockByNumber( loc.d_row - 1) );
            c.setPosition( c.position() + loc.d_col - 1 );
            c.movePosition(QTextCursor::EndOfWord,QTextCursor::KeepAnchor);
            QTextEdit::ExtraSelection sel;
            sel.format = missing;
            sel.cursor = c;
            d_missing << sel;

            s = s->next;
            if( s == syms )
                break;
        }
        updateExtraSelections();
    }

    void markNonTerms(const QList<Symbol*>& list)
    {
        d_nonTerms.clear();
        QTextCharFormat format;
        format.setBackground( QColor(247,245,243).darker(120) );
        foreach( const Symbol* sym, list )
        {
            if( sym->decl == 0 )
                continue;
            QTextCursor c( document()->findBlockByNumber( sym->pos.d_row - 1) );
            c.setPosition( c.position() + sym->pos.d_col - 1 );
            c.setPosition( c.position() + sym->decl->name.size(), QTextCursor::KeepAnchor );

            QTextEdit::ExtraSelection sel;
            sel.format = format;
            sel.cursor = c;

            d_nonTerms << sel;
        }
        updateExtraSelections();
    }

    void find( bool fromTop )
    {
        QTextCursor cur = textCursor();
        int line = cur.block().blockNumber();
        int col = cur.positionInBlock();

        if( fromTop )
        {
            line = 0;
            col = 0;
        }else
            col++;
        const int count = document()->blockCount();
        int pos = -1;
        const int start = qMax(line,0);
        bool turnedAround = false;
        for( int i = start; i < count; i++ )
        {
            pos = document()->findBlockByNumber(i).text().indexOf( d_find, col, Qt::CaseInsensitive );
            if( pos != -1 )
            {
                line = i;
                col = pos;
                break;
            }else if( i < count )
                col = 0;
            if( pos == -1 && start != 0 && !turnedAround && i == count - 1 )
            {
                turnedAround = true;
                i = -1;
            }
        }
        if( pos != -1 )
        {
            setCursorPosition( line, col, true, d_find.size() );
        }
    }
};

CodeNavigator::CodeNavigator(QWidget *parent) : QMainWindow(parent),d_pushBackLock(false)
{
    QWidget* pane = new QWidget(this);
    QVBoxLayout* vbox = new QVBoxLayout(pane);
    vbox->setMargin(0);
    vbox->setSpacing(0);

    d_pathTitle = new QLabel(this);
    d_pathTitle->setMargin(2);
    d_pathTitle->setWordWrap(true);
    d_pathTitle->setTextInteractionFlags(Qt::TextSelectableByMouse);
    vbox->addWidget(d_pathTitle);

    d_view = new Viewer(this);
    vbox->addWidget(d_view);

    setCentralWidget(pane);

    setDockNestingEnabled(true);
    setCorner( Qt::BottomRightCorner, Qt::RightDockWidgetArea );
    setCorner( Qt::BottomLeftCorner, Qt::LeftDockWidgetArea );
    setCorner( Qt::TopRightCorner, Qt::RightDockWidgetArea );
    setCorner( Qt::TopLeftCorner, Qt::LeftDockWidgetArea );

    d_pro = new Project(this);
    d_fs = new FileSystem(this);

    createModuleList();
    createDetails();
    createUsedBy();
    createLog();

    connect( d_view, SIGNAL( cursorPositionChanged() ), this, SLOT(  onCursorPositionChanged() ) );

    QSettings s;
    const QVariant state = s.value( "DockState" );
    if( !state.isNull() )
        restoreState( state.toByteArray() );

    new QShortcut(tr("ALT+LEFT"),this,SLOT(onGoBack()) );
    new QShortcut(tr("ALT+RIGHT"),this,SLOT(onGoForward()) );
    new QShortcut(tr("CTRL+Q"),this,SLOT(close()) );
    new QShortcut(tr("CTRL+L"),this,SLOT(onGotoLine()) );
    new QShortcut(tr("CTRL+F"),this,SLOT(onFindInFile()) );
    new QShortcut(tr("CTRL+G"),this,SLOT(onFindAgain()) );
    new QShortcut(tr("F3"),this,SLOT(onFindAgain()) );
    new QShortcut(tr("F2"),this,SLOT(onGotoDefinition()) );
    new QShortcut(tr("CTRL+O"),this,SLOT(onOpen()) );
    new QShortcut(Qt::CTRL + Qt::Key_Plus,this,SLOT(onIncreaseSize()) );
    new QShortcut(Qt::CTRL + Qt::Key_Minus,this,SLOT(onDecreaseSize()) );

    s_this = this;
    s_oldHandler = qInstallMessageHandler(messageHander);

    setWindowTitle( tr("%1 v%2").arg( qApp->applicationName() ).arg( qApp->applicationVersion() ) );

    logMessage(tr("Welcome to %1 %2\nAuthor: %3\nSite: %4\nLicense: GPL\n").arg( qApp->applicationName() )
               .arg( qApp->applicationVersion() ).arg( qApp->organizationName() ).arg( qApp->organizationDomain() ));
    logMessage(tr("Shortcuts:"));
    logMessage(tr("CTRL+O to open the directory containing the Lisa Pascal files") );
    logMessage(tr("Double-click on the elements in the Modules or Uses lists to show in source code") );
    logMessage(tr("CTRL-click or F2 on the idents in the source to navigate to declarations") );
    logMessage(tr("CTRL+L to go to a specific line in the source code file") );
    logMessage(tr("CTRL+F to find a string in the current file") );
    logMessage(tr("CTRL+G or F3 to find another match in the current file") );
    logMessage(tr("ALT+LEFT to move backwards in the navigation history") );
    logMessage(tr("ALT+RIGHT to move forward in the navigation history") );
    logMessage(tr("CTRL+PLUS increase browser font size") );
    logMessage(tr("CTRL+MINUS decrease browser font size") );
    logMessage(tr("ESC to close Message Log") );
}

CodeNavigator::~CodeNavigator()
{
    s_this = 0;
}

void CodeNavigator::open(const QString& sourceTreePath)
{
    d_msgLog->clear();
    d_usedBy->clear();
    d_view->d_path.clear();
    d_view->clear();
    d_pathTitle->clear();
    d_usedByTitle->clear();
    d_backHisto.clear();
    d_forwardHisto.clear();
    d_dir = sourceTreePath;
    QDir::setCurrent(sourceTreePath);
    setWindowTitle( tr("%3 - %1 v%2").arg( qApp->applicationName() ).arg( qApp->applicationVersion() )
                    .arg( QDir(sourceTreePath).dirName() ));
    QTimer::singleShot(500,this,SLOT(onRunReload()));
}

void CodeNavigator::logMessage(const QString& str)
{
    d_msgLog->parentWidget()->show();
    d_msgLog->appendPlainText(str);
}

void CodeNavigator::createModuleList()
{
    QDockWidget* dock = new QDockWidget( tr("Modules"), this );
    dock->setObjectName("Modules");
    dock->setAllowedAreas( Qt::AllDockWidgetAreas );
    dock->setFeatures( QDockWidget::DockWidgetMovable );
    d_mods = new QTreeWidget(dock);
    d_mods->setAlternatingRowColors(true);
    d_mods->setHeaderHidden(true);
    d_mods->setSortingEnabled(false);
    d_mods->setRootIsDecorated(true);
    dock->setWidget(d_mods);
    addDockWidget( Qt::LeftDockWidgetArea, dock );
    connect( d_mods, SIGNAL(itemDoubleClicked(QTreeWidgetItem*,int)),this,SLOT(onModsDblClicked(QTreeWidgetItem*,int)) );
}

void CodeNavigator::createDetails()
{
    QDockWidget* dock = new QDockWidget( tr("Declarations"), this );
    dock->setObjectName("Declarations");
    dock->setAllowedAreas( Qt::AllDockWidgetAreas );
    dock->setFeatures( QDockWidget::DockWidgetMovable );
    d_mod = new QTreeWidget(dock);
    d_mod->setAlternatingRowColors(true);
    d_mod->setHeaderHidden(true);
    d_mod->setSortingEnabled(false);
    d_mod->setAllColumnsShowFocus(true);
    d_mod->setRootIsDecorated(true);
    d_mod->setExpandsOnDoubleClick(false);
    dock->setWidget(d_mod);
    addDockWidget( Qt::LeftDockWidgetArea, dock );
    connect( d_mod,SIGNAL(doubleClicked(QModelIndex)), this, SLOT(onItemDblClick(QModelIndex)) );
}

void CodeNavigator::createUsedBy()
{
    QDockWidget* dock = new QDockWidget( tr("Uses"), this );
    dock->setObjectName("UsedBy");
    dock->setAllowedAreas( Qt::AllDockWidgetAreas );
    dock->setFeatures( QDockWidget::DockWidgetMovable );
    QWidget* pane = new QWidget(dock);
    QVBoxLayout* vbox = new QVBoxLayout(pane);
    vbox->setMargin(0);
    vbox->setSpacing(0);
    d_usedByTitle = new QLabel(pane);
    d_usedByTitle->setWordWrap(true);
    d_usedByTitle->setMargin(2);
    vbox->addWidget(d_usedByTitle);
    d_usedBy = new QTreeWidget(pane);
    d_usedBy->setAlternatingRowColors(true);
    d_usedBy->setHeaderHidden(true);
    d_usedBy->setSortingEnabled(false);
    d_usedBy->setAllColumnsShowFocus(true);
    d_usedBy->setRootIsDecorated(false);
    vbox->addWidget(d_usedBy);
    dock->setWidget(pane);
    addDockWidget( Qt::RightDockWidgetArea, dock );
    connect(d_usedBy, SIGNAL(itemDoubleClicked(QTreeWidgetItem*,int)), this, SLOT(onUsedByDblClicked()) );
}

void CodeNavigator::createLog()
{
    QDockWidget* dock = new QDockWidget( tr("Message Log"), this );
    dock->setObjectName("Log");
    dock->setAllowedAreas( Qt::AllDockWidgetAreas );
    dock->setFeatures( QDockWidget::DockWidgetMovable | QDockWidget::DockWidgetClosable );
    d_msgLog = new QPlainTextEdit(dock);
    d_msgLog->setReadOnly(true);
    d_msgLog->setLineWrapMode( QPlainTextEdit::NoWrap );
    new LogPainter(d_msgLog->document());
    dock->setWidget(d_msgLog);
    addDockWidget( Qt::BottomDockWidgetArea, dock );
    new QShortcut(tr("ESC"), dock, SLOT(close()) );
}

void CodeNavigator::pushLocation(const Place& loc)
{
    if( d_pushBackLock )
        return;
    if( !d_backHisto.isEmpty() && d_backHisto.last() == loc )
        return; // o ist bereits oberstes Element auf dem Stack.
    d_backHisto.removeAll( loc );
    d_backHisto.push_back( loc );
}

void CodeNavigator::showViewer(const CodeNavigator::Place& p)
{
    d_view->setPosition( p.d_loc, false, false );
    d_view->verticalScrollBar()->setValue(p.d_yoff);
}

static bool SymsLessThan( const Symbol* lhs, const Symbol* rhs )
{

    return false; // TODO lhs->d_loc.packed() < rhs->d_loc.packed();
}

static bool FilesLessThan(const QPair<QString,Ast::SymList>& lhs, const QPair<QString,Ast::SymList>& rhs)
{
    return false; // TODO lhs.first < rhs.first;
}

void CodeNavigator::fillUsedBy(Symbol* id, Declaration* nt)
{
#if 0 // TODO
    d_usedBy->clear();
    if( !nt->name.isEmpty() )
        d_usedByTitle->setText(QString("%1 '%2'").arg(nt->typeName()).arg(nt->name.data()) );
    else
        d_usedByTitle->setText(QString("%1").arg(nt->typeName()) );

   QList< QPair<QString,Declaration::SymList> > all; // filePath -> syms in file
    Declaration::Refs::const_iterator dri;
    for( dri = nt->d_refs.begin(); dri != nt->d_refs.end(); ++dri )
    {
        Declaration::SymList list = dri.value();
        std::sort(list.begin(),list.end(), SymsLessThan);
        all << qMakePair(dri.key(),list);
    }
    std::sort(all.begin(),all.end(),FilesLessThan);


    QTreeWidgetItem* curItem = 0;
    for( int i = 0; i < all.size(); i++ )
    {
        const QString path = all[i].first;
        if( path.isEmpty() )
            continue; // happens e.g. with SELF
        const FileSystem::File* file = d_mdl->getFs()->findFile(path);
        const QString fileName = file ? file->getVirtualPath(false) : QFileInfo(path).fileName();
        const Declaration::SymList& list = all[i].second;
        for( int j = 0; j < list.size(); j++ )
        {
            Symbol* sym = list[j];
            QTreeWidgetItem* item = new QTreeWidgetItem(d_usedBy);
            const bool isDecl = sym == nt->d_me;
            item->setText( 0, QString("%1 %2:%3%4").arg(fileName)
                        .arg(sym->d_loc.d_row).arg( sym->d_loc.d_col)
                           .arg( isDecl ? " decl" : "" ) );
            if( id && sym->d_loc == id->d_loc && path == d_view->d_path )
            {
                QFont f = item->font(0);
                f.setBold(true);
                item->setFont(0,f);
                curItem = item;
            }
            item->setToolTip( 0, item->text(0) );
            item->setData( 0, Qt::UserRole, QVariant::fromValue(FilePos(sym->d_loc,path)) );
            item->setData( 0, Qt::UserRole+1, QVariant::fromValue(sym) );
            if( path != d_view->d_path )
                item->setForeground( 0, Qt::gray );
            else if( curItem == 0 )
                curItem = item;
        }
    }
    if( curItem )
        d_usedBy->scrollToItem( curItem );
#endif
}

void CodeNavigator::setPathTitle(const FileSystem::File* f, int row, int col)
{
    if( f == 0 )
        d_pathTitle->setText("<no file>");
    else
        d_pathTitle->setText(QString("%1 - %2 - %3:%4").
                         arg(f->getVirtualPath(true)).
                         arg(f->d_realPath).
                         arg(row).
                             arg(col));
}

void CodeNavigator::syncModuleList()
{
#if 0 // TODO
    ModuleFile* uf = d_mdl->getFile(d_view->d_path);
    QModelIndex i = d_mdl->findThing( uf );
    if( i.isValid() )
    {
        d_mods->setCurrentIndex(i);
        d_mods->scrollTo( i ,QAbstractItemView::EnsureVisible );
    }
    if( uf && uf->d_module )
        d_mdl2->load( uf->d_module);
#endif
    d_mod->expandAll();
    d_mod->scrollToTop();
}

template<class T>
static void fillDir(const FileSystem::Dir& dir, T* parent)
{
    typedef QMap<QString,const FileSystem::Dir*> SortDir;
    SortDir sortDir;
    foreach(const FileSystem::Dir* d, dir.d_subdirs )
        sortDir.insert(d->d_name,d);
    SortDir::const_iterator i;
    for( i = sortDir.begin(); i != sortDir.end(); ++i )
    {
        QTreeWidgetItem* item = new QTreeWidgetItem(parent,1);
        item->setText(0, i.key());
        item->setToolTip( 0, item->text(0) );
        item->setIcon(0, QPixmap(":/images/folder.png") );
        fillDir(*i.value(), item);
    }
    typedef QMap<QString,const FileSystem::File*> SortFile;
    SortFile sortFile;
    foreach(const FileSystem::File* f, dir.d_files )
        sortFile.insert(f->d_name,f);
    SortFile::const_iterator j;
    for( j = sortFile.begin(); j != sortFile.end(); ++j )
    {
        QTreeWidgetItem* item = new QTreeWidgetItem(parent,2);
        item->setText(0, j.value()->d_moduleName);
        item->setToolTip( 0, j.value()->d_realPath );
        item->setIcon(0, QPixmap(":/images/module.png") );
    }
}

void CodeNavigator::fillMods()
{
    d_mods->clear();
    const FileSystem::Dir& root = d_fs->getRoot();
    if( root.d_files.isEmpty() && root.d_subdirs.size() == 1 && root.d_subdirs.first()->d_name.isEmpty() )
        fillDir(*root.d_subdirs.first(), d_mods);
    else
        fillDir(root, d_mods);
}

void CodeNavigator::closeEvent(QCloseEvent* event)
{
    QSettings s;
    s.setValue( "DockState", saveState() );
    event->setAccepted(true);
}

void CodeNavigator::onCursorPositionChanged()
{
    QTextCursor cur = d_view->textCursor();
    const int line = cur.blockNumber() + 1;
    const int col = cur.positionInBlock() + 1;

#if 0
    setPathTitle(d_mdl->getFs()->findFile(d_view->d_path), line, col);

    // TODO
    Symbol* id = d_mdl->findSymbolBySourcePos(d_view->d_path,line,col);
    if( id && id->d_decl && id->d_decl->isDeclaration() )
    {
        Declaration* d = (id->d_decl);
        fillUsedBy( id, d );

        // mark all symbols in file which have the same declaration
        QList<Symbol*> syms = d->d_refs.value(d_view->d_path);
        d_view->markNonTerms(syms);

        QModelIndex i = d_mdl2->findThing( d );
        if( i.isValid() )
        {
            d_mod->setCurrentIndex(i);
            d_mod->scrollTo( i ,QAbstractItemView::EnsureVisible );
        }
    }
#endif
}

void CodeNavigator::onModsDblClicked(QTreeWidgetItem* item,int)
{
    if( item == 0 || item->type() != 2)
        return;

    d_view->loadFile(item->toolTip(0));
}

void CodeNavigator::onItemDblClick(const QModelIndex& i)
{
#if 0 // TODO
    const Thing* nt = d_mdl->getThing(i);
    if( nt == 0 || !nt->isDeclaration() )
        return;

    d_view->setPosition( nt->getLoc(), true, true );
#endif
}

void CodeNavigator::onUsedByDblClicked()
{
    if( d_usedBy->currentItem() == 0 )
        return;

    FilePos pos = d_usedBy->currentItem()->data(0,Qt::UserRole).value<FilePos>();
    if( pos.d_filePath.isEmpty() )
        return;
    d_view->setPosition( pos, true, true );
}

void CodeNavigator::onGoBack()
{
    if( d_backHisto.size() <= 1 )
        return;

    d_pushBackLock = true;
    d_forwardHisto.push_back( d_backHisto.last() );
    d_backHisto.pop_back();
    showViewer(d_backHisto.last());
    d_pushBackLock = false;

}

void CodeNavigator::onGoForward()
{
    if( d_forwardHisto.isEmpty() )
        return;
    Place cur = d_forwardHisto.last();
    d_forwardHisto.pop_back();
    showViewer(cur);
    d_backHisto.push_back(cur);
}

void CodeNavigator::onGotoLine()
{
    QTextCursor cur = d_view->textCursor();
    int line = cur.blockNumber();
    bool ok	= false;
    line = QInputDialog::getInt(
                this, tr("Goto Line"),
        tr("Enter a valid line number:"),
        line + 1, 1, 999999, 1,	&ok );
    if( !ok )
        return;
    QTextBlock block = d_view->document()->findBlockByNumber(line-1);
    cur.setPosition( block.position() );
    d_view->setTextCursor( cur );
    d_view->centerCursor();
    d_view->updateExtraSelections();
}

void CodeNavigator::onFindInFile()
{
    bool ok	= false;
    const QString sel = d_view->textCursor().selectedText();
    QString res = QInputDialog::getText( this, tr("Find in File"),
        tr("Enter search string:"), QLineEdit::Normal, sel, &ok );
    if( !ok )
        return;
    d_view->d_find = res;
    d_view->find( sel.isEmpty() );
}

void CodeNavigator::onFindAgain()
{
    if( !d_view->d_find.isEmpty() )
        d_view->find( false );
}

void CodeNavigator::onGotoDefinition()
{
    QTextCursor cur = d_view->textCursor();
#if 0 // TODO
    Symbol* id = d_mdl->findSymbolBySourcePos(
                d_view->d_path,cur.blockNumber() + 1,cur.positionInBlock() + 1);
    if( id )
        d_view->setPosition( id, true, true );
#endif
}

void CodeNavigator::onOpen()
{
    QString path = QFileDialog::getExistingDirectory(this,tr("Open Project Directory"),QDir::currentPath() );
    if( path.isEmpty() )
        return;
    open(path);
}

void CodeNavigator::onRunReload()
{
    QElapsedTimer t;
    t.start();
    QApplication::setOverrideCursor(Qt::WaitCursor);
    d_fs->load(d_dir);
    d_pro->clear();
    QList<const FileSystem::File*> mods = d_fs->getAllPas();
    foreach(const FileSystem::File* f, mods)
        d_pro->addFile(f->d_realPath, f->d_moduleName);
    int errCount = 0;
    if( !d_pro->parse() )
    {
        const QList<Project::Error>& errs = d_pro->getErrors();
        const int off = d_fs->getRootPath().size();
        foreach( const Project::Error& e, errs )
        {
            const FileSystem::File* f = d_fs->findFile(e.path);
            const QString line = tr("%1:%2:%3: %4").arg( f ? f->getVirtualPath() : e.path.mid(off) ).arg(e.pos.d_row)
                    .arg(e.pos.d_col).arg(e.msg);
            qCritical() << line.toUtf8().constData();
            errCount++;
        }
    }
    fillMods();
    QApplication::restoreOverrideCursor();
    qDebug() << "parsed" << d_pro->getSloc() << "SLOC in" << t.elapsed() << "[ms]";
    qDebug() << "with" << errCount << "errors";
}

void CodeNavigator::onIncreaseSize()
{
    QFont f = d_view->font();
    f.setPointSize(f.pointSize()+1);
    d_view->setFont(f);
}

void CodeNavigator::onDecreaseSize()
{
    QFont f = d_view->font();
    f.setPointSize(qMax(f.pointSize()-1,3));
    d_view->setFont(f);
}


int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    a.setOrganizationName("me@rochus-keller.ch");
    a.setOrganizationDomain("github.com/rochus-keller/ActiveOberon");
    a.setApplicationName("AoCodeNavigator");
    a.setApplicationVersion("0.5.0");
    a.setStyle("Fusion");
    QFontDatabase::addApplicationFont(":/fonts/DejaVuSansMono.ttf"); 
#ifdef Q_OS_LINUX
    QFontDatabase::addApplicationFont(":/fonts/NotoSans.ttf"); 
    QFont af("Noto Sans",9);
    a.setFont(af);
#endif


    QString dirPath;
    const QStringList args = QCoreApplication::arguments();
    for( int i = 1; i < args.size(); i++ )
    {
        if( !args[ i ].startsWith( '-' ) )
        {
            if( !dirPath.isEmpty() )
            {
                qCritical() << "error: only one argument (path to source tree) supported";
                return -1;
            }
            dirPath = args[ i ];
        }else
        {
            qCritical() << "error: invalid command line option " << args[i] << endl;
            return -1;
        }
    }

    CodeNavigator* w = new CodeNavigator();
    w->showMaximized();
    if( !dirPath.isEmpty() )
        w->open(dirPath);

    const int res = a.exec();
    delete w;

    return res;
}
