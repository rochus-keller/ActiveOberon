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
#include "AoProject.h"
#include "AoLexer.h"
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
        // monospace.setPointSizeF(monospace.pointSizeF()*1.0); // this strangely leads to a bigger font on my debian bookworm mate desktop
        monospace.setPointSizeF(10.5);
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
        QByteArray buf = Ao::Lexer::extractText(&in);
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
        Symbol* syms = that()->d_pro->getSymbolsOfModule(that()->d_pro->findModuleByPath(d_path));
        Symbol* s = syms;
        while( s )
        {
            if( s->decl == 0 ) // we're not interested in Symbols pointing to a Declaration here
            {
                RowCol loc = s->pos;
                QTextCursor c( document()->findBlockByNumber( loc.d_row - 1) );
                c.setPosition( c.position() + loc.d_col - 1 );
                c.movePosition(QTextCursor::EndOfWord,QTextCursor::KeepAnchor);
                QTextEdit::ExtraSelection sel;
                sel.format = missing;
                sel.cursor = c;
                d_missing << sel;
            }

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

CodeNavigator::CodeNavigator(QWidget *parent) : QMainWindow(parent),d_pushBackLock(false),
    d_xrefLock(false), d_hierLock(false), d_detailLock(false)
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
    createHier();
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
    QWidget* pane = new QWidget(dock);
    QVBoxLayout* vbox = new QVBoxLayout(pane);
    vbox->setMargin(0);
    vbox->setSpacing(0);
    d_modTitle = new QLabel(pane);
    d_modTitle->setMargin(2);
    d_modTitle->setWordWrap(true);
    vbox->addWidget(d_modTitle);
    d_mod = new QTreeWidget(dock);
    d_mod->setHeaderHidden(true);
    d_mod->setExpandsOnDoubleClick(false);
    d_mod->setAlternatingRowColors(true);
    vbox->addWidget(d_mod);
    dock->setWidget(pane);
    addDockWidget( Qt::LeftDockWidgetArea, dock );
    connect( d_mod,SIGNAL(itemDoubleClicked(QTreeWidgetItem*,int)), this, SLOT(onItemDblClick(QTreeWidgetItem*,int)) );
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

static inline QString declKindName(Declaration* decl)
{
    switch( decl->kind )
    {
    case Declaration::Field:
        return "Field";
    case Declaration::VarDecl:
    case Declaration::LocalDecl:
        return "Variable";
    case Declaration::ParamDecl:
        return "Parameter";
    case Declaration::TypeDecl:
        return "Type";
    case Declaration::ConstDecl:
        return "Const";
    case Declaration::Import:
        return "Import";
    case Declaration::Builtin:
        return "BuiltIn";
    case Declaration::Procedure:
        return "Procedure";
    case Declaration::Module:
        return "Module";
    }
    return "???";
}

static const char* roleName( Symbol* e )
{
    switch( e->kind )
    {
    case Symbol::Decl:
    case Symbol::Module:
        return "decl";
    case Symbol::Lval:
        return "lhs";
    default:
        break;
    }
    return "";
}

static bool sortExList( const Symbol* lhs, Symbol* rhs )
{
    Declaration* lm = lhs->decl;
    Declaration* rm = rhs->decl;
    const QByteArray ln = lm ? lm->name : QByteArray();
    const QByteArray rn = rm ? rm->name : QByteArray();
    const quint32 ll = lhs->pos.packed();
    const quint32 rl = rhs->pos.packed();

    return ln < rn || (!(rn < ln) && ll < rl);
}

void CodeNavigator::fillUsedBy(Symbol* sym, Declaration* module)
{
    d_usedBy->clear();

    Declaration* decl = sym->decl;
    Q_ASSERT( decl != 0 );

    Project::UsageByMod usage = d_pro->getUsage(decl);

    QFont f = d_usedBy->font();
    f.setBold(true);

    const QString type = declKindName(decl);

    d_usedByTitle->setText(QString("%1 '%2'").arg(type).arg(decl->name.constData()));

    QTreeWidgetItem* black = 0;
    for( int i = 0; i < usage.size(); i++ )
    {
        SymList syms = usage[i].second;
        std::sort( syms.begin(), syms.end(), sortExList );
        QString modName;
        if( usage[i].first )
            modName = usage[i].first->name;
        else
            modName = "<global>";
        foreach( Symbol* s, syms )
        {
            QTreeWidgetItem* item = new QTreeWidgetItem(d_usedBy);
            item->setText( 0, QString("%1 (%2:%3 %4)")
                        .arg(modName)
                        .arg(s->pos.d_row).arg(s->pos.d_col)
                        .arg( roleName(s) ));
            if( s == sym )
            {
                item->setFont(0,f);
                black = item;
            }
            item->setToolTip( 0, item->text(0) );
            item->setData( 0, Qt::UserRole, QVariant::fromValue( s ) ); // symbol in module
            item->setData( 1, Qt::UserRole, QVariant::fromValue( usage[i].first ) ); // module where the sym was found
            if( usage[i].first != module )
                item->setForeground( 0, Qt::gray );
        }
    }
    if( black && !d_xrefLock )
    {
        d_usedBy->scrollToItem(black, QAbstractItemView::PositionAtCenter);
        d_usedBy->setCurrentItem(black);
    }
}

void CodeNavigator::setPathTitle(const FileSystem::File* f, int row, int col)
{
    if( f == 0 )
        d_pathTitle->setText("<no file>");
    else
        d_pathTitle->setText(QString("%1 - %2 - %3:%4").
                         arg(f->d_moduleName.constData()).
                         arg(f->d_realPath).
                         arg(row).
                             arg(col));
}

static Declaration* adjustForModIdx( Declaration* decl )
{
    while( decl )
    {
        switch( decl->kind )
        {
        case Declaration::Procedure:
        case Declaration::Module:
            return decl;
        }
        decl = decl->outer;
    }
    return 0;
}

void CodeNavigator::syncDetailView(Declaration* decl)
{
    QTreeWidgetItem* mi = d_modIdx.value(decl);
    if( mi == 0 )
        mi = d_modIdx.value(adjustForModIdx(decl));
    if( mi && !d_detailLock )
    {
        d_mod->scrollToItem(mi,QAbstractItemView::PositionAtCenter);
        mi->setExpanded(true);
        d_mod->setCurrentItem(mi);
    }
}

void CodeNavigator::createHier()
{
    QDockWidget* dock = new QDockWidget( tr("Hierarchy"), this );
    dock->setObjectName("Hierarchy");
    dock->setAllowedAreas( Qt::AllDockWidgetAreas );
    dock->setFeatures( QDockWidget::DockWidgetMovable | QDockWidget::DockWidgetClosable );
    QWidget* pane = new QWidget(dock);
    QVBoxLayout* vbox = new QVBoxLayout(pane);
    vbox->setMargin(0);
    vbox->setSpacing(0);
    d_hierTitle = new QLabel(pane);
    d_hierTitle->setMargin(2);
    d_hierTitle->setWordWrap(true);
    vbox->addWidget(d_hierTitle);
    d_hier = new QTreeWidget(dock);
    d_hier->setHeaderHidden(true);
    d_hier->setExpandsOnDoubleClick(false);
    d_hier->setAlternatingRowColors(true);
    vbox->addWidget(d_hier);
    dock->setWidget(pane);
    addDockWidget( Qt::LeftDockWidgetArea, dock );
    connect( d_hier, SIGNAL(itemDoubleClicked(QTreeWidgetItem*,int)),this,SLOT(onHierDblClicked(QTreeWidgetItem*,int)) );
}

void CodeNavigator::syncModuleList()
{
#if 0
    // TODO
    File* uf = d_pro->getFile(d_view->d_path);
    QModelIndex i = d_mdl->findThing( uf );
    if( i.isValid() )
    {
        d_mods->setCurrentIndex(i);
        d_mods->scrollTo( i ,QAbstractItemView::EnsureVisible );
    }
    if( uf && uf->d_module )
        d_mdl2->load( uf->d_module);
    d_mod->expandAll();
    d_mod->scrollToTop();
#endif
    fillDetails(d_pro->findModuleByPath(d_view->d_path));
}

static bool FileSort( const FileSystem::File* lhs, const FileSystem::File* rhs )
{
    return lhs->d_moduleName < rhs->d_moduleName;
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
    typedef QList<const FileSystem::File*> SortFile;
    SortFile sortFile;
    foreach(const FileSystem::File* f, dir.d_files )
        sortFile << f;
    std::sort(sortFile.begin(), sortFile.end(), FileSort);
    for( int j = 0; j < sortFile.size(); j++ )
    {
        QTreeWidgetItem* item = new QTreeWidgetItem(parent,2);
        item->setText(0, sortFile[j]->d_moduleName);
        item->setToolTip( 0, sortFile[j]->d_realPath );
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

void CodeNavigator::syncEditorMarks(Declaration* selected, Declaration* module)
{
    Symbol* syms = d_pro->getSymbolsOfModule(module);
    if( syms == 0 )
    {
        d_view->markNonTerms(SymList());
        return;
    }

    Symbol* s = syms;
    SymList marks;
    while( s )
    {
        if( s->decl == selected )
            marks << s;
        s = s->next;
        if( s == syms )
            break;
    }

    d_view->markNonTerms(marks);
}

void CodeNavigator::onCursorPositionChanged()
{
    QTextCursor cur = d_view->textCursor();
    const int line = cur.blockNumber() + 1;
    const int col = cur.positionInBlock() + 1;

    setPathTitle(d_fs->findFile(d_view->d_path), line, col);

    Symbol* id = d_pro->findSymbolBySourcePos(d_view->d_path,line,col);
    if( id && id->decl )
    {
        Declaration* module = d_pro->findModuleByPath(d_view->d_path);
        fillUsedBy( id, module );
        syncEditorMarks(id->decl, module);
        syncDetailView( id->decl );
        fillHier(id->decl);
    }
}

void CodeNavigator::onModsDblClicked(QTreeWidgetItem* item,int)
{
    if( item == 0 || item->type() != 2)
        return;

    d_view->loadFile(item->toolTip(0));
    Declaration* module = d_pro->findModuleByPath(d_view->d_path);
    fillDetails(module);
}

void CodeNavigator::onItemDblClick(QTreeWidgetItem* item,int)
{
    Declaration* s = item->data(0,Qt::UserRole).value<Declaration*>();
    if( s == 0 )
        return;

    d_detailLock = true;
    item->setExpanded(true);
    d_view->setPosition( FilePos(s->pos,d_view->d_path), true, true );
    d_detailLock = false;
}

void CodeNavigator::onUsedByDblClicked()
{
    QTreeWidgetItem* item = d_usedBy->currentItem();
    if( item )
    {
        Symbol* sym = item->data(0,Qt::UserRole).value<Symbol*>();
        Declaration* module = item->data(1,Qt::UserRole).value<Declaration*>();
        Q_ASSERT( sym != 0 );
        d_xrefLock = true;
        if( module )
        {
            ModuleData md = module->data.value<ModuleData>();
            d_view->setPosition( FilePos(sym->pos, md.sourcePath), true, true );
        }
        d_xrefLock = false;
    }
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
    Symbol* id = d_pro->findSymbolBySourcePos(
                d_view->d_path,cur.blockNumber() + 1,cur.positionInBlock() + 1);
    if( id )
        d_view->setPosition( id, true, true );
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

void CodeNavigator::onHierDblClicked(QTreeWidgetItem* item, int)
{
    Declaration* s = item->data(0,Qt::UserRole).value<Declaration*>();
    if( s == 0 )
        return;

    d_hierLock = true;
    Declaration* mod = s->getModule();
    if( mod )
    {
        ModuleData md = mod->data.value<ModuleData>();
        d_view->setPosition( FilePos(s->pos,md.sourcePath), true, true );
        item->setExpanded(true);
    }
    d_hierLock = false;
}

template<class T>
static QTreeWidgetItem* fillHierProc( T* parent, Declaration* p, Declaration* ref, Project* pro )
{
    QTreeWidgetItem* item = new QTreeWidgetItem(parent);
    Q_ASSERT( p->receiver && p->link && p->link->receiver && p->link->type() );

    item->setText(0, p->link->type()->decl->scopedName(true));
    item->setData(0, Qt::UserRole, QVariant::fromValue(p) );
    item->setIcon(0, QPixmap( p->visi >= Declaration::ReadWrite ? ":/images/func.png" : ":/images/func_priv.png" ) );
    item->setToolTip(0,item->text(0));

    QTreeWidgetItem* ret = 0;
    DeclList subs = pro->getSubs(p);
    foreach( Declaration* sub, subs )
    {
        QTreeWidgetItem* tmp = fillHierProc(item, sub, ref, pro);
        if( tmp )
            ret = tmp;
    }
    if( ret == 0 && p == ref )
            ret = item;
    return ret;
}

template<class T>
static QTreeWidgetItem* fillHierClass( T* parent, Type* cur, Type* ref, Project* pro )
{
    QTreeWidgetItem* item = new QTreeWidgetItem(parent);
    Declaration* name = cur->decl;
    if( name )
        item->setText(0, name->scopedName(true));
    item->setData(0, Qt::UserRole, QVariant::fromValue( name ) );
    item->setIcon(0, QPixmap( name->visi >= Declaration::ReadWrite ? ":/images/class.png" : ":/images/class_priv.png" ) );
    item->setToolTip(0,item->text(0));
    QTreeWidgetItem* ret = name == ref->decl ? item : 0;
    DeclList subs = pro->getSubs(name);
    foreach( Declaration* sub, subs )
    {
        QTreeWidgetItem* tmp = fillHierClass(item, sub->type(), ref, pro);
        if( tmp )
            ret = tmp;
    }
    return ret;
}

void CodeNavigator::fillHier(Declaration* decl)
{
    if( d_hierLock )
        return;
    d_hier->clear();
    d_hierTitle->clear();
    if( decl == 0 )
        return;
    QFont f = d_hier->font();
    f.setBold(true);
    QTreeWidgetItem* ref = 0;
    switch( decl->kind )
    {
    case Declaration::TypeDecl:
        if( decl->type() == 0 )
            break;
        switch( decl->type()->kind )
        {
        case Type::Record:
        case Type::Object:
            {
                Type* root = decl->type()->deref();
                Type* sub = root;
                d_hierTitle->setText( QString("Inheritance of class '%1'").arg( decl->name.constData() ) );
                while( root && root->type() )
                    root = root->type()->deref();
                ref = fillHierClass( d_hier, root, sub, d_pro );
            }
            break;
        }
        break;
    case Declaration::Procedure:
        {
            Declaration* super = decl;
            if( !super->receiver )
                return;
            d_hierTitle->setText( QString("Overrides of procedure '%1'").arg( decl->name.constData() ) );
            while( super->super )
                super = super->super;
            ref = fillHierProc( d_hier, super, decl, d_pro );
            Q_ASSERT( ref );
        }
        break;
    }
    d_hier->sortByColumn(0,Qt::AscendingOrder);
    if( ref )
    {
        ref->setFont(0,f);
        // d_hier->expandAll();
        ref->setExpanded(true);
        d_hier->scrollToItem(ref,QAbstractItemView::PositionAtCenter);
        d_hier->setCurrentItem(ref);
    }
}

static void decorateModItem(QTreeWidgetItem* item, Declaration* n, Type* r, QHash<Declaration*,QTreeWidgetItem*>& idx );

template <class T>
static void walkModItems(T* parent, Declaration* thisDecl, Type* declType, bool sort,
                         QHash<Declaration*,QTreeWidgetItem*>& idx, Project* pro)
{
    typedef QMultiMap<QByteArray,Declaration*> Sort;
    if( thisDecl && sort)
    {
        Sort tmp;
        Declaration* sub = thisDecl->link;
        while( sub )
        {
            tmp.insert( sub->name.toLower(), sub );
            sub = sub->getNext();
        }
        Sort::const_iterator i;
        for( i = tmp.begin(); i != tmp.end(); ++i )
            createModItem(parent,i.value(),0, sort, idx, pro);
    }else if( thisDecl )
    {
        Declaration* sub = thisDecl->link;
        while( sub )
        {
            createModItem(parent,sub,0, sort, idx, pro);
            sub = sub->getNext();
        }
    }
    if( declType && sort )
    {
        Sort tmp;
        foreach( Declaration* sub, declType->subs )
        {
            if( sub->kind == Declaration::Procedure )
                tmp.insert( sub->name.toLower(), sub );
        }
        Sort::const_iterator i;
        for( i = tmp.begin(); i != tmp.end(); ++i )
            createModItem(parent,i.value(),0, sort, idx, pro);
    }else if( declType )
    {
        foreach( Declaration* sub, declType->subs )
        {
            if( sub->kind == Declaration::Procedure )
                createModItem(parent,sub,0, sort, idx, pro);
        }
    }
}

static void fillRecord(QTreeWidgetItem* item, Declaration* thisDecl, Type* declType, bool sort,
                       QHash<Declaration*,QTreeWidgetItem*>& idx, Project* pro )
{
    decorateModItem(item,thisDecl, declType, idx);
    walkModItems(item,thisDecl,declType,sort, idx, pro);
    if( declType->type() )
        item->setText(0, item->text(0) + " ⇑");
    if( thisDecl->hasSubs )
    {
        DeclList subs = pro->getSubs(thisDecl);
        item->setText(0, item->text(0) + QString(" ⇓%1").arg(subs.size()));
    }
    item->setToolTip( 0, item->text(0) );
}

template<class T>
static void createModItem(T* parent, Declaration* thisDecl, Type* declType, bool sort,
                          QHash<Declaration*,QTreeWidgetItem*>& idx, Project* pro )
{
    if( declType == 0 )
        declType = thisDecl->type();

    if( idx.contains(thisDecl) )
    {
        // qWarning() << "fillMod recursion at" << n->getModule()->d_file << n->pos.d_row << n->name;
        return; // can legally happen if record decl contains a typedef using record, as e.g. Meta.Item.ParamCallVal.IP
    }
    switch( thisDecl->kind )
    {
    case Declaration::TypeDecl:
        if( declType == 0 )
            return;
        switch( declType->kind )
        {
        case Type::Record:
        case Type::Object:
            {
                QTreeWidgetItem* item = new QTreeWidgetItem(parent);
                fillRecord(item,thisDecl,declType,sort,idx, pro);
            }
            break;
        case Type::Pointer:
            if( declType->type() && declType->type()->kind == Type::Record )
            {
                // a pointer to an anonymous record (we don't need this for objects because they are already pointers)
                QTreeWidgetItem* item = new QTreeWidgetItem(parent);
                fillRecord(item,thisDecl,declType->type(),sort,idx, pro);
            }
            break;
#if 0
        case Type::NameRef:
            if( declType->deref()->kind == Type::Record || declType->deref()->kind == Type::Object )
                createModItem(parent,thisDecl,declType->deref(), sort, idx, pro);
            break;
#endif
        }
        break;
    case Declaration::Procedure:
        {
            QTreeWidgetItem* item = new QTreeWidgetItem(parent);
            decorateModItem(item,thisDecl, 0, idx);
            walkModItems(item,thisDecl,declType,sort, idx, pro);
            if( thisDecl->super )
                item->setText(0, item->text(0) + " ⇑");
            if( thisDecl->hasSubs )
            {
                DeclList subs = pro->getSubs(thisDecl);
                item->setText(0, item->text(0) + QString(" ⇓%1").arg(subs.size()));
            }
            item->setToolTip( 0, item->text(0) );
        }
        break;
    }
}

static void decorateModItem( QTreeWidgetItem* item, Declaration* thisDecl, Type* declType,
                          QHash<Declaration*,QTreeWidgetItem*>& idx )
{
    if( declType == 0 )
        declType = thisDecl->type();
    const bool pub = thisDecl->visi > Declaration::Private;
    item->setText(0,thisDecl->name);
    item->setData(0, Qt::UserRole, QVariant::fromValue(thisDecl) );
    idx.insert(thisDecl,item);
    switch( thisDecl->kind )
    {
    case Declaration::TypeDecl:
        if( declType && declType->kind == Type::Record )
            item->setIcon(0, QPixmap( pub ? ":/images/struct.png" : ":/images/struct_priv.png" ) );
        else if( declType && declType->kind == Type::Object )
            item->setIcon(0, QPixmap( pub ? ":/images/class.png" : ":/images/class_priv.png" ) );
        break;
    case Declaration::Procedure:
        item->setIcon(0, QPixmap( pub ? ":/images/func.png" : ":/images/func_priv.png" ) );
        break;
    }
}

void CodeNavigator::fillDetails(Declaration* module)
{
    d_mod->clear();
    d_modIdx.clear();
    d_modTitle->clear();
    if( module == 0 )
        return;
    d_modTitle->setText( QString("Module '%1'").arg(module->name.constData()) );
    walkModItems(d_mod, module, 0, true, d_modIdx, d_pro );
    d_mod->expandAll();
}

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    a.setOrganizationName("me@rochus-keller.ch");
    a.setOrganizationDomain("github.com/rochus-keller/ActiveOberon");
    a.setApplicationName("AoCodeNavigator");
    a.setApplicationVersion("0.5.1");
    a.setStyle("Fusion");
    QFontDatabase::addApplicationFont(":/fonts/DejaVuSansMono.ttf"); 
#ifdef Q_OS_LINUX
    QFontDatabase::addApplicationFont(":/fonts/NotoSans.ttf"); 
    QFont af("Noto Sans",10);
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

