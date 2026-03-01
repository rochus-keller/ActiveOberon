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

#include "AoIde.h"
#include "AoHighlighter2.h"
#include "AoAst.h"
#include "AoLexer.h"
#include "AoProject2.h"
#include <QtDebug>
#include <QDockWidget>
#include <QApplication>
#include <QStandardPaths>
#include <QDir>
#include <QDateTime>
#include <QSettings>
#include <QShortcut>
#include <QScrollBar>
#include <QMessageBox>
#include <QFileDialog>
#include <QFileInfo>
#include <QBuffer>
#include <QHeaderView>
#include <QLabel>
#include <QVBoxLayout>
#include <QDesktopWidget>
#include <QInputDialog>
#include <QTreeWidget>
#include <GuiTools/AutoMenu.h>
#include <GuiTools/CodeEditor.h>
#include <GuiTools/AutoShortcut.h>
#include <GuiTools/DocTabWidget.h>
using namespace Ao;
using namespace Ast;

#ifdef Q_OS_MAC
#define OBN_BREAK_SC "SHIFT+F8"
#define OBN_ABORT_SC "CTRL+SHIFT+Y"
#define OBN_CONTINUE_SC "CTRL+Y"
#define OBN_STEPIN_SC "CTRL+SHIFT+I"
#define OBN_STEPOVER_SC "CTRL+SHIFT+O"
#define OBN_STEPOUT_SC "SHIFT+F11" // TODO
#define OBN_ENDBG_SC "F4"
#define OBN_TOGBP_SC "F8"
#define OBN_GOBACK_SC "ALT+CTRL+Left"
#define OBN_GOFWD_SC "ALT+CTRL+Right"
#define OBN_NEXTDOC_SC "ALT+TAB"
#define OBN_PREVDOC_SC "ALT+SHIFT+TAB"
#else
#define OBN_BREAK_SC "SHIFT+F9"
#define OBN_ABORT_SC "SHIFT+F5"
#define OBN_CONTINUE_SC "F5"
#define OBN_STEPIN_SC "F11"
#define OBN_STEPOVER_SC "F10"
#define OBN_STEPOUT_SC "SHIFT+F11"
#define OBN_ENDBG_SC "F8"
#define OBN_TOGBP_SC "F9"
#define OBN_GOBACK_SC "ALT+Left"
#define OBN_GOFWD_SC "ALT+Right"
#define OBN_NEXTDOC_SC "CTRL+TAB"
#define OBN_PREVDOC_SC "CTRL+SHIFT+TAB"
#endif

class Ide::Editor : public CodeEditor
{
public:
    Editor(Ide* p, Project2* pro):CodeEditor(p),d_pro(pro),d_ide(p),dbgRow(0),dbgCol(0)
    {
        setCharPerTab(2);
        setTypingLatency(400);
        setPaintIndents(false);
        d_hl = new OberonPainter2(this);
        d_hl->addBuiltIns();
        d_hl->setDocument(document());
        updateTabWidth();
        QSettings set;
        if( !set.contains("CodeEditor/Font") )
        {
            QFont monospace("Monospace");
            if( !monospace.exactMatch() )
                monospace = QFont("DejaVu Sans Mono");
            monospace.setPointSizeF(10.5);
            setFont(monospace);
        }
    }

    ~Editor()
    {
    }

    Ide* d_ide;
    OberonPainter2* d_hl;
    Project2* d_pro;
    int dbgRow, dbgCol;

    void setExt( bool on )
    {
#if 0
        for( int i = Builtin::ABS; i < Builtin::Max; i++ )
            d_hl->addBuiltIn(Builtin::name[i]);
        for( int i = BasicType::Nil; i <= BasicType::Max; i++ )
            d_hl->addBuiltIn(BasicType::name[i]);
        d_hl->addBuiltIn("ANYREC");
#endif
    }

    void clearBackHisto()
    {
        d_backHisto.clear();
    }

    void markNonTerms(const SymList& syms)
    {
        d_nonTerms.clear();
        QTextCharFormat format;
        format.setBackground( QColor(237,235,243) );
        foreach( Symbol* s, syms )
        {
            Declaration* ident = s->decl;
            Q_ASSERT( ident );
            QTextCursor c( document()->findBlockByNumber( qMax(int(s->pos.d_row - 1),0)) );
            c.setPosition( c.position() + qMax(int(s->pos.d_col - 1), 0) );
            int pos = c.position();
            c.setPosition( pos + ident->name.size(), QTextCursor::KeepAnchor );

            QTextEdit::ExtraSelection sel;
            sel.format = format;
            sel.cursor = c;

            d_nonTerms << sel;
        }
        updateExtraSelections();
    }

    void updateExtraSelections()
    {
        ESL sum;

        QTextEdit::ExtraSelection line;
        line.format.setBackground(QColor(Qt::yellow).lighter(170));
        line.format.setProperty(QTextFormat::FullWidthSelection, true);
        line.cursor = textCursor();
        line.cursor.clearSelection();
        sum << line;

        sum << d_nonTerms;

        if( !d_pro->getErrors().isEmpty() )
        {
            QTextCharFormat errorFormat;
            errorFormat.setUnderlineStyle(QTextCharFormat::WaveUnderline);
            errorFormat.setUnderlineColor(Qt::magenta);
            for( int i = 0; i < d_pro->getErrors().size(); i++ )
            {
                const Project2::Error& e = d_pro->getErrors().at(i);
                if( e.path != getPath() )
                    continue;
                QTextCursor c( document()->findBlockByNumber(e.pos.d_row - 1) );

                c.setPosition( c.position() + e.pos.d_col - 1 );
                c.movePosition(QTextCursor::EndOfWord, QTextCursor::KeepAnchor);

                QTextEdit::ExtraSelection sel;
                sel.format = errorFormat;
                sel.cursor = c;
                sel.format.setToolTip(e.msg);

                sum << sel;
            }
        }

        sum << d_link;

        setExtraSelections(sum);
    }

    void mousePressEvent(QMouseEvent* e)
    {
        if( !d_link.isEmpty() )
        {
            QTextCursor cur = cursorForPosition(e->pos());
            d_ide->pushLocation( Ide::Location( getPath(), cur.blockNumber(), cur.positionInBlock(), verticalScrollBar()->value() ) );
            QApplication::restoreOverrideCursor();
            d_link.clear();
        }
        if( QApplication::keyboardModifiers() == Qt::ControlModifier )
        {
            QTextCursor cur = cursorForPosition(e->pos());
            Symbol* e = d_pro->findSymbolBySourcePos(
                        getPath(),cur.blockNumber() + 1,cur.positionInBlock() + 1);
            if( e )
            {
                Declaration* decl = e->decl;
                d_ide->pushLocation( Ide::Location( getPath(), cur.blockNumber(), cur.positionInBlock(), verticalScrollBar()->value() ) );
                if( decl->kind == Declaration::Import && e->pos == decl->pos )
                {
                    Import imp = decl->data.value<Import>();
                    decl = imp.resolved;
                }else if( decl->kind == Declaration::Procedure )
                {
                    if( decl->receiver && decl->super )
                        decl = decl->super;
                }
                d_ide->showEditor( decl, false, true );
                //setCursorPosition( sym->pos.d_row - 1, sym->pos.d_col - 1, true );
            }
            updateExtraSelections();
        }else
            QPlainTextEdit::mousePressEvent(e);
    }

    void mouseMoveEvent(QMouseEvent* e)
    {
        QPlainTextEdit::mouseMoveEvent(e);
        if( QApplication::keyboardModifiers() == Qt::ControlModifier )
        {
            QTextCursor cur = cursorForPosition(e->pos());
            Symbol* e = d_pro->findSymbolBySourcePos(
                        getPath(),cur.blockNumber() + 1,cur.positionInBlock() + 1);
            const bool alreadyArrow = !d_link.isEmpty();
            d_link.clear();
            if( e )
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
                /*
                d_linkLineNr = sym->pos.d_row - 1;
                d_linkColNr = sym->pos.d_col - 1;
                */
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

    void onUpdateModel()
    {
        markNonTerms(SymList());
    }
};

class Ide::DocTab : public DocTabWidget
{
public:
    DocTab(QWidget* p):DocTabWidget(p,false) {}

    // overrides
    bool isUnsaved(int i)
    {
        Ide::Editor* edit = static_cast<Ide::Editor*>( widget(i) );
        return edit->isModified();
    }

    bool save(int i)
    {
        Ide::Editor* edit = static_cast<Ide::Editor*>( widget(i) );
        if( !edit->saveToFile( edit->getPath(), false ) )
            return false;
        return true;
    }
};

static Ide* s_this = 0;
static void report(QtMsgType type, const QString& message )
{
    if( s_this )
    {
        switch(type)
        {
        case QtDebugMsg:
            // NOP s_this->logMessage(QLatin1String("INF: ") + message);
            break;
        case QtWarningMsg:
            s_this->logMessage(QLatin1String("WRN: ") + message);
            break;
        case QtCriticalMsg:
        case QtFatalMsg:
            s_this->logMessage(QLatin1String("ERR: ") + message, true);
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

static void log( const QString& msg )
{
    if( s_this )
        s_this->logMessage(msg);
}

Ide::Ide(QWidget *parent)
    : QMainWindow(parent),d_lock(false),d_filesDirty(false),d_pushBackLock(false),
      d_lock2(false),d_lock3(false),d_lock4(false),d_curModule(0)
{
    s_this = this;

    d_pro = new Project2(this);

    d_tab = new DocTab(this);
    d_tab->setCloserIcon( ":/images/close.png" );
    Gui::AutoMenu* pop = new Gui::AutoMenu( d_tab, true );
    pop->addCommand( tr("Forward Tab"), d_tab, SLOT(onDocSelect()), tr(OBN_NEXTDOC_SC) );
    pop->addCommand( tr("Backward Tab"), d_tab, SLOT(onDocSelect()), tr(OBN_PREVDOC_SC) );
    pop->addCommand( tr("Close Tab"), d_tab, SLOT(onCloseDoc()), tr("CTRL+W") );
    pop->addCommand( tr("Close All"), d_tab, SLOT(onCloseAll()) );
    pop->addCommand( tr("Close All Others"), d_tab, SLOT(onCloseAllButThis()) );
    addTopCommands( pop );

    new Gui::AutoShortcut( tr(OBN_NEXTDOC_SC), this, d_tab, SLOT(onDocSelect()) );
    new Gui::AutoShortcut( tr(OBN_PREVDOC_SC), this, d_tab, SLOT(onDocSelect()) );
    new Gui::AutoShortcut( tr("CTRL+W"), this, d_tab, SLOT(onCloseDoc()) );

    connect( d_tab, SIGNAL( currentChanged(int) ), this, SLOT(onTabChanged() ) );
    connect( d_tab, SIGNAL(closing(int)), this, SLOT(onTabClosing(int)) );

    setDockNestingEnabled(true);
    setCorner( Qt::BottomRightCorner, Qt::RightDockWidgetArea );
    setCorner( Qt::BottomLeftCorner, Qt::LeftDockWidgetArea );
    setCorner( Qt::TopRightCorner, Qt::RightDockWidgetArea );
    setCorner( Qt::TopLeftCorner, Qt::LeftDockWidgetArea );

    createMods();
    createMod();
    createHier();
    createXref();
    createErrs();
    createMenu();

    setCentralWidget(d_tab);

    createMenuBar();

    s_oldHandler = qInstallMessageHandler(messageHander);

    QSettings s;

    const QRect screen = QApplication::desktop()->screenGeometry();
    resize( screen.width() - 20, screen.height() - 30 ); // so that restoreState works
    if( s.value("Fullscreen").toBool() )
        showFullScreen();
    else
        showMaximized();

    const QVariant state = s.value( "DockState" );
    if( !state.isNull() )
        restoreState( state.toByteArray() );


    connect( d_pro,SIGNAL(sigRenamed()),this,SLOT(onCaption()) );
    connect( d_pro,SIGNAL(sigModified(bool)),this,SLOT(onCaption()) );
}

Ide::~Ide()
{
}

void Ide::loadFile(const QString& path)
{
    QFileInfo info(path);

    if( info.isDir() && info.suffix() != ".obpro" )
    {
        d_pro->initializeFromDir( path );
    }else
    {
        d_pro->loadFrom(path);
    }

    QDir::setCurrent(QFileInfo(path).absolutePath());

    onCaption();

    onCompile();
}

void Ide::logMessage(const QString& str, bool err)
{
    // TODO d_term->printText(str,err);
}

void Ide::closeEvent(QCloseEvent* event)
{
    QSettings s;
    s.setValue( "DockState", saveState() );
    const bool ok = checkSaved( tr("Quit Application"));
    event->setAccepted(ok);
}



void Ide::createMods()
{
    QDockWidget* dock = new QDockWidget( tr("Modules"), this );
    dock->setObjectName("Modules");
    dock->setAllowedAreas( Qt::AllDockWidgetAreas );
    dock->setFeatures( QDockWidget::DockWidgetMovable );
    d_mods = new QTreeWidget(dock);
    d_mods->setHeaderHidden(true);
    d_mods->setExpandsOnDoubleClick(false);
    d_mods->setAlternatingRowColors(true);
    dock->setWidget(d_mods);
    addDockWidget( Qt::LeftDockWidgetArea, dock );
    connect( d_mods, SIGNAL(itemDoubleClicked(QTreeWidgetItem*,int)),this,SLOT(onModsDblClicked(QTreeWidgetItem*,int)) );
}

void Ide::createMod()
{
    QDockWidget* dock = new QDockWidget( tr("Module"), this );
    dock->setObjectName("Module");
    dock->setAllowedAreas( Qt::AllDockWidgetAreas );
    dock->setFeatures( QDockWidget::DockWidgetMovable | QDockWidget::DockWidgetClosable );
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
    connect( d_mod, SIGNAL(itemDoubleClicked(QTreeWidgetItem*,int)),this,SLOT(onModDblClicked(QTreeWidgetItem*,int)) );
}

void Ide::createHier()
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

void Ide::createErrs()
{
    QDockWidget* dock = new QDockWidget( tr("Issues"), this );
    dock->setObjectName("Issues");
    dock->setAllowedAreas( Qt::AllDockWidgetAreas );
    dock->setFeatures( QDockWidget::DockWidgetMovable | QDockWidget::DockWidgetClosable );
    d_errs = new QTreeWidget(dock);
    d_errs->setSizePolicy(QSizePolicy::MinimumExpanding,QSizePolicy::Preferred);
    d_errs->setAlternatingRowColors(true);
    d_errs->setHeaderHidden(true);
    d_errs->setSortingEnabled(false);
    d_errs->setAllColumnsShowFocus(true);
    d_errs->setRootIsDecorated(false);
    d_errs->setColumnCount(3);
    d_errs->header()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
    d_errs->header()->setSectionResizeMode(1, QHeaderView::ResizeToContents);
    d_errs->header()->setSectionResizeMode(2, QHeaderView::Stretch);
    dock->setWidget(d_errs);
    addDockWidget( Qt::BottomDockWidgetArea, dock );
    connect(d_errs, SIGNAL(itemDoubleClicked(QTreeWidgetItem*,int)), this, SLOT(onErrorsDblClicked()) );
    connect( new QShortcut( tr("ESC"), this ), SIGNAL(activated()), dock, SLOT(hide()) );
}

void Ide::createXref()
{
    QDockWidget* dock = new QDockWidget( tr("Xref"), this );
    dock->setObjectName("Xref");
    dock->setAllowedAreas( Qt::AllDockWidgetAreas );
    dock->setFeatures( QDockWidget::DockWidgetMovable | QDockWidget::DockWidgetClosable );
    QWidget* pane = new QWidget(dock);
    QVBoxLayout* vbox = new QVBoxLayout(pane);
    vbox->setMargin(0);
    vbox->setSpacing(0);
    d_xrefTitle = new QLabel(pane);
    d_xrefTitle->setMargin(2);
    d_xrefTitle->setWordWrap(true);
    vbox->addWidget(d_xrefTitle);
    d_xref = new QTreeWidget(pane);
    d_xref->setAlternatingRowColors(true);
    d_xref->setHeaderHidden(true);
    d_xref->setAllColumnsShowFocus(true);
    d_xref->setRootIsDecorated(false);
    vbox->addWidget(d_xref);
    dock->setWidget(pane);
    addDockWidget( Qt::LeftDockWidgetArea, dock );
    connect(d_xref, SIGNAL(itemDoubleClicked(QTreeWidgetItem*,int)), this, SLOT(onXrefDblClicked()) );
}

void Ide::createMenu()
{
    Gui::AutoMenu* pop = new Gui::AutoMenu( d_mods, true );
    pop->addCommand( "Show File", this, SLOT(onOpenFile()) );
    pop->addAction("Expand all", d_mods, SLOT(expandAll()) );
    pop->addSeparator();
    pop->addCommand( "New Project", this, SLOT(onNewPro()), tr("CTRL+N"), false );
    pop->addCommand( "Open Project...", this, SLOT(onOpenPro()), tr("CTRL+O"), false );
    pop->addCommand( "Save Project", this, SLOT(onSavePro()), tr("CTRL+SHIFT+S"), false );
    pop->addCommand( "Save Project as...", this, SLOT(onSaveAs()) );
    pop->addSeparator();
    pop->addCommand( "New Module...", this, SLOT(onNewModule()), tr("CTRL+SHIFT+N"), false );
    pop->addCommand( "Add existing Modules...", this, SLOT(onAddFiles()) );
    pop->addCommand( "Remove Module...", this, SLOT(onRemoveFile()) );
    pop->addCommand( "Export minimized Module...", this, SLOT(onExpMod()) );
    pop->addSeparator();
    pop->addCommand( "Add Import Path...", this, SLOT(onAddDir()) );
    pop->addCommand( "Remove Import Path...", this, SLOT(onRemoveDir()) );
    pop->addSeparator();
    pop->addCommand( "Built-in Oakwood", this, SLOT(onOakwood()) );
    pop->addCommand( "Set Configuration Variables...", this, SLOT( onSetOptions()) );
    //pop->addCommand( "File System Root...", this, SLOT( onWorkingDir() ) );
    pop->addSeparator();
    pop->addCommand( "Compile", this, SLOT(onCompile()), tr("CTRL+T"), false );
    pop->addCommand( "Export C99...", this, SLOT(onExportC()) );
    addTopCommands(pop);

    new Gui::AutoShortcut( tr("CTRL+O"), this, this, SLOT(onOpenPro()) );
    new Gui::AutoShortcut( tr("CTRL+N"), this, this, SLOT(onNewPro()) );
    new Gui::AutoShortcut( tr("CTRL+SHIFT+N"), this, this, SLOT(onNewModule()) );
    new Gui::AutoShortcut( tr("CTRL+SHIFT+S"), this, this, SLOT(onSavePro()) );
    new Gui::AutoShortcut( tr("CTRL+S"), this, this, SLOT(onSaveFile()) );
    new Gui::AutoShortcut( tr("CTRL+T"), this, this, SLOT(onCompile()) );
    new Gui::AutoShortcut( tr(OBN_GOBACK_SC), this, this, SLOT(handleGoBack()) );
    new Gui::AutoShortcut( tr(OBN_GOFWD_SC), this, this, SLOT(handleGoForward()) );
}

void Ide::createMenuBar()
{
    Gui::AutoMenu* pop = new Gui::AutoMenu( tr("File"), this );
    pop->addCommand( "New Project", this, SLOT(onNewPro()), tr("CTRL+N"), false );
    pop->addCommand( "Open Project...", this, SLOT(onOpenPro()), tr("CTRL+O"), false );
    pop->addCommand( "Save Project", this, SLOT(onSavePro()), tr("CTRL+SHIFT+S"), false );
    pop->addCommand( "Save Project as...", this, SLOT(onSaveAs()) );
    pop->addSeparator();
    pop->addCommand( "Save", this, SLOT(onSaveFile()), tr("CTRL+S"), false );
    pop->addCommand( tr("Close file"), d_tab, SLOT(onCloseDoc()), tr("CTRL+W") );
    pop->addCommand( tr("Close all"), d_tab, SLOT(onCloseAll()) );
    pop->addSeparator();
    pop->addAutoCommand( "Print...", SLOT(handlePrint()), tr("CTRL+P"), true );
    pop->addAutoCommand( "Export PDF...", SLOT(handleExportPdf()), tr("CTRL+SHIFT+P"), true );
    pop->addSeparator();
    pop->addAction(tr("Quit"),qApp,SLOT(quit()), tr("CTRL+Q") );

    pop = new Gui::AutoMenu( tr("Edit"), this );
    pop->addAutoCommand( "Undo", SLOT(handleEditUndo()), tr("CTRL+Z"), true );
    pop->addAutoCommand( "Redo", SLOT(handleEditRedo()), tr("CTRL+Y"), true );
    pop->addSeparator();
    pop->addAutoCommand( "Cut", SLOT(handleEditCut()), tr("CTRL+X"), true );
    pop->addAutoCommand( "Copy", SLOT(handleEditCopy()), tr("CTRL+C"), true );
    pop->addAutoCommand( "Paste", SLOT(handleEditPaste()), tr("CTRL+V"), true );
    pop->addSeparator();
    pop->addAutoCommand( "Find...", SLOT(handleFind()), tr("CTRL+F"), true );
    pop->addAutoCommand( "Find again", SLOT(handleFindAgain()), tr("F3"), true );
    pop->addAutoCommand( "Replace...", SLOT(handleReplace()) );
    pop->addSeparator();
    pop->addAutoCommand( "&Go to line...", SLOT(handleGoto()), tr("CTRL+G"), true );
    pop->addSeparator();
    pop->addAutoCommand( "Indent", SLOT(handleIndent()) );
    pop->addAutoCommand( "Unindent", SLOT(handleUnindent()) );
    pop->addAutoCommand( "Fix Indents", SLOT(handleFixIndent()) );
    pop->addAutoCommand( "Set Indentation Level...", SLOT(handleSetIndent()) );

    pop = new Gui::AutoMenu( tr("Project"), this );
    pop->addCommand( "New Module...", this, SLOT(onNewModule()), tr("CTRL+SHIFT+N"), false );
    pop->addCommand( "Add existing Modules...", this, SLOT(onAddFiles()) );
    pop->addCommand( "Remove Module...", this, SLOT(onRemoveFile()) );
    pop->addSeparator();
    pop->addCommand( "Built-in Oakwood", this, SLOT(onOakwood()) );
    pop->addCommand( "Set Configuration Variables...", this, SLOT( onSetOptions()) );
    //pop->addCommand( "File System Root...", this, SLOT( onWorkingDir() ) );

    pop = new Gui::AutoMenu( tr("Build"), this );
    pop->addCommand( "Compile", this, SLOT(onCompile()), tr("CTRL+T"), false );
    pop->addCommand( "Export C99...", this, SLOT(onExportC()) );


    pop = new Gui::AutoMenu( tr("Window"), this );
    pop->addCommand( tr("Next Tab"), d_tab, SLOT(onDocSelect()), tr(OBN_NEXTDOC_SC) );
    pop->addCommand( tr("Previous Tab"), d_tab, SLOT(onDocSelect()), tr(OBN_PREVDOC_SC) );
    pop->addSeparator();
    pop->addCommand( "Go Back", this, SLOT(handleGoBack()), tr(OBN_GOBACK_SC), false );
    pop->addCommand( "Go Forward", this, SLOT(handleGoForward()), tr(OBN_GOFWD_SC), false );
    pop->addSeparator();
    pop->addAutoCommand( "Set &Font...", SLOT(handleSetFont()) );
    pop->addAutoCommand( "Show &Linenumbers", SLOT(handleShowLinenumbers()) );
    pop->addCommand( "Show Fullscreen", this, SLOT(onFullScreen()) );
    pop->addSeparator();
    QMenu* sub2 = createPopupMenu();
    sub2->setTitle( tr("Show Window") );
    pop->addMenu( sub2 );

    Gui::AutoMenu* help = new Gui::AutoMenu( tr("Help"), this, true );
    help->addCommand( "&About this application...", this, SLOT(onAbout()) );
    help->addCommand( "&About Qt...", this, SLOT(onQt()) );
}

void Ide::onSetOptions()
{
    ENABLED_IF(true);

    QByteArrayList l = d_pro->getOptions();
    qSort(l);

    bool ok;
    const QString options = QInputDialog::getMultiLineText(this,tr("Set Configuration Variables"),
                                                           tr("Please enter a unique identifier per variable:"),
                                                           l.join('\n'), &ok );
    if( !ok )
        return;

    Lexer lex;
    QList<Token> toks = lex.tokens(options);
    l.clear();
    QStringList errs;
    foreach( const Token& t, toks )
    {
        if( t.d_type == Tok_ident )
            l << t.d_val;
        else
            errs << QString::fromUtf8(t.d_val);
    }

    if( !errs.isEmpty() )
        QMessageBox::warning(this,tr("Set Configuration Variables"),
                             tr("The following entries are illegal and ignored: \n%1").arg(errs.join('\n')));

    d_pro->setOptions(l);
}

void Ide::onCompile()
{
    ENABLED_IF(true);
    compile();
}

void Ide::onNewPro()
{
    ENABLED_IF(true);

    if( !checkSaved( tr("New Project")) )
        return;

    // we need a path up front because this path is also the first root path to the source code
    QString fileName = QFileDialog::getSaveFileName(this, tr("New Project"),
                                                          QFileInfo(d_pro->getProjectPath()).absolutePath(),
                                                          tr("Oberon Project (*.obpro)") );

    if (fileName.isEmpty())
        return;

    QDir::setCurrent(QFileInfo(fileName).absolutePath());

    if( !fileName.endsWith(".obpro",Qt::CaseInsensitive ) )
        fileName += ".obpro";

    d_pro->createNew();
    d_tab->onCloseAll();
    compile();


    d_pro->saveTo(fileName);

}

void Ide::onOpenPro()
{
    ENABLED_IF( true );

    if( !checkSaved( tr("New Project")) )
        return;

    const QString fileName = QFileDialog::getOpenFileName(this, tr("Open Project"),QString(),
                                                          tr("Oberon Project (*.obpro)") );
    if (fileName.isEmpty())
        return;

    QDir::setCurrent(QFileInfo(fileName).absolutePath());

    d_tab->onCloseAll();
    clear();
    d_pro->loadFrom(fileName);

    compile();
}

void Ide::onSavePro()
{
    ENABLED_IF( d_pro->isDirty() );

    if( !d_pro->getProjectPath().isEmpty() )
        d_pro->save();
    else
        onSaveAs();
}

void Ide::onSaveFile()
{
    Editor* edit = static_cast<Editor*>( d_tab->getCurrentTab() );
    ENABLED_IF( edit && edit->isModified() );

    edit->saveToFile( edit->getPath() );
    Project2::File* f = d_pro->findFile( edit->getPath() );
    if( f )
        f->d_cache.clear();
}

void Ide::onSaveAs()
{
    ENABLED_IF(true);

    QString fileName = QFileDialog::getSaveFileName(this, tr("Save Project"),
                                                          QFileInfo(d_pro->getProjectPath()).absolutePath(),
                                                          tr("Oberon Project (*.obpro)") );

    if (fileName.isEmpty())
        return;

    QDir::setCurrent(QFileInfo(fileName).absolutePath());

    if( !fileName.endsWith(".obpro",Qt::CaseInsensitive ) )
        fileName += ".obpro";

    d_pro->saveTo(fileName);
    onCaption();
}

void Ide::onCaption()
{
    const QString star = d_pro->isDirty() || d_filesDirty ? "*" : "";
    if( d_pro->getProjectPath().isEmpty() )
    {
        setWindowTitle(tr("<unnamed>%2 - %1").arg(qApp->applicationName()).arg(star));
    }else
    {
        QFileInfo info(d_pro->getProjectPath());
        setWindowTitle(tr("%1%2 - %3").arg(info.fileName()).arg(star).arg(qApp->applicationName()) );
    }
}

void Ide::onGotoLnr(quint32 lnr)
{
    if( d_lock )
        return;
    d_lock = true;
    Editor* edit = static_cast<Editor*>( d_tab->getCurrentTab() );
    if( edit )
    {
        const int row = RowCol::unpackRow(lnr)-1;
        const int col = RowCol::unpackCol(lnr)-1;
        edit->setCursorPosition(row,col);
        edit->setFocus();
    }
    d_lock = false;
}

void Ide::onFullScreen()
{
    CHECKED_IF(true,isFullScreen());
    QSettings s;
    if( isFullScreen() )
    {
        showMaximized();
        s.setValue("Fullscreen", false );
    }else
    {
        showFullScreen();
        s.setValue("Fullscreen", true );
    }
}

void Ide::onCursor()
{
    fillXref();
    if( d_lock )
        return;
    d_lock = true;
    Editor* edit = static_cast<Editor*>( d_tab->getCurrentTab() );
    if( edit )
    {
        int row, col;
        edit->getCursorPosition(&row,&col);
    }
    d_lock = false;
}

void Ide::onModsDblClicked(QTreeWidgetItem* item, int)
{
    Declaration* s = item->data(0,Qt::UserRole).value<Declaration*>();
    if( s )
        showEditor( s );
    else
        showEditor(item->toolTip(0));
}

void Ide::onModDblClicked(QTreeWidgetItem* item, int)
{
    Declaration* s = item->data(0,Qt::UserRole).value<Declaration*>();
    if( s == 0 )
        return;

    d_curModule = s->getModule();
    d_lock2 = true;
    const QString path = d_tab->getCurrentDoc().toString();
    item->setExpanded(true);
    showEditor( path, s->pos.d_row, s->pos.d_col, false, true );
    d_lock2 = false;
}

void Ide::onHierDblClicked(QTreeWidgetItem* item, int)
{
    Declaration* s = item->data(0,Qt::UserRole).value<Declaration*>();
    if( s == 0 )
        return;

    d_lock4 = true;
    Declaration* mod = s->getModule();
    if( mod )
    {
        ModuleData md = mod->data.value<ModuleData>();
        showEditor( md.sourcePath, s->pos.d_row, s->pos.d_col, false, true );
        item->setExpanded(true);
    }
    d_lock4 = false;
}

void Ide::onTabChanged()
{
    const QString path = d_tab->getCurrentDoc().toString();

    onEditorChanged();

    if( !path.isEmpty() )
    {
        Project2::File* f = d_pro->findFile(path);
        if( f )
        {
            fillModule(f->d_mod);
            onCursor();
            return;
        }
    }
    // else
    fillModule(0);
}

void Ide::onTabClosing(int i)
{
    d_pro->findFile(d_tab->getDoc(i).toString())->d_cache.clear();
}

void Ide::onEditorChanged()
{
    // only fired once when editor switches from unmodified to modified and back
    // not fired for every key press
    d_filesDirty = false;
    for( int i = 0; i < d_tab->count(); i++ )
    {
        Editor* e = static_cast<Editor*>( d_tab->widget(i) );
        if( e->isModified() )
            d_filesDirty = true;
        const QString path = d_tab->getDoc(i).toString();
        Project2::File* f = d_pro->findFile(path);
        QString name;
        if( f && f->d_mod )
            name = f->d_mod->name;
        else
            name = QFileInfo( path ).fileName();
        d_tab->setTabText(i, name + ( e->isModified() ? "*" : "" ) );
        d_tab->setTabToolTip( i, path );
    }
    onCaption();
}

void Ide::onErrorsDblClicked()
{
    QTreeWidgetItem* item = d_errs->currentItem();
    showEditor( item->data(0, Qt::UserRole ).toString(),
                item->data(1, Qt::UserRole ).toInt(), item->data(2, Qt::UserRole ).toInt() );
}

void Ide::onErrors()
{
    d_errs->clear();
    QList<Project2::Error> errs = d_pro->getErrors();

    for( int i = 0; i < errs.size(); i++ )
    {
        QTreeWidgetItem* item = new QTreeWidgetItem(d_errs);
        item->setText(2, errs[i].msg );
        item->setToolTip(2, item->text(2) );
        //if( errs[i].d_isErr )
            item->setIcon(0, QPixmap(":/images/exclamation-red.png") );
#if 0
        else
            item->setIcon(0, QPixmap(":/images/exclamation-circle.png") );
#endif
        Project2::File* f = d_pro->findFile(errs[i].path);
        if( f && f->d_mod )
            item->setText(0, f->d_mod->name );
        else
            item->setText(0, QFileInfo(errs[i].path).completeBaseName() );
        item->setToolTip(0, errs[i].path );
        item->setText(1, QString("%1:%2").arg(errs[i].pos.d_row).arg(errs[i].pos.d_col));
        item->setData(0, Qt::UserRole, errs[i].path );
        item->setData(1, Qt::UserRole, errs[i].pos.d_row );
        item->setData(2, Qt::UserRole, errs[i].pos.d_col );
    }
    if( errs.size() )
        d_errs->parentWidget()->show();

    for( int i = 0; i < d_tab->count(); i++ )
    {
        Editor* e = static_cast<Editor*>( d_tab->widget(i) );
        Q_ASSERT( e );
        e->updateExtraSelections();
    }
}

void Ide::onOpenFile()
{
    ENABLED_IF( d_mods->currentItem() );

    onModsDblClicked( d_mods->currentItem(), 0 );
}

void Ide::onOakwood()
{
    CHECKED_IF( true, d_pro->useBuiltInOakwood() );

    d_pro->setUseBuiltInOakwood( !d_pro->useBuiltInOakwood() );
}

void Ide::onAddFiles()
{
    ENABLED_IF(true);

    QByteArrayList path;

    QList<QTreeWidgetItem*> sel = d_mods->selectedItems();
    if( sel.size() == 1 && sel.first()->type() == 1 )
        path = sel.first()->text(0).toLatin1().split('.');

    QString filter;
    foreach( const QString& suf, d_pro->getSuffixes() )
        filter += " *" + suf;
    const QStringList files = QFileDialog::getOpenFileNames(this,tr("Add Modules"),QString(),filter );
    foreach( const QString& f, files )
    {
        if( !d_pro->addFile(f,path) )
            qWarning() << "cannot add module" << f;
    }
    compile();
}

static bool isValidIdent( const QString& name )
{
    Q_ASSERT(!name.isEmpty());
    if( !name[0].isLetter() || name[0].unicode() > 255 )
        return false;
    for( int i = 1; i < name.size(); i++ )
    {
        if( !name[0].isLetterOrNumber() || name[0].unicode() > 255 )
            return false;
    }
    return true;
}
void Ide::onNewModule()
{
    ENABLED_IF(true);

    QByteArrayList path;

    QList<QTreeWidgetItem*> sel = d_mods->selectedItems();
    if( sel.size() == 1 && sel.first()->type() == 1 )
        path = sel.first()->text(0).toLatin1().split('.');

    const QString name = QInputDialog::getText(this,tr("New Module"), tr("Enter a unique module name:") ).toLatin1();
    if( name.isEmpty() )
        return;

    if( !isValidIdent(name) )
    {
        QMessageBox::critical(this,tr("New Module"), tr("'%1' is not a valid module name").arg(name) );
        return;
    }
    QDir dir;
    const Project2::FileGroup* fg = d_pro->findFileGroup(path);
    for( int i = 0; i < fg->d_files.size(); i++ )
    {
        if( i == 0 )
            dir = fg->d_files[i]->d_filePath;
        if( fg->d_files[i]->d_mod != 0 && fg->d_files[i]->d_mod->name == name )
        {
            QString where;
            if( !path.isEmpty() )
                where = " in " + path.join('.');
            QMessageBox::critical(this,tr("New Module"), tr("'%1' is not unique%2")
                                  .arg(name.constData()).arg(where) );
            return;
        }
    }

    QString filePath = QFileDialog::getSaveFileName(this,tr("New Module"), dir.absoluteFilePath(name + ".luon"),"*.luon");
    if( filePath.isEmpty() )
        return;

    if( !filePath.toLower().endsWith(".luon") )
        filePath += ".luon";

    QFile f(filePath);
    if( !f.open(QIODevice::WriteOnly) )
    {
        QMessageBox::critical(this,tr("New Module"), tr("Cannot open file for writing: '%1'").arg(filePath) );
        return;
    }
    f.write("module ");
    f.write(name.toUtf8());
    f.write("\n\n\n");
    f.write("end ");
    f.write(name.toUtf8());
    f.write("\n");
    f.close();

    if( !d_pro->addFile(filePath,path) )
        qWarning() << "cannot add module" << filePath;
    compile();
}

void Ide::onAddDir()
{
    ENABLED_IF(true);

    bool ok;
    const QByteArray path = QInputDialog::getText(this,tr("Add Inport Path"), tr("Idents separated by '.':"),
                                                  QLineEdit::Normal, QString(), &ok ).toLatin1();
    if( !ok )
        return;
    if( !path.isEmpty() && !::isalpha( path[0] ) )
    {
        QMessageBox::critical(this,tr("Add Inport Path"),tr("import path starts with invalid character: %1").arg(path[0]));
        return;
    }
    for( int i = 1; i < path.size(); i++ )
    {
        const char ch = path[i];
        if( !::isalnum(ch) && ch != '.' )
        {
            QMessageBox::critical(this,tr("Add Inport Path"),tr("invalid character in import path: %1").arg(ch));
            return;
        }
    }
    QByteArrayList segments = path.split('.');
    for( int i = 0; i < segments.size(); i++ )
    {
        if( segments[i].isEmpty() )
        {
            QMessageBox::critical(this,tr("Add Inport Path"),tr("identifier cannot be empty"));
            return;
        }
    }
    d_pro->addPackagePath(segments);
    fillMods();
}

void Ide::onRemoveFile()
{
    ENABLED_IF( d_mods->currentItem() && d_mods->currentItem()->type() == 0 );

    Project2::File* f = d_mods->currentItem()->data(0,Qt::UserRole).value<Project2::File*>();
    if( f == 0 )
        return;

    if( QMessageBox::warning( this, tr("Remove Module"),
                              tr("Do you really want to remove module '%1' from project?").arg(f->d_name.constData()),
                           QMessageBox::Yes | QMessageBox::Cancel, QMessageBox::Yes ) != QMessageBox::Yes )
        return;

    for( int i = 0; i < d_tab->count(); i++ )
    {
        Editor* e = static_cast<Editor*>( d_tab->widget(i) );
        if( e->getPath() == f->d_filePath )
        {
            d_tab->closeTab(i);
            break;
        }
    }

    if( !d_pro->removeFile( f->d_filePath ) )
        qWarning() << "cannot remove module" << f->d_name;
    else
        compile();
}

void Ide::onRemoveDir()
{
    ENABLED_IF( d_mods->currentItem() && d_mods->currentItem()->type() == 1
                && d_mods->currentItem()->childCount() == 0 );

    if( QMessageBox::warning( this, tr("Remove Import Path"),
                              tr("Do you really want to remove '%1' from project?").arg(d_mods->currentItem()->text(0)),
                           QMessageBox::Yes | QMessageBox::Cancel, QMessageBox::Yes ) != QMessageBox::Yes )
        return;
    QByteArrayList path = d_mods->currentItem()->text(0).toLatin1().split('.');
    if( !d_pro->removePackagePath( path ) )
        qWarning() << "cannot remove import path" << d_mods->currentItem()->text(0);
    fillMods();
}

bool Ide::checkSaved(const QString& title)
{
    if( d_filesDirty )
    {
        switch( QMessageBox::critical( this, title, tr("There are modified files; do you want to save them?"),
                               QMessageBox::Yes | QMessageBox::No | QMessageBox::Cancel, QMessageBox::Yes ) )
        {
        case QMessageBox::Yes:
            // TODO
            break;
        case QMessageBox::No:
            break;
        default:
            return false;
        }
    }
    if( d_pro->isDirty() )
    {
        switch( QMessageBox::critical( this, title, tr("The the project has not been saved; do you want to save it?"),
                               QMessageBox::Yes | QMessageBox::No | QMessageBox::Cancel, QMessageBox::Yes ) )
        {
        case QMessageBox::Yes:
            if( !d_pro->getProjectPath().isEmpty() )
                return d_pro->save();
            else
            {
                const QString path = QFileDialog::getSaveFileName( this, title, QString(), "Oberon Project (*.obpro)" );
                if( path.isEmpty() )
                    return false;
                QDir::setCurrent(QFileInfo(path).absolutePath());
                return d_pro->saveTo(path);
            }
            break;
        case QMessageBox::No:
            return true;
        default:
            return false;
        }
    }
    return true;
}

bool Ide::compile()
{
    for( int i = 0; i < d_tab->count(); i++ )
    {
        Editor* e = static_cast<Editor*>( d_tab->widget(i) );
        Project2::File* f = d_pro->findFile( e->getPath() );
        if( f == 0 )
            continue;
        if( e->isModified() )
            f->d_cache = e->toPlainText().toUtf8();
        else
            f->d_cache.clear();
    }
    d_pro->parse();
    onErrors();
    fillMods();
    fillModule(0);
    fillHier(0);
    fillXref();
    onTabChanged();
    return true;
}

static bool sortNamed( const Project2::File* lhs, const Project2::File* rhs )
{
    QByteArray l;
    l = lhs->d_name;
    QByteArray r;
    r = rhs->d_name;
    return l.toLower() < r.toLower();
}

typedef QPair<QByteArray,QList<Project2::File*> > Group;

static bool sortNamed1( const Group& lhs, const Group& rhs )
{
    return lhs.first.toLower() < rhs.first.toLower();
}

typedef QList<Project2::File*> ModuleSort;

template<class T>
static void fillModTree( T* parent, const ModuleSort& mods )
{
    foreach( Project2::File* m, mods )
    {
        QTreeWidgetItem* item = new QTreeWidgetItem(parent);
        if( m->d_mod )
            item->setText(0,m->d_mod->name);
        else
            item->setText(0, m->d_name.constData() );
        item->setToolTip(0,m->d_filePath);
#if 0
        if( m->d_isDef )
            item->setIcon(0, QPixmap(":/images/definition.png") );
        else
#endif
            item->setIcon(0, QPixmap(":/images/module.png") );
        item->setData(0,Qt::UserRole,QVariant::fromValue(m) );
    }
}

void Ide::fillMods()
{
    d_mods->clear();

    const Project2::FileGroups& paths = d_pro->getFileGroups();
    typedef QList<Group> Sort1;
    Sort1 sort1;
    foreach( const Project2::FileGroup& fg, paths )
        sort1.append( qMakePair( fg.d_package.join('.'), fg.d_files ) );
    std::sort( sort1.begin(), sort1.end(), sortNamed1 );

    for( int j = 0; j < sort1.size(); j++ )
    {
        QTreeWidgetItem* item = 0;
        if( !sort1[j].first.isEmpty() )
        {
            item = new QTreeWidgetItem(d_mods,1);
            item->setText(0, sort1[j].first);
            item->setToolTip( 0, item->text(0) );
            item->setIcon(0, QPixmap(":/images/folder.png") );
        }

        const QList<Project2::File*>& files = sort1[j].second;
        ModuleSort sort;
        for( int i = 0; i < files.size(); i++ )
            sort << files[i];
        std::sort( sort.begin(), sort.end(), sortNamed );
        if( item )
            fillModTree(item,sort);
        else
            fillModTree( d_mods, sort );
    }
    d_mods->expandAll();
}

void Ide::addTopCommands(Gui::AutoMenu* pop)
{
    Q_ASSERT( pop != 0 );
    pop->addSeparator();
    pop->addCommand( "Go Back", this, SLOT(handleGoBack()), tr(OBN_GOBACK_SC), false );
    pop->addCommand( "Go Forward", this, SLOT(handleGoForward()), tr(OBN_GOFWD_SC), false );
    pop->addSeparator();
    pop->addAutoCommand( "Set &Font...", SLOT(handleSetFont()) );
    pop->addAutoCommand( "Show &Linenumbers", SLOT(handleShowLinenumbers()) );
    pop->addCommand( "Show Fullscreen", this, SLOT(onFullScreen()) );
    pop->addSeparator();
    pop->addAction(tr("Quit"),qApp,SLOT(quit()) );
}

Ide::Editor* Ide::showEditor(const QString& path, int row, int col, bool setMarker, bool center )
{
    QString filePath = path;
    Project2::File* f = d_pro->findFile(path);
    if( f == 0 )
        return 0;
    filePath = f->d_filePath;

    const int i = d_tab->findDoc(filePath);
    Editor* edit = 0;
    if( i != -1 )
    {
        d_tab->setCurrentIndex(i);
        edit = static_cast<Editor*>( d_tab->widget(i) );
    }else
    {
        edit = new Editor(this,d_pro);
        createMenu(edit);

        connect(edit, SIGNAL(modificationChanged(bool)), this, SLOT(onEditorChanged()) );
        connect(edit,SIGNAL(cursorPositionChanged()),this,SLOT(onCursor()));
        connect(edit,SIGNAL(sigUpdateLocation(int,int)),this,SLOT(onUpdateLocation(int,int)));

        edit->setExt(true);
        edit->loadFromFile(filePath);

        d_tab->addDoc(edit,filePath);
        onEditorChanged();
    }
    if( row > 0 && col > 0 )
    {
        edit->setCursorPosition( row-1, col-1, center );
        if( setMarker )
            edit->setPositionMarker(row-1);
    }
    edit->setFocus();
    return edit;
}

void Ide::showEditor(Declaration* n, bool setMarker, bool center)
{
    if( n == 0 )
        return;
    Declaration* mod = n->getModule();
    if( mod )
    {
        d_curModule = mod;
        ModuleData md = mod->data.value<ModuleData>();
        showEditor( md.sourcePath, n->pos.d_row, n->pos.d_col, setMarker, center );
    }
}

void Ide::showEditor(const Ide::Location& loc)
{
    Editor* e = showEditor( loc.d_file, loc.d_line+1, loc.d_col+1 );
    if( e )
        e->verticalScrollBar()->setValue(loc.d_yoff);
}

void Ide::createMenu(Ide::Editor* edit)
{
    Gui::AutoMenu* pop = new Gui::AutoMenu( edit, true );
    pop->addCommand( "Save", this, SLOT(onSaveFile()), tr("CTRL+S"), false );
    pop->addSeparator();
    pop->addCommand( "Compile", this, SLOT(onCompile()), tr("CTRL+T"), false );
    pop->addSeparator();
    pop->addCommand( "Undo", edit, SLOT(handleEditUndo()), tr("CTRL+Z"), true );
    pop->addCommand( "Redo", edit, SLOT(handleEditRedo()), tr("CTRL+Y"), true );
    pop->addSeparator();
    pop->addCommand( "Cut", edit, SLOT(handleEditCut()), tr("CTRL+X"), true );
    pop->addCommand( "Copy", edit, SLOT(handleEditCopy()), tr("CTRL+C"), true );
    pop->addCommand( "Paste", edit, SLOT(handleEditPaste()), tr("CTRL+V"), true );
    pop->addSeparator();
    pop->addCommand( "Find...", edit, SLOT(handleFind()), tr("CTRL+F"), true );
    pop->addCommand( "Find again", edit, SLOT(handleFindAgain()), tr("F3"), true );
    pop->addCommand( "Replace...", edit, SLOT(handleReplace()) );
    pop->addSeparator();
    pop->addCommand( "&Goto...", edit, SLOT(handleGoto()), tr("CTRL+G"), true );
    pop->addSeparator();
    pop->addCommand( "Indent", edit, SLOT(handleIndent()) );
    pop->addCommand( "Unindent", edit, SLOT(handleUnindent()) );
    pop->addCommand( "Fix Indents", edit, SLOT(handleFixIndent()) );
    pop->addCommand( "Set Indentation Level...", edit, SLOT(handleSetIndent()) );
    pop->addSeparator();
    pop->addCommand( "Print...", edit, SLOT(handlePrint()), tr("CTRL+P"), true );
    pop->addCommand( "Export PDF...", edit, SLOT(handleExportPdf()), tr("CTRL+SHIFT+P"), true );
    addTopCommands(pop);
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

void Ide::fillXref()
{
    Editor* edit = static_cast<Editor*>( d_tab->getCurrentTab() );
    if( edit == 0 )
    {
        d_xref->clear();
        d_xrefTitle->clear();
        return;
    }
    int line, col;
    edit->getCursorPosition( &line, &col );
    line += 1;
    col += 1;
    Declaration* scope = 0;
    Symbol* hit = d_pro->findSymbolBySourcePos(edit->getPath(), line, col, &scope);
    if( hit && hit->decl )
    {
        Declaration* module = moduleOfCurrentEditor();
        fillXrefForSym(hit, module);
        syncModView(hit->decl);
        syncEditorMarks(hit->decl, module);
        fillHier(hit->decl);
    }else
        edit->markNonTerms(SymList());
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

void Ide::fillXrefForSym(Symbol* sym, Declaration* module)
{
    d_xref->clear();

    Declaration* decl = sym->decl;
    Q_ASSERT( decl != 0 );

    Project2::UsageByMod usage = d_pro->getUsage(decl);

    QFont f = d_xref->font();
    f.setBold(true);

    const QString type = declKindName(decl);

    d_xrefTitle->setText(QString("%1 '%2'").arg(type).arg(decl->name.constData()));

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
            QTreeWidgetItem* item = new QTreeWidgetItem(d_xref);
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
    if( black && !d_lock3 )
    {
        d_xref->scrollToItem(black, QAbstractItemView::PositionAtCenter);
        d_xref->setCurrentItem(black);
    }
}

void Ide::syncModView(Declaration* decl)
{
    QTreeWidgetItem* mi = d_modIdx.value(decl);
    if( mi == 0 )
        mi = d_modIdx.value(adjustForModIdx(decl));
    if( mi && !d_lock2 )
    {
        d_mod->scrollToItem(mi,QAbstractItemView::PositionAtCenter);
        mi->setExpanded(true);
        d_mod->setCurrentItem(mi);
    }
}

void Ide::syncEditorMarks(Declaration* selected, Declaration* module)
{
    Editor* edit = static_cast<Editor*>( d_tab->getCurrentTab() );
    if( edit == 0 )
        return;
    Symbol* syms = d_pro->getSymbolsOfModule(module);
    if( syms == 0 )
    {
        edit->markNonTerms(SymList());
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

    edit->markNonTerms(marks);
}

Declaration*Ide::moduleOfCurrentEditor()
{
    Editor* edit = static_cast<Editor*>( d_tab->getCurrentTab() );
    if( edit == 0 )
        return 0;
    Project2::File* f = d_pro->findFile(edit->getPath());
    if( f == 0 )
        return 0;
    return f->d_mod;
}

static void fillModItems(QTreeWidgetItem* item, Declaration* n, Declaration* p, Type* r,
                         bool sort, QHash<Declaration*,QTreeWidgetItem*>& idx , Project2* pro);

static void fillRecord(QTreeWidgetItem* item, Declaration* n, Type* r, bool sort,
                       QHash<Declaration*,QTreeWidgetItem*>& idx, Project2* pro )
{
    fillModItems(item,n, 0, r, sort, idx, pro);
    if( r->type() )
        item->setText(0, item->text(0) + " ⇑");
    if( n->hasSubs )
    {
        DeclList subs = pro->getSubs(n);
        item->setText(0, item->text(0) + QString(" ⇓%1").arg(subs.size()));
    }
    item->setToolTip( 0, item->text(0) );
}

template<class T>
static void createModItem(T* parent, Declaration* n, Type* t, bool nonbound, bool sort,
                          QHash<Declaration*,QTreeWidgetItem*>& idx, Project2* pro )
{
    bool isAlias = false;
    if( t == 0 )
        t = n->type();
    else
        isAlias = true;
    if( t == 0 )
        return;
    if( idx.contains(n) )
    {
        // qWarning() << "fillMod recursion at" << n->getModule()->d_file << n->pos.d_row << n->name;
        return; // can legally happen if record decl contains a typedef using record, as e.g. Meta.Item.ParamCallVal.IP
    }
    switch( n->kind )
    {
    case Declaration::TypeDecl:
        switch( t->kind )
        {
        case Type::Record:
            {
                QTreeWidgetItem* item = new QTreeWidgetItem(parent);
                if( !isAlias  )
                    fillRecord(item,n,t,sort,idx, pro);
                else
                    fillModItems(item,n, 0, 0, sort, idx, pro);
            }
            break;
        case Type::NameRef:
            if( t->deref()->kind == Type::Record )
                createModItem(parent,n,t->deref(),nonbound, sort, idx, pro);
            break;
        }
        break;
    case Declaration::Procedure:
        if( !nonbound || !n->receiver )
        {
            QTreeWidgetItem* item = new QTreeWidgetItem(parent);
            fillModItems(item,n, n, 0, sort, idx, pro);
            if( n->super )
                item->setText(0, item->text(0) + " ⇑");
            if( n->hasSubs )
            {
                DeclList subs = pro->getSubs(n);
                item->setText(0, item->text(0) + QString(" ⇓%1").arg(subs.size()));
            }
            item->setToolTip( 0, item->text(0) );
        }
        break;
    }
}

template <class T>
static void walkModItems(T* parent, Declaration* p, Type* r, bool sort,
                         QHash<Declaration*,QTreeWidgetItem*>& idx, Project2* pro)
{
    typedef QMultiMap<QByteArray,Declaration*> Sort;
    if( p && sort)
    {
        Sort tmp;
        Declaration* n = p->link;
        while( n )
        {
            tmp.insert( n->name.toLower(), n );
            n = n->getNext();
        }
        Sort::const_iterator i;
        for( i = tmp.begin(); i != tmp.end(); ++i )
            createModItem(parent,i.value(),0,true, sort, idx, pro);
    }else if( p )
    {
        Declaration* n = p->link;
        while( n )
        {
            createModItem(parent,n,0,true, sort, idx, pro);
            n = n->getNext();
        }
    }
    if( r && sort )
    {
        Sort tmp;
        foreach( Declaration* n, r->subs )
        {
            if( n->kind == Declaration::Procedure )
                tmp.insert( n->name.toLower(), n );
        }
        Sort::const_iterator i;
        for( i = tmp.begin(); i != tmp.end(); ++i )
            createModItem(parent,i.value(),0,false, sort, idx, pro);
    }else if( r )
    {
        foreach( Declaration* n, r->subs )
        {
            if( n->kind == Declaration::Procedure )
                createModItem(parent,n,0,false, sort, idx, pro);
        }
    }
}

static void fillModItems( QTreeWidgetItem* item, Declaration* n, Declaration* p, Type* r,
                          bool sort, QHash<Declaration*,QTreeWidgetItem*>& idx, Project2* pro )
{
    const bool pub = n->visi > Declaration::Private;
    item->setText(0,n->name);
    item->setData(0, Qt::UserRole, QVariant::fromValue(n) );
    idx.insert(n,item);
    switch( n->kind )
    {
    case Declaration::TypeDecl:
        if( r && r->type() == 0 && r->subs.isEmpty() )
            item->setIcon(0, QPixmap( pub ? ":/images/struct.png" : ":/images/struct_priv.png" ) );
        else if( r == 0 && p == 0 )
            item->setIcon(0, QPixmap( pub ? ":/images/alias.png" : ":/images/alias_priv.png" ) );
        else
            item->setIcon(0, QPixmap( pub ? ":/images/class.png" : ":/images/class_priv.png" ) );
        break;
    case Declaration::Procedure:
        item->setIcon(0, QPixmap( pub ? ":/images/func.png" : ":/images/func_priv.png" ) );
        break;
    }
    walkModItems(item,p,r,sort, idx, pro);
}

void Ide::fillModule(Declaration* m)
{
    d_mod->clear();
    d_modIdx.clear();
    d_modTitle->clear();
    if( m == 0 )
        return;
    d_modTitle->setText( QString("'%1'").arg(m->name.constData()) );
    walkModItems(d_mod, m, 0, true, d_modIdx, d_pro );
}

template<class T>
static QTreeWidgetItem* fillHierProc( T* parent, Declaration* p, Declaration* ref, Project2* pro )
{
    QTreeWidgetItem* item = new QTreeWidgetItem(parent);
    Q_ASSERT( p->receiver && p->link && p->link->receiver && p->link->type() );

    Qualident record = p->link->type()->decl->data.value<Qualident>();
    item->setText(0, record.second);
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
static QTreeWidgetItem* fillHierClass( T* parent, Declaration* n, Type* p, Type* ref, Project2* pro )
{
    QTreeWidgetItem* item = new QTreeWidgetItem(parent);
    Declaration* name = p->decl;
    if( name )
#if 0
        // TODO
        item->setText(0, name->getQualifiedName().join('.') );
    else
#endif
        item->setText(0, name->name);
    item->setData(0, Qt::UserRole, QVariant::fromValue( name ) );
    item->setIcon(0, QPixmap( name->visi >= Declaration::ReadWrite ? ":/images/class.png" : ":/images/class_priv.png" ) );
    item->setToolTip(0,item->text(0));
    QTreeWidgetItem* ret = 0;
    DeclList subs = pro->getSubs(n);
    foreach( Declaration* sub, subs )
    {
        QTreeWidgetItem* tmp = fillHierClass(item, sub, sub->type(), ref, pro);
        if( tmp )
            ret = tmp;
    }
    if( ret == 0 )
            ret = item;
    return ret;
}

void Ide::fillHier(Declaration* n)
{
    if( d_lock4 )
        return;
    d_hier->clear();
    d_hierTitle->clear();
    if( n == 0 )
        return;
    QFont f = d_hier->font();
    f.setBold(true);
    QTreeWidgetItem* ref = 0;
    switch( n->kind )
    {
    case Declaration::TypeDecl:
        switch( n->type()->kind )
        {
        case Type::Record:
            {
                Type* r = n->type();
                Type* r0 = r;
                d_hierTitle->setText( QString("Inheritance of class '%1'").arg( n->name.constData() ) );
                while( r->type() )
                    r = r->type();
                ref = fillHierClass( d_hier, n, r, r0, d_pro );
                Q_ASSERT( ref );
            }
            break;
        }
        break;
    case Declaration::Procedure:
        {
            Declaration* p = n;
            if( !p->receiver )
                return;
            d_hierTitle->setText( QString("Overrides of procedure '%1'").arg( n->name.constData() ) );
            while( p->super )
                p = p->super;
            ref = fillHierProc( d_hier, p, n, d_pro );
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

void Ide::removePosMarkers()
{
    for( int i = 0; i < d_tab->count(); i++ )
    {
        Editor* e = static_cast<Editor*>( d_tab->widget(i) );
        e->setPositionMarker(-1);
    }
}

void Ide::handleGoBack()
{
    ENABLED_IF( d_backHisto.size() > 1 );

    d_pushBackLock = true;
    d_forwardHisto.push_back( d_backHisto.last() );
    d_backHisto.pop_back();
    showEditor( d_backHisto.last() );
    d_pushBackLock = false;
}

void Ide::handleGoForward()
{
    ENABLED_IF( !d_forwardHisto.isEmpty() );

    Location cur = d_forwardHisto.last();
    d_forwardHisto.pop_back();
    showEditor( cur );
}

void Ide::onUpdateLocation(int line, int col)
{
    Editor* e = static_cast<Editor*>( sender() );
    e->clearBackHisto();
    pushLocation(Location(e->getPath(), line,col,e->verticalScrollBar()->value()));
}

void Ide::onXrefDblClicked()
{
    QTreeWidgetItem* item = d_xref->currentItem();
    if( item )
    {
        Symbol* sym = item->data(0,Qt::UserRole).value<Symbol*>();
        Declaration* module = item->data(1,Qt::UserRole).value<Declaration*>();
        Q_ASSERT( sym != 0 );
        d_lock3 = true;
        if( module )
        {
            d_curModule = module;
            ModuleData md = module->data.value<ModuleData>();
            showEditor( md.sourcePath, sym->pos.d_row, sym->pos.d_col, false, true );
        }
        d_lock3 = false;
    }
}

void Ide::onWorkingDir()
{
    ENABLED_IF(true);

    bool ok;
    const QString res = QInputDialog::getText(this,tr("Oberon File System Root"),
                                              tr("Enter Path (supports %PRODIR% and %APPDIR%):"), QLineEdit::Normal,
                                              d_pro->getWorkingDir(), &ok );
    if( !ok )
        return;
    d_pro->setWorkingDir(res);
}

void Ide::pushLocation(const Ide::Location& loc)
{
    if( d_pushBackLock )
        return;
    if( !d_backHisto.isEmpty() && d_backHisto.last() == loc )
        return; // o ist bereits oberstes Element auf dem Stack.
    d_backHisto.removeAll( loc );
    d_backHisto.push_back( loc );
}

void Ide::clear()
{
    d_backHisto.clear();
    d_forwardHisto.clear();
    d_pro->clear();
    d_mods->clear();
    d_mod->clear();
    d_hier->clear();
    d_modIdx.clear();
    d_scopes.clear();
    d_xrefTitle->clear();
    d_modTitle->clear();
    d_hierTitle->clear();
    d_xref->clear();
    d_errs->clear();
}

void Ide::onAbout()
{
    ENABLED_IF(true);

    QMessageBox::about( this, qApp->applicationName(),
      tr("<html>Release: %1   Date: %2<br><br>"

      "Welcome to the ActiveOberon IDE.<br>"
      "See <a href=\"https://github.com/rochus-keller/ActiveOberon\">"
         "here</a> for more information.<br><br>"

      "Author: Rochus Keller, me@rochus-keller.ch<br><br>"

      "Licese: <a href=\"https://www.gnu.org/licenses/license-list.html#GNUGPL\">GNU GPL v2 or v3</a>"
      "</html>" ).arg( qApp->applicationVersion() ).arg( QDateTime::currentDateTime().toString("yyyy-MM-dd") ));
}

void Ide::onQt()
{
    ENABLED_IF(true);
    QMessageBox::aboutQt(this,tr("About the Qt Framework") );
}

void Ide::onExpMod()
{
    ENABLED_IF( d_mods->currentItem() );

    Declaration* m = d_mods->currentItem()->data(0,Qt::UserRole).value<Declaration*>();
    if( m == 0 || m->kind != Declaration::Module )
        return;

    const QString path = QFileDialog::getSaveFileName( this, tr("Export Module"), m->getModuleFullName(true) + ".luon" );

    if( path.isEmpty() )
        return;
    ModuleData md = m->data.value<ModuleData>();
    // TODO d_pro->printTreeShaken( md.source, path );
}

void Ide::onExportC()
{
    ENABLED_IF( d_pro->getErrors().isEmpty() );

    const QString dirPath = QFileDialog::getExistingDirectory(this, tr("Save C"), d_pro->getBuildDir(true) );

    if (dirPath.isEmpty())
        return;

    if( !compile() ) // otherwise allocated flag is already set after one generator run
        return;

    if( !d_pro->generateC(dirPath) )
        QMessageBox::critical(this,tr("Save C"),tr("There was an error when generating C; "
                                                   "see Output window for more information"));
}

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    a.setOrganizationName("me@rochus-keller.ch");
    a.setOrganizationDomain("github.com/rochus-keller/ActiveOberon");
    a.setApplicationName("ActiveOberon IDE");
    a.setApplicationVersion("0.1.1");
    a.setStyle("Fusion");
    QFontDatabase::addApplicationFont(":/fonts/DejaVuSansMono.ttf"); // "DejaVu Sans Mono"

#ifdef QT_STATIC
    QFontDatabase::addApplicationFont(":/fonts/NotoSans.ttf"); // "Noto Sans"
    QFont af("Noto Sans",10);
    a.setFont(af);
#endif

#ifdef Q_OS_MAC
    QDir cur = QCoreApplication::applicationDirPath();
    if( cur.path().endsWith("/Contents/MacOS") )
    {
        // we're in a bundle
        cur.cdUp();
        cur.cdUp();
        cur.cdUp();
    }
    QDir::setCurrent(cur.path());
#endif

    Ide w;
    if( a.arguments().size() > 1 )
        w.loadFile(a.arguments()[1] );

    return a.exec();
}
