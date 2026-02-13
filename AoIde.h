#ifndef _LuonIde_H
#define _LuonIde_H

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

#include <QMainWindow>
#include <ActiveOberon/AoAst.h>

class QTreeWidget;
class QTreeWidgetItem;
class QLabel;

namespace Gui
{
class AutoMenu;
}

namespace Ao
{
    class Project;

    class Ide : public QMainWindow
    {
        Q_OBJECT

    public:
        Ide(QWidget *parent = 0);
        ~Ide();

        void loadFile( const QString& path );

    protected:
        class Editor;
        struct Location
        {
            // Qt-Koordinaten
            quint32 d_line;
            quint16 d_col;
            quint16 d_yoff;
            QString d_file;
            bool operator==( const Location& rhs ) { return d_line == rhs.d_line && d_col == rhs.d_col &&
                        d_file == rhs.d_file; }
            Location(const QString& f, quint32 l, quint16 c, quint16 y ):d_file(f),d_line(l),d_col(c),d_yoff(y){}
        };
        void createMods();
        void createMod();
        void createHier();
        void createErrs();
        void createXref();
        void createMenu();
        void createMenuBar();
        void closeEvent(QCloseEvent* event);
        bool checkSaved( const QString& title );
        bool compile(bool doGenerate = false);
        void fillMods();
        void showDocument( const QString& filePath );
        void addTopCommands(Gui::AutoMenu * pop);
        Editor* showEditor(const QString& path, int row = -1, int col = -1, bool setMarker = false , bool center = false);
        void showEditor(Ast::Declaration*, bool setMarker = false, bool center = false);
        void showEditor( const Location& );
        void createMenu( Editor* );
        void fillXref();
        void fillXrefForSym(Ast::Symbol*, Ast::Declaration* module);
        void syncModView(Ast::Declaration*);
        void syncEditorMarks(Ast::Declaration*selected, Ast::Declaration* module);
        Ast::Declaration* moduleOfCurrentEditor();
        void fillModule(Ast::Declaration*);
        void fillHier(Ast::Declaration*);
        void pushLocation( const Location& );
        void clear();

    protected slots:
        void onCompile();
        void onGenerate();
        void onNewPro();
        void onOpenPro();
        void onSavePro();
        void onSaveFile();
        void onSaveAs();
        void onCaption();
        void onGotoLnr(quint32);
        void onFullScreen();
        void onCursor();
        void onModsDblClicked(QTreeWidgetItem*,int);
        void onModDblClicked(QTreeWidgetItem*,int);
        void onHierDblClicked(QTreeWidgetItem*,int);
        void onTabChanged();
        void onTabClosing(int);
        void onEditorChanged();
        void onErrorsDblClicked();
        void onErrors();
        void onOpenFile();
        void onOakwood();
        void onSetOptions();
        void onSetArguments();
        void onAddFiles();
        void onNewModule();
        void onAddDir();
        void onRemoveFile();
        void onRemoveDir();
        void handleGoBack();
        void handleGoForward();
        void onUpdateLocation(int line, int col );
        void onXrefDblClicked();
        void onWorkingDir();
        void onAbout();
        void onQt();
    private:
        class DocTab;
        class Debugger;
        DocTab* d_tab;
        Project* d_pro;
        QTreeWidget* d_mods;
        QTreeWidget* d_mod;
        QTreeWidget* d_hier;
        QHash<Ast::Declaration*,QTreeWidgetItem*> d_modIdx;
        QTreeWidget* d_stack;
        QVector<Ast::Declaration*> d_scopes;
        QTreeWidget* d_locals;
        QLabel* d_xrefTitle;
        QLabel* d_modTitle;
        QLabel* d_hierTitle;
        QTreeWidget* d_xref;
        QTreeWidget* d_errs;
        QList<Location> d_backHisto; // d_backHisto.last() ist aktuell angezeigtes Objekt
        QList<Location> d_forwardHisto;
        QByteArray d_curBc;
        Ast::Declaration* d_curModule;
        bool d_lock, d_lock2, d_lock3, d_lock4;
        bool d_filesDirty;
        bool d_pushBackLock;
    };
}

#endif // _LuonIde_H
