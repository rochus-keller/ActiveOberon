#ifndef LISACODENAVIGATOR_H
#define LISACODENAVIGATOR_H

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

#include <QMainWindow>
#include "AoAst.h"
#include "AoFileSystem.h"

class QLabel;
class QPlainTextEdit;
class QTreeWidgetItem;
class QTreeWidget;
class QModelIndex;

namespace Ao
{
class CodeModel;
class ModuleDetailMdl;
class Project;
class FileSystem;

class CodeNavigator : public QMainWindow
{
    Q_OBJECT
public:
    explicit CodeNavigator(QWidget *parent = 0);
    ~CodeNavigator();

    void open( const QString& sourceTreePath);
    void logMessage(const QString&);

protected:
    struct Place
    {
        FilePos d_loc;
        quint16 d_yoff;
        bool operator==( const Place& rhs ) { return d_loc.d_pos.d_row == rhs.d_loc.d_pos.d_row &&
                    d_loc.d_pos.d_col == rhs.d_loc.d_pos.d_col && d_loc.d_filePath == rhs.d_loc.d_filePath; }
        Place(const FilePos& loc, quint16 y ):d_loc(loc),d_yoff(y){}
        Place():d_yoff(0) {}
    };

    void createModuleList();
    void createDetails();
    void createUsedBy();
    void createLog();
    void pushLocation( const Place& );
    void showViewer( const Place& );
    void fillUsedBy(Ast::Symbol* id, Ast::Declaration*);
    void setPathTitle(const FileSystem::File* f, int row, int col);
    void syncModuleList();
    void fillMods();

    // overrides
    void closeEvent(QCloseEvent* event);

protected slots:
    void onCursorPositionChanged();
    void onModsDblClicked(QTreeWidgetItem*, int);
    void onItemDblClick(const QModelIndex&);
    void onUsedByDblClicked();
    void onGoBack();
    void onGoForward();
    void onGotoLine();
    void onFindInFile();
    void onFindAgain();
    void onGotoDefinition();
    void onOpen();
    void onRunReload();
    void onIncreaseSize();
    void onDecreaseSize();

private:
    class Viewer;
    Viewer* d_view;
    Project* d_pro;
    FileSystem* d_fs;
    QLabel* d_pathTitle;
    QPlainTextEdit* d_msgLog;
    QTreeWidget* d_mods;
    QTreeWidget* d_mod;
    QLabel* d_usedByTitle;
    QTreeWidget* d_usedBy;
    ModuleDetailMdl* d_mdl2;
    QString d_dir;

    QList<Place> d_backHisto; // d_backHisto.last() is current place
    QList<Place> d_forwardHisto;
    bool d_pushBackLock;
};
}

#endif // LISACODENAVIGATOR_H
