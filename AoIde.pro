#/*
#* Copyright 2024 Rochus Keller <mailto:me@rochus-keller.ch>
#*
#* This file is part of the Luon IDE application.
#*
#* The following is the license that applies to this copy of the
#* application. For a license to use the application under conditions
#* other than those described here, please email to me@rochus-keller.ch.
#*
#* GNU General Public License Usage
#* This file may be used under the terms of the GNU General Public
#* License (GPL) versions 2.0 or 3.0 as published by the Free Software
#* Foundation and appearing in the file LICENSE.GPL included in
#* the packaging of this file. Please review the following information
#* to ensure GNU General Public Licensing requirements will be met:
#* http://www.fsf.org/licensing/licenses/info/GPLv2.html and
#* http://www.gnu.org/copyleft/gpl.html.
#*/

QT       += core gui widgets
TARGET = OberonIDE
TEMPLATE = app

INCLUDEPATH += .. 

SOURCES += AoIde.cpp \
    ../GuiTools/CodeEditor.cpp \
    ../GuiTools/DocSelector.cpp \
    ../GuiTools/DocTabWidget.cpp \
    AoBuilins.cpp \
    AoCeeGen.cpp \
    AoProject2.cpp \
    AoRowCol.cpp \
    AoToken.cpp \
    AoLexer.cpp \
    AoTokenType.cpp \
    AoParser2.cpp \
    AoAst.cpp \
    AoHighlighter2.cpp \
    AoClosureLifter.cpp \
    AoValidator2.cpp
    
HEADERS  += AoIde.h \
    ../GuiTools/CodeEditor.h \
    ../GuiTools/DocSelector.h \
    ../GuiTools/DocTabWidget.h \
    AoBuilins.h \
    AoCeeGen.h \
    AoProject2.h \
    AoRowCol.h \
    AoToken.h \
    AoLexer.h \
    AoTokenType.h \
    AoParser2.h \
    AoAst.h \
    AoHighlighter2.h \
    AoClosureLifter.h \
    AoValidator2.h


include( AoParser.pri )
include( ../GuiTools/Menu.pri )

CONFIG(debug, debug|release) {
        DEFINES += _DEBUG
    # DEFINES += LUA_USE_ASSERT
    # DEFINES +=  _INSERT_DGBTRACE
}

!win32 {
    QMAKE_CXXFLAGS += -Wno-reorder -Wno-unused-parameter -Wno-unused-function -Wno-unused-variable
}

RESOURCES += \
    AoIde.qrc

