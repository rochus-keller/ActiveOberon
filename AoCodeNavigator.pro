QT       += core gui widgets

TARGET = AoCodeNavigator
TEMPLATE = app

INCLUDEPATH +=  ..

CONFIG(debug, debug|release) {
        DEFINES += _DEBUG
}

!win32{
QMAKE_CXXFLAGS += -Wno-reorder -Wno-unused-parameter -Wno-unused-function -Wno-unused-variable
}

DEFINES += _AO_HAVE_FILESYSTEM_

SOURCES += \
    AoRowCol.cpp \
    AoToken.cpp \
    AoLexer.cpp \
    AoParser.cpp \
    AoSynTree.cpp \
    AoTokenType.cpp \
    AoFileSystem.cpp \
    AoCodeModel.cpp \
    AoCodeNavigator.cpp \
    AoHighlighter.cpp

HEADERS += \
    AoRowCol.h \
    AoToken.h \
    AoLexer.h \
    AoParser.h \
    AoSynTree.h \
    AoTokenType.h \
    AoFileSystem.h \
    AoCodeModel.h \
    AoCodeNavigator.h \
    AoHighlighter.h



RESOURCES += \
    AoCodeNavigator.qrc


