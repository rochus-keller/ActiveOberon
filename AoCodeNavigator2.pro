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

SOURCES += \
    AoRowCol.cpp \
    AoToken.cpp \
    AoLexer.cpp \
    AoTokenType.cpp \
    AoFileSystem.cpp \
    AoParser2.cpp \
    AoAst.cpp \
    AoValidator.cpp \
    AoProject.cpp \
    AoCodeModel2.cpp \
    AoCodeNavigator2.cpp \
    AoHighlighter2.cpp

HEADERS += \
    AoRowCol.h \
    AoToken.h \
    AoLexer.h \
    AoTokenType.h \
    AoFileSystem.h \
    AoParser2.h \
    AoAst.h \
    AoValidator.h \
    AoProject.h \
    AoCodeModel2.h \
    AoCodeNavigator2.h \
    AoHighlighter2.h



RESOURCES += \
    AoCodeNavigator.qrc


