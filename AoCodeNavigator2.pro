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
    AoAsmToIntelXpiler.cpp \
    AoBuilins.cpp \
    AoCeeGen.cpp \
    AoRowCol.cpp \
    AoToken.cpp \
    AoLexer.cpp \
    AoTokenType.cpp \
    AoFileSystem.cpp \
    AoParser2.cpp \
    AoAst.cpp \
    AoProject.cpp \
    AoCodeNavigator2.cpp \
    AoHighlighter2.cpp \
    AoClosureLifter.cpp \
    AoValidator2.cpp

HEADERS += \
    AoAsmToIntelXpiler.h \
    AoBuilins.h \
    AoCeeGen.h \
    AoRowCol.h \
    AoToken.h \
    AoLexer.h \
    AoTokenType.h \
    AoFileSystem.h \
    AoParser2.h \
    AoAst.h \
    AoProject.h \
    AoCodeNavigator2.h \
    AoHighlighter2.h \
    AoClosureLifter.h \
    AoValidator2.h



RESOURCES += \
    AoCodeNavigator.qrc


