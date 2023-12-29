QT       += core

QT       -= gui

TARGET = ParserTest
CONFIG   += console
CONFIG   -= app_bundle

TEMPLATE = app

INCLUDEPATH += ../..

DEFINES += _DEBUG

SOURCES += \
    ParserTest.cpp \
    AoRowCol.cpp \
    AoToken.cpp \
    AoLexer.cpp \
    AoParser.cpp \
    AoSynTree.cpp \
    AoTokenType.cpp

HEADERS += \
    AoRowCol.h \
    AoToken.h \
    AoLexer.h \
    AoParser.h \
    AoSynTree.h \
    AoTokenType.h



