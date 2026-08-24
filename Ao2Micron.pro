QT       += core

QT       -= gui

TARGET = o2m
CONFIG   += console
CONFIG   -= app_bundle

TEMPLATE = app

INCLUDEPATH += . ..

#DEFINES += _DEBUG
DEFINES += AO_PROJECT2_MICRON_GEN

SOURCES += \
        ./AoBuilins.cpp \
        ./AoRowCol.cpp \
        ./AoToken.cpp \
        ./AoLexer.cpp \
        ./AoTokenType.cpp \
        ./AoParser2.cpp \
        ./AoAst.cpp \
        ./AoValidator2.cpp \
        ./AoCeeGen.cpp \
        ./AoComments.cpp \
        ./AoMicronGen.cpp \
        ./AoClosureLifter.cpp \
        ./Ao2Micron.cpp \
        ./AoProject2.cpp


HEADERS += \
        ./AoBuilins.h \
        ./AoRowCol.h \
        ./AoToken.h \
        ./AoLexer.h \
        ./AoTokenType.h \
        ./AoParser2.h \
        ./AoAst.h \
        ./AoValidator2.h \
        ./AoCeeGen.h \
        ./AoComments.h \
        ./AoMicronGen.h \
        ./AoClosureLifter.h \
        ./AoProject2.h

!win32{
QMAKE_CXXFLAGS += -Wno-reorder -Wno-unused-parameter -Wno-unused-function -Wno-unused-variable -Wno-deprecated-declarations
}
