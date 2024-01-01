## About this project

This is a parser and source code navigation tool for the ActiveOberon language as specified [in the 2004 language report](https://web.archive.org/web/20110524062849/http://bluebottle.ethz.ch/languagereport/ActiveReport.pdf).

The main purpose of this project is to provide tools to analyze and study the source code of the [ETH Bluebottle Active Object System](https://github.com/OberonSystem3/AOS_Bluebottle_Sources). 

[EbnfStudio](https://github.com/rochus-keller/EbnfStudio) was used to develop the grammar the grammar in Ao.ebnf, and also to generate the included AoParser/AoSynTree/AoTokenType.h/.cpp. 

Here is a screenshot of the code navigator:

![AoCodeNavigator Screenshot](http://software.rochus-keller.ch/AoCodeNavigator_v0.3_screenshot.png)


NOTE that this project is work in progress.

### Binary versions

Not yet available.

### Build Steps

This project can be built using qmake and Qt5. Use the .pro files to run the build as described in the Qt documentation. 

Alternatively the Code Navigator can be built using LeanQt and the BUSY build system (with no other dependencies than a C++98 compiler); follow these steps:

1. Create a new directory; we call it the root directory here
1. Download https://github.com/rochus-keller/ActiveOberon/archive/refs/heads/master.zip and unpack it to the root directory; rename the resulting directory to "LisaPascal".
1. Download https://github.com/rochus-keller/LeanQt/archive/refs/heads/master.zip and unpack it to the root directory; rename the resulting directory to "LeanQt".
1. Download https://github.com/rochus-keller/BUSY/archive/refs/heads/master.zip and unpack it to the root directory; rename the resulting directory to "build".
1. Open a command line in the build directory and type `cc *.c -O2 -lm -o lua` or `cl /O2 /MD /Fe:lua.exe *.c` depending on whether you are on a Unix or Windows machine; wait a few seconds until the Lua executable is built.
1. Now type `./lua build.lua ../ActiveOberon` (or `lua build.lua ../ActiveOberon` on Windows); wait until the AoCodeNavigator executable is built; you find it in the output subdirectory.

## Support

If you need support or would like to post issues or feature requests please use the Github issue list at https://github.com/rochus-keller/ActiveOberon/issues or send an email to the author.

