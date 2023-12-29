## About this project

This is a parser for the ActiveOberon language as specified [in the language report](https://web.archive.org/web/20110524062849/http://bluebottle.ethz.ch/languagereport/ActiveReport.pdf).

The grammar in Ao.ebnf is compatible with [EbnfStudio](https://github.com/rochus-keller/EbnfStudio) which was also used to generate the included AoParser/AoSynTree/AoTokenType.h/.cpp. 

The current version of the parser is able to parse all 635 module files in https://github.com/OberonSystem3/AOS_Bluebottle_Sources in 8 seconds on a EliteBook 2530p with no error.

NOTE that this project is work in progress.


### Binary versions

Not yet available.

### Build Steps

This project can be built using qmake and Qt5. Use the .pro files to run the build as described in the Qt documentation. 

## Support

If you need support or would like to post issues or feature requests please use the Github issue list at https://github.com/rochus-keller/ActiveOberon/issues or send an email to the author.

