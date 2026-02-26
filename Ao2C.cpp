/*
* Copyright 2026 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the ActiveOberon project.
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

#include <QCoreApplication>
#include <QDir>
#include <QTextStream>
#include "AoProject2.h"
using namespace Ao;

int main(int argc, char *argv[])
{
    QCoreApplication a(argc, argv);

    Project2 pro;

    QDir where;
    if( a.arguments().size() == 2 )
    {
        QFileInfo info(a.arguments()[1]);
        if( info.isFile() )
            where = info.absoluteDir();
        else
            where = info.absoluteFilePath();
    }
    pro.initializeFromDir(where);

    QTextStream out(stdout);
    out << "o2c (c) 2026 by me@rochus-keller.ch" << endl;
    if( pro.getFiles().isEmpty() )
    {
        out << "ERROR: no Oberon source files found in " << where.absolutePath() << endl;
        return 1;
    }else
        out << "found " << pro.getFiles().size() << " Oberon source files in " << where.absolutePath() << endl;
    if( !pro.parse() )
    {
        foreach( const Project2::Error& e, pro.getErrors() )
           out << "Parser ERROR: " << QFileInfo(e.path).baseName() << ":" << e.pos.d_row << ":" << e.pos.d_col << ": " << e.msg << endl;
        return 1;
    }
    out << "Successfully parsed " << pro.getFiles().size() << " Oberon source files" << endl;

    if( pro.generateC(where.absolutePath() ) )
        out << "Successfully generated " << pro.getFiles().size() << " C source files" << endl;
    else
    {
        foreach( const Project2::Error& e, pro.getErrors() )
           out << "Generator ERROR: " << QFileInfo(e.path).baseName() << ":" << e.pos.d_row << ":" << e.pos.d_col << ": " << e.msg << endl;
        return 1;
    }

    return 0;
}
