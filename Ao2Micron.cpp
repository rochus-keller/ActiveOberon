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
    a.setApplicationName("o2m");
    a.setApplicationVersion("0.3");

    QTextStream out(stdout);
    out << "o2m (c) 2026 by me@rochus-keller.ch" << endl;

    QStringList args = a.arguments();
    args.removeFirst();
    int level = 4; // target language level
    bool obDiv = true; // generate Oberon's DIV/MOD -> obdiv/obmod
    bool cmds = false; // generate the cmd_ lookup procedure per module
    for( int i = 0; i < args.size(); i++ )
    {
        if( args[i] == "-l" && i + 1 < args.size() )
        {
            level = args[i+1].toInt();
            args.removeAt(i);
            args.removeAt(i);
            i--;
        }else if( args[i] == "--plaindiv" )
        {
            obDiv = false;
            args.removeAt(i);
            i--;
        }else if( args[i] == "--cmds" )
        {
            cmds = true;
            args.removeAt(i);
            i--;
        }
    }
    if( args.isEmpty() )
    {
        out << "usage: o2m [-l <level>] [--plaindiv] [--cmds] <project>.obpro [<outdir>]" << endl;
        out << "       o2m [-l <level>] [--plaindiv] [--cmds] <source dir> [<outdir>]" << endl;
        out << "  -l <level>   the Micron language level of the generated modules (default 4)" << endl;
        out << "  --plaindiv   use DIV/MOD instead of OBDIV/OBMOD where an operand could be negative" << endl;
        out << "  --cmds       generate a cmd_ procedure per module to look up its commands" << endl;
        return 1;
    }

    Project2 pro;
    pro.setAggregateComments(true); // the generated Micron code includes the Oberon comments

    const QFileInfo info(args[0]);
    QDir where;
    if( info.isFile() )
    {
        if( !pro.loadFrom(info.absoluteFilePath()) )
        {
            out << "ERROR: cannot open project file " << info.absoluteFilePath() << endl;
            return 1;
        }
        where = info.absoluteDir();
    }else
    {
        where = QDir(info.absoluteFilePath());
        pro.initializeFromDir(where);
    }

    QDir outDir = where;
    if( args.size() > 1 )
    {
        outDir = QDir(QFileInfo(args[1]).absoluteFilePath());
        if( !outDir.exists() && !outDir.mkpath(outDir.absolutePath()) )
        {
            out << "ERROR: cannot create output directory " << outDir.absolutePath() << endl;
            return 1;
        }
    }

    if( pro.getFiles().isEmpty() )
    {
        out << "ERROR: no Oberon source files found in " << where.absolutePath() << endl;
        return 1;
    }else
        out << "found " << pro.getFiles().size() << " Oberon source files" << endl;

    if( !pro.parse() )
    {
        foreach( const Project2::Error& e, pro.getErrors() )
           out << "Parser ERROR: " << QFileInfo(e.path).baseName() << ":" << e.pos.d_row << ":"
               << e.pos.d_col << ": " << e.msg << endl;
        return 1;
    }
    out << "successfully parsed " << pro.getFiles().size() << " Oberon source files" << endl;

    if( !pro.generateMicron(outDir.absolutePath(), level, obDiv, cmds) || !pro.getErrors().isEmpty() )
    {
        foreach( const Project2::Error& e, pro.getErrors() )
           out << "Generator ERROR: " << QFileInfo(e.path).baseName() << ":" << e.pos.d_row << ":"
               << e.pos.d_col << ": " << e.msg << endl;
        return 1;
    }
    out << "successfully generated " << pro.getFiles().size() << " Micron source files in "
        << outDir.absolutePath() << endl;

    return 0;
}
