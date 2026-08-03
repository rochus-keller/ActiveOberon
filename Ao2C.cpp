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

    QTextStream out(stdout);
    out << "o2c (c) 2026 by me@rochus-keller.ch" << endl;

    const QStringList args = a.arguments();
    if (args.size() < 2) {
        out << "Usage:\n" << "  o2c [-p] [-r] <files_or_dir>\n";
        return 2;
    }

    QDir where;
    bool justParse = false;
    bool recursive = false;
    bool hasPath = false;
    QStringList files;
    for( int i=1;i<args.size();++i ) {
        if (args[i] == "-p")
            justParse = true;
        else if (args[i] == "-r")
            recursive = true;
        else {
            QFileInfo info(a.arguments()[i]);
            if( info.isFile() )
                files << info.absoluteFilePath();
            else {
                hasPath = true;
                where = info.absoluteDir();
            }
        }
    }
    Project2 pro;
    if( hasPath )
        pro.initializeFromDir(where, recursive);
    else
    {
        foreach( const QString& file, files )
            pro.addFile(file);
    }

    if( pro.getFiles().isEmpty() )
    {
        out << "ERROR: no Oberon source files found in " << where.absolutePath() << endl;
        return 1;
    }else
        out << "found " << pro.getFiles().size() << " Oberon source files in " << where.absolutePath() << (recursive ? " and subdirectories" : "" ) << endl;
    if( !pro.parse() )
    {
        foreach( const Project2::Error& e, pro.getErrors() )
           out << "Parser ERROR: " << QFileInfo(e.path).baseName() << ":" << e.pos.d_row << ":" << e.pos.d_col << ": " << e.msg << endl;
        return 1;
    }
    out << "Successfully parsed " << pro.getFiles().size() << " Oberon source files" << endl;

    if( justParse )
        return 0;
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
