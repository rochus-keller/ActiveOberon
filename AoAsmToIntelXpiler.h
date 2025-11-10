#ifndef AOASMTOINTELXPILER_H
#define AOASMTOINTELXPILER_H

/*
* Copyright 2025 Rochus Keller <mailto:me@rochus-keller.ch>
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

#include <QString>
#include <QSet>

namespace Ao
{

struct TranspileOptions {
  bool outputCString;          // produce a C string literal
  bool prependIntelSyntax;     // prepend ".intel_syntax noprefix\n\t"
  bool emitGasData;            // map DB/DW/DD/DS to .byte/.word/.long/.asciz (for .S)
  QString labelPrefix;         // prefix for local labels
  TranspileOptions():outputCString(false),prependIntelSyntax(false),emitGasData(false),labelPrefix(".Loba"){}
};

class AsmToIntelXpiler {
public:
  static QString transform(const QString& src, const TranspileOptions& opt, QString* err = 0);

private:
  static QString stripTargetBlocks(const QString& s);
  static QString stripOberonComments(const QString& s);
  static QString makeBlockId(const QString& s);
  static QSet<QString> collectLabels(const QString& s);
  static bool isLabelDef(const QString& line);
  static QString mangleLabel(const QString& name, const QString& prefix, const QString& blk);
  static QString rewriteLabelRefs(const QString& line, const QSet<QString>& labels,
                                  const QString& prefix, const QString& blk);
  static QString convertHexHTo0x(const QString& line);
  static QString fixDispBeforeBracket(const QString& line);
  static QString fixAbsoluteAtAddressing(const QString& line);
  static QString insertPtrAfterSizeBeforeMem(const QString& line);
  static QString ensurePtrForCallJumpMem(const QString& line);
  static QString lowerRegsAndOps(const QString& line, const QSet<QString>& labels);
  static QString mapDataDirectives(const QString& line);
};

}

#endif // AOASMTOINTELXPILER_H
