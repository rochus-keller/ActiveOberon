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

#include "AoAsmToIntelXpiler.h"
using namespace Ao;

class ClobberDetector {
public:
    static QStringList detectClobbers(const QString& normalizedAsm) {
        QSet<QString> clobbers;
        clobbers.insert("memory");  // always safe for Oberon inline asm

        QStringList lines = normalizedAsm.split('\n');
        bool hasFlagModify = false;

        for (int i = 0; i < lines.size(); ++i) {
            QString line = lines[i].trimmed().toLower();
            if (line.isEmpty() || line.startsWith('#') || line.startsWith(';')) continue;

            // Extract mnemonic (first token)
            QStringList tokens = line.split(QRegExp("\\s+"));
            if (tokens.isEmpty()) continue;
            QString mnem = tokens[0];

            // Flag-modifying instructions (arithmetic, logic, compare, shift)
            if (modifiesFlags(mnem)) hasFlagModify = true;

            // Extract destination operands and implicit clobbers
            QSet<QString> lineClobbers = extractClobbers(mnem, line);
            clobbers.unite(lineClobbers);
        }

        if (hasFlagModify) clobbers.insert("cc");

        // Remove ESP (never clobber stack pointer)
        clobbers.remove("esp");

        // Convert to sorted list for deterministic output
        QStringList result = clobbers.toList();
        qSort(result);
        return result;
    }

private:
    static bool modifiesFlags(const QString& mnem) {
        // Instructions that modify EFLAGS
        static QSet<QString> flagOps;
        if (flagOps.isEmpty()) {
            QStringList ops;
            ops << "add" << "sub" << "adc" << "sbb" << "and" << "or" << "xor"
                << "cmp" << "test" << "inc" << "dec" << "neg" << "not"
                << "shl" << "shr" << "sal" << "sar" << "rol" << "ror" << "rcl" << "rcr"
                << "bt" << "bts" << "btr" << "btc" << "bsf" << "bsr" << "shld" << "shrd"
                << "mul" << "imul" << "div" << "idiv";
            foreach (QString s, ops) flagOps.insert(s);
        }
        return flagOps.contains(mnem);
    }

    static QSet<QString> extractClobbers(const QString& mnem, const QString& line) {
        QSet<QString> regs;

        // Implicit clobbers for specific instructions
        if (mnem == "mul" || mnem == "imul") {
            regs.insert("eax"); regs.insert("edx");
        } else if (mnem == "div" || mnem == "idiv") {
            regs.insert("eax"); regs.insert("edx");
        } else if (mnem == "cpuid") {
            regs.insert("eax"); regs.insert("ebx"); regs.insert("ecx"); regs.insert("edx");
        } else if (mnem == "pushad" || mnem == "popad") {
            regs.insert("eax"); regs.insert("ebx"); regs.insert("ecx"); regs.insert("edx");
            regs.insert("esi"); regs.insert("edi"); regs.insert("ebp");
            // ESP implicitly modified but never clobber it
        } else if (mnem == "rdtsc") {
            regs.insert("eax"); regs.insert("edx");
        } else if (mnem == "rdmsr" || mnem == "wrmsr") {
            regs.insert("eax"); regs.insert("ecx"); regs.insert("edx");
        }

        // Parse explicit destination operands
        // For 2-operand Intel syntax: "op dst, src" → dst is modified
        QRegExp operands(",");
        int commaPos = line.indexOf(',');
        if (commaPos > 0) {
            QString beforeComma = line.left(commaPos);
            QStringList parts = beforeComma.split(QRegExp("\\s+"));
            if (parts.size() >= 2) {
                QString dst = parts[1].trimmed();
                QString reg = extractRegister(dst);
                if (!reg.isEmpty()) regs.insert(reg);
            }
        } else {
            // 1-operand: "op dst" → dst is modified (INC, DEC, NEG, NOT, POP, etc.)
            if (mnem == "inc" || mnem == "dec" || mnem == "neg" || mnem == "not" ||
                    mnem == "pop" || mnem == "shl" || mnem == "shr" || mnem == "sal" ||
                    mnem == "sar" || mnem == "rol" || mnem == "ror") {
                QStringList parts = line.split(QRegExp("\\s+"));
                if (parts.size() >= 2) {
                    QString dst = parts[1].trimmed();
                    QString reg = extractRegister(dst);
                    if (!reg.isEmpty()) regs.insert(reg);
                }
            }
        }

        // XCHG modifies both operands
        if (mnem == "xchg") {
            QStringList parts = line.split(QRegExp("[\\s,]+"));
            for (int i = 1; i < parts.size(); ++i) {
                QString reg = extractRegister(parts[i]);
                if (!reg.isEmpty()) regs.insert(reg);
            }
        }

        return regs;
    }

    static QString extractRegister(const QString& operand) {
        // Extract register name from operand (strip memory syntax, size prefixes, etc.)
        QString op = operand.trimmed().toLower();

        // Remove BYTE/WORD/DWORD PTR prefix
        op.remove(QRegExp("\\b(byte|word|dword|qword|tbyte)\\s+ptr\\s+"));

        // Remove memory brackets and extract base register
        // Examples: "[eax]", "[esp+12]", "[eax+ebx*4]"
        QRegExp memRegex("\\[([a-z0-9]+)");
        if (memRegex.indexIn(op) >= 0) {
            return ""; // Memory operand - not a direct register clobber
        }

        // Check if it's a register (simple: 2-3 letter register names)
        static QSet<QString> validRegs;
        if (validRegs.isEmpty()) {
            QStringList r;
            r << "eax" << "ebx" << "ecx" << "edx" << "esi" << "edi" << "ebp" << "esp"
              << "ax" << "bx" << "cx" << "dx" << "si" << "di" << "bp" << "sp"
              << "al" << "bl" << "cl" << "dl" << "ah" << "bh" << "ch" << "dh"
              << "cr0" << "cr2" << "cr3" << "cr4" << "dr0" << "dr1" << "dr2" << "dr3"
              << "dr6" << "dr7";
            foreach (QString s, r) validRegs.insert(s);
        }

        // Normalize to 32-bit register name for clobber list
        if (validRegs.contains(op)) {
            // Convert 8/16-bit to 32-bit equivalent
            if (op == "al" || op == "ah" || op == "ax") return "eax";
            if (op == "bl" || op == "bh" || op == "bx") return "ebx";
            if (op == "cl" || op == "ch" || op == "cx") return "ecx";
            if (op == "dl" || op == "dh" || op == "dx") return "edx";
            if (op == "si") return "esi";
            if (op == "di") return "edi";
            if (op == "bp") return "ebp";
            if (op == "sp") return "esp";
            return op; // Already 32-bit or special register
        }

        return ""; // Not a register
    }
};

static QString escapeForCString(const QString& s) {
    QString out; out.reserve(s.size()*2);
    for (int i=0;i<s.size();++i) {
        QChar c = s[i];
        if (c == '\\' || c == '\"') { out += '\\'; out += c; }
        else if (c == '\n') { out += "\\n"; }
        else if (c == '\t') { out += "\\t"; }
        else if (c.unicode() < 32) { out += ' '; }
        else { out += c; }
    }
    return out;
}

QString AsmToIntelXpiler::transform(const QString& src, const TranspileOptions& opt, QString* err) {
    QString s = src;
    // 0) Strip Oberon target blocks and (* ... *) comments
    s = stripTargetBlocks(s);
    s = stripOberonComments(s);

    // 1) Collect labels and allocate a unique block id
    QString blockId = makeBlockId(s);
    QSet<QString> labels = collectLabels(s);

    // 2) Normalize line-by-line
    QStringList outLines;
    if (opt.prependIntelSyntax) {
        outLines << ".intel_syntax noprefix";
    }
    QStringList lines = s.split(QRegExp("\\r?\\n"));
    for (int li=0; li<lines.size(); ++li) {
        QString line = lines[li];

        // trim whitespace
        QString raw = line;
        QString trimmed = raw.trimmed();
        if (trimmed.isEmpty()) continue;

        // Convert ';' comment to GAS comment or remove
        int sem = trimmed.indexOf(';');
        QString cmt;
        if (sem >= 0) {
            cmt = trimmed.mid(sem+1).trimmed();
            trimmed = trimmed.left(sem).trimmed();
        }

        // Skip empty after removing comment
        // but keep a standalone comment line if present
        bool hasOnlyComment = trimmed.isEmpty() && !cmt.isEmpty();

        // 3) Hex H-suffix to 0x
        trimmed = convertHexHTo0x(trimmed);

        // 4) Addressing: disp[addr] -> [addr+disp]
        trimmed = fixDispBeforeBracket(trimmed);

        // 5) Absolute addressing: "@ expr" -> "[expr]"
        trimmed = fixAbsoluteAtAddressing(trimmed);

        // 6) Size keywords before memory: BYTE|WORD|DWORD|QWORD|TBYTE -> add PTR when followed by '['
        trimmed = insertPtrAfterSizeBeforeMem(trimmed);

        // 7) Force PTR for call/jmp mem if size absent
        trimmed = ensurePtrForCallJumpMem(trimmed);

        // 8) Lowercase registers/ops for GAS friendliness; mnemonics can be lower; labels case preserved
        trimmed = lowerRegsAndOps(trimmed, labels);

        // 9) Mangle labels on definition and references
        if (isLabelDef(trimmed)) {
            QString name = trimmed.left(trimmed.indexOf(':')).trimmed();
            QString m = mangleLabel(name, opt.labelPrefix, blockId);
            trimmed = m + ":";
        } else {
            trimmed = rewriteLabelRefs(trimmed, labels, opt.labelPrefix, blockId);
        }

        // 10) Map data directives for .S if requested
        if (opt.emitGasData) {
            trimmed = mapDataDirectives(trimmed);
        }

        // Reattach comment
        if (!cmt.isEmpty()) {
            trimmed += "  # " + cmt;
        }
        if (!trimmed.isEmpty() || hasOnlyComment) outLines << trimmed;
    }

#if 0
    QString out = outLines.join("\n");
    if (opt.outputCString) {
        // Wrap as single C string with embedded newlines and tabs
        // Common pattern for inline asm: asm volatile("..."); caller can add constraints as desired
        out = "\"" + escapeForCString(out) + "\"";
    }
    if (err) *err = QString();
    return out;
#else
    QString normalizedAsm = outLines.join("\n");

    if (opt.outputCString) {
        // Detect clobbers automatically
        QStringList clobbers = ClobberDetector::detectClobbers(normalizedAsm);

        // Build clobber list string
        QString clobberStr;
        if (!clobbers.isEmpty()) {
            QStringList quoted;
            foreach (QString c, clobbers) {
                quoted << "\"" + c + "\"";
            }
            clobberStr = quoted.join(", ");
        }

        // Emit complete asm volatile statement
        QStringList lines = escapeForCString(normalizedAsm).split("\\n");

        QString wrapper = "asm volatile(\n";
        foreach( const QString& l, lines )
            wrapper += "    \"" + l + "\"\n";
        wrapper +=  QString(
                    "    :   /* no outputs */\n"
                    "    :   /* no inputs */\n"
                    "    : %1\n"
                    ");"
                    ).arg(clobberStr);

        return wrapper;
    }

    return normalizedAsm;
#endif
}

QString AsmToIntelXpiler::stripTargetBlocks(const QString& s) {
    // Remove lines that start with { ... }
    QStringList out;
    QStringList lines = s.split(QRegExp("\\r?\\n"));
    for (int i=0;i<lines.size();++i) {
        QString t = lines[i].trimmed();
        if (t.startsWith('{')) {
            int close = t.indexOf('}');
            if (close >= 0) continue; // drop entire line
        }
        out << lines[i];
    }
    return out.join("\n");
}

QString AsmToIntelXpiler::stripOberonComments(const QString& s) {
    // Remove (* ... *) possibly spanning lines
    QString r = s;
    QRegExp rx("\\(\\*[^*]*\\*\\)");
    rx.setMinimal(true);
    int pos = 0;
    while ((pos = rx.indexIn(r, pos)) >= 0) {
        r.remove(pos, rx.matchedLength());
    }
    return r;
}

QString AsmToIntelXpiler::makeBlockId(const QString& s) {
    // cheap hash: count length and a checksum
    quint32 sum = 0;
    for (int i=0;i<s.size();++i) sum = (sum*131) + s[i].unicode();
    return QString::number(sum, 16);
}

QSet<QString> AsmToIntelXpiler::collectLabels(const QString& s) {
    QSet<QString> labs;
    QStringList lines = s.split(QRegExp("\\r?\\n"));
    QRegExp def("^\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*:\\s*$");
    for (int i=0;i<lines.size();++i) {
        if (def.indexIn(lines[i]) >= 0) {
            labs.insert(def.cap(1).toUpper());
        }
    }
    return labs;
}

bool AsmToIntelXpiler::isLabelDef(const QString& line) {
    QRegExp def("^\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*:\\s*$");
    return def.indexIn(line) >= 0;
}

QString AsmToIntelXpiler::mangleLabel(const QString& name, const QString& prefix, const QString& blk) {
    return prefix + blk + "_" + name;
}

QString AsmToIntelXpiler::rewriteLabelRefs(const QString& line, const QSet<QString>& labels,
                                           const QString& prefix, const QString& blk) {
    // Replace whole-word occurrences of known labels with mangled form,
    // but avoid replacing definitions (handled earlier).
    QString out = line;
    // Token-wise replace using regex with word boundaries
    QSet<QString>::const_iterator it = labels.begin();
    for (; it!=labels.end(); ++it) {
        QString L = *it;
        QRegExp rx(QString("\\b%1\\b").arg(QRegExp::escape(L)));
        int pos = 0;
        while ((pos = rx.indexIn(out, pos)) >= 0) {
            out.replace(pos, rx.matchedLength(), mangleLabel(L, prefix, blk));
            pos += mangleLabel(L, prefix, blk).size();
        }
    }
    return out;
}

QString AsmToIntelXpiler::convertHexHTo0x(const QString& line) {
    // Replace <HEX>H with 0x<hex>
    QString out = line;
    QRegExp hexWord("\\b([0-9A-Fa-f]+)H\\b");
    int pos = 0;
    while ((pos = hexWord.indexIn(out, pos)) >= 0) {
        QString hh = hexWord.cap(1);
        out.replace(pos, hexWord.matchedLength(), "0x" + hh);
        pos += 2 + hh.size();
    }
    return out;
}

QString AsmToIntelXpiler::fixDispBeforeBracket(const QString& line) {
    // Pattern: <disp>[<addr>]  -> [<addr>+<disp>]
    // Allow hex 0x..., decimal, or symbol names (already normalized) — but
    // we only reposition numeric displacements here; symbolic stays as-is.
    QString out = line;
    QRegExp rxNumDisp("\\b((?:0x[0-9A-Fa-f]+)|(?:[0-9]+))\\s*\\[([^\\]]+)\\]");
    int pos = 0;
    while ((pos = rxNumDisp.indexIn(out, pos)) >= 0) {
        QString disp = rxNumDisp.cap(1);
        QString addr = rxNumDisp.cap(2).trimmed();
        QString repl = "[" + addr;
        // add plus if addr doesn't already end with '+' or start with minus disp
        if (!addr.isEmpty() && addr[addr.size()-1] != '+') repl += "+";
        repl += disp + "]";
        out.replace(pos, rxNumDisp.matchedLength(), repl);
        pos += repl.size();
    }
    return out;
}

QString AsmToIntelXpiler::fixAbsoluteAtAddressing(const QString& line) {
    // '@ expr' -> '[expr]'
    QString out = line;
    QRegExp rxAt("@\\s*([^\\s,\\]]+)");
    int pos = 0;
    while ((pos = rxAt.indexIn(out, pos)) >= 0) {
        QString expr = rxAt.cap(1);
        QString repl = "[" + expr + "]";
        out.replace(pos, rxAt.matchedLength(), repl);
        pos += repl.size();
    }
    return out;
}

QString AsmToIntelXpiler::insertPtrAfterSizeBeforeMem(const QString& line) {
    // Convert 'SIZE [addr]' or 'SIZE <disp>[addr]' to 'SIZE PTR [addr]' after we normalized disp form.
    QString out = line;
    QRegExp rxSizeBeforeBracket("\\b(BYTE|WORD|DWORD|QWORD|TBYTE)\\b\\s*(?=\\[)");
    int pos = 0;
    while ((pos = rxSizeBeforeBracket.indexIn(out, pos)) >= 0) {
        QString kw = rxSizeBeforeBracket.cap(1);
        QString repl = kw + " PTR ";
        out.replace(pos, rxSizeBeforeBracket.matchedLength(), repl);
        pos += repl.size();
    }
    return out;
}

QString AsmToIntelXpiler::ensurePtrForCallJumpMem(const QString& line) {
    // If "call [expr]" or "jmp [expr]" without explicit SIZE, insert "DWORD PTR "
    QString out = line;
    QRegExp rxCallJmp("^\\s*(call|jmp)\\s*\\[");
    if (rxCallJmp.indexIn(out) >= 0) {
        out.replace(rxCallJmp, "\\1 DWORD PTR [");
    }
    return out;
}

QString AsmToIntelXpiler::lowerRegsAndOps(const QString& line, const QSet<QString>& labels) {
    // Lowercase everything except labels (already rewritten/mangled), hex 0x, numbers, and PTR keyword
    // A simple approach: split by quotes and operate on non-quoted segments.
    QString out;
    out.reserve(line.size());
    for (int i=0; i<line.size(); ++i) {
        QChar c = line[i];
        // keep case for letters inside 0x hex literals and 'PTR'
        out += c.toLower();
    }
    // Restore 'PTR' in uppercase if present
    out.replace(" ptr ", " PTR ");
    // Preserve segment names if user wants uppercase; GAS is case-insensitive anyway.
    return out;
}

QString AsmToIntelXpiler::mapDataDirectives(const QString& line) {
    // DB => .byte, DW => .word, DD => .long, DS "str" => .asciz "str"
    QString out = line;
    QRegExp rxDS("^\\s*ds\\s+\"(.*)\"\\s*$", Qt::CaseInsensitive);
    if (rxDS.indexIn(out) >= 0) {
        QString str = rxDS.cap(1);
        return ".asciz \"" + str + "\"";
    }
    QRegExp rxDB("^\\s*db\\s+(.*)$", Qt::CaseInsensitive);
    QRegExp rxDW("^\\s*dw\\s+(.*)$", Qt::CaseInsensitive);
    QRegExp rxDD("^\\s*dd\\s+(.*)$", Qt::CaseInsensitive);
    if (rxDB.indexIn(out) >= 0) return ".byte " + rxDB.cap(1);
    if (rxDW.indexIn(out) >= 0) return ".word " + rxDW.cap(1);
    if (rxDD.indexIn(out) >= 0) return ".long " + rxDD.cap(1);
    return out;
}
