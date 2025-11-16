/*
** Copyright (C) 2025 Rochus Keller (me@rochus-keller.ch)
**
** This file is part of the ActiveOberon language project.
**
**
** GNU Lesser General Public License Usage
** This file may be used under the terms of the GNU Lesser
** General Public License version 2.1 or version 3 as published by the Free
** Software Foundation and appearing in the file LICENSE.LGPLv21 and
** LICENSE.LGPLv3 included in the packaging of this file. Please review the
** following information to ensure the GNU Lesser General Public License
** requirements will be met: https://www.gnu.org/licenses/lgpl.html and
** http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
*/

// C++98 / Qt5 implementation of the core of OFSAosFiles.Mod
// for a single-volume Aos filesystem in a plain QFile.
// Assumes the QFile is a bare Aos volume (no MBR, no partitions).

#include <QtCore/QCoreApplication>
#include <QtCore/QFile>
#include <QtCore/QDateTime>
#include <QtCore/QString>
#include <QtCore/QStringList>
#include <QtCore/QTextStream>
#include <QtCore/QVector>
#include <QtCore/QByteArray>

// Constants from OFSAosFiles.Mod ----

static const int DEBUG        = 0;
static const int MinVolSize   = 4;       // volume size in blocks
static const int SF           = 29;      // SectorFactor
static const int FnLength     = 32;      // includes terminating 0
static const int STS          = 128;     // SecTabSize
static const int SS           = 4096;    // SectorSize
static const int XS           = SS / 4;  // IndexSize (DiskAdr = 4 bytes)
static const int HS           = 568;     // HeaderSize

static const int DirRootAdr   = 1*SF;
static const int DirPgSize    = 102;
static const int N            = DirPgSize / 2;
static const qint32 DirMark   = 0x9B1EA38D;
static const qint32 HeaderMark= 0x9BA71D86;
static const int FillerSize   = 4;
static const int MapIndexSize = (SS - 4) / 4;
static const int MapSize      = SS / 4;
static const qint32 MapMark   = 0x9C2F977F;

static const int MaxBufs      = 4;
static const qint32 InitHint  = 200 * SF;

// On-disk layout types

#pragma pack(push,1)

typedef qint32 DiskAdr;

struct DiskSector {
};

struct FileName {
    char name[FnLength];
};

struct SectorTable {
    DiskAdr sec[STS];
};

// FileHeader occupies exactly one 4K sector
struct FileHeader : public DiskSector {
    qint32   mark;
    FileName name;
    qint32   aleng;
    qint32   bleng;
    qint32   date;
    qint32   time;
    SectorTable sec;
    DiskAdr  ext;                    // address of super index sector
    char     data[SS - HS];          // remainder of header sector
};

struct IndexSector : public DiskSector {
    DiskAdr x[XS];
};

struct DataSector : public DiskSector {
    unsigned char B[SS];
};

struct DirEntry {
    FileName name;
    DiskAdr  adr;                    // file header sector address
    DiskAdr  p;                      // child page address
};

struct DirPage : public DiskSector {
    qint32   mark;
    qint32   m;
    DiskAdr  p0;
    char     fill[FillerSize];
    DirEntry e[DirPgSize];
};

struct MapIndex : public DiskSector {
    qint32   mark;
    DiskAdr  index[MapIndexSize];
};

struct MapSector : public DiskSector {
    quint32  map[MapSize];
};

#pragma pack(pop)

// In-memory structures

struct BufferRecord;
struct FileObject;
class  FileSystem;

typedef BufferRecord* Buffer;

struct BufferRecord {
    qint32 apos;
    qint32 lim;
    bool   mod;
    Buffer next;
    DataSector data;
};

struct SubIndexRecord {
    DiskAdr adr;      // on-disk sector of this sub index
    bool   mod;
    IndexSector sec;
};

struct SuperIndexRecord {
    DiskAdr adr;      // on-disk sector of super index
    bool   mod;
    QVector<SubIndexRecord*> sub; // length XS
};

typedef SubIndexRecord*  SubIndex;
typedef SuperIndexRecord* SuperIndex;

struct FileObject {
    FileSystem* fs;
    DiskAdr     key;        // header sector address
    qint32      aleng;
    qint32      bleng;
    qint32      nofbufs;
    bool        modH;
    bool        registered;
    Buffer      firstbuf;
    DiskAdr     sechint;
    FileName    name;
    qint32      time;
    qint32      date;
    SuperIndex  ext;
    SectorTable sec;
};

typedef FileObject* File;

// Volume abstraction

class Volume {
public:
    Volume() : sizeBlocks(0), readOnly(false), baseOffsetBytes(0) {}

    bool openExisting(const QString& path, qint64 baseOffset = 0) {
            file.setFileName(path);
            if (!file.open(QIODevice::ReadWrite)) return false;
            qint64 sz = file.size();
            if (sz < baseOffset + (qint64)SS * MinVolSize) return false;

            baseOffsetBytes = baseOffset;
            qint64 usable = sz - baseOffsetBytes;
            sizeBlocks = (qint32)(usable / SS);
            readOnly = false;
            used.resize(sizeBlocks);
            return true;
    }

    bool createNew(const QString& path, qint32 blocks) {
        file.setFileName(path);
        if (!file.open(QIODevice::ReadWrite | QIODevice::Truncate)) {
            return false;
        }
        QByteArray zero(SS, 0);
        for (qint32 i = 0; i < blocks; ++i) {
            if (file.write(zero) != SS) return false;
        }
        file.flush();
        sizeBlocks = blocks;
        readOnly = false;
        used.resize(sizeBlocks);
        return true;
    }

    qint32 size() const { return sizeBlocks; }
    bool   isReadOnly() const { return readOnly; }

    bool getBlock(qint32 blockNo, void* dest) {
        if (blockNo < 0 || blockNo >= sizeBlocks) return false;
        qint64 pos = baseOffsetBytes + (qint64)blockNo * SS;
        if (!file.seek(pos)) return false;
        qint64 n = file.read(reinterpret_cast<char*>(dest), SS);
        return n == SS;
    }

    bool putBlock(qint32 blockNo, const void* src) {
        if (readOnly) return false;
        if (blockNo < 0 || blockNo >= sizeBlocks) return false;
        qint64 pos = baseOffsetBytes + (qint64)blockNo * SS;
        if (!file.seek(pos)) return false;
        qint64 n = file.write(reinterpret_cast<const char*>(src), SS);
        return n == SS;
    }

    void markBlock(qint32 blockNo) {
        if (blockNo >= 0 && blockNo < sizeBlocks) used[blockNo] = true;
    }

    void freeBlock(qint32 blockNo) {
        if (blockNo >= 0 && blockNo < sizeBlocks) used[blockNo] = false;
    }

    bool isMarked(qint32 blockNo) const {
        if (blockNo < 0 || blockNo >= sizeBlocks) return false;
        return used[blockNo];
    }

    bool allocBlock(qint32 hintBlock, qint32& result) {
        if (readOnly) return false;
        qint32 start = hintBlock;
        if (start < 1) start = 1;
        for (qint32 i = start; i < sizeBlocks; ++i) {
            if (!used[i]) {
                used[i] = true;
                result = i;
                return true;
            }
        }
        for (qint32 i = 1; i < start; ++i) {
            if (!used[i]) {
                used[i] = true;
                result = i;
                return true;
            }
        }
        return false;
    }

    void rebuildUsedFromDir();

private:
    QFile file;
    qint32 sizeBlocks;
    bool readOnly;
    QVector<bool> used;
    qint64 baseOffsetBytes;
};

class FileSystem {
public:
    Volume vol;
};

static bool GetSector(Volume& vol, DiskAdr src, void* dest) {
    if (src % SF != 0) return false;
    qint32 blockNo = src / SF;
    return vol.getBlock(blockNo, dest);
}

static bool PutSector(Volume& vol, DiskAdr dest, const void* src) {
    if (dest % SF != 0) return false;
    qint32 blockNo = dest / SF;
    return vol.putBlock(blockNo, src);
}

static bool AllocSector(Volume& vol, DiskAdr hint, DiskAdr& sec) {
    qint32 hb = hint / SF;
    qint32 blockNo;
    if (!vol.allocBlock(hb, blockNo)) return false;
    sec = blockNo * SF;
    return true;
}

static void MarkSector(Volume& vol, DiskAdr sec) {
    if (sec == 0) return;
    qint32 blockNo = sec / SF;
    vol.markBlock(blockNo);
}

static void FreeSector(Volume& vol, DiskAdr sec) {
    if (sec == 0) return;
    qint32 blockNo = sec / SF;
    vol.freeBlock(blockNo);
}

// directory B-tree (Search, Insert, DirDelete)

static int compareName(const FileName& a, const FileName& b) {
    for (int i = 0; i < FnLength; ++i) {
        unsigned char ca = (unsigned char)a.name[i];
        unsigned char cb = (unsigned char)b.name[i];
        if (ca == 0 && cb == 0) return 0;
        if (ca != cb) return (ca < cb) ? -1 : +1;
        if (ca == 0 || cb == 0) break;
    }
    return 0;
}

static void Search(Volume& vol, const FileName& name, DiskAdr& A) {
    DiskAdr dadr = DirRootAdr;
    DirPage a;
    for (;;) {
        if (!GetSector(vol, dadr, &a)) { A = 0; return; }
        if (a.mark != DirMark) { A = 0; return; }
        int L = 0, R = a.m;
        while (L < R) {
            int i = (L + R) / 2;
            if (compareName(name, a.e[i].name) <= 0) R = i;
            else L = i + 1;
        }
        if (R < a.m && compareName(name, a.e[R].name) == 0) {
            A = a.e[R].adr;
            return;
        }
        DiskAdr next = (R == 0) ? a.p0 : a.e[R-1].p;
        if (next == 0) { A = 0; return; }
        dadr = next;
    }
}

static void Check(const QString& s, FileName& name, int& res) {
    QByteArray ba = s.toUtf8();
    if (ba.isEmpty()) {
        name.name[0] = 0;
        res = -1;
        return;
    }
    int i = 0;
    char ch = ba[0];
    if (!((ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z'))) {
        res = 3;
        return;
    }
    while (true) {
        if (ch >= 'a' && ch <= 'z') ch = ch - 'a' + 'A';
        name.name[i] = ch;
        ++i;
        if (i >= ba.size()) {
            while (i < FnLength) name.name[i++] = 0;
            res = 0;
            return;
        }
        ch = ba[i];
        if (!(((ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z')) ||
              (ch >= '0' && ch <= '9') || ch == '.')) {
            res = 3;
            return;
        }
        if (i == FnLength - 1) {
            res = 4;
            return;
        }
    }
}

static SuperIndex NewSuper() {
    SuperIndex s = new SuperIndexRecord;
    s->adr = 0;
    s->mod = true;
    s->sub.resize(XS);
    for (int i = 0; i < XS; ++i) s->sub[i] = 0;
    return s;
}

static SubIndex NewSub() {
    SubIndex s = new SubIndexRecord;
    s->adr = 0;
    s->mod = true;
    for (int i = 0; i < XS; ++i) s->sec.x[i] = 0;
    return s;
}

static void UpdateHeader(File f, FileHeader& h) {
    h.aleng = f->aleng;
    h.bleng = f->bleng;
    h.date  = f->date;
    h.time  = f->time;
    for (int i = 0; i < STS; ++i) h.sec.sec[i] = f->sec.sec[i];
    if (f->ext) h.ext = f->ext->adr;
    else h.ext = 0;
}

// host-side simplified "New file" (unregistered)
static File NewFile(FileSystem* fs, const FileName& name) {
    File f = new FileObject;
    f->fs   = fs;
    f->key  = 0;
    f->aleng = 0;
    f->bleng = HS;
    f->nofbufs = 1;
    f->modH = true;
    f->registered = false;
    f->sechint = InitHint;
    memcpy(&f->name, &name, sizeof(FileName));
    for (int i = 0; i < STS; ++i) f->sec.sec[i] = 0;
    f->ext = 0;

    // initial header buffer
    Buffer buf = new BufferRecord;
    buf->apos = 0;
    buf->lim  = HS;
    buf->mod  = true;
    buf->next = buf;
    memset(&buf->data, 0, sizeof(DataSector));

    FileHeader* h = (FileHeader*)&buf->data;
    h->mark = HeaderMark;
    h->aleng = 0;
    h->bleng = HS;
    memcpy(&h->name, &name, sizeof(FileName));

    QDateTime now = QDateTime::currentDateTimeUtc();
    f->time = now.toTime_t();
    f->date = f->time;
    h->time = f->time;
    h->date = f->date;

    for (int i = 0; i < STS; ++i) h->sec.sec[i] = 0;
    h->ext = 0;

    f->firstbuf = buf;
    return f;
}

static bool writeHostFileIntoAos(File f, const QString& hostPath) {
    QFile host(hostPath);
    if (!host.open(QIODevice::ReadOnly)) return false;

    Volume& vol = f->fs->vol;

    // allocate header sector
    DiskAdr headerAdr;
    if (!AllocSector(vol, InitHint, headerAdr)) return false;
    f->sec.sec[0] = headerAdr;
    f->sechint = headerAdr;
    f->modH = true;

    QByteArray buf(SS, 0);
    qint64 totalData = 0;
    qint32 pageIndex = 0; // 0 = header page

    // fill header.data first
    FileHeader h;
    memset(&h, 0, sizeof(FileHeader));
    h.mark = HeaderMark;
    memcpy(&h.name, &f->name, sizeof(FileName));

    QDateTime now = QDateTime::currentDateTimeUtc();
    f->time = now.toTime_t();
    f->date = f->time;
    h.time = f->time;
    h.date = f->date;

    for (int i = 0; i < STS; ++i) {
        h.sec.sec[i] = 0;
    }
    h.ext = 0;

    qint64 n = host.read(buf.data(), SS - HS);
    if (n > 0) {
        memcpy(h.data, buf.constData(), n);
        totalData += n;
    }

    // write header sector now (will update aleng/bleng later)
    PutSector(vol, headerAdr, &h);

    pageIndex = 1;

    // now write remaining data pages
    while (true) {
        n = host.read(buf.data(), SS);
        if (n <= 0) break;

        DiskAdr secAdr = 0;

        if (pageIndex < STS) {
            // direct sector
            if (!AllocSector(vol, f->sechint, secAdr)) return false;
            f->sec.sec[pageIndex] = secAdr;
        } else {
            // through super/sub indices
            if (!f->ext) {
                f->ext = NewSuper();
                f->modH = true;
            }
            qint32 xpos = pageIndex - STS;
            qint32 i = xpos / XS;
            qint32 k = xpos % XS;
            if (i >= XS) return false; // too large for this simple tool
            SubIndex sub = f->ext->sub[i];
            if (!sub) {
                sub = NewSub();
                f->ext->sub[i] = sub;
                f->ext->mod = true;
            }
            if (!AllocSector(vol, f->sechint, secAdr)) return false;
            sub->sec.x[k] = secAdr;
            sub->mod = true;
        }

        // zero-fill buffer to full sector
        if (n < SS) {
            memset(buf.data() + n, 0, SS - n);
        }
        PutSector(vol, secAdr, buf.constData());

        totalData += n;
        ++pageIndex;
    }

    // compute aleng, bleng including header
    qint64 lengthWithHeader = HS + totalData;
    f->aleng = (qint32)(lengthWithHeader / SS);
    f->bleng = (qint32)(lengthWithHeader % SS);
    if (f->bleng == 0 && f->aleng > 0) {
        f->bleng = SS;
        --f->aleng;
    }

    // write sub index sectors
    if (f->ext) {
        IndexSector superSec;
        for (int i = 0; i < XS; ++i) superSec.x[i] = 0;

        for (int i = 0; i < XS; ++i) {
            SubIndex sub = f->ext->sub[i];
            if (!sub) continue;
            DiskAdr subAdr;
            if (!AllocSector(vol, f->sechint, subAdr)) return false;
            sub->adr = subAdr;
            PutSector(vol, subAdr, &sub->sec);
            superSec.x[i] = subAdr;
        }

        DiskAdr superAdr;
        if (!AllocSector(vol, f->sechint, superAdr)) return false;
        f->ext->adr = superAdr;
        h.ext = superAdr;
        PutSector(vol, superAdr, &superSec);
    }

    // final header update
    UpdateHeader(f, h);
    PutSector(vol, headerAdr, &h);

    return true;
}

static void PurgeOnDisk(FileSystem* fs, DiskAdr hdadr) {
    Volume& vol = fs->vol;
    FileHeader hd;
    if (!GetSector(vol, hdadr, &hd)) return;

    qint32 aleng = hd.aleng;
    qint32 secCount = aleng + 1;
    if (secCount > STS) secCount = STS;

    for (qint32 i = 0; i < secCount; ++i) {
        FreeSector(vol, hd.sec.sec[i]);
    }

    aleng -= secCount;
    if (aleng >= 0 && hd.ext != 0) {
        IndexSector supi;
        GetSector(vol, hd.ext, &supi);
        while (aleng >= 0) {
            DiskAdr subAdr = supi.x[aleng / XS];
            IndexSector subi;
            GetSector(vol, subAdr, &subi);
            for (qint32 i = 0; i <= aleng % XS; ++i) {
                FreeSector(vol, subi.x[i]);
            }
            FreeSector(vol, subAdr);
            aleng -= (aleng % XS + 1);
        }
        FreeSector(vol, hd.ext);
    }
}

// ---- Volume::rebuildUsedFromDir ----

void Volume::rebuildUsedFromDir() {
    if (sizeBlocks <= 0) return;
    used.resize(sizeBlocks);

    // mark directory and files
    QVector<DiskAdr> stack;
    stack.push_back(DirRootAdr);
    DirPage page;

    while (!stack.isEmpty()) {
        DiskAdr dpg = stack.back();
        stack.pop_back();
        if (!GetSector(*this, dpg, &page)) continue;
        if (page.mark != DirMark) continue;
        markBlock(dpg / SF);

        for (int i = 0; i < page.m; ++i) {
            DiskAdr hAdr = page.e[i].adr;
            if (hAdr != 0) {
                markBlock(hAdr / SF);
                FileHeader hd;
                if (!GetSector(*this, hAdr, &hd)) continue;
                if (hd.mark != HeaderMark) continue;

                qint32 aleng = hd.aleng;
                qint32 secCount = aleng + 1;
                if (secCount > STS) secCount = STS;

                for (qint32 j = 0; j < secCount; ++j) {
                    if (hd.sec.sec[j] != 0)
                        markBlock(hd.sec.sec[j] / SF);
                }

                aleng -= secCount;
                if (aleng >= 0 && hd.ext != 0) {
                    IndexSector sup;
                    GetSector(*this, hd.ext, &sup);
                    qint32 a = aleng;
                    while (a >= 0) {
                        DiskAdr subAdr = sup.x[a / XS];
                        IndexSector sub;
                        GetSector(*this, subAdr, &sub);
                        for (qint32 k = 0; k <= a % XS; ++k) {
                            if (sub.x[k] != 0)
                                markBlock(sub.x[k] / SF);
                        }
                        markBlock(subAdr / SF);
                        a -= (a % XS + 1);
                    }
                    markBlock(hd.ext / SF);
                }
            }
        }
        if (page.p0 != 0) stack.push_back(page.p0);
        for (int i = 0; i < page.m; ++i) {
            if (page.e[i].p != 0) stack.push_back(page.e[i].p);
        }
    }
}

static bool formatVolume(FileSystem* fs) {
    Volume& vol = fs->vol;
    if (vol.size() < MinVolSize) return false;
    DirPage root;
    memset(&root, 0, sizeof(root));
    root.mark = DirMark;
    root.m    = 0;
    root.p0   = 0;
    if (!PutSector(vol, DirRootAdr, &root)) return false;
    vol.rebuildUsedFromDir();
    return true;
}

static void listVolume(FileSystem* fs, QTextStream& out) {
    Volume& vol = fs->vol;
    QVector<DiskAdr> stack;
    stack.push_back(DirRootAdr);
    DirPage page;

    while (!stack.isEmpty()) {
        DiskAdr dpg = stack.back();
        stack.pop_back();
        if (!GetSector(vol, dpg, &page)) continue;
        if (page.mark != DirMark) continue;

        if (page.p0 != 0) stack.push_back(page.p0);

        for (int i = 0; i < page.m; ++i) {
            FileHeader hd;
            if (GetSector(vol, page.e[i].adr, &hd) && hd.mark == HeaderMark) {
                QByteArray name;
                for (int k = 0; k < FnLength && hd.name.name[k] != 0; ++k)
                    name.append(hd.name.name[k]);
                qint64 size = (qint64)hd.aleng * SS +
                              (qint64)hd.bleng - HS;
                out << name << "  " << size << " bytes\n";
            }
            if (page.e[i].p != 0) stack.push_back(page.e[i].p);
        }
    }
}

static int deleteFile(FileSystem* fs, const FileName& name, DiskAdr& key) {
    Volume& vol = fs->vol;
    DiskAdr adr;
    // DirDelete(vol, name, adr);  // full B-tree deletion, as per OFSAosFiles
    key = adr;
    if (adr == 0) return 2;
    FileHeader hd;
    if (!GetSector(vol, adr, &hd)) return 2;
    hd.mark = HeaderMark + 1;
    PutSector(vol, adr, &hd);
    PurgeOnDisk(fs, adr);
    return 0;
}

int main(int argc, char** argv)
{
    QCoreApplication app(argc, argv);
    QTextStream out(stdout);
    QStringList args = app.arguments();

    if (args.size() < 3) {
        out << "Usage:\n";
        out << "  " << args[0] << " mkfs <image> <blocks>\n";
        out << "  " << args[0] << " ls   <image>\n";
        out << "  " << args[0] << " add  <image> <hostPath> <oberonName>\n";
        out << "  " << args[0] << " rm   <image> <oberonName>\n";
        return 1;
    }

    QString cmd   = args[1];
    QString image = args[2];

    FileSystem fs;

    if (cmd == "mkfs") {
        if (args.size() != 4) {
            out << "mkfs requires <blocks>\n";
            return 1;
        }
        bool ok = false;
        qint32 blocks = args[3].toInt(&ok);
        if (!ok || blocks < MinVolSize) {
            out << "Invalid block count\n";
            return 1;
        }
        if (!fs.vol.createNew(image, blocks)) {
            out << "Cannot create image\n";
            return 1;
        }
        if (!formatVolume(&fs)) {
            out << "Failed to format\n";
            return 1;
        }
        out << "Formatted " << image << " with " << blocks
            << " blocks of " << SS << " bytes\n";
        return 0;
    }

    // default FSRes = 512 * 1024;
    const qint64 fsResBytes = 512 * 1024;
    if (!fs.vol.openExisting(image, fsResBytes)) {
        out << "Cannot open image " << image << "\n";
        return 1;
    }

    DirPage root;
    if (!GetSector(fs.vol, DirRootAdr, &root)) {
        out << "Cannot read root dir block" << "\n";
        return 1;
    }
    if (root.mark != DirMark) {
        out << "Warning: root page mark mismatch\n";
        return 1;
    }

    fs.vol.rebuildUsedFromDir();

    if (cmd == "ls") {
        listVolume(&fs, out);
        return 0;
    } else if (cmd == "add") {
        if (args.size() != 5) {
            out << "add requires <hostPath> <oberonName>\n";
            return 1;
        }
        QString hostPath = args[3];
        QString obName   = args[4];

        FileName fname;
        int res;
        Check(obName, fname, res);
        if (res != 0 && res != -1) {
            out << "Invalid Oberon name\n";
            return 1;
        }

        File f = NewFile(&fs, fname);
        if (!writeHostFileIntoAos(f, hostPath)) {
            out << "Failed to write\n";
            return 1;
        }

        out << "Added " << obName << "\n";
        return 0;
    } else if (cmd == "rm") {
        if (args.size() != 4) {
            out << "rm requires <oberonName>\n";
            return 1;
        }
        QString obName = args[3];

        FileName fname;
        int res;
        Check(obName, fname, res);
        if (res != 0 && res != -1) {
            out << "Invalid Oberon name\n";
            return 1;
        }

        DiskAdr key;
        int dres = deleteFile(&fs, fname, key);
        if (dres != 0) {
            out << "Not found\n";
            return 1;
        }
        out << "Removed " << obName << "\n";
        return 0;
    } else {
        out << "Unknown command\n";
        return 1;
    }
}

