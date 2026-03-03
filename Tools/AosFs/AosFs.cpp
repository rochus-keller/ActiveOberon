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

#include <QDir>

// Constants from OFSAosFiles.Mod

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
        // Pad the file size to QEMU's default CHS cylinder boundary
        // 1 Cylinder = 16 heads * 63 sectors * 512 bytes = 516,096 bytes.
        // 516,096 bytes is exactly 126 AosFS blocks (4096 bytes).
        // Prevents QEMU from rounding down the apparent disk size and crashing Oberon.
        qint32 remainder = blocks % 126;
        if (remainder != 0) {
            blocks += (126 - remainder);
        }

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
        if (!GetSector(vol, dadr, &a)) {
            A = 0;
            return;
        }
        if (a.mark != DirMark) {
            A = 0;
            return;
        }
        int L = 0, R = a.m;
        while (L < R) {
            int i = (L + R) / 2;
            if (compareName(name, a.e[i].name) <= 0)
                R = i;
            else
                L = i + 1;
        }
        if (R < a.m && compareName(name, a.e[R].name) == 0) {
            A = a.e[R].adr;
            return;
        }
        DiskAdr next = (R == 0) ? a.p0 : a.e[R-1].p;
        if (next == 0) {
            A = 0;
            return;
        }
        dadr = next;
    }
}

static void insert(Volume& vol, const FileName& name, DiskAdr dpg0,
                   bool& h, DirEntry& v, DiskAdr fad, DiskAdr& replacedFad) {
    DirPage a;
    if (!GetSector(vol, dpg0, &a)) return;

    int L = 0, R = a.m;
    while (L < R) {
        int i = (L + R) / 2;
        if (compareName(name, a.e[i].name) <= 0) R = i;
        else L = i + 1;
    }

    replacedFad = 0;
    if (R < a.m && compareName(name, a.e[R].name) == 0) {
        replacedFad = a.e[R].adr;
        a.e[R].adr = fad;
        PutSector(vol, dpg0, &a); // replace
    } else { // not on this page
        DiskAdr dpg1 = (R == 0) ? a.p0 : a.e[R - 1].p;
        DirEntry u;
        if (dpg1 == 0) { // not in tree, insert
            u.adr = fad; u.p = 0; h = true;
            memcpy(&u.name, &name, sizeof(FileName));
        } else {
            insert(vol, name, dpg1, h, u, fad, replacedFad);
        }

        if (h) { // insert u to the left of e[R]
            if (a.m < DirPgSize) {
                h = false;
                int i = a.m;
                while (i > R) { a.e[i] = a.e[i - 1]; --i; }
                a.e[R] = u;
                a.m++;
                PutSector(vol, dpg0, &a);
            } else { // split page and assign the middle element to v
                a.m = N; a.mark = DirMark;
                DiskAdr newDpg;
                AllocSector(vol, dpg0, newDpg);

                DirPage newPage;
                memset(&newPage, 0, sizeof(DirPage));
                newPage.mark = DirMark;
                newPage.m = N;

                if (R < N) { // insert in left half
                    v = a.e[N - 1];
                    int i = N - 1;
                    while (i > R) { a.e[i] = a.e[i - 1]; --i; }
                    a.e[R] = u;
                    PutSector(vol, dpg0, &a);

                    i = 0;
                    while (i < N) { newPage.e[i] = a.e[i + N]; ++i; }
                } else { // insert in right half
                    PutSector(vol, dpg0, &a);

                    int i = 0;
                    int R_adj = R - N;
                    if (R_adj == 0) {
                        v = u;
                    } else {
                        v = a.e[N];
                        while (i < R_adj - 1) { newPage.e[i] = a.e[N + 1 + i]; ++i; }
                        newPage.e[i] = u; ++i;
                    }
                    while (i < N) { newPage.e[i] = a.e[N + i]; ++i; }
                }
                newPage.p0 = v.p;
                v.p = newDpg;
                PutSector(vol, newDpg, &newPage);
            }
        }
    }
}

static void Insert(Volume& vol, const FileName& name, DiskAdr fad, DiskAdr& replacedFad) {
    bool h = false;
    DirEntry U;
    insert(vol, name, DirRootAdr, h, U, fad, replacedFad);
    if (h) { // root overflow
        DirPage a;
        GetSector(vol, DirRootAdr, &a);

        DiskAdr oldroot;
        AllocSector(vol, DirRootAdr, oldroot);
        PutSector(vol, oldroot, &a);

        DirPage newRoot;
        memset(&newRoot, 0, sizeof(DirPage));
        newRoot.mark = DirMark;
        newRoot.m = 1;
        newRoot.p0 = oldroot;
        newRoot.e[0] = U;
        PutSector(vol, DirRootAdr, &newRoot);
    }
}

static void underflow(Volume& vol, DirPage& c, DiskAdr dpg0, int s, bool& h) {
    DirPage a, b;
    DiskAdr dpg1;
    GetSector(vol, dpg0, &a);

    if (s < c.m) {
        dpg1 = c.e[s].p;
        GetSector(vol, dpg1, &b);
        int k = (b.m - N + 1) / 2;
        a.e[N - 1] = c.e[s];
        a.e[N - 1].p = b.p0;
        if (k > 0) {
            int i = 0;
            while (i < k - 1) { a.e[i + N] = b.e[i]; ++i; }
            c.e[s] = b.e[i];
            b.p0 = c.e[s].p;
            c.e[s].p = dpg1;
            b.m -= k;
            i = 0;
            while (i < b.m) { b.e[i] = b.e[i + k]; ++i; }
            PutSector(vol, dpg1, &b);
            a.m = N - 1 + k;
            h = false;
        } else {
            int i = 0;
            while (i < N) { a.e[i + N] = b.e[i]; ++i; }
            i = s;
            c.m--;
            while (i < c.m) { c.e[i] = c.e[i + 1]; ++i; }
            a.m = 2 * N;
            h = c.m < N;
            FreeSector(vol, dpg1);
        }
        PutSector(vol, dpg0, &a);
    } else {
        s--;
        dpg1 = (s == 0) ? c.p0 : c.e[s - 1].p;
        GetSector(vol, dpg1, &b);
        int k = (b.m - N + 1) / 2;
        if (k > 0) {
            int i = N - 1;
            while (i > 0) { --i; a.e[i + k] = a.e[i]; }
            i = k - 1;
            a.e[i] = c.e[s];
            a.e[i].p = a.p0;
            b.m -= k;
            while (i > 0) { --i; a.e[i] = b.e[i + b.m + 1]; }
            c.e[s] = b.e[b.m];
            a.p0 = c.e[s].p;
            c.e[s].p = dpg0;
            a.m = N - 1 + k;
            h = false;
            PutSector(vol, dpg0, &a);
        } else {
            c.e[s].p = a.p0;
            b.e[N] = c.e[s];
            int i = 0;
            while (i < N - 1) { b.e[i + N + 1] = a.e[i]; ++i; }
            b.m = 2 * N;
            c.m--;
            h = c.m < N;
            FreeSector(vol, dpg0);
        }
        PutSector(vol, dpg1, &b);
    }
}

static void del(Volume& vol, DiskAdr dpg1, bool& h, DirPage& a, int R) {
    DirPage b;
    GetSector(vol, dpg1, &b);
    DiskAdr dpg2 = b.e[b.m - 1].p;
    if (dpg2 != 0) {
        del(vol, dpg2, h, a, R);
        if (h) {
            underflow(vol, b, dpg2, b.m, h);
            PutSector(vol, dpg1, &b);
        }
    } else {
        b.e[b.m - 1].p = a.e[R].p;
        a.e[R] = b.e[b.m - 1];
        b.m--;
        h = b.m < N;
        PutSector(vol, dpg1, &b);
    }
}

static void deleteNode(Volume& vol, const FileName& name, DiskAdr dpg0, bool& h, DiskAdr& fad) {
    DirPage a;
    if (!GetSector(vol, dpg0, &a)) return;

    int L = 0, R = a.m;
    while (L < R) {
        int i = (L + R) / 2;
        if (compareName(name, a.e[i].name) <= 0) R = i;
        else L = i + 1;
    }

    DiskAdr dpg1 = (R == 0) ? a.p0 : a.e[R - 1].p;
    if (R < a.m && compareName(name, a.e[R].name) == 0) {
        fad = a.e[R].adr;
        if (dpg1 == 0) {
            a.m--;
            h = a.m < N;
            int i = R;
            while (i < a.m) { a.e[i] = a.e[i + 1]; ++i; }
        } else {
            del(vol, dpg1, h, a, R);
            if (h) underflow(vol, a, dpg1, R, h);
        }
        PutSector(vol, dpg0, &a);
    } else if (dpg1 != 0) {
        deleteNode(vol, name, dpg1, h, fad);
        if (h) {
            underflow(vol, a, dpg1, R, h);
            PutSector(vol, dpg0, &a);
        }
    } else {
        fad = 0;
    }
}

static void DirDelete(Volume& vol, const FileName& name, DiskAdr& fad) {
    bool h = false;
    deleteNode(vol, name, DirRootAdr, h, fad);
    if (h) { // root underflow
        DirPage a;
        GetSector(vol, DirRootAdr, &a);
        if (a.m == 0 && a.p0 != 0) {
            DiskAdr newroot = a.p0;
            GetSector(vol, newroot, &a);
            PutSector(vol, DirRootAdr, &a); // discard newroot
            FreeSector(vol, newroot);
        }
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
    // first character must be a letter
    if (!((ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z'))) {
        res = 3;
        return;
    }

    while (true) {
        // do NOT change case here; preserve original spelling
        name.name[i] = ch;
        ++i;

        if (i >= ba.size()) {
            // zero‑pad the remaining bytes
            while (i < FnLength) name.name[i++] = 0;
            res = 0;
            return;
        }

        ch = ba[i];

        // allow letters, digits and '.'
        if (!(((ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z')) ||
              (ch >= '0' && ch <= '9') || ch == '.')) {
            res = 3;
            return;
        }

        if (i == FnLength - 1) {
            res = 4;   // too long
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

static void PurgeOnDisk(FileSystem* fs, DiskAdr hdadr);

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
            f->sechint = secAdr; // Update hint to keep contiguous allocation
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
            f->sechint = secAdr; // Update hint
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
            f->sechint = subAdr; // Update hint
            PutSector(vol, subAdr, &sub->sec);
            superSec.x[i] = subAdr;
        }

        DiskAdr superAdr;
        if (!AllocSector(vol, f->sechint, superAdr)) return false;
        f->ext->adr = superAdr;
        f->sechint = superAdr; // Update hint
        h.ext = superAdr;
        PutSector(vol, superAdr, &superSec);
    }

    // final header update
    UpdateHeader(f, h);
    PutSector(vol, headerAdr, &h);

    // Insert into the tree and safely overwrite collisions
    DiskAdr replacedFad = 0;
    Insert(vol, f->name, headerAdr, replacedFad);

    if (replacedFad != 0 && replacedFad != headerAdr) {
        PurgeOnDisk(f->fs, replacedFad); // Replaces existing file of same name
    }
    return true;
}

// Resolve the DiskAdr of page 'pos' (0 = header, 1..aleng = data pages)
// using the same scheme as OFSAosFiles.ReadBuf. pos>0 only is used here.
static bool getFilePageSector(Volume& vol,
                              const FileHeader& h,
                              qint32 pos,
                              DiskAdr& secAdr)
{
    if (pos < 0) return false;
    if (pos < STS) {
        secAdr = h.sec.sec[pos];
        return secAdr != 0;
    }

    if (h.ext == 0) return false;

    qint32 xpos = pos - STS;
    qint32 i = xpos / XS;
    qint32 k = xpos % XS;
    if (i < 0 || i >= XS) return false;

    IndexSector superSec;
    if (!GetSector(vol, h.ext, &superSec)) return false;

    DiskAdr subAdr = superSec.x[i];
    if (subAdr == 0) return false;

    IndexSector subSec;
    if (!GetSector(vol, subAdr, &subSec)) return false;

    secAdr = subSec.x[k];
    return secAdr != 0;
}

// Export an Aos file identified by FileName 'name' to 'hostPath'.
// Returns false on not found or any error.
static bool extractAosFile(FileSystem* fs,
                           const FileName& name,
                           const QString& hostPath)
{
    Volume& vol = fs->vol;

    // 1) Find header sector by name (directory B-tree Search)
    DiskAdr headerAdr = 0;
    Search(vol, name, headerAdr);
    if (headerAdr == 0) {
        return false; // not found
    }

    // 2) Read header
    FileHeader h;
    if (!GetSector(vol, headerAdr, &h)) {
        return false;
    }
    if (h.mark != HeaderMark) {
        return false; // invalid / deleted file
    }

    // 3) Compute data length (excluding header)
    qint64 dataLen = (qint64)h.aleng * SS + (qint64)h.bleng - HS;
    if (dataLen < 0) {
        return false;
    }

    // 4) Open host file for writing in current directory
    QFile host(hostPath);
    if (!host.open(QIODevice::WriteOnly | QIODevice::Truncate)) {
        return false;
    }

    qint64 remaining = dataLen;

    // 5) First chunk is stored in header.data
    qint64 first = qMin(remaining, (qint64)(SS - HS));
    if (first > 0) {
        if (host.write(h.data, first) != first) {
            return false;
        }
        remaining -= first;
    }

    if (remaining <= 0) {
        return true; // everything was in the header
    }

    // 6) Remaining data sectors: pages 1..h.aleng
    DataSector ds;
    for (qint32 pos = 1; pos <= h.aleng && remaining > 0; ++pos) {
        DiskAdr secAdr = 0;
        if (!getFilePageSector(vol, h, pos, secAdr)) {
            return false;
        }
        if (!GetSector(vol, secAdr, &ds)) {
            return false;
        }

        qint64 toWrite = qMin(remaining, (qint64)SS);
        if (toWrite > 0) {
            if (host.write(reinterpret_cast<const char*>(ds.B), toWrite)
                != toWrite) {
                return false;
            }
            remaining -= toWrite;
        }
    }

    return remaining == 0;
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

void Volume::rebuildUsedFromDir() {
    if (sizeBlocks <= 0) return;
    used.resize(sizeBlocks);
    used.fill(false); // explicitly clear memory mapping

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

    // 1. Create the AosFS Boot Block (Block 0)
    QByteArray blockZero(SS, 0);
    unsigned char* boot = reinterpret_cast<unsigned char*>(blockZero.data());

    // Oberon counts disk layout in 512-byte blocks.
    // We reserve the first 4096 bytes (8 * 512-byte blocks) for the boot sector.
    qint32 reservedBlocks = 8;

    // The rest of the 4K blocks belong to the filesystem
    qint32 fsSizeBlocks = vol.size() - 1;

    // Offset 0x1F0: Reserved blocks before FS starts
    boot[0x1F0] = (reservedBlocks & 0xFF);
    boot[0x1F1] = ((reservedBlocks >> 8) & 0xFF);
    boot[0x1F2] = ((reservedBlocks >> 16) & 0xFF);
    boot[0x1F3] = ((reservedBlocks >> 24) & 0xFF);

    // Offset 0x1F4: Size of the filesystem in 4K blocks
    boot[0x1F4] = (fsSizeBlocks & 0xFF);
    boot[0x1F5] = ((fsSizeBlocks >> 8) & 0xFF);
    boot[0x1F6] = ((fsSizeBlocks >> 16) & 0xFF);
    boot[0x1F7] = ((fsSizeBlocks >> 24) & 0xFF);

    // Offset 0x1F8: "AOS!" Filesystem ID
    boot[0x1F8] = 0x41; // 'A'
    boot[0x1F9] = 0x4F; // 'O'
    boot[0x1FA] = 0x53; // 'S'
    boot[0x1FB] = 0x21; // '!'

    // Offset 0x1FC: FS Version (1)
    boot[0x1FC] = 1;

    // Offset 0x1FD: Log2 of Sector Size (12, because 2^12 = 4096)
    boot[0x1FD] = 12;

    // Offset 0x1FE: Standard PC Boot Signature (0x55AA)
    boot[0x1FE] = 0x55;
    boot[0x1FF] = 0xAA;

    // Write the boot metadata to physical block 0
    if (!vol.putBlock(0, blockZero.constData())) return false;

    // Create the Root Directory (Block 1)
    DirPage root;
    memset(&root, 0, sizeof(root));
    root.mark = DirMark;
    root.m    = 0;
    root.p0   = 0;

    // DirRootAdr is 1*SF, which writes exactly to block 1
    if (!PutSector(vol, DirRootAdr, &root)) return false;

    vol.rebuildUsedFromDir();
    return true;
}

typedef QList< QPair<QString,int> > Files;
static void listVolume(FileSystem* fs, Files& out) {
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
                out << qMakePair(name, size);
            }
            if (page.e[i].p != 0) stack.push_back(page.e[i].p);
        }
    }
}

static void listVolume(FileSystem* fs, QTextStream& out) {
    Files files;
    listVolume(fs, files);
    qSort(files);
    for( int i = 0; i < files.size(); i++ )
        out << files[i].first << "  " << files[i].second << " bytes\n";

}

static int deleteFile(FileSystem* fs, const FileName& name, DiskAdr& key) {
    Volume& vol = fs->vol;
    DiskAdr adr;
    DirDelete(vol, name, adr);  // Perform full B-tree deletion
    key = adr;
    if (adr == 0) return 2;
    FileHeader hd;
    if (!GetSector(vol, adr, &hd)) return 2;
    hd.mark = HeaderMark + 1;
    PutSector(vol, adr, &hd);
    PurgeOnDisk(fs, adr);
    return 0;
}

// Boot / partition layout helpers (from Partitions.Mod)

static const int BS = 512;              // boot / partition block size (Disks.blockSize)
static const int LoaderSize = 4;        // OBL.Bin size in 512-byte blocks
static const qint32 FSID_AOS = 0x21534F41; // "AOS!" in little endian
static const qint32 ConfigType = 8;     // config-string entry type
static const qint32 FragType = 7;       // bootfile fragment entry type

// Little-endian helpers for a QByteArray representing raw disk bytes (512-byte blocks)
static inline qint32 get4(const QByteArray& b, int pos)
{
    const unsigned char* p =
        reinterpret_cast<const unsigned char*>(b.constData() + pos);
    return (qint32)(p[0] | (p[1] << 8) | (p[2] << 16) | (p[3] << 24));
}

static inline qint32 get2(const QByteArray& b, int pos)
{
    const unsigned char* p =
        reinterpret_cast<const unsigned char*>(b.constData() + pos);
    return (qint32)(p[0] | (p[1] << 8));
}

static inline void put4(QByteArray& b, int pos, qint32 v)
{
    unsigned char* p = reinterpret_cast<unsigned char*>(b.data() + pos);
    p[0] = (unsigned char)(v & 0xFF);
    p[1] = (unsigned char)((v >> 8) & 0xFF);
    p[2] = (unsigned char)((v >> 16) & 0xFF);
    p[3] = (unsigned char)((v >> 24) & 0xFF);
}

// Small helper for printing %HH escapes when dumping config values
static inline char hexDigit(int v)
{
    v &= 0xF;
    return (char)(v < 10 ? ('0' + v) : ('A' + (v - 10)));
}

// Helpers for decoding %HH when reading config from a text file
static inline bool isHexChar(QChar ch)
{
    ushort c = ch.unicode();
    return (c >= '0' && c <= '9') ||
           (c >= 'A' && c <= 'F') ||
           (c >= 'a' && c <= 'f');
}

static inline int hexVal(QChar ch)
{
    ushort c = ch.unicode();
    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'A' && c <= 'F') return 10 + (c - 'A');
    if (c >= 'a' && c <= 'f') return 10 + (c - 'a');
    return 0;
}

// Read OBL variables (tsize, reserved, fsOfs) from the first 512-byte block.
// Returns false if no valid Oberon boot block is present.
static bool getBootParams(const QString& imagePath,
                          int& tsize,
                          int& reserved,
                          qint32& fsOfs)
{
    QFile f(imagePath);
    if (!f.open(QIODevice::ReadOnly))
        return false;

    QByteArray b = f.read(BS);
    if (b.size() != BS)
        return false;

    // Check standard 0x55AA signature
    if ((unsigned char)b[510] != 0x55 || (unsigned char)b[511] != 0xAA)
        return false;

    // Check "OBERON" signature at bytes 3..8
    if (b.mid(3, 6) != QByteArray("OBERON"))
        return false;

    // Config table size in blocks
    tsize = (unsigned char)b[0x10];
    if (tsize <= 0)
        return false;

    // Reserved blocks (includes loader, config table, boot file area, maybe N2KFS)
    reserved = get2(b, 0x0E);
    if (reserved < LoaderSize + tsize)
        return false;

    // Check for AosFS table ("AOS!") id at 0x1F8
    qint32 id = get4(b, 0x1F8);
    if (id == FSID_AOS) {
        fsOfs = get4(b, 0x1F0);   // fileSystemOfs in 512-byte blocks
    } else {
        // Old layout: use reserved for fsOfs
        fsOfs = reserved;
    }

    return true;
}

// Detect where the AosFS volume starts in a raw image.
// If an Oberon boot block is present, use its fsOfs; otherwise,
// fall back to treating the image as a bare AosFS volume and
// look for DirMark in the first 4K sector.
static bool detectAosFsOffset(const QString& imagePath,
                              qint64& baseOffsetBytes)
{
    int tsize = 0;
    int reserved = 0;
    qint32 fsOfs = 0;

    // Try boot parameters first (for Native Oberon partitioned disk images)
    if (getBootParams(imagePath, tsize, reserved, fsOfs)) {
        baseOffsetBytes = (qint64)fsOfs * BS;
        return true;
    }

    // Fallback: bare AosFS volume
    // The root directory sector (DirRootAdr) is at block 1 (offset SS / 4096 bytes)
    QFile f(imagePath);
    if (!f.open(QIODevice::ReadOnly))
        return false;

    if (!f.seek(SS)) // Seek to block 1
        return false;

    QByteArray first = f.read(SS);
    if (first.size() != SS)
        return false;

    qint32 mark = 0;
    ::memcpy(&mark, first.constData(), sizeof(qint32));
    if (mark == DirMark) {
        baseOffsetBytes = 0;
        return true;
    }

    return false;
}

// Read the config table into 'table' and return its size in blocks (tsize).
// Returns false if there is no valid Oberon boot block.
static bool readConfigTable(const QString& imagePath,
                            QByteArray& table,
                            int& tsize)
{
    int reserved = 0;
    qint32 fsOfs = 0;
    if (!getBootParams(imagePath, tsize, reserved, fsOfs))
        return false;

    QFile f(imagePath);
    if (!f.open(QIODevice::ReadOnly))
        return false;

    qint64 offset = (qint64)LoaderSize * BS; // blocks 4..(4+tsize-1)
    if (!f.seek(offset))
        return false;

    table.resize(tsize * BS);
    qint64 n = f.read(table.data(), table.size());
    return n == table.size();
}

// Write back the config table (same size tsize*BS).
static bool writeConfigTable(const QString& imagePath,
                             const QByteArray& table,
                             int tsize)
{
    QFile f(imagePath);
    if (!f.open(QIODevice::ReadWrite))
        return false;

    qint64 offset = (qint64)LoaderSize * BS;
    if (!f.seek(offset))
        return false;

    qint64 n = f.write(table.constData(), tsize * BS);
    return n == (qint64)tsize * BS;
}

// Find the next entry of the specified type in 'table', starting at byte index 'start'.
// Returns the byte index of the entry header (where the type field lives), or -1.
static int findEntryType(const QByteArray& table,
                         int start,
                         qint32 type)
{
    int pos = start;
    const int len = table.size();

    while (pos + 8 <= len) {
        qint32 t = get4(table, pos);
        qint32 size = get4(table, pos + 4);
        if (t == type)
            return pos;
        if (t == -1)
            return -1;
        if (size <= 0 || pos + size > len)
            return -1;
        pos += size;
    }
    return -1;
}

// Print configuration strings (type 8 entry) to 'out'.
static bool printConfigToStream(const QString& imagePath,
                                QTextStream& out)
{
    QByteArray table;
    int tsize = 0;
    if (!readConfigTable(imagePath, table, tsize))
        return false;

    int pos = findEntryType(table, 0, ConfigType);
    if (pos < 0) {
        out << "No config (type 8) entry found\n";
        return true; // table is valid but has no config entry
    }

    int dataPos = pos + 8;
    const int endPos = pos + get4(table, pos + 4);
    if (endPos > table.size())
        return false;

    out << "Config:\n";

    while (dataPos < endPos && table[dataPos] != 0) {
        // Read name
        QByteArray name;
        while (dataPos < endPos && table[dataPos] != 0) {
            name.append(table[dataPos]);
            ++dataPos;
        }
        if (dataPos >= endPos)
            break;
        ++dataPos; // skip name 0

        // Read value
        QByteArray value;
        while (dataPos < endPos && table[dataPos] != 0) {
            value.append(table[dataPos]);
            ++dataPos;
        }
        if (dataPos >= endPos)
            break;
        ++dataPos; // skip value 0

        // Print: name="value"
        out << " " << name.constData() << "=\"";
        for (int i = 0; i < value.size(); ++i) {
            unsigned char ch = (unsigned char)value[i];
            if (ch >= ' ' && ch < 0x7F && ch != '%') {
                out << (char)ch;
            } else {
                out << "%" << hexDigit(ch >> 4) << hexDigit(ch & 0xF);
            }
        }
        out << "\"\n";
    }

    return true;
}

// Parse a text file with lines of the form: NAME="value"
// and build a config payload (name 0 value 0 ... 0, final 0).
// Returns false on syntax error.
static bool buildConfigFromTextFile(const QString& cfgPath,
                                    QByteArray& payload,
                                    QTextStream& out)
{
    QFile cfg(cfgPath);
    if (!cfg.open(QIODevice::ReadOnly | QIODevice::Text)) {
        out << "Cannot open config file " << cfgPath << "\n";
        return false;
    }

    QTextStream in(&cfg);
    in.setCodec("ISO-8859-1");

    payload.clear();

    while (!in.atEnd()) {
        QString line = in.readLine();
        QString trimmed = line.trimmed();
        if (trimmed.isEmpty())
            continue;
        if (trimmed.startsWith('#'))
            continue;

        int eqPos = trimmed.indexOf('=');
        if (eqPos <= 0) {
            out << "Syntax error in config line: " << trimmed << "\n";
            return false;
        }

        QString nameStr = trimmed.left(eqPos).trimmed();
        QString rest = trimmed.mid(eqPos + 1).trimmed();
        if (rest.isEmpty() || !rest.startsWith('"')) {
            out << "Expected quoted value in line: " << trimmed << "\n";
            return false;
        }

        // Find closing quote
        int endQuote = rest.indexOf('"', 1);
        if (endQuote < 0) {
            out << "Unterminated value in line: " << trimmed << "\n";
            return false;
        }

        QString valStr = rest.mid(1, endQuote - 1);

        // Encode name as ASCII
        QByteArray nameBytes = nameStr.toLatin1();
        if (nameBytes.isEmpty()) {
            out << "Empty name in line: " << trimmed << "\n";
            return false;
        }

        // Decode %HH in value
        QByteArray valBytes;
        for (int i = 0; i < valStr.size(); ) {
            QChar ch = valStr.at(i);
            if (ch == '%' && i + 2 < valStr.size() &&
                isHexChar(valStr.at(i+1)) && isHexChar(valStr.at(i+2))) {
                int v = (hexVal(valStr.at(i+1)) << 4) + hexVal(valStr.at(i+2));
                if (v == 0) {
                    out << "Config value contains NUL (not allowed)\n";
                    return false;
                }
                valBytes.append((char)v);
                i += 3;
            } else {
                if (ch.unicode() == 0) {
                    out << "Config value contains NUL (not allowed)\n";
                    return false;
                }
                valBytes.append((char)ch.toLatin1());
                ++i;
            }
        }

        // Append name 0 value 0
        payload.append(nameBytes);
        payload.append('\0');
        payload.append(valBytes);
        payload.append('\0');
    }

    // Final 0 terminator
    payload.append('\0');
    return true;
}

// Replace all type-8 entries with a single new one built from 'payload'.
static bool writeConfigFromPayload(const QString& imagePath,
                                   const QByteArray& payload,
                                   QTextStream& out)
{
    QByteArray table;
    int tsize = 0;
    if (!readConfigTable(imagePath, table, tsize)) {
        out << "No valid Oberon boot block / config table\n";
        return false;
    }

    const int totalSize = table.size();
    QByteArray newTable(totalSize, 0);

    // Copy all entries except type 8 into newTable
    int posOld = 0;
    int posNew = 0;

    while (posOld + 8 <= totalSize) {
        qint32 t = get4(table, posOld);
        qint32 size = get4(table, posOld + 4);
        if (t == -1 || size <= 0 || posOld + size > totalSize)
            break;

        if (t != ConfigType) {
            if (posNew + size + 8 > totalSize) {
                out << "Config table overflow while copying entries\n";
                return false;
            }
            ::memcpy(newTable.data() + posNew,
                     table.constData() + posOld,
                     size);
            posNew += size;
        }

        posOld += size;
    }

    // Add new type-8 entry
    int dsize = payload.size();
    int entrySize = ((dsize + 3) / 4) * 4 + 8; // padded to multiple of 4 + header
    if (posNew + entrySize + 8 > totalSize) {
        out << "Config payload too large for table\n";
        return false;
    }

    put4(newTable, posNew, ConfigType);
    put4(newTable, posNew + 4, entrySize);

    ::memcpy(newTable.data() + posNew + 8,
             payload.constData(),
             dsize);

    // Zero padding
    int padStart = posNew + 8 + dsize;
    int padEnd   = posNew + entrySize;
    for (int i = padStart; i < padEnd; ++i)
        newTable[i] = 0;

    posNew += entrySize;

    // End marker (type = -1, size = 8)
    if (posNew + 8 <= totalSize) {
        put4(newTable, posNew, -1);
        put4(newTable, posNew + 4, 8);
        // Remaining bytes stay 0
    }

    if (!writeConfigTable(imagePath, newTable, tsize)) {
        out << "Failed to write config table\n";
        return false;
    }

    return true;
}

// Extract Native.Bin (boot file) from the boot area and write it to 'hostPath'.
static bool extractBootFileToHost(const QString& imagePath,
                                  const QString& hostPath,
                                  QTextStream& out)
{
    int tsize = 0;
    int reserved = 0;
    qint32 fsOfs = 0;
    if (!getBootParams(imagePath, tsize, reserved, fsOfs)) {
        out << "No valid Oberon boot block / AosFS info\n";
        return false;
    }

    QByteArray table;
    if (!readConfigTable(imagePath, table, tsize)) {
        out << "Cannot read config table\n";
        return false;
    }

    int pos = findEntryType(table, 0, FragType);
    if (pos < 0) {
        out << "No bootfile (type 7) entry found\n";
        return false;
    }

    int size = get4(table, pos + 4);
    int dataPos = pos + 8;
    if (dataPos + 20 > table.size() || pos + size > table.size()) {
        out << "Invalid bootfile entry in config table\n";
        return false;
    }

    // Layout from InitBootFile:
    // data[0]  = LoadAdr
    // data[4]  = Frags + (sum << 16)
    // data[8]  = StartAdr
    // data[12] = pos (rel. to start)
    // data[16] = number of blocks
    qint32 loadAdr    = get4(table, dataPos + 0);
    qint32 fragsSum   = get4(table, dataPos + 4);
    qint32 startAdr   = get4(table, dataPos + 8);
    qint32 relPos     = get4(table, dataPos + 12);
    qint32 numBlocks  = get4(table, dataPos + 16);

    Q_UNUSED(loadAdr);
    Q_UNUSED(startAdr);
    Q_UNUSED(relPos);

    int frags = fragsSum & 0xFFFF;
    if (frags != 1) {
        out << "Unsupported number of fragments: " << frags << "\n";
        return false;
    }
    if (numBlocks <= 0) {
        out << "Bootfile has zero size\n";
        return false;
    }

    qint32 startBlock = LoaderSize + tsize;
    qint64 bootOffset = (qint64)startBlock * BS;
    qint64 bootBytes  = (qint64)numBlocks * BS;
    qint64 fsOffset   = (qint64)fsOfs * BS;

    // Sanity: bootfile must lie completely before the filesystem start
    if (bootOffset + bootBytes > fsOffset) {
        out << "Bootfile overlaps filesystem area\n";
        return false;
    }

    QFile img(imagePath);
    if (!img.open(QIODevice::ReadOnly)) {
        out << "Cannot open image " << imagePath << "\n";
        return false;
    }
    if (!img.seek(bootOffset)) {
        out << "Seek failed in image\n";
        return false;
    }

    QFile host(hostPath);
    if (!host.open(QIODevice::WriteOnly | QIODevice::Truncate)) {
        out << "Cannot open output file " << hostPath << "\n";
        return false;
    }

    const qint64 chunk = 64 * 1024;
    qint64 remaining = bootBytes;
    QByteArray buf;
    buf.resize((int)qMin(chunk, remaining));

    while (remaining > 0) {
        qint64 toRead = qMin(chunk, remaining);
        if (buf.size() < toRead)
            buf.resize((int)toRead);
        qint64 n = img.read(buf.data(), (int)toRead);
        if (n != toRead) {
            out << "Short read while copying bootfile\n";
            return false;
        }
        if (host.write(buf.constData(), (int)toRead) != toRead) {
            out << "Write error while copying bootfile\n";
            return false;
        }
        remaining -= toRead;
    }

    out << "Extracted bootfile (" << numBlocks
        << " blocks) to " << hostPath << "\n";
    return true;
}

#ifndef _AOSFS_NO_MAIN_
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
        out << "  " << args[0] << " addir  <image> <hostDirPath>\n";
        out << "  " << args[0] << " get <image> <oberonName|pattern>\n";
        out << "  " << args[0] << " rm   <image> <oberonName>\n";
        out << "  " << args[0] << " getboot <image> [hostPath]\n";
        out << "  " << args[0] << " getconf <image>\n";
        out << "  " << args[0] << " setconf <image> <configTextFile>\n";
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

#if 0
    qint64 baseOffsetBytes = 512 * 1024;
#else
    // For all other commands, auto-detect AosFS offset (boot area vs bare volume)
    qint64 baseOffsetBytes = 0;
    if (!detectAosFsOffset(image, baseOffsetBytes)) {
        out << "Cannot detect AosFS in image " << image << "\n";
        return 1;
    }
#endif

    if (!fs.vol.openExisting(image, baseOffsetBytes)) {
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
    }  else if (cmd == "addir") {
        if (args.size() != 4) {
            out << "adddir requires <hostDirPath>\n";
            return 1;
        }
        QDir dir = args[3];

        QStringList files = dir.entryList( QDir::Files, QDir::Name );
        foreach( const QString& f, files )
        {
            FileName fname;
            int res;
            Check(f, fname, res);
            if (res != 0 && res != -1) {
                out << "Invalid Oberon name (not added): " << f << endl;
            }else
            {
                File of = NewFile(&fs, fname);
                if (!writeHostFileIntoAos(of, dir.absoluteFilePath(f))) {
                    out << "Failed to add " << f << endl;
                }else
                    out << "Added " << f << endl;
            }
        }
        return 0;
    }else if (cmd == "get") {
        if (args.size() != 4) {
            out << "get requires <oberonName|pattern>\n";
            return 1;
        }

        QString obName = args[3];
        QStringList obNames;
        if( obName.contains('*') || obName.contains('?') )
        {
            Files files;
            listVolume(&fs, files);
            for(int i = 0; i < files.size(); i++ )
                obNames << files[i].first;
            QRegExp rx(obName);
            rx.setPatternSyntax(QRegExp::Wildcard);  // interpret * and ? as wildcards
            rx.setCaseSensitivity(Qt::CaseInsensitive);
            obNames = obNames.filter(rx);
        }else
            obNames << obName;

        foreach( const QString& name, obNames )
        {
            FileName fname;
            int res;
            Check(name, fname, res);
            if (res != 0 && res != -1) {
                out << "Invalid Oberon name\n";
                return 1;
            }

            // Write to a file with the same name in the current directory.
            // You could also map to a different host filename if desired.
            QString hostPath = name;

            if (!extractAosFile(&fs, fname, hostPath)) {
                out << "Not found or failed to extract " << name << endl;
            }else
                out << "Extracted " << name << " -> " << hostPath << endl;
        }
        return 0;

    } else if (cmd == "getboot") {
        // Usage: aosfs getboot <image> [hostPath]
        QString hostPath;
        if (args.size() >= 4)
            hostPath = args[3];
        else
            hostPath = "Native.bin";

        if (!extractBootFileToHost(image, hostPath, out))
            return 1;
        return 0;

    } else if (cmd == "getconf") {
        // Usage: aosfs getconf <image>
        if (!printConfigToStream(image, out))
            return 1;
        return 0;

    } else if (cmd == "setconf") {
        // Usage: aosfs setconf <image> <configTextFile>
        if (args.size() != 4) {
            out << "setconf requires <image> <configTextFile>\n";
            return 1;
        }
        QString cfgPath = args[3];
        QByteArray payload;
        if (!buildConfigFromTextFile(cfgPath, payload, out))
            return 1;
        if (!writeConfigFromPayload(image, payload, out))
            return 1;
        out << "Config written\n";
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
#endif

