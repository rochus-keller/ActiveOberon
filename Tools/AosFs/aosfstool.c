/*
 * aosfstool.c - Oberon Aos File System Tool
 *
 * A C99 tool to create, list, add, get, and remove files on
 * Oberon Aos file system volumes (raw image files, with or without MBR).
 *
 * Based on OFSAosFiles.Mod and OFSDiskVolumes.Mod from ETH Oberon.
 *
 * Copyright (C) 2025 Rochus Keller (me@rochus-keller.ch)
 *
 * GNU Lesser General Public License Usage
 * This file may be used under the terms of the GNU Lesser
 * General Public License version 2.1 or version 3 as published by the Free
 * Software Foundation and appearing in the file LICENSE.LGPLv21 and
 * LICENSE.LGPLv3 included in the packaging of this file. Please review the
 * following information to ensure the GNU Lesser General Public License
 * requirements will be met: https://www.gnu.org/licenses/lgpl.html and
 * http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <time.h>
#include <errno.h>

/* ---- Constants ---- */

#define SF          29        /* SectorFactor: DiskAdr = blockNum * SF */
#define SS          4096      /* SectorSize (bytes per block) */
#define BS          512       /* Device block size */
#define HS          568       /* FileHeader overhead in first sector */
#define FnLength    32        /* Max filename length including NUL */
#define STS         128       /* Number of direct sector refs in header */
#define XS          (SS / 4)  /* IndexSize = 1024 entries per index sector */
#define DirPgSize   102       /* Entries per directory page */
#define N           (DirPgSize / 2)  /* 51 - minimum fill for B-tree node */
#define FillerSize  4

#define DirRootAdr  (1 * SF)  /* Directory root disk address */
#define InitHint    (200 * SF)
#define MinVolSize  4         /* Minimum volume size in blocks */

#define DirMark     0x9B1EA38DU
#define HeaderMark  0x9BA71D86U
#define MapMark     0x9C2F977FU

#define FSID        0x21534F41U  /* "AOS!" */
#define FSVer       1
#define AosSS       4096

#define MapIndexSize ((SS - 4) / 4)  /* 1023 */
#define MapSize     (SS / 4)         /* 1024 */

#define MAX_FILES   8192  /* Max files we support listing */

/* ---- On-disk types (all little-endian, packed) ---- */

typedef int32_t  DiskAdr;
typedef uint8_t  Sector[SS];

typedef struct {
    char name[FnLength];  /* 32 bytes */
    DiskAdr adr;          /*  4 bytes - sector address of file header */
    DiskAdr p;            /*  4 bytes - sector address of child page */
} DirEntry;               /* 40 bytes total */

typedef struct {
    uint32_t mark;                    /*   4 */
    int32_t  m;                       /*   4 */
    DiskAdr  p0;                      /*   4 */
    char     fill[FillerSize];        /*   4 */
    DirEntry e[DirPgSize];            /* 102 * 40 = 4080 */
} DirPage;                            /* 4096 total */

typedef struct {
    uint32_t mark;                    /*   4 */
    char     name[FnLength];          /*  32 */
    int32_t  aleng;                   /*   4 */
    int32_t  bleng;                   /*   4 */
    int32_t  date;                    /*   4 */
    int32_t  time_;                   /*   4 */
    DiskAdr  sec[STS];                /* 512 */
    DiskAdr  ext;                     /*   4 */
    uint8_t  data[SS - HS];           /* 3528 */
} FileHeader;                         /* 4096 total */

typedef struct {
    DiskAdr x[XS];                    /* 1024 * 4 = 4096 */
} IndexSector;

typedef struct {
    uint32_t mark;                    /*    4 */
    DiskAdr  index[MapIndexSize];     /* 1023 * 4 = 4092 */
} MapIndex;                           /* 4096 */

typedef struct {
    uint32_t map[MapSize];            /* 1024 * 4 = 4096 */
} MapSector;

/* ---- Volume structure ---- */

typedef struct {
    FILE    *fp;
    int32_t  reserved;    /* offset in BS-byte blocks to start of FS */
    int32_t  size;        /* number of SS-byte blocks in the FS */
    /* In-memory allocation bitmap: 1 = allocated, 0 = free */
    uint32_t *bitmap;     /* ceil(size+1 / 32) uint32_t words */
    int32_t  bitmap_words;
} Volume;

/* ---- Public file info struct ---- */

typedef struct {
    char     name[FnLength];
    DiskAdr  headerAdr;   /* disk address of file header sector */
    uint32_t mark;
    int32_t  aleng;
    int32_t  bleng;
    int32_t  date;        /* raw Oberon-encoded date */
    int32_t  time_;       /* raw Oberon-encoded time */
    DiskAdr  ext;
    int32_t  size;        /* computed file size in bytes (data only) */
} FileInfo;

/* ---- Helpers ---- */

static void put4(uint8_t *buf, int off, uint32_t val) {
    buf[off+0] = (uint8_t)(val);
    buf[off+1] = (uint8_t)(val >> 8);
    buf[off+2] = (uint8_t)(val >> 16);
    buf[off+3] = (uint8_t)(val >> 24);
}

static uint32_t get4(const uint8_t *buf, int off) {
    return (uint32_t)buf[off]
         | ((uint32_t)buf[off+1] << 8)
         | ((uint32_t)buf[off+2] << 16)
         | ((uint32_t)buf[off+3] << 24);
}

/* ---- Sector I/O ---- */

/* Read a 4096-byte block at DiskAdr (which is block_num * SF).
   block_num is 1-based. Byte offset = (reserved + (block_num-1)*8) * 512 */
static int GetSector(Volume *vol, DiskAdr adr, void *dest) {
    if (adr % SF != 0) {
        fprintf(stderr, "GetSector: bad alignment adr=%d\n", adr);
        return -1;
    }
    int32_t block_num = adr / SF;
    if (block_num < 1 || block_num > vol->size) {
        fprintf(stderr, "GetSector: block %d out of range [1..%d]\n", block_num, vol->size);
        return -1;
    }
    long offset = ((long)vol->reserved + (long)(block_num - 1) * (SS / BS)) * BS;
    if (fseek(vol->fp, offset, SEEK_SET) != 0) {
        perror("GetSector: fseek");
        return -1;
    }
    if (fread(dest, SS, 1, vol->fp) != 1) {
        perror("GetSector: fread");
        return -1;
    }
    return 0;
}

static int PutSector(Volume *vol, DiskAdr adr, const void *src) {
    if (adr % SF != 0) {
        fprintf(stderr, "PutSector: bad alignment adr=%d\n", adr);
        return -1;
    }
    int32_t block_num = adr / SF;
    if (block_num < 1 || block_num > vol->size) {
        fprintf(stderr, "PutSector: block %d out of range [1..%d]\n", block_num, vol->size);
        return -1;
    }
    long offset = ((long)vol->reserved + (long)(block_num - 1) * (SS / BS)) * BS;
    if (fseek(vol->fp, offset, SEEK_SET) != 0) {
        perror("PutSector: fseek");
        return -1;
    }
    if (fwrite(src, SS, 1, vol->fp) != 1) {
        perror("PutSector: fwrite");
        return -1;
    }
    return 0;
}

/* ---- Bitmap operations ---- */

static int IsMarked(Volume *vol, DiskAdr adr) {
    int32_t block = adr / SF;
    if (block < 1 || block > vol->size) return 0;
    return (vol->bitmap[block / 32] >> (block % 32)) & 1;
}

static void MarkBlock(Volume *vol, DiskAdr adr) {
    int32_t block = adr / SF;
    if (block < 1 || block > vol->size) return;
    vol->bitmap[block / 32] |= (1U << (block % 32));
}

static void FreeBlock(Volume *vol, DiskAdr adr) {
    int32_t block = adr / SF;
    if (block < 1 || block > vol->size) return;
    vol->bitmap[block / 32] &= ~(1U << (block % 32));
}

/* Allocate a sector, searching from hint upward, wrapping around.
   Returns the DiskAdr (block*SF) or 0 on failure. */
static DiskAdr AllocSector(Volume *vol, DiskAdr hint) {
    int32_t start = hint / SF;
    if (start < 1) start = 1;
    if (start > vol->size) start = 1;

    /* Search from start to end */
    for (int32_t b = start; b <= vol->size; b++) {
        if (!((vol->bitmap[b / 32] >> (b % 32)) & 1)) {
            vol->bitmap[b / 32] |= (1U << (b % 32));
            return b * SF;
        }
    }
    /* Wrap around */
    for (int32_t b = 1; b < start; b++) {
        if (!((vol->bitmap[b / 32] >> (b % 32)) & 1)) {
            vol->bitmap[b / 32] |= (1U << (b % 32));
            return b * SF;
        }
    }
    fprintf(stderr, "AllocSector: volume full\n");
    return 0;
}

/* ---- Build bitmap by traversing directory ---- */

/* Forward declarations */
static void TraverseDirForBitmap(Volume *vol, DiskAdr dpg, DiskAdr *file_addrs, int *file_count);
static void MarkFileSectors(Volume *vol, DiskAdr *file_addrs, int file_count);

static void BuildBitmap(Volume *vol) {
    /* Clear bitmap */
    memset(vol->bitmap, 0, vol->bitmap_words * sizeof(uint32_t));

    DiskAdr file_addrs[MAX_FILES];
    int file_count = 0;

    /* Traverse directory tree to collect all file header addresses and mark dir pages */
    TraverseDirForBitmap(vol, DirRootAdr, file_addrs, &file_count);

    /* Mark file sectors */
    MarkFileSectors(vol, file_addrs, file_count);
}

static void TraverseDirForBitmap(Volume *vol, DiskAdr dpg, DiskAdr *file_addrs, int *file_count) {
    DirPage a;
    if (GetSector(vol, dpg, &a) != 0) return;
    MarkBlock(vol, dpg);

    for (int i = 0; i < a.m; i++) {
        if (*file_count < MAX_FILES) {
            file_addrs[*file_count] = a.e[i].adr;
            (*file_count)++;
        }
    }

    if (a.p0 != 0) {
        TraverseDirForBitmap(vol, a.p0, file_addrs, file_count);
        for (int i = 0; i < a.m; i++) {
            if (a.e[i].p != 0) {
                TraverseDirForBitmap(vol, a.e[i].p, file_addrs, file_count);
            }
        }
    }
}

static void MarkFileSectors(Volume *vol, DiskAdr *file_addrs, int file_count) {
    FileHeader hd;
    IndexSector supi, subi;

    for (int f = 0; f < file_count; f++) {
        if (GetSector(vol, file_addrs[f], &hd) != 0) continue;

        int32_t aleng = hd.aleng;
        int32_t sec_count = aleng + 1;
        if (sec_count > STS) sec_count = STS;

        for (int i = 0; i < sec_count; i++) {
            if (hd.sec[i] != 0) MarkBlock(vol, hd.sec[i]);
        }

        if (aleng >= STS && hd.ext != 0) {
            MarkBlock(vol, hd.ext);
            if (GetSector(vol, hd.ext, &supi) != 0) continue;

            int32_t n = (aleng - STS) / XS;
            for (int i = 0; i <= n; i++) {
                if (supi.x[i] != 0) {
                    MarkBlock(vol, supi.x[i]);
                    if (GetSector(vol, supi.x[i], &subi) != 0) continue;
                    int32_t jmax = (i < n) ? XS : ((aleng - STS) % XS + 1);
                    for (int j = 0; j < jmax; j++) {
                        if (subi.x[j] != 0) MarkBlock(vol, subi.x[j]);
                    }
                }
            }
        }
    }
}

/* ---- Save bitmap to disk (MapIndex at last sector) ---- */

static int SaveBitmap(Volume *vol) {
    /* Mirrors OFSAosFiles.DirCleanup behavior:
       - choose free sectors near the end to temporarily store map sectors
       - write MapIndex into the last sector
       Note: map sectors themselves remain "free" (unmarked) on purpose.
     */

    DiskAdr lastAdr = vol->size * SF;
    if (IsMarked(vol, lastAdr)) {
        /* last sector must be free to store MapIndex */
        return -1;
    }

    MapIndex mi;
    MapSector ms;
    memset(&mi, 0, sizeof(mi));

    int32_t j = 0;
    int32_t sec = 1;
    int32_t search_b = vol->size; /* will decrement to find free map sectors */

    for (;;) {
        /* Find next free sector from the end */
        DiskAdr map_adr = 0;
        do {
            search_b--;
            if (search_b <= 0) return -1;
        } while (IsMarked(vol, search_b * SF));

        map_adr = search_b * SF;
        mi.index[j] = map_adr;
        j++;
        if (j >= MapIndexSize) return -1;

        memset(&ms, 0, sizeof(ms));
        for (;;) {
            if (IsMarked(vol, sec * SF)) {
                ms.map[(sec / 32) % MapSize] |= (1U << (sec % 32));
            }
            if (sec == vol->size) {
                if (PutSector(vol, map_adr, &ms) != 0) return -1;
                goto done;
            }
            sec++;
            if (sec % (MapSize * 32) == 0) {
                if (PutSector(vol, map_adr, &ms) != 0) return -1;
                break;
            }
        }
    }

done:
    while (j < MapIndexSize) { mi.index[j] = 0; j++; }
    mi.mark = MapMark;
    if (PutSector(vol, lastAdr, &mi) != 0) return -1;
    return 0;
}

/* ---- MBR partition detection ---- */

/* Oberon partition type used in MBR partition tables */
#define OBERON_PTYPE  0x4C

/* Try to read the Aos boot block from a given 512-byte sector offset.
   Returns 1 if a valid Aos boot block was found, 0 otherwise.
   On success, *out_reserved and *out_size are set. */
static int CheckAosBootBlock(FILE *fp, long sector_offset,
                              int32_t *out_reserved, int32_t *out_size) {
    uint8_t boot[BS];
    if (fseek(fp, sector_offset * BS, SEEK_SET) != 0) return 0;
    if (fread(boot, BS, 1, fp) != 1) return 0;
    if (boot[510] != 0x55 || boot[511] != 0xAA) return 0;
    if (get4(boot, 0x1F8) != FSID) return 0;
    if (boot[0x1FC] != FSVer) return 0;
    if ((1 << boot[0x1FD]) != AosSS) return 0;
    *out_reserved = (int32_t)get4(boot, 0x1F0);
    *out_size = (int32_t)get4(boot, 0x1F4);
    return 1;
}

/* Scan an MBR partition table for an Aos partition.
   Returns the start sector of the first Aos partition found, or -1 if none. */
static long FindAosPartition(FILE *fp) {
    uint8_t mbr[BS];
    if (fseek(fp, 0, SEEK_SET) != 0) return -1;
    if (fread(mbr, BS, 1, fp) != 1) return -1;

    /* Must have 55 AA signature */
    if (mbr[510] != 0x55 || mbr[511] != 0xAA) return -1;

    /* If sector 0 already has FSID, it's a raw Aos image, not an MBR */
    if (get4(mbr, 0x1F8) == FSID) return -1;

    /* Get total device size for validation */
    if (fseek(fp, 0, SEEK_END) != 0) return -1;
    long file_size = ftell(fp);
    long device_sectors = file_size / BS;

    /* Scan 4 primary partition entries */
    for (int i = 0; i < 4; i++) {
        int off = 0x1BE + i * 16;
        uint8_t ptype = mbr[off + 4];
        uint32_t pstart = get4(mbr, off + 8);
        uint32_t psize  = get4(mbr, off + 12);

        if (ptype == 0 || psize == 0) continue;
        if ((long)pstart + (long)psize > device_sectors) continue;

        /* Check if partition type is Oberon (0x4C) or if the partition
           contains an Aos boot block regardless of type */
        int32_t dummy_res, dummy_sz;
        if (ptype == OBERON_PTYPE ||
            CheckAosBootBlock(fp, (long)pstart, &dummy_res, &dummy_sz)) {
            return (long)pstart;
        }
    }
    return -1;
}

/* ---- Open an existing volume ---- */

static Volume *OpenVolume(const char *path, const char *mode) {
    FILE *fp = fopen(path, mode);
    if (!fp) {
        fprintf(stderr, "Cannot open '%s': %s\n", path, strerror(errno));
        return NULL;
    }

    /* Determine where the Aos boot block is:
       - If sector 0 is a raw Aos boot block, partition_start = 0
       - If sector 0 is an MBR, find the Aos partition within it */
    long partition_start = 0;  /* in 512-byte sectors from start of file */
    int32_t reserved = 0;
    int32_t vol_size = 0;

    if (!CheckAosBootBlock(fp, 0, &reserved, &vol_size)) {
        /* Sector 0 is not an Aos boot block — try MBR */
        long part_off = FindAosPartition(fp);
        if (part_off < 0) {
            fprintf(stderr, "Not an Aos volume and no Aos partition found in MBR\n");
            fclose(fp);
            return NULL;
        }
        partition_start = part_off;
        if (!CheckAosBootBlock(fp, partition_start, &reserved, &vol_size)) {
            fprintf(stderr, "Aos partition at sector %ld has invalid boot block\n", partition_start);
            fclose(fp);
            return NULL;
        }
        fprintf(stderr, "Found Aos partition at MBR sector %ld\n", partition_start);
    }

    /* reserved is relative to the Aos boot block.
       Adjust to be an absolute offset in BS-byte sectors from file start. */
    reserved += (int32_t)partition_start;

    Volume *vol = calloc(1, sizeof(Volume));
    if (!vol) { fclose(fp); return NULL; }
    vol->fp = fp;
    vol->reserved = reserved;
    vol->size = vol_size;
    vol->bitmap_words = (vol_size + 32) / 32;
    vol->bitmap = calloc(vol->bitmap_words, sizeof(uint32_t));
    if (!vol->bitmap) { free(vol); fclose(fp); return NULL; }

    /* Check directory root marker */
    DirPage root;
    if (GetSector(vol, DirRootAdr, &root) != 0) {
        fprintf(stderr, "Cannot read directory root\n");
        free(vol->bitmap);
        free(vol);
        fclose(fp);
        return NULL;
    }
    if (root.mark != DirMark) {
        fprintf(stderr, "Invalid directory mark (0x%08X, expected 0x%08X)\n", root.mark, DirMark);
        free(vol->bitmap);
        free(vol);
        fclose(fp);
        return NULL;
    }

    /* Build allocation bitmap from directory traversal */
    BuildBitmap(vol);

    return vol;
}

static void CloseVolume(Volume *vol) {
    if (!vol) return;
    if (vol->fp) {
        fflush(vol->fp);
        fclose(vol->fp);
    }
    free(vol->bitmap);
    free(vol);
}

/* ---- Directory search ---- */

static DiskAdr DirSearch(Volume *vol, const char *name) {
    DiskAdr dadr = DirRootAdr;
    DirPage a;
    char fn[FnLength];

    memset(fn, 0, FnLength);
    strncpy(fn, name, FnLength - 1);

    for (;;) {
        if (GetSector(vol, dadr, &a) != 0) return 0;
        if (a.mark != DirMark) return 0;

        int L = 0, R = a.m;
        while (L < R) {
            int i = (L + R) / 2;
            if (strncmp(fn, a.e[i].name, FnLength) <= 0) R = i; else L = i + 1;
        }
        if (R < a.m && strncmp(fn, a.e[R].name, FnLength) == 0) {
            return a.e[R].adr;  /* found */
        }
        dadr = (R == 0) ? a.p0 : a.e[R - 1].p;
        if (dadr == 0) return 0;  /* not found */
    }
}

/* ---- Directory insert ---- */

static int dir_insert(Volume *vol, char fn[FnLength], DiskAdr dpg0,
                      int *h, DirEntry *v, DiskAdr fad, DiskAdr *replacedFad);

static int DirInsert(Volume *vol, char fn[FnLength], DiskAdr fad, DiskAdr *replacedFad) {
    int h = 0;
    DirEntry U;
    memset(&U, 0, sizeof(U));
    *replacedFad = 0;

    if (dir_insert(vol, fn, DirRootAdr, &h, &U, fad, replacedFad) != 0)
        return -1;

    if (h) {
        /* Root overflow */
        DirPage a;
        if (GetSector(vol, DirRootAdr, &a) != 0) return -1;
        DiskAdr oldroot = AllocSector(vol, DirRootAdr);
        if (oldroot == 0) return -1;
        if (PutSector(vol, oldroot, &a) != 0) return -1;
        a.mark = DirMark;
        a.m = 1;
        a.p0 = oldroot;
        a.e[0] = U;
        if (PutSector(vol, DirRootAdr, &a) != 0) return -1;
    }
    return 0;
}

static int dir_insert(Volume *vol, char fn[FnLength], DiskAdr dpg0,
                      int *h, DirEntry *v, DiskAdr fad, DiskAdr *replacedFad) {
    DirPage a;
    if (GetSector(vol, dpg0, &a) != 0) return -1;

    int L = 0, R = a.m;
    while (L < R) {
        int i = (L + R) / 2;
        if (strncmp(fn, a.e[i].name, FnLength) <= 0) R = i; else L = i + 1;
    }

    *replacedFad = 0;
    if (R < a.m && strncmp(fn, a.e[R].name, FnLength) == 0) {
        /* Replace existing entry */
        *replacedFad = a.e[R].adr;
        a.e[R].adr = fad;
        return PutSector(vol, dpg0, &a);
    }

    /* Not on this page */
    DiskAdr dpg1 = (R == 0) ? a.p0 : a.e[R - 1].p;
    DirEntry u;
    memset(&u, 0, sizeof(u));

    if (dpg1 == 0) {
        /* Not in tree, insert */
        u.adr = fad;
        u.p = 0;
        memcpy(u.name, fn, FnLength);
        *h = 1;
    } else {
        if (dir_insert(vol, fn, dpg1, h, &u, fad, replacedFad) != 0)
            return -1;
    }

    if (*h) {
        /* Insert u to the left of e[R] */
        if (a.m < DirPgSize) {
            *h = 0;
            for (int i = a.m; i > R; ) {
                i--;
                a.e[i + 1] = a.e[i];
            }
            a.e[R] = u;
            a.m++;
        } else {
            /* Split page */
            a.m = N;
            a.mark = DirMark;
            if (R < N) {
                /* Insert in left half */
                *v = a.e[N - 1];
                for (int i = N - 1; i > R; ) {
                    i--;
                    a.e[i + 1] = a.e[i];
                }
                a.e[R] = u;
                if (PutSector(vol, dpg0, &a) != 0) return -1;
                DiskAdr newpg = AllocSector(vol, dpg0);
                if (newpg == 0) return -1;
                for (int i = 0; i < N; i++) a.e[i] = a.e[i + N];
                a.p0 = v->p;
                v->p = newpg;
                return PutSector(vol, newpg, &a);
            } else {
                /* Insert in right half */
                if (PutSector(vol, dpg0, &a) != 0) return -1;
                DiskAdr newpg = AllocSector(vol, dpg0);
                if (newpg == 0) return -1;
                int Rn = R - N;
                int i = 0;
                if (Rn == 0) {
                    *v = u;
                } else {
                    *v = a.e[N];
                    while (i < Rn - 1) { a.e[i] = a.e[N + 1 + i]; i++; }
                    a.e[i] = u;
                    i++;
                }
                while (i < N) { a.e[i] = a.e[N + i]; i++; }
                a.p0 = v->p;
                v->p = newpg;
                return PutSector(vol, newpg, &a);
            }
        }
        return PutSector(vol, dpg0, &a);
    }
    return 0;
}

/* ---- Directory delete ---- */

static int dir_delete(Volume *vol, char fn[FnLength], DiskAdr dpg0,
                      int *h, DiskAdr *fad);
static int underflow(Volume *vol, DirPage *c, DiskAdr dpg0, int s, int *h);

static int DirDelete(Volume *vol, char fn[FnLength], DiskAdr *fad) {
    int h = 0;
    *fad = 0;
    if (dir_delete(vol, fn, DirRootAdr, &h, fad) != 0)
        return -1;

    if (h) {
        DirPage a;
        if (GetSector(vol, DirRootAdr, &a) != 0) return -1;
        if (a.m == 0 && a.p0 != 0) {
            DiskAdr newroot = a.p0;
            DirPage b;
            if (GetSector(vol, newroot, &b) != 0) return -1;
            if (PutSector(vol, DirRootAdr, &b) != 0) return -1;
            /* Free old page */
            FreeBlock(vol, newroot);
        }
    }
    return 0;
}

/* del helper for delete - find rightmost entry to replace deleted one */
static int dir_del(Volume *vol, DiskAdr dpg1, int *h, DirPage *a, int R) {
    DirPage b;
    if (GetSector(vol, dpg1, &b) != 0) return -1;
    DiskAdr dpg2 = b.e[b.m - 1].p;
    if (dpg2 != 0) {
        if (dir_del(vol, dpg2, h, a, R) != 0) return -1;
        if (*h) {
            if (underflow(vol, &b, dpg2, b.m, h) != 0) return -1;
            return PutSector(vol, dpg1, &b);
        }
        return 0;
    } else {
        b.e[b.m - 1].p = a->e[R].p;
        a->e[R] = b.e[b.m - 1];
        b.m--;
        *h = (b.m < N) ? 1 : 0;
        return PutSector(vol, dpg1, &b);
    }
}

static int dir_delete(Volume *vol, char fn[FnLength], DiskAdr dpg0,
                      int *h, DiskAdr *fad) {
    DirPage a;
    if (GetSector(vol, dpg0, &a) != 0) return -1;

    int L = 0, R = a.m;
    while (L < R) {
        int i = (L + R) / 2;
        if (strncmp(fn, a.e[i].name, FnLength) <= 0) R = i; else L = i + 1;
    }

    DiskAdr dpg1 = (R == 0) ? a.p0 : a.e[R - 1].p;

    if (R < a.m && strncmp(fn, a.e[R].name, FnLength) == 0) {
        /* Found */
        *fad = a.e[R].adr;
        if (dpg1 == 0) {
            /* Leaf page */
            a.m--;
            *h = (a.m < N) ? 1 : 0;
            for (int i = R; i < a.m; i++) a.e[i] = a.e[i + 1];
        } else {
            if (dir_del(vol, dpg1, h, &a, R) != 0) return -1;
            if (*h) {
                if (underflow(vol, &a, dpg1, R, h) != 0) return -1;
            }
        }
        return PutSector(vol, dpg0, &a);
    } else if (dpg1 != 0) {
        if (dir_delete(vol, fn, dpg1, h, fad) != 0) return -1;
        if (*h) {
            if (underflow(vol, &a, dpg1, R, h) != 0) return -1;
            return PutSector(vol, dpg0, &a);
        }
        return 0;
    } else {
        /* Not in tree */
        *fad = 0;
        return 0;
    }
}

static int underflow(Volume *vol, DirPage *c, DiskAdr dpg0, int s, int *h) {
    DirPage a, b;
    if (GetSector(vol, dpg0, &a) != 0) return -1;

    if (s < c->m) {
        /* b = page to the right of a */
        DiskAdr dpg1 = c->e[s].p;
        if (GetSector(vol, dpg1, &b) != 0) return -1;
        int k = (b.m - N + 1) / 2;
        a.e[N - 1] = c->e[s];
        a.e[N - 1].p = b.p0;
        if (k > 0) {
            /* Move k-1 items from b to a, one to c */
            int i = 0;
            while (i < k - 1) { a.e[i + N] = b.e[i]; i++; }
            c->e[s] = b.e[i];
            b.p0 = c->e[s].p;
            c->e[s].p = dpg1;
            b.m -= k;
            i = 0;
            while (i < b.m) { b.e[i] = b.e[i + k]; i++; }
            if (PutSector(vol, dpg1, &b) != 0) return -1;
            a.m = N - 1 + k;
            *h = 0;
        } else {
            /* Merge a and b */
            int i = 0;
            while (i < N) { a.e[i + N] = b.e[i]; i++; }
            i = s;
            c->m--;
            while (i < c->m) { c->e[i] = c->e[i + 1]; i++; }
            a.m = 2 * N;
            *h = (c->m < N) ? 1 : 0;
        }
        if (PutSector(vol, dpg0, &a) != 0) return -1;
    } else {
        /* b = page to the left of a */
        s--;
        DiskAdr dpg1 = (s == 0) ? c->p0 : c->e[s - 1].p;
        if (GetSector(vol, dpg1, &b) != 0) return -1;
        int k = (b.m - N + 1) / 2;
        if (k > 0) {
            int i = N - 1;
            while (i > 0) { i--; a.e[i + k] = a.e[i]; }
            i = k - 1;
            a.e[i] = c->e[s];
            a.e[i].p = a.p0;
            b.m -= k;
            while (i > 0) { i--; a.e[i] = b.e[i + b.m + 1]; }
            c->e[s] = b.e[b.m];
            a.p0 = c->e[s].p;
            c->e[s].p = dpg0;
            a.m = N - 1 + k;
            *h = 0;
            if (PutSector(vol, dpg0, &a) != 0) return -1;
        } else {
            /* Merge a and b */
            c->e[s].p = a.p0;
            b.e[N] = c->e[s];
            int i = 0;
            while (i < N - 1) { b.e[i + N + 1] = a.e[i]; i++; }
            b.m = 2 * N;
            c->m--;
            *h = (c->m < N) ? 1 : 0;
        }
        if (PutSector(vol, dpg1, &b) != 0) return -1;
    }
    return 0;
}

/* ---- Free all sectors of a file on disk ---- */

static int PurgeFile(Volume *vol, DiskAdr hdadr) {
    FileHeader hd;
    if (GetSector(vol, hdadr, &hd) != 0) return -1;

    int32_t aleng = hd.aleng;
    int32_t sec_count = aleng + 1;
    if (sec_count > STS) sec_count = STS;

    for (int i = 0; i < sec_count; i++) {
        if (hd.sec[i] != 0) FreeBlock(vol, hd.sec[i]);
    }

    int32_t rem = aleng - sec_count;  /* remaining sectors beyond direct table */
    if (rem >= 0 && hd.ext != 0) {
        IndexSector supi, subi;
        if (GetSector(vol, hd.ext, &supi) != 0) return -1;

        while (rem >= 0) {
            DiskAdr subAdr = supi.x[rem / XS];
            if (subAdr != 0) {
                if (GetSector(vol, subAdr, &subi) != 0) return -1;
                for (int i = 0; i <= rem % XS; i++) {
                    if (subi.x[i] != 0) FreeBlock(vol, subi.x[i]);
                }
                FreeBlock(vol, subAdr);
            }
            rem = rem - (rem % XS + 1);
        }
        FreeBlock(vol, hd.ext);
    }
    return 0;
}

/* ---- Enumerate directory (collect FileInfo array) ---- */

static void EnumDir(Volume *vol, DiskAdr dpg, FileInfo *files, int *count, int max_files) {
    DirPage a;
    if (GetSector(vol, dpg, &a) != 0) return;
    if (a.mark != DirMark) return;

    for (int i = 0; i < a.m; i++) {
        /* Visit left subtree first (in-order traversal for sorted output) */
        DiskAdr child = (i == 0) ? a.p0 : a.e[i - 1].p;
        if (child != 0) {
            EnumDir(vol, child, files, count, max_files);
        }

        /* Visit this entry */
        if (*count < max_files) {
            FileHeader hd;
            if (GetSector(vol, a.e[i].adr, &hd) == 0 && hd.mark == HeaderMark) {
                FileInfo *fi = &files[*count];
                memcpy(fi->name, a.e[i].name, FnLength);
                fi->headerAdr = a.e[i].adr;
                fi->mark = hd.mark;
                fi->aleng = hd.aleng;
                fi->bleng = hd.bleng;
                fi->date = hd.date;
                fi->time_ = hd.time_;
                fi->ext = hd.ext;
                fi->size = hd.aleng * SS + hd.bleng - HS;
                (*count)++;
            }
        }
    }
    /* Visit rightmost subtree */
    if (a.m > 0 && a.e[a.m - 1].p != 0) {
        EnumDir(vol, a.e[a.m - 1].p, files, count, max_files);
    }
}

/* Fill array of FileInfo from the volume. Returns count of files. */
static int ListFiles(Volume *vol, FileInfo *files, int max_files) {
    int count = 0;
    EnumDir(vol, DirRootAdr, files, &count, max_files);
    return count;
}

/* ---- Date/Time encoding/decoding ---- */

static void DecodeDateTime(int32_t date, int32_t time_val,
                           int *year, int *month, int *day,
                           int *hour, int *min, int *sec) {
    *day   = date % 32;
    *month = (date / 32) % 16;
    *year  = 1900 + date / 512;
    *sec   = (time_val % 64) * 2;
    *min   = (time_val / 64) % 64;
    *hour  = time_val / 4096;
}

static void EncodeDateTime(int year, int month, int day,
                           int hour, int min, int sec,
                           int32_t *date, int32_t *time_val) {
    *date = (year - 1900) * 512 + month * 32 + day;
    *time_val = hour * 4096 + min * 64 + sec / 2;
}

static void GetCurrentDateTime(int32_t *date, int32_t *time_val) {
    time_t now = time(NULL);
    struct tm *t = localtime(&now);
    EncodeDateTime(t->tm_year + 1900, t->tm_mon + 1, t->tm_mday,
                   t->tm_hour, t->tm_min, t->tm_sec,
                   date, time_val);
}

/* ---- Check file name validity ---- */

static int CheckName(const char *s, char fn[FnLength]) {
    memset(fn, 0, FnLength);
    int i = 0;
    char ch = s[0];

    if (!((ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z'))) {
        if (ch == 0) return -1; /* empty */
        return 3; /* bad first char */
    }

    while (ch != 0) {
        fn[i] = ch;
        i++;
        if (i >= FnLength - 1) return 4; /* too long */
        ch = s[i];
        if (ch != 0 &&
            !((ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') ||
              (ch >= '0' && ch <= '9') || ch == '.')) {
            return 3; /* invalid char */
        }
    }
    return 0;
}

/* ---- Read file data from volume ---- */

static int ReadFileData(Volume *vol, DiskAdr headerAdr, uint8_t *buf, int32_t bufsize, int32_t *bytesRead) {
    FileHeader hd;
    if (GetSector(vol, headerAdr, &hd) != 0) return -1;
    if (hd.mark != HeaderMark) return -1;

    int32_t fileLen = hd.aleng * SS + hd.bleng - HS;
    if (fileLen < 0) fileLen = 0;
    int32_t toRead = (fileLen < bufsize) ? fileLen : bufsize;

    /* First sector contains HS bytes of header, then data */
    int32_t pos = 0;
    /* Copy data from header sector */
    int32_t copyLen = SS - HS;
    if (copyLen > toRead) copyLen = toRead;
    if (copyLen > 0) {
        memcpy(buf, hd.data, copyLen);
        pos += copyLen;
    }

    /* Read subsequent direct sectors (sec[1] .. sec[aleng or STS-1]) */
    int32_t secIdx = 1;
    while (pos < toRead && secIdx < STS && secIdx <= hd.aleng) {
        Sector sector;
        if (hd.sec[secIdx] == 0) break;
        if (GetSector(vol, hd.sec[secIdx], &sector) != 0) return -1;
        int32_t copyAmt = SS;
        if (pos + copyAmt > toRead) copyAmt = toRead - pos;
        memcpy(buf + pos, sector, copyAmt);
        pos += copyAmt;
        secIdx++;
    }

    /* Read sectors from index (for files > STS sectors) */
    if (pos < toRead && hd.aleng >= STS && hd.ext != 0) {
        IndexSector supi;
        if (GetSector(vol, hd.ext, &supi) != 0) return -1;

        int32_t xpos = 0;  /* position in extended area (sector STS onwards) */
        int32_t remaining_aleng = hd.aleng - STS;

        while (pos < toRead && xpos <= remaining_aleng) {
            int32_t si = xpos / XS;
            int32_t sj = xpos % XS;

            if (supi.x[si] == 0) break;

            IndexSector subi;
            if (GetSector(vol, supi.x[si], &subi) != 0) return -1;

            while (pos < toRead && sj < XS && xpos <= remaining_aleng) {
                if (subi.x[sj] == 0) break;
                Sector sector;
                if (GetSector(vol, subi.x[sj], &sector) != 0) return -1;
                int32_t copyAmt = SS;
                if (pos + copyAmt > toRead) copyAmt = toRead - pos;
                memcpy(buf + pos, sector, copyAmt);
                pos += copyAmt;
                sj++;
                xpos++;
            }
            if (sj >= XS) continue;
            else break;
        }
    }

    *bytesRead = pos;
    return 0;
}

/* ---- Write a new file to volume ---- */

static int WriteNewFile(Volume *vol, const char *oberon_name, const uint8_t *data, int32_t dataLen) {
    char fn[FnLength];
    int res = CheckName(oberon_name, fn);
    if (res != 0) {
        fprintf(stderr, "Invalid Oberon file name '%s' (error %d)\n", oberon_name, res);
        return -1;
    }

    /* Calculate sector layout */
    int32_t totalLen = dataLen + HS;  /* total including header overhead */
    int32_t aleng = (totalLen > 0) ? (totalLen - 1) / SS : 0;
    int32_t bleng = totalLen - aleng * SS;

    /* Need aleng+1 data sectors total */
    int32_t totalSectors = aleng + 1;
    int32_t needSuper = (aleng >= STS) ? 1 : 0;
    int32_t nSubIndex = needSuper ? ((aleng - STS) / XS + 1) : 0;

    /* Allocate all sectors first */
    DiskAdr sechint = InitHint;

    /* Header / direct sectors */
    DiskAdr sec[STS];
    memset(sec, 0, sizeof(sec));
    int32_t directCount = (totalSectors < STS) ? totalSectors : STS;

    for (int i = 0; i < directCount; i++) {
        sec[i] = AllocSector(vol, sechint);
        if (sec[i] == 0) return -1;
        sechint = sec[i];
    }

    /* Extended index sectors */
    DiskAdr superAdr = 0;
    IndexSector superSec;
    memset(&superSec, 0, sizeof(superSec));

    /* Sub-index sectors and their data sectors */
    typedef struct { DiskAdr adr; IndexSector sec; } SubInfo;
    SubInfo *subs = NULL;

    if (needSuper) {
        superAdr = AllocSector(vol, sechint);
        if (superAdr == 0) return -1;
        sechint = superAdr;

        subs = calloc(nSubIndex, sizeof(SubInfo));
        if (!subs) return -1;

        for (int si = 0; si < nSubIndex; si++) {
            subs[si].adr = AllocSector(vol, sechint);
            if (subs[si].adr == 0) { free(subs); return -1; }
            sechint = subs[si].adr;
            memset(&subs[si].sec, 0, sizeof(IndexSector));
            superSec.x[si] = subs[si].adr;

            int32_t jmax = ((si < nSubIndex - 1) ? XS : ((aleng - STS) % XS + 1));
            for (int j = 0; j < jmax; j++) {
                DiskAdr da = AllocSector(vol, sechint);
                if (da == 0) { free(subs); return -1; }
                sechint = da;
                subs[si].sec.x[j] = da;
            }
        }
    }

    /* Now write data */
    int32_t pos = 0;

    /* Build file header */
    FileHeader hdr;
    memset(&hdr, 0, sizeof(hdr));
    hdr.mark = HeaderMark;
    memcpy(hdr.name, fn, FnLength);
    hdr.aleng = aleng;
    hdr.bleng = bleng;
    GetCurrentDateTime(&hdr.date, &hdr.time_);
    memcpy(hdr.sec, sec, sizeof(sec));
    hdr.ext = superAdr;

    /* Copy first chunk of data into header */
    int32_t firstCopy = SS - HS;
    if (firstCopy > dataLen) firstCopy = dataLen;
    if (firstCopy > 0) memcpy(hdr.data, data, firstCopy);
    pos = firstCopy;

    if (PutSector(vol, sec[0], &hdr) != 0) { free(subs); return -1; }

    /* Write direct data sectors (sec[1] .. sec[directCount-1]) */
    for (int i = 1; i < directCount; i++) {
        Sector sector;
        memset(sector, 0, SS);
        int32_t copyAmt = dataLen - pos;
        if (copyAmt > SS) copyAmt = SS;
        if (copyAmt > 0) memcpy(sector, data + pos, copyAmt);
        pos += copyAmt;
        if (PutSector(vol, sec[i], sector) != 0) { free(subs); return -1; }
    }

    /* Write extended data sectors */
    if (needSuper) {
        for (int si = 0; si < nSubIndex; si++) {
            int32_t jmax = ((si < nSubIndex - 1) ? XS : ((aleng - STS) % XS + 1));
            for (int j = 0; j < jmax; j++) {
                Sector sector;
                memset(sector, 0, SS);
                int32_t copyAmt = dataLen - pos;
                if (copyAmt > SS) copyAmt = SS;
                if (copyAmt > 0) memcpy(sector, data + pos, copyAmt);
                pos += copyAmt;
                if (PutSector(vol, subs[si].sec.x[j], sector) != 0) { free(subs); return -1; }
            }
            /* Write sub-index sector */
            if (PutSector(vol, subs[si].adr, &subs[si].sec) != 0) { free(subs); return -1; }
        }
        /* Write super-index sector */
        if (PutSector(vol, superAdr, &superSec) != 0) { free(subs); return -1; }
        free(subs);
    }

    /* Insert into directory */
    DiskAdr replacedFad = 0;
    if (DirInsert(vol, fn, sec[0], &replacedFad) != 0) return -1;

    /* If replaced an existing file, purge its sectors */
    if (replacedFad != 0 && replacedFad != sec[0]) {
        PurgeFile(vol, replacedFad);
    }

    return 0;
}

/* ---- Create a new empty volume ---- */

/* QEMU default CHS geometry: 16 heads, 63 sectors/track, 512 bytes/sector.
   1 cylinder = 16 * 63 * 512 = 516096 bytes = 126 AosFS blocks (4096 bytes).
   Padding the image to a cylinder boundary prevents QEMU from rounding down
   the apparent disk size, which would crash Oberon. */
#define CYL_BYTES  (16 * 63 * BS)  /* 516096 */

static int CreateVolume(const char *path, int32_t size_mb) {
    int32_t reserved = 8;  /* 8 * 512 = 4096 bytes for boot block area */
    int64_t total_bytes = (int64_t)size_mb * 1024 * 1024;

    /* Round total image size up to the next CHS cylinder boundary */
    if (total_bytes % CYL_BYTES != 0) {
        total_bytes = ((total_bytes / CYL_BYTES) + 1) * CYL_BYTES;
    }

    int32_t vol_size = (int32_t)((total_bytes - (int64_t)reserved * BS) / SS);

    if (vol_size < MinVolSize) {
        fprintf(stderr, "Volume too small (need at least %d blocks, got %d)\n", MinVolSize, vol_size);
        return -1;
    }

    /* Create file */
    FILE *fp = fopen(path, "wb");
    if (!fp) {
        fprintf(stderr, "Cannot create '%s': %s\n", path, strerror(errno));
        return -1;
    }

    /* Write boot block */
    uint8_t boot[BS];
    memset(boot, 0, BS);

    /* Boot jump */
    boot[0] = 0xEB; boot[1] = 0x29; boot[2] = 0x90;

    /* "OBERON" label */
    boot[3] = 'O'; boot[4] = 'B'; boot[5] = 'E';
    boot[6] = 'R'; boot[7] = 'O'; boot[8] = 'N';

    /* Oberon reserved at 0x0E */
    boot[0x0E] = (uint8_t)(reserved & 0xFF);
    boot[0x0F] = (uint8_t)((reserved >> 8) & 0xFF);

    /* AOS identification */
    put4(boot, 0x1F0, reserved);                       /* reserved blocks (512-byte) */
    put4(boot, 0x1F4, (uint32_t)vol_size);             /* FS size in blocks */
    put4(boot, 0x1F8, FSID);                           /* "AOS!" */
    boot[0x1FC] = FSVer;                               /* version */
    boot[0x1FD] = 12;                                  /* log2(4096) */

    /* Boot signature */
    boot[0x1FE] = 0x55;
    boot[0x1FF] = 0xAA;

    if (fwrite(boot, BS, 1, fp) != 1) {
        fprintf(stderr, "Write error\n");
        fclose(fp);
        return -1;
    }

    /* Pad reserved area to full SS boundary */
    uint8_t zeros[BS];
    memset(zeros, 0, BS);
    for (int i = 1; i < reserved; i++) {
        if (fwrite(zeros, BS, 1, fp) != 1) {
            fprintf(stderr, "Write error\n");
            fclose(fp);
            return -1;
        }
    }

    /* Write all blocks (initialized to zero) */
    Sector zero_sector;
    memset(zero_sector, 0, SS);

    /* First block is the directory root */
    DirPage root;
    memset(&root, 0, sizeof(root));
    root.mark = DirMark;
    root.m = 0;
    root.p0 = 0;
    if (fwrite(&root, SS, 1, fp) != 1) {
        fprintf(stderr, "Write error\n");
        fclose(fp);
        return -1;
    }

    /* Remaining blocks */
    for (int32_t i = 2; i <= vol_size; i++) {
        if (fwrite(zero_sector, SS, 1, fp) != 1) {
            fprintf(stderr, "Write error\n");
            fclose(fp);
            return -1;
        }
    }

    fclose(fp);
    printf("Created Aos volume '%s': %lld bytes (%d MB padded to CHS boundary), %d blocks\n",
           path, (long long)total_bytes, size_mb, vol_size);
    return 0;
}

/* ---- Command: list ---- */

static int cmd_list(const char *vol_path) {
    Volume *vol = OpenVolume(vol_path, "rb");
    if (!vol) return -1;

    FileInfo *files = calloc(MAX_FILES, sizeof(FileInfo));
    if (!files) { CloseVolume(vol); return -1; }

    int count = ListFiles(vol, files, MAX_FILES);

    printf("%-31s %9s %8s %6s %6s %8s %10s %10s  %s\n",
           "Name", "Bytes", "HdrAdr", "aleng", "bleng", "ext", "rawDate", "rawTime", "Decoded");
    printf("------------------------------------------------------------------------------------------------------\n");
    for (int i = 0; i < count; i++) {
        int year, month, day, hour, min, sec;
        DecodeDateTime(files[i].date, files[i].time_, &year, &month, &day, &hour, &min, &sec);
        printf("%-31s %9d %8d %6d %6d %8d %10d %10d  %04d-%02d-%02d %02d:%02d:%02d\n",
               files[i].name,
               files[i].size,
               files[i].headerAdr,
               files[i].aleng,
               files[i].bleng,
               files[i].ext,
               files[i].date,
               files[i].time_,
               year, month, day, hour, min, sec);
    }
    printf("------------------------------------------------------------------------------------------------------\n");
    printf("%d files\n", count);

    free(files);
    CloseVolume(vol);
    return 0;
}

/* ---- Command: getall ---- */

static int cmd_get(const char *vol_path, const char *oberon_name, const char *host_path);

static int cmd_getall(const char *vol_path, const char *host_path) {
    Volume *vol = OpenVolume(vol_path, "rb");
    if (!vol) return -1;

    FileInfo *files = calloc(MAX_FILES, sizeof(FileInfo));
    if (!files) { CloseVolume(vol); return -1; }

    int count = ListFiles(vol, files, MAX_FILES);

    enum { MAX_PATH = 1024 };
    char path[MAX_PATH] = {0};
    int len = host_path ? strlen(host_path) : 0;
    if( len )
    {
        strcpy(path, host_path);
        if( path[len-1] != '/' )
            path[len++] = '/';
    }

    for (int i = 0; i < count; i++) {
        if( len )
            strcpy( path + len, files[i].name);
        cmd_get(vol_path, files[i].name, path);
    }

    free(files);
    CloseVolume(vol);
    return 0;
}

/* ---- Command: get ---- */

static int cmd_get(const char *vol_path, const char *oberon_name, const char *host_path) {
    Volume *vol = OpenVolume(vol_path, "rb");
    if (!vol) return -1;

    DiskAdr adr = DirSearch(vol, oberon_name);
    if (adr == 0) {
        fprintf(stderr, "File '%s' not found\n", oberon_name);
        CloseVolume(vol);
        return -1;
    }

    /* Read file header to get size */
    FileHeader hd;
    if (GetSector(vol, adr, &hd) != 0) { CloseVolume(vol); return -1; }
    if (hd.mark != HeaderMark) {
        fprintf(stderr, "Invalid file header\n");
        CloseVolume(vol);
        return -1;
    }

    int32_t fileLen = hd.aleng * SS + hd.bleng - HS;
    if (fileLen < 0) fileLen = 0;

    uint8_t *buf = malloc(fileLen + 1);
    if (!buf) { CloseVolume(vol); return -1; }

    int32_t bytesRead = 0;
    if (ReadFileData(vol, adr, buf, fileLen, &bytesRead) != 0) {
        free(buf);
        CloseVolume(vol);
        return -1;
    }

    /* Use oberon name as default host path */
    const char *outpath = host_path ? host_path : oberon_name;

    FILE *out = fopen(outpath, "wb");
    if (!out) {
        fprintf(stderr, "Cannot create '%s': %s\n", outpath, strerror(errno));
        free(buf);
        CloseVolume(vol);
        return -1;
    }
    if (bytesRead > 0) {
        fwrite(buf, 1, bytesRead, out);
    }
    fclose(out);

    printf("Exported '%s' (%d bytes) -> '%s'\n", oberon_name, bytesRead, outpath);

    free(buf);
    CloseVolume(vol);
    return 0;
}

/* ---- Command: add ---- */

static int cmd_add(const char *vol_path, const char *host_path, const char *oberon_name) {
    /* Read host file */
    FILE *in = fopen(host_path, "rb");
    if (!in) {
        fprintf(stderr, "Cannot open '%s': %s\n", host_path, strerror(errno));
        return -1;
    }
    fseek(in, 0, SEEK_END);
    long fileLen = ftell(in);
    fseek(in, 0, SEEK_SET);

    uint8_t *data = NULL;
    if (fileLen > 0) {
        data = malloc(fileLen);
        if (!data) { fclose(in); return -1; }
        if ((long)fread(data, 1, fileLen, in) != fileLen) {
            fprintf(stderr, "Read error\n");
            free(data);
            fclose(in);
            return -1;
        }
    }
    fclose(in);

    /* Determine Oberon name */
    const char *oname = oberon_name;
    if (!oname) {
        /* Use basename of host_path */
        const char *p = strrchr(host_path, '/');
        oname = p ? p + 1 : host_path;
    }

    Volume *vol = OpenVolume(vol_path, "r+b");
    if (!vol) { free(data); return -1; }

    if (WriteNewFile(vol, oname, data, (int32_t)fileLen) != 0) {
        fprintf(stderr, "Failed to write file\n");
        free(data);
        CloseVolume(vol);
        return -1;
    }

    /* Save bitmap */
    SaveBitmap(vol);

    printf("Added '%s' (%ld bytes) as '%s'\n", host_path, fileLen, oname);

    free(data);
    CloseVolume(vol);
    return 0;
}

/* ---- Command: remove ---- */

static int cmd_remove(const char *vol_path, const char *oberon_name) {
    Volume *vol = OpenVolume(vol_path, "r+b");
    if (!vol) return -1;

    char fn[FnLength];
    int res = CheckName(oberon_name, fn);
    if (res != 0) {
        fprintf(stderr, "Invalid name '%s'\n", oberon_name);
        CloseVolume(vol);
        return -1;
    }

    DiskAdr fad = 0;
    if (DirDelete(vol, fn, &fad) != 0) {
        fprintf(stderr, "Delete failed\n");
        CloseVolume(vol);
        return -1;
    }

    if (fad == 0) {
        fprintf(stderr, "File '%s' not found\n", oberon_name);
        CloseVolume(vol);
        return -1;
    }

    /* Invalidate header mark */
    FileHeader hd;
    if (GetSector(vol, fad, &hd) == 0) {
        hd.mark = HeaderMark + 1;
        PutSector(vol, fad, &hd);
    }

    /* Free sectors */
    PurgeFile(vol, fad);

    /* Save bitmap */
    SaveBitmap(vol);

    printf("Removed '%s'\n", oberon_name);
    CloseVolume(vol);
    return 0;
}

/* ---- Command: new ---- */

static int cmd_new(const char *vol_path, const char *size_str) {
    int size_mb = atoi(size_str);
    if (size_mb < 1) {
        fprintf(stderr, "Invalid size '%s' (must be >= 1 MB)\n", size_str);
        return -1;
    }
    return CreateVolume(vol_path, size_mb);
}

/* ---- Usage ---- */

static void usage(const char *prog) {
    fprintf(stderr, "Oberon Aos File System Tool\n\n");
    fprintf(stderr, "Usage:\n");
    fprintf(stderr, "  %s new    <volume> <size_MB>              Create a new volume\n", prog);
    fprintf(stderr, "  %s list   <volume>                        List files\n", prog);
    fprintf(stderr, "  %s add    <volume> <host_file> [ob_name]  Add a file\n", prog);
    fprintf(stderr, "  %s get    <volume> <ob_name> [host_file]  Export a file\n", prog);
    fprintf(stderr, "  %s getall <volume> [host_dir]             Export all files\n", prog);
    fprintf(stderr, "  %s remove <volume> <ob_name>              Remove a file\n", prog);
}

/* ---- Main ---- */

int main(int argc, char *argv[]) {
    if (argc < 3) {
        usage(argv[0]);
        return 1;
    }

    const char *cmd = argv[1];

    if (strcmp(cmd, "new") == 0) {
        if (argc < 4) { usage(argv[0]); return 1; }
        return cmd_new(argv[2], argv[3]) == 0 ? 0 : 1;
    }
    else if (strcmp(cmd, "list") == 0) {
        return cmd_list(argv[2]) == 0 ? 0 : 1;
    }
    else if (strcmp(cmd, "add") == 0) {
        if (argc < 4) { usage(argv[0]); return 1; }
        const char *ob_name = (argc >= 5) ? argv[4] : NULL;
        return cmd_add(argv[2], argv[3], ob_name) == 0 ? 0 : 1;
    }
    else if (strcmp(cmd, "get") == 0) {
        if (argc < 4) { usage(argv[0]); return 1; }
        const char *host_file = (argc >= 5) ? argv[4] : NULL;
        return cmd_get(argv[2], argv[3], host_file) == 0 ? 0 : 1;
    }
    else if (strcmp(cmd, "getall") == 0) {
        if (argc < 3) { usage(argv[0]); return 1; }
        const char *host_file = (argc >= 4) ? argv[3] : NULL;
        return cmd_getall(argv[2], host_file) == 0 ? 0 : 1;
    }
    else if (strcmp(cmd, "remove") == 0) {
        if (argc < 4) { usage(argv[0]); return 1; }
        return cmd_remove(argv[2], argv[3]) == 0 ? 0 : 1;
    }
    else {
        fprintf(stderr, "Unknown command '%s'\n\n", cmd);
        usage(argv[0]);
        return 1;
    }
}
