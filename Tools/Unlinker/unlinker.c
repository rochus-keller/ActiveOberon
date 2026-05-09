/*
 * unlinker.c - Tool to list the modules of AOS bootloaders
 * e.g. `./unlinker AosIDE.Bin`
 *
 * Based on AosLinker.Mod and AosLinker0.Mod from Aos 12.03.2004.
 *
 * Copyright (C) 2026 Rochus Keller (me@rochus-keller.ch)
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
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>

uint8_t *binary = NULL;
uint32_t file_size = 0;

// read a 32-bit little-endian integer
uint32_t read32(uint32_t offset) {
    if (offset + 3 >= file_size) return 0;
    return (uint32_t)binary[offset] | ((uint32_t)binary[offset+1] << 8) | 
           ((uint32_t)binary[offset+2] << 16) | ((uint32_t)binary[offset+3] << 24);
}

// validate if a string looks like a standard Oberon Module name
bool is_valid_name(const char* name) {
    if (name[0] == '\0') return false;
    if (!isalpha(name[0]) && name[0] != '_') return false;
    for (int i = 0; i < 32; i++) {
        if (name[i] == '\0') return true;
        if (!isalnum(name[i]) && name[i] != '_') return false;
    }
    return false; // Name must be null-terminated within 32 chars
}

typedef struct {
    uint32_t offset;
    uint32_t next_offset; // 0 if NIL
    char name[32];
    int indegree;
} ModuleCandidate;

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <AOS_binary>\n", argv[0]);
        return 1;
    }
    
    FILE *f = fopen(argv[1], "rb");
    if (!f) {
        perror("Failed to open file");
        return 1;
    }
    
    fseek(f, 0, SEEK_END);
    file_size = ftell(f);
    fseek(f, 0, SEEK_SET);
    
    binary = malloc(file_size);
    if (!binary || fread(binary, 1, file_size, f) != file_size) {
        fprintf(stderr, "Failed to read file\n");
        if(binary) free(binary);
        if(f) fclose(f);
        return 1;
    }
    fclose(f);
    
    if (file_size < 0x3C) {
        fprintf(stderr, "File too small to be an AOS bootlinker binary\n");
        free(binary);
        return 1;
    }
    
    // Extract baseAdr and loadAdr
    // EndBlockOfs is 0x38. It stores relocated freeAdr = baseAdr + file_size
    uint32_t val_38 = read32(0x38);
    uint32_t baseAdr = val_38 - file_size;
    uint32_t loadAdr = baseAdr;
    uint32_t init_off = 0;
    
    uint8_t byte0 = binary[0];
    if (byte0 == 0x60) { // PUSHAD: relocation stub is present
        loadAdr = read32(2); // MOV ESI, loadAdr
        init_off = read32(21) + 25 - baseAdr + loadAdr;
    } else if (byte0 == 0xE8) { // CALL: image runs from load destination
        init_off = read32(1) + 5;
    } else {
        fprintf(stderr, "Unknown boot stub format (byte0 = %02X). Might not be AOS.\n", byte0);
        free(binary);
        return 1;
    }
    
    printf("--- Derived Parameters ---\n");
    printf("baseAdr  : 0x%08X\n", baseAdr);
    printf("loadAdr  : 0x%08X\n", loadAdr);
    printf("fileSize : %u bytes\n\n", file_size);
    
    // Parse the init block to find the exact number of modules
    // InitTable writes `n` calls to modules, 1 call to AosActive.Terminate, then a HALT (6A FF CC)
    uint32_t curr = init_off;
    int num_calls = 0;
    while (curr + 2 < file_size) {
        if (binary[curr] == 0xE8) {
            num_calls++;
            curr += 5;
        } else if (binary[curr] == 0x6A && binary[curr+1] == 0xFF && binary[curr+2] == 0xCC) {
            break; // HALT reached
        } else {
            break;
        }
    }
    
    int num_modules = (num_calls > 0) ? num_calls - 1 : 0;
    printf("Expected linked modules : %d\n\n", num_modules);
    
    // Scan the binary for Module object descriptors
    // In AosLinker0.Mod: BlockSize=32, ProtOfs=48. m = ptr + 52. ptr % 32 = 28. -> m % 32 = 16.
    ModuleCandidate candidates[2048];
    int num_candidates = 0;
    
    for (uint32_t off = 16; off + 40 < file_size; off += 32) {
        char name[32];
        memcpy(name, &binary[off + 8], 32);
        name[31] = '\0';
        
        if (!is_valid_name(name)) continue;
        
        // Check `next` pointer
        uint32_t next_ptr = read32(off + 4);
        uint32_t next_off = 0;
        if (next_ptr != 0) {
            if (next_ptr < baseAdr || next_ptr >= baseAdr + file_size) continue;
            next_off = next_ptr - baseAdr;
            if ((next_off % 32) != 16) continue;
        }
        
        candidates[num_candidates].offset = off;
        candidates[num_candidates].next_offset = next_off;
        strcpy(candidates[num_candidates].name, name);
        candidates[num_candidates].indegree = 0;
        num_candidates++;
        if (num_candidates >= 2048) break;
    }
    
    // Trace the `next` pointers to rebuild the list
    for (int i = 0; i < num_candidates; i++) {
        if (candidates[i].next_offset != 0) {
            for (int j = 0; j < num_candidates; j++) {
                if (candidates[j].offset == candidates[i].next_offset) {
                    candidates[j].indegree++;
                    break;
                }
            }
        }
    }
    
    // The bootlinker pushes new modules to the head, meaning the reverse list is the load order.
    // The head of the list (root module) is the one with an indegree of 0.
    int root_idx = -1;
    for (int i = 0; i < num_candidates; i++) {
        if (candidates[i].indegree == 0) {
            int len = 0, c = i;
            while (c != -1 && len < 2048) {
                len++;
                int next_idx = -1;
                if (candidates[c].next_offset != 0) {
                    for (int j = 0; j < num_candidates; j++) {
                        if (candidates[j].offset == candidates[c].next_offset) {
                            next_idx = j;
                            break;
                        }
                    }
                }
                c = next_idx;
            }
            if (len == num_modules) {
                root_idx = i;
                break;
            }
        }
    }
    
    if (root_idx == -1) {
        printf("Warning: Could not perfectly resolve the linked list.\n");
        printf("Falling back to listing all discovered valid module blocks:\n");
        for (int i = 0; i < num_candidates; i++) {
            printf("  - %s\n", candidates[i].name);
        }
    } else {
        printf("--- Linked Modules (in Initialization Order) ---\n");
        int chain[2048];
        int chain_len = 0;
        int c = root_idx;
        while (c != -1 && chain_len < 2048) {
            chain[chain_len++] = c;
            int next_idx = -1;
            if (candidates[c].next_offset != 0) {
                for (int j = 0; j < num_candidates; j++) {
                    if (candidates[j].offset == candidates[c].next_offset) {
                        next_idx = j;
                        break;
                    }
                }
            }
            c = next_idx;
        }
        
        // Print the array in reverse (oldest linked is at the tail of the heap's linked list)
        for (int i = chain_len - 1; i >= 0; i--) {
            printf(" %3d. %s\n", chain_len - i, candidates[chain[i]].name);
        }
    }
    
    free(binary);
    return 0;
}
