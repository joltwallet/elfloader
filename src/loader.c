/*
 * A elf module loader for esp32
 *
 * Author: niicoooo <1niicoooo1@gmail.com>
 * Copyright (C) 2017 by niicoooo <1niicoooo1@gmail.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 */

/*
 * Based on the work of the elf-loader project
 * https://github.com/embedded2014/elf-loader
 * Copyright (C) 2013 Martin Ribelotta (martinribelott@gmail.com) Licensed under GNU GPL v2 or later
 * Modified by Jim Huang (jserv.tw@gmail.com)
 */


#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#if CONFIG_ELFLOADER_PROFILER_EN
#include <esp_timer.h> // profiling
#endif // CONFIG_ELFLOADER_PROFILER_EN

#include "elfloader.h"
#include "elf.h"
#include "unaligned.h"

#ifdef __linux__

#include <malloc.h>
#define LOADER_ALLOC_EXEC(size) memalign(4, size)
#define LOADER_ALLOC_DATA(size) memalign(4, size)

#define MSG(...) printf(__VA_ARGS__); printf("\n");
#define ERR(...) printf(__VA_ARGS__); printf("\n");

#define LOADER_GETDATA(ctx, off, buffer, size) \
    if(fseek(ctx->fd, off, SEEK_SET) != 0) { assert(0); goto err; }\
    if(fread(buffer, 1, size, ctx->fd) != size) { assert(0); goto err; }

#elif ESP_PLATFORM

#include "esp_system.h"
#include "esp_heap_caps.h"
#include "esp_log.h"

static const char* TAG = "elfLoader";

#define MSG(...) ESP_LOGI(TAG,  __VA_ARGS__);
#define ERR(...) ESP_LOGE(TAG,  __VA_ARGS__);


#define CEIL4(x) ((x+3)&~0x03)
#define LOADER_ALLOC_EXEC(size) heap_caps_malloc(size, MALLOC_CAP_EXEC | MALLOC_CAP_32BIT)
#define LOADER_ALLOC_DATA(size) heap_caps_malloc(size, MALLOC_CAP_8BIT)

#if CONFIG_ELFLOADER_POSIX
// Works with filesystem, but is quite slow
#define LOADER_GETDATA(ctx, off, buffer, size) \
    if(fseek(ctx->fd, off, SEEK_SET) != 0) { assert(0); goto err; }\
    if(fread(buffer, 1, size, ctx->fd) != size) { assert(0); goto err; }
#elif CONFIG_ELFLOADER_MEMORY_POINTER
// operate directly on memory, much faster
#define LOADER_GETDATA(ctx, off, buffer, size) \
        unalignedCpy(buffer, ctx->fd + off, size); if(0) goto err;
#endif

#endif

typedef struct ELFLoaderSection_t {
    void *data;
    int secIdx;
    size_t size;
    off_t relSecIdx;                 // 
    struct ELFLoaderSection_t* next; // Next Header in Singly Linked List
} ELFLoaderSection_t;

struct ELFLoaderContext_t {
    LOADER_FD_T fd;
    void* exec;
    void* text;
    const ELFLoaderEnv_t *env;

    size_t e_shnum;
    off_t e_shoff;

    size_t symtab_count;
    off_t symtab_offset;
    off_t strtab_offset;
#if CONFIG_ELFLOADER_POSIX && CONFIG_ELFLOADER_CACHE_STRTAB
    char *shstrtab; // for caching the section header string table
#else
    off_t shstrtab_offset;
#endif

    ELFLoaderSection_t* section; // First element of singly linked list sections.
};

/* Profiler Variables */
#if CONFIG_ELFLOADER_PROFILER_EN
typedef struct profiler_timer_t{
    int64_t t;       // time in uS spent
    uint32_t n;      // times start has been called
    bool running;    // if timer is running
} profiler_timer_t;

static profiler_timer_t profiler_readSection = { 0 };
static profiler_timer_t profiler_readSymbol = { 0 };
static profiler_timer_t profiler_readSymbolFunc = { 0 };
static profiler_timer_t profiler_relocateSymbol = { 0 };
static profiler_timer_t profiler_findSymAddr = { 0 };
static profiler_timer_t profiler_findSection = { 0 };
static profiler_timer_t profiler_relocateSection = { 0 };

#define PROFILER_START(x) \
    if(!x.running) { \
        x.running = true; \
        x.t -= esp_timer_get_time(); \
    }

#define PROFILER_INC(x) \
    x.n++;

#define PROFILER_STOP(x) \
    if(x.running) { \
        x.running = false; \
        x.t += esp_timer_get_time(); \
    }

#define PROFILER_START_READSECTION     PROFILER_START(profiler_readSection)
#define PROFILER_START_READSYMBOL      PROFILER_START(profiler_readSymbol)
#define PROFILER_START_READSYMBOLFUNC  PROFILER_START(profiler_readSymbolFunc)
#define PROFILER_START_RELOCATESYMBOL  PROFILER_START(profiler_relocateSymbol)
#define PROFILER_START_FINDSYMADDR     PROFILER_START(profiler_findSymAddr)
#define PROFILER_START_FINDSECTION     PROFILER_START(profiler_findSection)
#define PROFILER_START_RELOCATESECTION PROFILER_START(profiler_relocateSection)

#define PROFILER_STOP_READSECTION     PROFILER_STOP(profiler_readSection)
#define PROFILER_STOP_READSYMBOL      PROFILER_STOP(profiler_readSymbol)
#define PROFILER_STOP_READSYMBOLFUNC  PROFILER_STOP(profiler_readSymbolFunc)
#define PROFILER_STOP_RELOCATESYMBOL  PROFILER_STOP(profiler_relocateSymbol)
#define PROFILER_STOP_FINDSYMADDR     PROFILER_STOP(profiler_findSymAddr)
#define PROFILER_STOP_FINDSECTION     PROFILER_STOP(profiler_findSection)
#define PROFILER_STOP_RELOCATESECTION PROFILER_STOP(profiler_relocateSection)

#define PROFILER_INC_READSECTION     PROFILER_INC(profiler_readSection)
#define PROFILER_INC_READSYMBOL      PROFILER_INC(profiler_readSymbol)
#define PROFILER_INC_READSYMBOLFUNC  PROFILER_INC(profiler_readSymbolFunc)
#define PROFILER_INC_RELOCATESYMBOL  PROFILER_INC(profiler_relocateSymbol)
#define PROFILER_INC_FINDSYMADDR     PROFILER_INC(profiler_findSymAddr)
#define PROFILER_INC_FINDSECTION     PROFILER_INC(profiler_findSection)
#define PROFILER_INC_RELOCATESECTION PROFILER_INC(profiler_relocateSection)

#else
// dummy macros

#define PROFILER_START_READSECTION
#define PROFILER_START_READSYMBOL
#define PROFILER_START_READSYMBOLFUNC
#define PROFILER_START_RELOCATESYMBOL 
#define PROFILER_START_FINDSYMADDR 
#define PROFILER_START_RELOCATESECTION 

#define PROFILER_STOP_READSECTION
#define PROFILER_STOP_READSYMBOL
#define PROFILER_STOP_READSYMBOLFUNC
#define PROFILER_STOP_RELOCATESYMBOL 
#define PROFILER_STOP_FINDSYMADDR 
#define PROFILER_STOP_RELOCATESECTION 

#define PROFILER_INC_READSECTION
#define PROFILER_INC_READSYMBOL
#define PROFILER_INC_READSYMBOLFUNC
#define PROFILER_INC_RELOCATESYMBOL
#define PROFILER_INC_FINDSYMADDR
#define PROFILER_INC_FINDSECTION 
#define PROFILER_INC_RELOCATESECTION

#endif

/*** Read data functions ***/

/* Reads section header and name for given section index
 * n - Section Index to query
 * h - Returns Section Header
 * name - Returns Section Name
 * name_len - Length of Name buffer
 */
static int readSection(ELFLoaderContext_t *ctx, int n, Elf32_Shdr *h,
        char *name, const size_t name_len) {
    PROFILER_START_READSECTION;
    PROFILER_INC_READSECTION;

    off_t offset;

    /* Read Section Header */
    if( h != NULL ) {
        offset = ctx->e_shoff + n * sizeof(Elf32_Shdr);
        LOADER_GETDATA(ctx, offset, h, sizeof(Elf32_Shdr));
    }

    /* Read Section Name */
    if (NULL != name && h->sh_name) {
        // h->sh_name is the offset into the StringTable where the 
        // NULL-terminated string is located.
#if CONFIG_ELFLOADER_POSIX && CONFIG_ELFLOADER_CACHE_STRTAB
        strlcpy(name, ctx->shstrtab + h->sh_name, name_len);
#else
        offset = ctx->shstrtab_offset + h->sh_name;
        LOADER_GETDATA(ctx, offset, name, name_len);
#endif
    }

    PROFILER_STOP_READSECTION;
    return 0;

err:
    PROFILER_STOP_READSECTION;
    return -1;
}

/* Populates sym, name */
static int readSymbol(ELFLoaderContext_t *ctx, int n, Elf32_Sym *sym,
        char *name, const size_t nlen) {
    PROFILER_START_READSYMBOL;
    PROFILER_INC_READSYMBOL;

    off_t pos = ctx->symtab_offset + n * sizeof(Elf32_Sym);
    LOADER_GETDATA(ctx, pos, sym, sizeof(Elf32_Sym))
    if (sym->st_name) {
        // Read in Name of Symbol from the strtab index
        off_t offset = ctx->strtab_offset + sym->st_name;
        LOADER_GETDATA(ctx, offset, name, nlen);
    } 
    else {
        Elf32_Shdr shdr;
        PROFILER_STOP_READSYMBOL;
        return readSection(ctx, sym->st_shndx, &shdr, name, nlen);
    }

    PROFILER_STOP_READSYMBOL;
    return 0;

err:
    PROFILER_STOP_READSYMBOL;
    return -1;
}

/* Only reads functions; speeds up setting entrypoint */
static int readSymbolFunc(ELFLoaderContext_t *ctx, int n, Elf32_Sym *sym,
        char *name, const size_t nlen) {
    PROFILER_START_READSYMBOLFUNC;
    PROFILER_INC_READSYMBOLFUNC;

    off_t pos = ctx->symtab_offset + n * sizeof(Elf32_Sym);
    LOADER_GETDATA(ctx, pos, sym, sizeof(Elf32_Sym))
    if( STT_FUNC == ELF32_ST_TYPE(sym->st_info) && sym->st_name ) {
        off_t offset = ctx->strtab_offset + sym->st_name;
        LOADER_GETDATA(ctx, offset, name, nlen);
    }

    PROFILER_STOP_READSYMBOLFUNC;
    return 0;
err:
    PROFILER_STOP_READSYMBOLFUNC;
    return -1;
}

/*** Relocation functions ***/


static const char *type2String(int symt) {
#define STRCASE(name) case name: return #name;
    switch (symt) {
        STRCASE(R_XTENSA_NONE)
        STRCASE(R_XTENSA_32)
        STRCASE(R_XTENSA_ASM_EXPAND)
        STRCASE(R_XTENSA_SLOT0_OP)
    default:
        return "R_<unknow>";
    }
#undef STRCASE
}


static int relocateSymbol(Elf32_Addr relAddr, int type, Elf32_Addr symAddr,
        Elf32_Addr defAddr, uint32_t* from, uint32_t* to) {

    PROFILER_START_RELOCATESYMBOL;
    PROFILER_INC_RELOCATESYMBOL;

    if (symAddr == 0xffffffff) {
        if (defAddr == 0x00000000) {
            ERR("Relocation: undefined symAddr");
            goto err;
        } else {
            symAddr = defAddr;
        }
    }
    switch (type) {
        case R_XTENSA_32: {
            *from = unalignedGet32((void*)relAddr);
            *to  = symAddr + *from;
            unalignedSet32((void*)relAddr, *to);
            break;
        }
        case R_XTENSA_SLOT0_OP: {
            uint32_t v = unalignedGet32((void*)relAddr);
            *from = v;

            /* *** Format: L32R *** */
            if ((v & 0x00000F) == 0x000001) {
                int32_t delta =  symAddr - ((relAddr + 3) & 0xfffffffc);
                if (delta & 0x0000003) {
                    ERR("Relocation: L32R error");
                    goto err;
                }
                delta =  delta >> 2;
                unalignedSet8((void*)(relAddr + 1), ((uint8_t*)&delta)[0]);
                unalignedSet8((void*)(relAddr + 2), ((uint8_t*)&delta)[1]);
                *to = unalignedGet32((void*)relAddr);
                break;
            }

            /* *** Format: CALL *** */
            /* *** CALL0, CALL4, CALL8, CALL12, J *** */
            if ((v & 0x00000F) == 0x000005) {
                int32_t delta =  symAddr - ((relAddr + 4) & 0xfffffffc);
                if (delta & 0x0000003) {
                    ERR("Relocation: CALL error");
                    return -1;
                }
                delta =  delta >> 2;
                delta =  delta << 6;
                delta |= unalignedGet8((void*)(relAddr + 0));
                unalignedSet8((void*)(relAddr + 0), ((uint8_t*)&delta)[0]);
                unalignedSet8((void*)(relAddr + 1), ((uint8_t*)&delta)[1]);
                unalignedSet8((void*)(relAddr + 2), ((uint8_t*)&delta)[2]);
                *to = unalignedGet32((void*)relAddr);
                break;
            }

            /* *** J *** */
            if ((v & 0x00003F) == 0x000006) {
                int32_t delta =  symAddr - (relAddr + 4);
                delta =  delta << 6;
                delta |= unalignedGet8((void*)(relAddr + 0));
                unalignedSet8((void*)(relAddr + 0), ((uint8_t*)&delta)[0]);
                unalignedSet8((void*)(relAddr + 1), ((uint8_t*)&delta)[1]);
                unalignedSet8((void*)(relAddr + 2), ((uint8_t*)&delta)[2]);
                *to = unalignedGet32((void*)relAddr);
                break;
            }

            /* *** Format: BRI8  *** */
            /* *** BALL, BANY, BBC, BBCI, BBCI.L, BBS,  BBSI, BBSI.L, BEQ,
             * *** BGE,  BGEU, BLT, BLTU, BNALL, BNE,  BNONE, LOOP, 
             * *** BEQI, BF, BGEI, BGEUI, BLTI, BLTUI, BNEI,  BT, LOOPGTZ,
             * *** LOOPNEZ *** */
            if (((v & 0x00000F) == 0x000007) || ((v & 0x00003F) == 0x000026) ||  ((v & 0x00003F) == 0x000036 && (v & 0x0000FF) != 0x000036)) {
                int32_t delta =  symAddr - (relAddr + 4);
                unalignedSet8((void*)(relAddr + 2), ((uint8_t*)&delta)[0]);
                *to = unalignedGet32((void*)relAddr);
                if ((delta < - (1 << 7)) || (delta >= (1 << 7))) {
                    ERR("Relocation: BRI8 out of range");
                    goto err;
                }
                break;
            }

            /* *** Format: BRI12 *** */
            /* *** BEQZ, BGEZ, BLTZ, BNEZ *** */
            if ((v & 0x00003F) == 0x000016) {
                int32_t delta =  symAddr - (relAddr + 4);
                delta =  delta << 4;
                delta |=  unalignedGet32((void*)(relAddr + 1));
                unalignedSet8((void*)(relAddr + 1), ((uint8_t*)&delta)[0]);
                unalignedSet8((void*)(relAddr + 2), ((uint8_t*)&delta)[1]);
                *to = unalignedGet32((void*)relAddr);
                delta =  symAddr - (relAddr + 4);
                if ((delta < - (1 << 11)) || (delta >= (1 << 11))) {
                    ERR("Relocation: BRI12 out of range");
                    goto err;
                }
                break;
            }

            /* *** Format: RI6  *** */
            /* *** BEQZ.N, BNEZ.N *** */
            if ((v & 0x008F) == 0x008C) {
                int32_t delta =  symAddr - (relAddr + 4);
                int32_t d2 = delta & 0x30;
                int32_t d1 = (delta << 4) & 0xf0;
                d2 |=  unalignedGet32((void*)(relAddr + 0));
                d1 |=  unalignedGet32((void*)(relAddr + 1));
                unalignedSet8((void*)(relAddr + 0), ((uint8_t*)&d2)[0]);
                unalignedSet8((void*)(relAddr + 1), ((uint8_t*)&d1)[0]);
                *to = unalignedGet32((void*)relAddr);
                if ((delta < 0) || (delta > 0x111111)) {
                    ERR("Relocation: RI6 out of range");
                    goto err;
                }
                break;
            }

            ERR("Relocation: unknown opcode %08X", v);
            goto err;
            break;
        }
        case R_XTENSA_ASM_EXPAND: {
            *from = unalignedGet32((void*)relAddr);
            *to = unalignedGet32((void*)relAddr);
            break;
        }
        default:
            MSG("Relocation: undefined relocation %d %s", type, type2String(type));
            assert(0);
            goto err;
    }

    PROFILER_STOP_RELOCATESYMBOL;
    return 0;
err:
    PROFILER_STOP_RELOCATESYMBOL;
    return -1;
}

/* Iterate through the singly linked list of sections until you find the one
 * that matches the provided index.
 * All these section structs are in RAM. */
static ELFLoaderSection_t *findSection(ELFLoaderContext_t* ctx, int index) {
    PROFILER_START_FINDSECTION;
    PROFILER_INC_FINDSECTION;

    for (ELFLoaderSection_t* section=ctx->section; section != NULL; section = section->next) {
        if (section->secIdx == index) {
            PROFILER_STOP_FINDSECTION;
            return section;
        }
    }
    PROFILER_STOP_FINDSECTION;
    return NULL;
}


static Elf32_Addr findSymAddr(ELFLoaderContext_t* ctx,
        Elf32_Sym *sym, const char *sName) {
    PROFILER_START_FINDSYMADDR;
    PROFILER_INC_FINDSYMADDR;
    #if CONFIG_ELFLOADER_SEARCH_LINEAR
    {
        for (int i = 0; i < ctx->env->exported_size; i++) {
            if (strcmp(ctx->env->exported[i].name, sName) == 0) {
                PROFILER_STOP_FINDSYMADDR;
                return (Elf32_Addr)(ctx->env->exported[i].ptr);
            }
        }
    }
    #elif CONFIG_ELFLOADER_SEARCH_BINARY
    {
        // Binary search for symbol
        int first, middle, last, res;
        first = 0;
        last = ctx->env->exported_size - 1;
        middle = (first + last)/2;
        while (first <= last) {
            res = strcmp(sName, ctx->env->exported[middle].name);
            if(res < 0) {
                last = middle - 1;
            }
            else if(res > 0){
                first = middle + 1;
            }
            else {
                PROFILER_STOP_FINDSYMADDR;
                return (Elf32_Addr)(ctx->env->exported[middle].ptr);
            }
            middle = (first + last)/2;
        }
    }
    #endif

    ELFLoaderSection_t *symSec = findSection(ctx, sym->st_shndx);
    if (symSec) {
        PROFILER_STOP_FINDSYMADDR;
        return ((Elf32_Addr) symSec->data) + sym->st_value;
    }
    PROFILER_STOP_FINDSYMADDR;
    return 0xffffffff;
}


static int relocateSection(ELFLoaderContext_t *ctx, ELFLoaderSection_t *s) {
    PROFILER_START_RELOCATESECTION;
    PROFILER_INC_RELOCATESECTION;

    char name[32] = "<unamed>";
    Elf32_Shdr sectHdr;
    PROFILER_STOP_RELOCATESECTION;
    if (readSection(ctx, s->relSecIdx, &sectHdr, name, sizeof(name)) != 0) {
        ERR("Error reading section header");
        goto err;
    }
    PROFILER_START_RELOCATESECTION;
    if (!(s->relSecIdx)) {
        PROFILER_STOP_RELOCATESECTION;
        MSG("  Section %s: no relocation index", name);
        return 0;
    }
    if (!(s->data)) {
        ERR("Section not loaded: %s", name);
        goto err;
    }
    MSG("  Section %s", name);
    int r = 0;
    Elf32_Rela rel;
    size_t relEntries = sectHdr.sh_size / sizeof(rel);
    MSG("  Offset   Sym  Type                      relAddr  "
        "symAddr  defValue                    Name + addend");
    for (size_t relCount = 0; relCount < relEntries; relCount++) {
        LOADER_GETDATA(ctx, sectHdr.sh_offset + relCount * (sizeof(rel)),
                &rel, sizeof(rel))
        Elf32_Sym sym;
        char name[65] = "<unnamed>";
        int symEntry = ELF32_R_SYM(rel.r_info);
        int relType = ELF32_R_TYPE(rel.r_info);

        /* data to be updated address */
        Elf32_Addr relAddr = ((Elf32_Addr) s->data) + rel.r_offset;
        PROFILER_STOP_RELOCATESECTION;
        readSymbol(ctx, symEntry, &sym, name, sizeof(name));
        PROFILER_START_RELOCATESECTION;

        /* Target Symbol Address */
        PROFILER_STOP_RELOCATESECTION;
        Elf32_Addr symAddr = findSymAddr(ctx, &sym, name) + rel.r_addend;
        PROFILER_START_RELOCATESECTION;

        uint32_t from, to;
        if (relType == R_XTENSA_NONE || relType == R_XTENSA_ASM_EXPAND) {
#if 0
            MSG("  %08X %04X %04X %-20s %08X          %08X"
                "                    %s + %X",
                rel.r_offset, symEntry, relType, type2String(relType),
                relAddr, sym.st_value, name, rel.r_addend);
#endif
        }
        else if ( (symAddr == 0xffffffff) && (sym.st_value == 0x00000000) ) {
            ERR("Relocation - undefined symAddr: %s", name);
            MSG("  %08X %04X %04X %-20s %08X %08X %08X"
                "                    %s + %X",
                rel.r_offset, symEntry, relType, type2String(relType),
                relAddr, symAddr, sym.st_value, name, rel.r_addend);
            r = -1;
        }
        else if(relocateSymbol(relAddr, relType, symAddr, sym.st_value, &from, &to) != 0) {
            ERR("  %08X %04X %04X %-20s %08X %08X %08X %08X->%08X %s + %X",
                    rel.r_offset, symEntry, relType, type2String(relType),
                    relAddr, symAddr, sym.st_value, from, to, name, rel.r_addend);
            r = -1;
        }
        else {
            MSG("  %08X %04X %04X %-20s %08X %08X %08X %08X->%08X %s + %X",
                    rel.r_offset, symEntry, relType, type2String(relType),
                    relAddr, symAddr, sym.st_value, from, to, name, rel.r_addend);
        }
    }
    PROFILER_STOP_RELOCATESECTION;
    return r;
err:
    PROFILER_STOP_RELOCATESECTION;
    ERR("Error reading relocation data");
    return -1;
}


/*** Main functions ***/


void elfLoaderFree(ELFLoaderContext_t* ctx) {
    if (ctx) {
        ELFLoaderSection_t* section = ctx->section;
        ELFLoaderSection_t* next;
        while(section != NULL) {
            if (section->data) {
                free(section->data);
            }
            next = section->next;
            free(section);
            section = next;
        }
        free(ctx);
    }
}

/* First Operation: Performs the following:
 *     * Allocates space for the returned Context (constant size)
 *     * Populates Context with:
 *         * Pointer/FD to the beginning of the ELF file
 *         * Pointer to env, which is a table to exported host function_names and their pointer in memory. 
 *         * Number of sections in the ELF file.
 *         * offset to SectionHeaderTable, which maps an index to a offset in ELF where the section header begins.
 *         * offset to StringTable, which is a list of null terminated strings.
 */
ELFLoaderContext_t *elfLoaderInit(LOADER_FD_T fd, const ELFLoaderEnv_t *env) {
    Elf32_Ehdr header;
    Elf32_Shdr section;
    ELFLoaderContext_t *ctx = NULL;

    /********************************************
     * Print All Exported Functions (Debugging) *
     ********************************************/
    MSG("ENV:");
    for (int i = 0; i < env->exported_size; i++) {
        MSG("       %08X %s", (unsigned int) env->exported[i].ptr, env->exported[i].name);
    }

    /***********************************************
     * Initialize the context object with pointers *
     ***********************************************/
    ctx = malloc(sizeof(ELFLoaderContext_t));
    if(NULL == ctx) {
        ERR("Insufficient memory for ElfLoaderContext_t");
        goto err;
    }
    memset(ctx, 0, sizeof(ELFLoaderContext_t));
    ctx->fd = fd;
    ctx->env = env;

    /********************************************************************
     * Load the ELF header (Ehdr), located at the beginning of the file *
     ********************************************************************/
    LOADER_GETDATA(ctx, 0, &header, sizeof(Elf32_Ehdr));

    /* Make sure that we have a correct and compatible ELF header. */
    char ElfMagic[] = { 0x7f, 'E', 'L', 'F', '\0' };
    if ( memcmp(header.e_ident, ElfMagic, strlen(ElfMagic)) != 0 ) {
        ERR("Bad ELF Identification");
        goto err;
    }

    /* Populate context with ELF Header information*/
    ctx->e_shnum = header.e_shnum; // Number of Sections
    ctx->e_shoff = header.e_shoff; // offset to SectionHeaderTable

    /*********************************************
     * Load the SectionHeader of the StringTable *
     *********************************************/
    /* Purpose: Store in context where the StringTable begins in the file */
    /* The StringTable is at index header.e_shstrndx of the SectionHeaderTable,
     * which begins at header.e_shoff. */
    LOADER_GETDATA(ctx, header.e_shoff + header.e_shstrndx * sizeof(Elf32_Shdr),
            &section, sizeof(Elf32_Shdr));
#if CONFIG_ELFLOADER_POSIX && CONFIG_ELFLOADER_CACHE_STRTAB
    // Read the StringTable into memory
    MSG("String Table Size: %d", section.sh_size);
    ctx->shstrtab = malloc( section.sh_size );
    if( NULL == ctx->shstrtab ) {
        ERR("Insufficient memory for StringTable\n");
        goto err;
    }
    LOADER_GETDATA(ctx, section.sh_offset, ctx->shstrtab, section.sh_size);
#else
    // Store the offset from beginning of ELF where the StringTable begins.
    ctx->shstrtab_offset = section.sh_offset;
#endif

    return ctx;

err:
    if( NULL != ctx ) {
        free(ctx);
    }
    return NULL;
}

/* user has to remember to free allocated content buffer */
void *elfLoaderLoadSectionByName(const ELFLoaderContext_t *ctx, const char *target, size_t *data_len ) {
    for (int n = 1; n < ctx->e_shnum; n++) {
        Elf32_Shdr sectHdr;
        char name[33] = "<unamed>";
        if (readSection(ctx, n, &sectHdr, name, sizeof(name)) != 0) {
            ERR("Error reading section");
            //goto err;
        }
        if( 0 == strcmp(name, target) ) {
            // Read contents
            if( NULL != data_len ) {
                *data_len = sectHdr.sh_size;
            }
            void *data;
            if (sectHdr.sh_flags & SHF_EXECINSTR) {
                data = LOADER_ALLOC_EXEC(CEIL4(sectHdr.sh_size));
            } else {
                data = LOADER_ALLOC_DATA(CEIL4(sectHdr.sh_size));
            }
            if (!data) {
                ERR("Section malloc failed: %s", name);
                goto err;
            }
            if (sectHdr.sh_type != SHT_NOBITS) {
                LOADER_GETDATA(ctx, sectHdr.sh_offset, data,
                        CEIL4(sectHdr.sh_size));
            }
            return data;
        }
    }
err:
    return NULL;
}

/* Second Operation: 
 *     1) For Each Section Header:
 *         a) Read Section Header from ELF to stack variable
 *             i) Read the section's name from StringTable
 *         b) If SH_ALLOC:
 *             i) Allocates and populates memory for data
 *            If SH_RELA:
 *             i) Finds the section where the rela information applies
 *             ii) Link that section to this section
 *         c) Store index of .text, .strtab, .symtab
 * General Notes:
 *     * ELFLoaderSection structs form a singly linked list.
 * */
ELFLoaderContext_t* elfLoaderLoad(ELFLoaderContext_t *ctx) {
    MSG("Scanning ELF sections         relAddr      size");
    // Iterate through all section_headers
    for (int n = 1; n < ctx->e_shnum; n++) {
        Elf32_Shdr sectHdr;
        char name[33] = "<unamed>";

        /***********************
         * Read Section Header *
         ***********************/
        // Read the section header at index n
        // Populates "secHdr" with the Section's Header
        // Populates "name" with the section's name retrieved from the StringTable.
        if (readSection(ctx, n, &sectHdr, name, sizeof(name)) != 0) {
            ERR("Error reading section");
            goto err;
        }

        if (sectHdr.sh_flags & SHF_ALLOC) { // This  section  occupies  memory during process execution
            if (!sectHdr.sh_size) {
                MSG("  section %2d: %-15s no data", n, name);
            } 
            else {
                // Allocate space for Section Struct (not data)
                ELFLoaderSection_t* section = malloc(sizeof(ELFLoaderSection_t));
                assert(section);
                memset(section, 0, sizeof(ELFLoaderSection_t));

                /* populate the Section elements */
                section->secIdx = n;
                section->size = sectHdr.sh_size;
                // Add it to the beginning of the SinglyLinkedList
                section->next = ctx->section;
                ctx->section = section;

                /* Allocate memory */
                section->data = ( sectHdr.sh_flags & SHF_EXECINSTR ) ?
                        LOADER_ALLOC_EXEC(CEIL4(sectHdr.sh_size)) : // Executable Memory
                        LOADER_ALLOC_DATA(CEIL4(sectHdr.sh_size)) ; // Normal Memory
                if (!section->data) {
                    ERR("Section malloc failed: %s", name);
                    goto err;
                }

                /* Load Section into allocated data */
                if (sectHdr.sh_type != SHT_NOBITS) {
                    LOADER_GETDATA(ctx, sectHdr.sh_offset, section->data, CEIL4(sectHdr.sh_size));
                }

                /* If this is the text section, populate the Context field */
                if (strcmp(name, ".text") == 0) {
                    ctx->text = section->data;
                }

                MSG("  section %2d: %-15s %08X %6i", n, name, (unsigned int) section->data, sectHdr.sh_size);
            }
        }
        else if (sectHdr.sh_type == SHT_RELA) { // Relocation entries with addends
            /* sh_info holds extra information that depends on sh_type.
             * For sh_type SHT_RELA:
             *     The section header index of the section to which the 
             *     relocation applies.
             */

            /* If the index is greater than the number of sections that exist,
             * it must be erroneous */
            if (sectHdr.sh_info >= n) {
                ERR("Rela section: bad linked section (%i:%s -> %i)", n, name, sectHdr.sh_info);
                goto err;
            }

            /* iterate through the singly linked list of sections until you
             * find the one that matches sh_info */
            ELFLoaderSection_t* section = findSection(ctx, sectHdr.sh_info);
            if (section == NULL) { // Cannot find section
                MSG("  section %2d: %-15s -> %2d: ignoring", n, name, sectHdr.sh_info);
            } else {
                section->relSecIdx = n;
                MSG("  section %2d: %-15s -> %2d: ok", n, name, sectHdr.sh_info);
            }
        } else {
            MSG("  section %2d: %s", n, name);
            if (strcmp(name, ".symtab") == 0) {
                ctx->symtab_offset = sectHdr.sh_offset;
                ctx->symtab_count = sectHdr.sh_size / sizeof(Elf32_Sym);
            } else if (strcmp(name, ".strtab") == 0) {
                ctx->strtab_offset = sectHdr.sh_offset;
            }
        }
    }
    if (ctx->symtab_offset == 0 || ctx->strtab_offset == 0) {
        ERR("Missing .symtab or .strtab section");
        goto err;
    }
    return ctx;
err:
    return NULL;
}

ELFLoaderContext_t* elfLoaderRelocate(ELFLoaderContext_t *ctx) {
    MSG("Relocating sections");
    int r = 0;
    for (ELFLoaderSection_t* section = ctx->section; section != NULL; section = section->next) {
        r |= relocateSection(ctx, section);
    }
    if (r != 0) {
        MSG("Relocation failed");
        goto err;
    }
    return ctx;

err:
    return NULL;
}

int elfLoaderSetFunc(ELFLoaderContext_t *ctx, char* funcname) {
    ctx->exec = 0;
    MSG("Scanning ELF symbols");
    MSG("  Sym  Symbol                         sect value    size relAddr");
    // Start from the end because it'll probably be closer
    for (int symCount = ctx->symtab_count - 1; symCount >= 0; symCount--) {
        Elf32_Sym sym;
        char name[33] = "<unnamed>";
        if (readSymbolFunc(ctx, symCount, &sym, name, sizeof(name)) != 0) {
            ERR("Error reading symbol");
            return -1;
        }
        if(strcmp(name, funcname) == 0) {
            Elf32_Addr symAddr = findSymAddr(ctx, &sym, name);
            if (symAddr == 0xffffffff) {
                MSG("  %04X %-30s %04X %08X %04X ????????", symCount, name, sym.st_shndx, sym.st_value, sym.st_size);
            } else {
                ctx->exec = (void*)symAddr;
                MSG("  %04X %-30s %04X %08X %04X %08X", symCount, name, sym.st_shndx, sym.st_value, sym.st_size, symAddr);
                break;
            }
        } else {
            MSG("  %04X %-30s %04X %08X %04X", symCount, name, sym.st_shndx, sym.st_value, sym.st_size);
        }
    }
    if (ctx->exec == 0) {
        ERR("Function symbol not found: %s", funcname);
        return -1;
    }
    return 0;
}

int elfLoaderRun(ELFLoaderContext_t *ctx, int argc, char **argv) {
    if (!ctx->exec) {
        return 0;
    }
    typedef int (*func_t)(int, char**);
    func_t func = (func_t)ctx->exec;
    MSG("Running...");
    int r = func(argc, argv);
    MSG("Result: %08X", r);
    return r;
}

/* Loads a file_descriptor, environment, function name, and arguments */
int elfLoader(LOADER_FD_T fd, const ELFLoaderEnv_t *env, char* funcname, int argc, char **argv) {
    ELFLoaderContext_t *ctx;
    int r;
    if( NULL == (ctx = elfLoaderInit(fd, &env)) ||
        NULL == elfLoaderLoad(ctx) ||
        NULL == elfLoaderRelocate(ctx) ||
        0 != elfLoaderSetFunc(ctx, funcname) ) {
        r = -1; goto err;
    }
    #if CONFIG_ELFLOADER_PROFILER_EN
    elfLoaderProfilerPrint();
    #endif
    r = elfLoaderRun(ctx, argc, argv);
err:
    elfLoaderFree(ctx);
    return r;
}

void* elfLoaderGetTextAddr(ELFLoaderContext_t *ctx) {
    return ctx->text;
}

#if CONFIG_ELFLOADER_PROFILER_EN
/* Sets all profiler variables to 0 */
void elfLoaderProfilerReset() {
    memset(&profiler_readSection, 0, sizeof(profiler_timer_t));
    memset(&profiler_readSymbol, 0, sizeof(profiler_timer_t));
    memset(&profiler_readSymbolFunc, 0, sizeof(profiler_timer_t));
    memset(&profiler_relocateSymbol, 0, sizeof(profiler_timer_t));
    memset(&profiler_findSymAddr, 0, sizeof(profiler_timer_t));
    memset(&profiler_findSection, 0, sizeof(profiler_timer_t));
    memset(&profiler_relocateSection, 0, sizeof(profiler_timer_t));
}

/* Prints the profiler results to uart console */
void elfLoaderProfilerPrint() {
    int64_t total_time;
    total_time = 
        profiler_readSection.t      +
        profiler_readSymbol.t       +
        profiler_readSymbolFunc.t   +
        profiler_relocateSymbol.t   +
        profiler_findSymAddr.t      + 
        profiler_relocateSection.t
        ;

    MSG("\nELF Loader Profiling Results:\n"
            "Function Name          Time (uS)    Calls \n"
            "readSection:           %10lld    %8d\n"
            "readSymbol:            %10lld    %8d\n"
            "readSymbolFunc:        %10lld    %8d\n"
            "relocateSymbol:        %10lld    %8d\n"
            "findSymAddr:           %10lld    %8d\n"
            "relocateSection:       %10lld    %8d\n"
            "total time:            %10lld\n"
            "\n",
            profiler_readSection.t,     profiler_readSection.n,
            profiler_readSymbol.t,      profiler_readSymbol.n,
            profiler_readSymbolFunc.t,  profiler_readSymbolFunc.n,
            profiler_relocateSymbol.t,  profiler_relocateSymbol.n,
            profiler_findSymAddr.t,     profiler_findSymAddr.n,
            profiler_relocateSection.t, profiler_relocateSection.n,
            total_time);
}
#endif

