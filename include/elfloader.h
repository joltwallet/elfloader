#ifndef __ELFLOADER_H__
#define __ELFLOADER_H__
#include <stdio.h>
#include <stdint.h>


#if defined(__linux__)
#define LOADER_FD_T FILE *
#endif

#if !(defined(__linux__))
#define LOADER_FD_T void*
#endif


typedef struct {
    const char *name; /*!< Name of symbol */
    void *ptr; /*!< Pointer of symbol in memory */
} ELFLoaderSymbol_t;

typedef struct {
    const ELFLoaderSymbol_t *exported; /*!< Pointer to exported symbols array */
    unsigned int exported_size; /*!< Elements on exported symbol array */
} ELFLoaderEnv_t;

typedef struct ELFLoaderContext_t ELFLoaderContext_t;


int elfLoader(LOADER_FD_T fd, const ELFLoaderEnv_t *env, char *funcname, int argc, char **argv);
int elfLoaderRun(ELFLoaderContext_t *ctx, int argc, char **argv);
int elfLoaderSetFunc(ELFLoaderContext_t *ctx,char *funcname);

ELFLoaderContext_t *elfLoaderInit(LOADER_FD_T fd, const ELFLoaderEnv_t *env);
ELFLoaderContext_t* elfLoaderLoad(ELFLoaderContext_t *ctx);
ELFLoaderContext_t* elfLoaderRelocate(ELFLoaderContext_t *ctx);

ELFLoaderContext_t *elfLoaderInitLoadAndRelocate(ELFLoaderContext_t *ctx);
void elfLoaderFree(ELFLoaderContext_t *ctx);
void* elfLoaderGetTextAddr(ELFLoaderContext_t *ctx);


void *elfLoaderLoadSectionByName(const ELFLoaderContext_t *ctx, const char *name, size_t * data_len );
#define INTERFACE 0
#endif
