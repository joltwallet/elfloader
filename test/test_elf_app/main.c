#include <stddef.h>

extern void printf(char *, ...);
extern void sqrt(double);
extern int crypto_generichash(unsigned char *, size_t, const unsigned char *, unsigned long long, const unsigned char *, size_t);
extern char *sodium_bin2hex(char * const, const size_t,
                             const unsigned char * const, const size_t);

int app_main(int argc, char **argv) {
    printf("Hello World!\n");

    unsigned char hash[32];
    unsigned char key[32] = { 0 };
    crypto_generichash(hash, sizeof(hash), "meow", 4, 
            key, sizeof(key));
    char hash_hex[65];
    sodium_bin2hex(hash_hex, sizeof(hash_hex), hash, sizeof(hash) );
    printf("Hash result: %s\n", hash_hex);
    return 0;
}
