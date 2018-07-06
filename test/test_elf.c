#include "unity.h"
#include <stdio.h>
#include <string.h>
#include <esp_spi_flash.h>
#include <esp_partition.h>
#include <esp_err.h>

#include "loader.h"

static const ELFLoaderSymbol_t exports[] = {
    { "puts", (void*) puts }
};
static const ELFLoaderEnv_t env = { exports, sizeof(exports) / sizeof(*exports) };

TEST_CASE("hello_world", "[elfLoader]") {
    esp_err_t err;

    // Get User App Partition Handle
    const esp_partition_iterator_t app_iter = esp_partition_find(
            0x41, ESP_PARTITION_SUBTYPE_ANY, "user");
    const esp_partition_t *app = esp_partition_get(app_iter);
    esp_partition_iterator_release(app_iter);

    // Map that partition to a virtual address in data space
    const void *vaddr;
    spi_flash_mmap_handle_t out_handle;
    err = esp_partition_mmap(app, 0, app->size, SPI_FLASH_MMAP_DATA, &vaddr, &out_handle);

    int r = elfLoader(vaddr, &env, "app_main", 0x10);
    TEST_ASSERT( r == 0x11 );
}
