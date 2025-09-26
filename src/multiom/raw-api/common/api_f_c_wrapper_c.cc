
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <iomanip>
#include <iostream>
#include "eccodes.h"

typedef struct {
    long long object_id;
    size_t buf_size;
    size_t obj_size;
    size_t hash;
    int8_t* bytes;
} f_c_wrapper;

extern "C" {

int c_allocate_bytes( void** mem, size_t size ){

  *mem = (void*) malloc( size );
  if (*mem == NULL) {
    return 1;
  }
  memset(*mem, 0, size);
  return 0;
}

int c_free_bytes( void** mem ){

  // Check if the pointer is NULL
  if (*mem == NULL) {
      return 1;
  }

  free( *mem );
  *mem = NULL;

  // Exit point on success
  return 0;
}

int c_allocate_wrapper( void** mem, size_t size ){

  // Check if the pointer is NULL
  if (*mem != NULL) {
      return 1;
  }

  // Compute the size of the buffer
  size_t sz = sizeof(f_c_wrapper);

  // Compare the size with the requested size
  if ( size != sz) {
    printf("Error: requested size %zu does not match expected size %zu\n", size, sz);
    return 1;
  }

  // Allocate the memory
  *mem = (void*) malloc( sz );
  if (*mem == NULL) {
    return 1;
  }

  // Initialize the object
  f_c_wrapper* wrapper = (f_c_wrapper*)*mem;
  wrapper->object_id = 0;
  wrapper->buf_size = size;
  wrapper->obj_size = 0;
  wrapper->hash = 0;
  wrapper->bytes = NULL;

  // Exit point on success
  return 0;
}

int c_free_wrapper( void** mem ){

  // Check if the pointer is NULL
  if (*mem == NULL) {
      return 1;
  }

  // Free the memory
  free( *mem );

  // Reset the pointer
  *mem = NULL;

  // Exit point on success
  return 0;
}

int hash_int8_array(const int8_t* data, size_t len, size_t* hash ) {

    const uint32_t fnv_prime = 16777619u; // FNV prime
    const uint32_t fnv_offset = 2166136261u; // FNV offset basis
    uint32_t fnv_hash = fnv_offset;
    for (size_t i = 0; i < len; ++i) {
        fnv_hash ^= (uint32_t)(uint8_t)data[i];          // cast to uint8_t for consistency
        fnv_hash *= fnv_prime;          // FNV prime
    }
    *hash = (size_t)fnv_hash; // cast to size_t for compatibility
    // MIVAL: code for debug interoperability on different compilers
    // printf( "compute hash: %llu, %p, %llu\n", *hash, data, len );
    return 0;
}


}
