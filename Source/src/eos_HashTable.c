/*********************************************************************
 * Class Name : eos_HashTable
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 *
 * Copyright -- see file named COPYRIGHTNOTICE
 *
 *********************************************************************/
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "eos_HashTable.h"

// Returns true if given data is monotonically increasing
EOS_BOOLEAN isMonoInc(EOS_REAL* data, EOS_INTEGER size, EOS_INTEGER stride) {
  int i;
  for (i = 0; i < size - 1; i++) {
    if (data[(i + 1) * stride] < data[i * stride]) {
      return EOS_FALSE;
    }
  }
  return EOS_TRUE;
}

// Convenience function that copies n bytes from src into dest, returning dest + n
EOS_CHAR* pack(EOS_CHAR* dest, EOS_CHAR* src, size_t n) {
  memcpy(dest, src, n);
  return dest + n;
}

// Convenience function that copies n bytes from src into dest, returning src + n
EOS_CHAR* unpack(EOS_CHAR* dest, EOS_CHAR* src, size_t n) {
  memcpy(dest, src, n);
  return src + n;
}

// Convenience function that returns an offset to make all numbers in data positive
static inline EOS_REAL eos_calcOffset(EOS_REAL* x, EOS_INTEGER ix) {
  // Account for negative numbers with magic number multiplier offset
  if (x[0] < 0) {
    return -x[0] * 1.118018298230850348;
  } else if (x[0] == 0.0) {
    return x[1 * ix] * 0.118018298230850348;
  }
  return 0.0;
}

/**
 * @brief Populates the given pre-allocated 1D hashtable with indices into data
 * 
 * @param ht A pointer to the hashtable, must already be allocated
 * @param data Pointer to data to genereate table from
 * @param size Number of elements in data
 * @param stride Distance in cells between each element in data
 */
void HashTable1D_populate(eos_HashTable1D* ht, EOS_REAL* data, EOS_INTEGER size, EOS_INTEGER stride) {
  int i;

  // Account for negative numbers with magic number multiplier offset
  EOS_REAL offset = eos_calcOffset(data, stride);
  ht->valueOffset = offset;
  ht->n = size;

  const int maxHash = eos_expHashFn(data[(size - 1) * stride] + offset);
  const int minHash = eos_expHashFn(data[0] + offset); 
  ht->size = maxHash - minHash + 1;
  ht->offset = minHash;
  ht->table = (short*)malloc(ht->size * sizeof(EOS_INTEGER));

  // Initialize the hash table with highest index into x
  for (i = 0; i < ht->size; i++) {
    ht->table[i] = size - 2;
  }

  // Populate the hash table with indices
  for (i = 0; i < size; i++) {
    int index = eos_expHashFn(data[i * stride] + offset) - ht->offset;
    ht->table[index] = bmin(ht->table[index], i);
  }

  // Correct for if an order of magnitude got skipped
  for (i = ht->size - 1; i > 0; i--) {
    int upperBound = ht->table[i];
    while (ht->table[i - 1] > upperBound) {
      ht->table[i - 1] = upperBound - 1;
      i--;
    }
  }
}

eos_HashTable1D* eos_HashTable1D_gen(EOS_REAL* data, EOS_INTEGER size)
{
  if (!isMonoInc(data, size, 1)) {
    return NULL;
  }
  eos_HashTable1D* ht = (eos_HashTable1D*)malloc(sizeof(eos_HashTable1D));
  HashTable1D_populate(ht, data, size, 1);
  return ht;
}

void eos_HashTable1D_free(eos_HashTable1D* ht)
{
  free(ht->table);
  free(ht);
}

size_t eos_HashTable1D_byteSize(eos_HashTable1D* ht)
{
  return
    sizeof(EOS_INTEGER) * 3     // size, n, offset
    + sizeof(EOS_REAL)          // valueOffset
    + sizeof(short) * ht->size; // table
}

size_t eos_HashTable1D_pack(eos_HashTable1D* ht, EOS_CHAR* packInto)
{
  EOS_CHAR* start = packInto;
  packInto = pack(packInto, (EOS_CHAR*)&ht->size, sizeof(EOS_INTEGER));
  packInto = pack(packInto, (EOS_CHAR*)&ht->n, sizeof(EOS_INTEGER));
  packInto = pack(packInto, (EOS_CHAR*)&ht->offset, sizeof(EOS_INTEGER));
  packInto = pack(packInto, (EOS_CHAR*)&ht->valueOffset, sizeof(EOS_REAL));
  packInto = pack(packInto, (EOS_CHAR*)ht->table, sizeof(short) * ht->size);
  assert(packInto - start == eos_HashTable1D_byteSize(ht));
  return eos_HashTable1D_byteSize(ht);
}

size_t eos_HashTable1D_unpack(eos_HashTable1D* ht, EOS_CHAR* unpackFrom)
{
  EOS_CHAR* start = unpackFrom;
  unpackFrom = unpack((EOS_CHAR*)&ht->size, unpackFrom, sizeof(EOS_INTEGER));
  unpackFrom = unpack((EOS_CHAR*)&ht->n, unpackFrom, sizeof(EOS_INTEGER));
  unpackFrom = unpack((EOS_CHAR*)&ht->offset, unpackFrom, sizeof(EOS_INTEGER));
  unpackFrom = unpack((EOS_CHAR*)&ht->valueOffset, unpackFrom, sizeof(EOS_REAL));
  ht->table = (short*)malloc(sizeof(short) * ht->size);
  unpackFrom = unpack((EOS_CHAR*)ht->table, unpackFrom, sizeof(short) * ht->size);
  assert(unpackFrom - start == eos_HashTable1D_byteSize(ht));
  return eos_HashTable1D_byteSize(ht);
}

eos_HashTable2D* eos_HashTable2D_gen(EOS_REAL** data, EOS_INTEGER cols, EOS_INTEGER rows)
{
  int r, c;
  eos_HashTable2D* ht = (eos_HashTable2D*)malloc(sizeof(eos_HashTable2D));

  ht->rows = rows;
  ht->cols = cols;

  // Populate the row tables (skipping those that are not monotonically increasing)
  ht->rowTables = (eos_HashTable1D*)calloc(rows, sizeof(eos_HashTable1D));
  for (r = 0; r < rows; r++) {
    if (isMonoInc(data[r], cols, 1)) {
      HashTable1D_populate(&ht->rowTables[r], data[r], cols, 1);
    }
  }

  // Populate the column tables (skipping those that are not monotonically increasing)
  ht->colTables = (eos_HashTable1D*)calloc(cols, sizeof(eos_HashTable1D));
  for (c = 0; c < cols; c++) {
    if (isMonoInc(&data[0][c], rows, cols)) {
      HashTable1D_populate(&ht->colTables[c], &data[0][c], rows, cols);
    }
  }
  
  return ht;
}

void eos_HashTable2D_free(eos_HashTable2D* ht)
{
  int r, c;
  // Free rowtables
  for (r = 0; r < ht->rows; r++) {
    if ((&ht->rowTables[r])->size > 0) {
      free((&ht->rowTables[r])->table);
    }
  }
  free(ht->rowTables);
  // Free coltables
  for (c = 0; c < ht->cols; c++) {
    if ((&ht->colTables[c])->size > 0) {
      free((&ht->colTables[c])->table);
    }
  }
  free(ht->colTables);

  free(ht);
}

size_t eos_HashTable2D_byteSize(eos_HashTable2D* ht)
{
  int r, c;
  size_t bytes = 0;
  bytes += sizeof(EOS_INTEGER) * 2; // cols, rows
  for (r = 0; r < ht->rows; r++) {
    // Byte the signifies if row hashtable exists
    bytes += sizeof(EOS_CHAR);
    if ((&ht->rowTables[r])->size > 0) {
      bytes += eos_HashTable1D_byteSize(&ht->rowTables[r]);
    }
  }
  for (c = 0; c < ht->cols; c++) {
    // Byte the signifies if col hashtable exists
    bytes += sizeof(EOS_CHAR);
    if ((&ht->colTables[c])->size > 0) {
      bytes += eos_HashTable1D_byteSize(&ht->colTables[c]);
    }
  }
  return bytes;
}

size_t eos_HashTable2D_pack(eos_HashTable2D* ht, EOS_CHAR* packInto)
{
  int r, c;
  EOS_CHAR* start = packInto;
  packInto = pack(packInto, (EOS_CHAR*)&ht->cols, sizeof(EOS_INTEGER));
  packInto = pack(packInto, (EOS_CHAR*)&ht->rows, sizeof(EOS_INTEGER));

  for (r = 0; r < ht->rows; r++) {
    // Byte the signifies if row hashtable exists
    EOS_CHAR exists = ((&ht->rowTables[r])->size > 0);
    packInto = pack(packInto, &exists, sizeof(EOS_CHAR));
    if (exists) {
      packInto += eos_HashTable1D_pack(&ht->rowTables[r], packInto);
    }
  }

  for (c = 0; c < ht->cols; c++) {
    // Byte the signifies if col hashtable exists
    EOS_CHAR exists = ((&ht->colTables[c])->size > 0);
    packInto = pack(packInto, &exists, sizeof(EOS_CHAR));
    if (exists) {
      packInto += eos_HashTable1D_pack(&ht->colTables[c], packInto);
    }
  }

  assert(packInto - start == eos_HashTable2D_byteSize(ht));
  return eos_HashTable2D_byteSize(ht);
}

size_t eos_HashTable2D_unpack(eos_HashTable2D* ht, EOS_CHAR* unpackFrom)
{
  int r, c;
  EOS_CHAR* start = unpackFrom;
  unpackFrom = unpack((EOS_CHAR*)&ht->cols, unpackFrom, sizeof(EOS_INTEGER));
  unpackFrom = unpack((EOS_CHAR*)&ht->rows, unpackFrom, sizeof(EOS_INTEGER));

  // Unpack the rowtables
  ht->rowTables = (eos_HashTable1D*)calloc(ht->rows, sizeof(eos_HashTable1D));
  for (r = 0; r < ht->rows; r++) {
    EOS_CHAR exists;
    unpackFrom = unpack(&exists, unpackFrom, sizeof(EOS_CHAR));
    if (exists) {
      unpackFrom += eos_HashTable1D_unpack(&ht->rowTables[r], unpackFrom);
    }
  }

  // Unpack the coltables
  ht->colTables = (eos_HashTable1D*)calloc(ht->cols, sizeof(eos_HashTable1D));
  for (c = 0; c < ht->cols; c++) {
    EOS_CHAR exists;
    unpackFrom = unpack(&exists, unpackFrom, sizeof(EOS_CHAR));
    if (exists) {
      unpackFrom += eos_HashTable1D_unpack(&ht->colTables[c], unpackFrom);
    }
  }

  assert(unpackFrom - start == eos_HashTable2D_byteSize(ht));
  return eos_HashTable2D_byteSize(ht);
}
