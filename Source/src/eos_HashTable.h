/*********************************************************************
 * Class Name : eos_HashTable 
 * ---------------------------------------------------------
 * Filetype: (HEADER)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 * This file defines hashtable structs and related functions.
 * (see expHashSearch() eos_Utils.c)
 * 
 *********************************************************************/
#ifndef EOS_HASHTABLE_H
#define EOS_HASHTABLE_H

#include <stddef.h>
#include "eos_universal_types.h"

/**
 * @brief Extracts the base 2 exponent from the given double-precision floating point number
 * 
 * @param x Value to hash
 * @return EOS_INTEGER The normalized base-2 exponent
 */
static inline EOS_INTEGER eos_expHashFn(EOS_REAL x) {
  // Converts x to an unsigned long pointer so that the exponent (bits 62-52) can be extracted
  // Subtracts 1022 because that is the offset to normalize the stored value to a signed value
  // https://en.wikipedia.org/wiki/Double-precision_floating-point_format
  return (EOS_INTEGER)(0x7ff & ((*(unsigned long*)(&x)) >> 52)) - 1022;
}

// Return the smallest of two integers without branching
static inline EOS_INTEGER bmin(EOS_INTEGER a, EOS_INTEGER b) {
  return a ^ ((b ^ a) & -(b < a));
}

// Return the largest of two integers without branching
static inline EOS_INTEGER bmax(EOS_INTEGER a, EOS_INTEGER b) {
  return a ^ ((a ^ b) & -(a < b));
}

// Fastest way to calculate max(a, 0)
static inline EOS_INTEGER bmax0(EOS_INTEGER a) {
  return a & -(0 < a);
}

// Return valTrue if condition is not 0, else return valFalse, without branching
static inline EOS_INTEGER bchoose(EOS_INTEGER condition, EOS_INTEGER valTrue, EOS_INTEGER valFalse) {
  return valTrue ^ ((valFalse ^ valTrue) & -(!condition));
}

/**
 * @brief A 1D exponent hash table, stores indices into a 1D array for searching with eos_Search
 * 
 * Typically used to index into Density and Temperature 1D arrays
 */
typedef struct
{
  EOS_INTEGER size;         /**< Size of the table */
  EOS_INTEGER n;            /**< Size of the array this table was created for at time of generation */
  EOS_INTEGER offset;       /**< Amount to add to the result of hash to get an index into this table */
  EOS_REAL    valueOffset;  /**< Amount to add to search values  */
  short*      table;        /**< Table of indices into the corresponding array */
} eos_HashTable1D;

/**
 * @brief Allocates and populates a 1D hashtable from the given 1D data with given size
 * 
 * @param data The data to generate hashtable from (must be monotonically increasing!)
 * @param size Length of data
 * @return eos_HashTable1D* - The generated hashtable, NULL if data is not monotonically increasing
 */
eos_HashTable1D* eos_HashTable1D_gen(EOS_REAL* data, EOS_INTEGER size);

/**
 * @brief Returns the number of packed bytes of memory hashtable takes up
 * 
 * @param ht Hashtable to count bytes of
 * @return size_t Number of bytes
 */
size_t eos_HashTable1D_byteSize(eos_HashTable1D* ht);

/**
 * @brief Pack given hashtable into array of bytes
 * 
 * @param ht Hashtable to pack
 * @param packInto Array of byts to pack into
 * @return size_t Number of bytes packed (same as returned by eos_HashTable1D_byteSize())
 */
size_t eos_HashTable1D_pack(eos_HashTable1D* ht, EOS_CHAR* packInto);

/**
 * @brief Frees all memory allocated to hashtable
 * 
 * @param ht 1D hashtable to free
 */
void eos_HashTable1D_free(eos_HashTable1D* ht);

/**
 * @brief Unpack hashtable from byte array
 * 
 * @param ht Pre-allocated memory location for hashtable
 * @param unpackFrom Byte array to unpack hashtable from
 * @return size_t Number of bytes unpacked (same as returned by eos_HashTable1D_byteSize())
 */
size_t eos_HashTable1D_unpack(eos_HashTable1D* ht, EOS_CHAR* unpackFrom);

/**
 * @brief A 2D exponent hash table, stores indices into a 2D array for searching with eos_Search
 * 
 * Typically used to index into 2D SESAME tables
 */
typedef struct
{
  EOS_INTEGER cols;           /**< Number of column tables in the 2D hash table */
  EOS_INTEGER rows;           /**< Number of row tables in the 2D hash table */
  eos_HashTable1D* rowTables; /**< Array of row tables, one for each row */
  eos_HashTable1D* colTables; /**< Array of column tables, one for each column */
} eos_HashTable2D;

/**
 * @brief Allocates and populates a 2D hashtable from given data with given size
 * 
 * @param data 2D data to gen from
 * @param cols Number of columns in data
 * @param rows Number of rows in data
 * @return eos_HashTable2D* - The generated 2D hashtable
 */
eos_HashTable2D* eos_HashTable2D_gen(EOS_REAL** data, EOS_INTEGER cols, EOS_INTEGER rows);

/**
 * @brief Frees all memory allocated to hashtable
 * 
 * @param ht 2D hashtable to free
 */
void eos_HashTable2D_free(eos_HashTable2D* ht);

/**
 * @brief Returns the number of packed bytes of memory hashtable takes up
 * 
 * @param ht Hashtable to count bytes of
 * @return size_t Number of bytes
 */
size_t eos_HashTable2D_byteSize(eos_HashTable2D* ht);

/**
 * @brief Pack given hashtable into array of bytes
 * 
 * @param ht Hashtable to pack
 * @param packInto Array of byts to pack into
 * @return size_t Number of bytes packed (same as returned by eos_HashTable1D_byteSize())
 */
size_t eos_HashTable2D_pack(eos_HashTable2D* ht, EOS_CHAR* packInto);

/**
 * @brief Unpack hashtable from byte array
 * 
 * @param ht Pre-allocated memory location for hashtable
 * @param unpackFrom Byte array to unpack hashtable from
 * @return size_t Number of bytes unpacked (same as returned by eos_HashTable1D_byteSize())
 */
size_t eos_HashTable2D_unpack(eos_HashTable2D* ht, EOS_CHAR* unpackFrom);

#endif