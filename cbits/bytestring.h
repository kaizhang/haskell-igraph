#ifndef HASKELL_IGRAPH_BYTESTRING
#define HASKELL_IGRAPH_BYTESTRING

#include "igraph.h"

__BEGIN_DECLS

typedef struct bytestring_t {
  unsigned long int len;
  char *value;
} bytestring_t;

typedef struct bsvector_t {
  bytestring_t **data;
  long int len;
} bsvector_t;

#define BSVECTOR_INIT_FINALLY(v, size) \
  do { IGRAPH_CHECK(bsvector_init(v, size)); \
  IGRAPH_FINALLY(bsvector_destroy, v); } while (0)

/**
 * \define STR
 * Indexing string vectors
 *
 * This is a macro which allows to query the elements of a string vector in
 * simpler way than \ref igraph_strvector_get(). Note this macro cannot be
 * used to set an element, for that use \ref igraph_strvector_set().
 * \param sv The string vector
 * \param i The the index of the element.
 * \return The element at position \p i.
 *
 * Time complexity: O(1).
 */
#define BS(sv,i) ((const bytestring_t *)((sv).data[(i)]))

int bsvector_init(bsvector_t *sv, long int len);

DECLDIR void bsvector_destroy(bsvector_t *sv);

DECLDIR void bsvector_get(const bsvector_t *sv, long int idx, bytestring_t **value);

DECLDIR int bsvector_set(bsvector_t *sv, long int idx, const bytestring_t *value);

DECLDIR void bsvector_remove_section(bsvector_t *v, long int from, long int to);

DECLDIR void bsvector_remove(bsvector_t *v, long int elem);

/*
void bsvector_move_interval(bsvector_t *v, long int begin,
				   long int end, long int to) {
  long int i;
  assert(v != 0);
  assert(v->data != 0);
  for (i=to; i<to+end-begin; i++) {
    if (v->data[i] != 0) {
      destroy_bytestring(v->data[i]);
    }
  }
  for (i=0; i<end-begin; i++) {
    if (v->data[begin+i] != 0) {
      size_t len=strlen(v->data[begin+i])+1;
      v->data[to+i]=igraph_Calloc(len, char);
      memcpy(v->data[to+i], v->data[begin+i], sizeof(char)*len);
    }
  }
}
*/

int bsvector_copy(bsvector_t *to, const bsvector_t *from);

int bsvector_append(bsvector_t *to, const bsvector_t *from);

void bsvector_clear(bsvector_t *sv);

int bsvector_resize(bsvector_t* v, long int newsize);

/**
 * \ingroup strvector
 * \function igraph_strvector_permdelete
 * \brief Removes elements from a string vector (for internal use)
 */

void bsvector_permdelete(bsvector_t *v, const igraph_vector_t *index,
				long int nremove);

/**
 * \ingroup strvector
 * \function igraph_strvector_remove_negidx
 * \brief Removes elements from a string vector (for internal use)
 */

void bsvector_remove_negidx(bsvector_t *v, const igraph_vector_t *neg,
				   long int nremove);

int bsvector_index(const bsvector_t *v, bsvector_t *newv,
                   const igraph_vector_t *idx);

long int bsvector_size(const bsvector_t *sv);

bytestring_t* new_bytestring(int n);

void destroy_bytestring(bytestring_t* str);

char* bytestring_to_char(bytestring_t* from);

bytestring_t* char_to_bytestring(char* from);

igraph_strvector_t* bsvector_to_strvector(bsvector_t* from);

bsvector_t* strvector_to_bsvector(igraph_strvector_t* from);

__END_DECLS

#endif
