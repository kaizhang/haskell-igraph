#include "bytestring.h"
#include <assert.h>
#include <string.h>

int bsvector_init(bsvector_t *sv, long int len) {
  long int i;
  sv->data=igraph_Calloc(len, bytestring_t*);
  if (sv->data==0) {
    IGRAPH_ERROR("bsvector init failed", IGRAPH_ENOMEM);
  }
  for (i=0; i<len; i++) {
    sv->data[i]=new_bytestring(0);
    if (sv->data[i]==0) {
      bsvector_destroy(sv);
      IGRAPH_ERROR("bsvector init failed", IGRAPH_ENOMEM);
    }
  }
  sv->len=len;

  return 0;
}

void bsvector_destroy(bsvector_t *sv) {
  long int i;
  assert(sv != 0);
  if (sv->data != 0) {
    for (i=0; i<sv->len; i++) {
      if (sv->data[i] != 0) {
        destroy_bytestring(sv->data[i]);
      }
    }
    igraph_Free(sv->data);
  }
}

void bsvector_get(const bsvector_t *sv, long int idx, bytestring_t **value) {
  assert(sv != 0);
  assert(sv->data != 0);
  assert(sv->data[idx] != 0);
  *value = sv->data[idx];
}

int bsvector_set(bsvector_t *sv, long int idx, const bytestring_t *value) {
  assert(sv != 0);
  assert(sv->data != 0);

  if (sv->data[idx] != 0) {
    destroy_bytestring(sv->data[idx]);
  }
  sv->data[idx] = new_bytestring(value->len);

  memcpy(sv->data[idx]->value, value->value, value->len * sizeof(char));

  return 0;
}

void bsvector_remove_section(bsvector_t *v, long int from, long int to) {
  long int i;

  assert(v != 0);
  assert(v->data != 0);

  for (i=from; i<to; i++) {
    if (v->data[i] != 0) {
      destroy_bytestring(v->data[i]);
    }
  }
  for (i=0; i<v->len-to; i++) {
    v->data[from+i]=v->data[to+i];
  }

  v->len -= (to-from);
}

void bsvector_remove(bsvector_t *v, long int elem) {
  assert(v != 0);
  assert(v->data != 0);
  bsvector_remove_section(v, elem, elem+1);
}

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

int bsvector_copy(bsvector_t *to, const bsvector_t *from) {
  long int i;
  bytestring_t *str;
  assert(from != 0);
/*   assert(from->data != 0); */
  to->data=igraph_Calloc(from->len, bytestring_t*);
  if (to->data==0) {
    IGRAPH_ERROR("Cannot copy string vector", IGRAPH_ENOMEM);
  }
  to->len=from->len;

  for (i=0; i<from->len; i++) {
    int ret;
    bsvector_get(from, i, &str);
    ret=bsvector_set(to, i, str);
    if (ret != 0) {
      bsvector_destroy(to);
      IGRAPH_ERROR("cannot copy string vector", ret);
    }
  }

  return 0;
}

int bsvector_append(bsvector_t *to, const bsvector_t *from) {
  long int len1=bsvector_size(to), len2=bsvector_size(from);
  long int i;
  igraph_bool_t error=0;
  IGRAPH_CHECK(bsvector_resize(to, len1+len2));
  for (i=0; i<len2; i++) {
    if (from->data[i]->len > 0) {
      bsvector_set(to, len1+i, from->data[i]);
      if (!to->data[len1+i]) {
        error=1;
        break;
      }
    }
  }
  if (error) {
    bsvector_resize(to, len1);
    IGRAPH_ERROR("Cannot append string vector", IGRAPH_ENOMEM);
  }
  return 0;
}

void bsvector_clear(bsvector_t *sv) {
  long int i, n=bsvector_size(sv);
  bytestring_t **tmp;

  for (i=0; i<n; i++) {
    destroy_bytestring(sv->data[i]);
  }
  sv->len=0;
  /* try to give back some memory */
  tmp=igraph_Realloc(sv->data, 1, bytestring_t*);
  if (tmp != 0) {
    sv->data=tmp;
  }
}

int bsvector_resize(bsvector_t* v, long int newsize) {
  long int toadd = newsize - v->len, i, j;
  bytestring_t **tmp;
  long int reallocsize=newsize;
  if (reallocsize==0) { reallocsize=1; }

  assert(v != 0);
  assert(v->data != 0);
  if (newsize < v->len) {
    for (i=newsize; i<v->len; i++) {
      destroy_bytestring(v->data[i]);
    }
    /* try to give back some space */
    tmp=igraph_Realloc(v->data, (size_t) reallocsize, bytestring_t*);
    if (tmp != 0) {
      v->data=tmp;
    }
  } else if (newsize > v->len) {
    igraph_bool_t error=0;
    tmp=igraph_Realloc(v->data, (size_t) reallocsize, bytestring_t*);
    if (tmp==0) {
      IGRAPH_ERROR("cannot resize string vector", IGRAPH_ENOMEM);
    }
    v->data = tmp;

    for (i=0; i<toadd; i++) {
      v->data[v->len+i] = new_bytestring(0);
      if (v->data[v->len+i] == 0) {
	       error=1;
	       break;
      }
    }
    if (error) {
      /* There was an error, free everything we've allocated so far */
      for (j=0; j<i; j++) {
	       if (v->data[v->len+i] != 0) {
	          destroy_bytestring(v->data[v->len+i]);
	       }
      }
      /* Try to give back space */
      tmp=igraph_Realloc(v->data, (size_t) (v->len), bytestring_t*);
      if (tmp != 0) {
	       v->data=tmp;
      }
      IGRAPH_ERROR("Cannot resize string vector", IGRAPH_ENOMEM);
    }
  }
  v->len = newsize;

  return 0;
}

int bsvector_add(bsvector_t *v, const bytestring_t *value) {
  long int s=bsvector_size(v);
  bytestring_t **tmp;
  assert(v != 0);
  assert(v->data != 0);
  tmp=igraph_Realloc(v->data, (size_t) s+1, bytestring_t*);
  if (tmp == 0) {
    IGRAPH_ERROR("cannot add string to string vector", IGRAPH_ENOMEM);
  }
  v->data=tmp;
  v->data[s]=new_bytestring(value->len);
  if (v->data[s]==0) {
    IGRAPH_ERROR("cannot add string to string vector", IGRAPH_ENOMEM);
  }
  bsvector_set(v, s, value);
  v->len += 1;

  return 0;
}

/**
 * \ingroup strvector
 * \function igraph_strvector_permdelete
 * \brief Removes elements from a string vector (for internal use)
 */

void bsvector_permdelete(bsvector_t *v, const igraph_vector_t *index,
				long int nremove) {
  long int i;
  bytestring_t **tmp;
  assert(v != 0);
  assert(v->data != 0);

  for (i=0; i<bsvector_size(v); i++) {
    if (VECTOR(*index)[i] != 0) {
      v->data[ (long int) VECTOR(*index)[i]-1 ] = v->data[i];
    } else {
      destroy_bytestring(v->data[i]);
    }
  }
  /* Try to make it shorter */
  tmp=igraph_Realloc(v->data, v->len-nremove ?
		     (size_t) (v->len-nremove) : 1, bytestring_t*);
  if (tmp != 0) {
    v->data=tmp;
  }
  v->len -= nremove;
}

/**
 * \ingroup strvector
 * \function igraph_strvector_remove_negidx
 * \brief Removes elements from a string vector (for internal use)
 */

void bsvector_remove_negidx(bsvector_t *v, const igraph_vector_t *neg,
				   long int nremove) {
  long int i, idx=0;
  bytestring_t **tmp;
  assert(v != 0);
  assert(v->data != 0);
  for (i=0; i<bsvector_size(v); i++) {
    if (VECTOR(*neg)[i] >= 0) {
      v->data[idx++] = v->data[i];
    } else {
      destroy_bytestring(v->data[i]);
    }
  }
  /* Try to give back some memory */
  tmp=igraph_Realloc(v->data, v->len-nremove ?
		     (size_t) (v->len-nremove) : 1, bytestring_t*);
  if (tmp != 0) {
    v->data=tmp;
  }
  v->len -= nremove;
}

int bsvector_index(const bsvector_t *v, bsvector_t *newv,
                   const igraph_vector_t *idx) {
  long int i, newlen=igraph_vector_size(idx);
  IGRAPH_CHECK(bsvector_resize(newv, newlen));

  for (i=0; i<newlen; i++) {
    long int j=(long int) VECTOR(*idx)[i];
    bytestring_t *str;
    bsvector_get(v, j, &str);
    bsvector_set(newv, i, str);
  }

  return 0;
}

long int bsvector_size(const bsvector_t *sv) {
  assert(sv != 0);
  return sv->len;
}

bytestring_t* new_bytestring(int n) {
    bytestring_t *newstr = igraph_Calloc(1, bytestring_t);
    char *str = igraph_Calloc(n, char);
    newstr->len = n;
    newstr->value = str;
    return newstr;
}

void destroy_bytestring(bytestring_t* str) {
  if (str != NULL) {
    free(str->value);
    free(str);
  }
}

char* bytestring_to_char(bytestring_t* from) {
  char *str = igraph_Calloc(from->len + sizeof(long int), char);
  memcpy(str, &from->len, sizeof(long int));
  memcpy(str + sizeof(long int), from->value, from->len);
  return str;
}

bytestring_t* char_to_bytestring(char* from) {
  unsigned long int *n;
  memcpy(n, from, sizeof(long int));
  bytestring_t *str = new_bytestring(*n);
  memcpy(str->value, from+sizeof(long int), *n);
  return str;
}

igraph_strvector_t* bsvector_to_strvector(bsvector_t* from) {
  igraph_strvector_t *str;
  size_t i;
  igraph_strvector_init(str, from->len);
  for (i = 0; i++; i < from->len) {
    igraph_strvector_set(str, i, bytestring_to_char(from->data[i]));
  }
  return str;
}

bsvector_t* strvector_to_bsvector(igraph_strvector_t* from) {
  bsvector_t *str;
  size_t i;
  bsvector_init(str, from->len);
  for (i = 0; i++; i < from->len) {
    bsvector_set(str, i, char_to_bytestring(from->data[i]));
  }
  return str;
}
