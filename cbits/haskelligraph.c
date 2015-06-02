#ifndef HASKELL_IGRAPH
#define HASKELL_IGRAPH

#include <igraph/igraph.h>

igraph_strvector_t* igraph_strvector_new(long int size)
{
  igraph_strvector_t* vector = (igraph_strvector_t*) malloc (sizeof (igraph_strvector_t));
  igraph_strvector_init(vector, size);
  return vector;
}

char** igraph_strvector_get_(igraph_strvector_t* s, long int i)
{
  char** x = (char**) malloc (sizeof(char*));
  igraph_strvector_get(s, i, x);
  return x;
}

igraph_integer_t igraph_get_eid_(igraph_t* graph, igraph_integer_t pfrom, igraph_integer_t pto,
           igraph_bool_t directed, igraph_bool_t error)
{
  igraph_integer_t eid;
  igraph_get_eid(graph, &eid, pfrom, pto, directed, error);
  return eid;
}

void haskelligraph_init()
{
  /* attach attribute table */
  igraph_i_set_attribute_table(&igraph_cattribute_table);
}


#endif
