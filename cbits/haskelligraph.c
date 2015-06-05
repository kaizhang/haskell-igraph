#ifndef HASKELL_IGRAPH
#define HASKELL_IGRAPH

#include <igraph/igraph.h>

igraph_integer_t igraph_get_eid_(igraph_t* graph, igraph_integer_t pfrom, igraph_integer_t pto,
           igraph_bool_t directed, igraph_bool_t error)
{
  igraph_integer_t eid;
  igraph_get_eid(graph, &eid, pfrom, pto, directed, error);
  return eid;
}

char** igraph_strvector_get_(igraph_strvector_t* s, long int i)
{
  char** x = (char**) malloc (sizeof(char*));
  igraph_strvector_get(s, i, x);
  return x;
}

igraph_arpack_options_t* igraph_arpack_new()
{
  igraph_arpack_options_t *arpack = (igraph_arpack_options_t*) malloc(sizeof(igraph_arpack_options_t));
  igraph_arpack_options_init(arpack);
  return arpack;
}

void igraph_arpack_destroy(igraph_arpack_options_t* arpack)
{
  if (arpack)
    free(arpack);
  arpack = NULL;
}

void haskelligraph_init()
{
  /* attach attribute table */
  igraph_i_set_attribute_table(&igraph_cattribute_table);
}


#endif
