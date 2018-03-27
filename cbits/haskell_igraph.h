#ifndef HASKELL_IGRAPH
#define HASKELL_IGRAPH

#include <igraph/igraph.h>

igraph_integer_t igraph_get_eid_(igraph_t* graph, igraph_integer_t pfrom, igraph_integer_t pto,
           igraph_bool_t directed, igraph_bool_t error);

char** igraph_strvector_get_(igraph_strvector_t* s, long int i);

igraph_arpack_options_t* igraph_arpack_new();

void igraph_arpack_destroy(igraph_arpack_options_t* arpack);

void haskelligraph_init();

#endif
