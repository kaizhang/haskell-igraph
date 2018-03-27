#include <igraph/igraph.h>
#include "haskell_attributes.h"

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

const igraph_attribute_table_t igraph_haskell_attribute_table={
  &igraph_haskell_attribute_init, &igraph_haskell_attribute_destroy,
  &igraph_haskell_attribute_copy, &igraph_haskell_attribute_add_vertices,
  &igraph_haskell_attribute_permute_vertices,
  &igraph_haskell_attribute_combine_vertices, &igraph_haskell_attribute_add_edges,
  &igraph_haskell_attribute_permute_edges,
  &igraph_haskell_attribute_combine_edges,
  &igraph_haskell_attribute_get_info,
  &igraph_haskell_attribute_has_attr, &igraph_haskell_attribute_gettype,
  &igraph_haskell_attribute_get_numeric_graph_attr,
  &igraph_haskell_attribute_get_string_graph_attr,
  &igraph_haskell_attribute_get_bool_graph_attr,
  &igraph_haskell_attribute_get_numeric_vertex_attr,
  &igraph_haskell_attribute_get_string_vertex_attr,
  &igraph_haskell_attribute_get_bool_vertex_attr,
  &igraph_haskell_attribute_get_numeric_edge_attr,
  &igraph_haskell_attribute_get_string_edge_attr,
  &igraph_haskell_attribute_get_bool_edge_attr
};

void haskelligraph_init()
{
  /* attach attribute table */
  igraph_i_set_attribute_table(&igraph_haskell_attribute_table);
}
