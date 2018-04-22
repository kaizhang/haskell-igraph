#include <igraph/igraph.h>
#include "haskell_attributes.h"

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
