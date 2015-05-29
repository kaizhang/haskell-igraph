#include <igraph/igraph.h>

igraph_vector_t* igraph_vector_new(long int size)
{
  igraph_vector_t* vector = (igraph_vector_t*) malloc (sizeof (igraph_vector_t));
  igraph_vector_init(vector, size);
  return vector;
}

igraph_vector_ptr_t* igraph_vector_ptr_new(long int size)
{
  igraph_vector_ptr_t* vptr = (igraph_vector_ptr_t*) malloc (sizeof (igraph_vector_ptr_t));
  igraph_vector_ptr_init(vptr, size);
  return vptr;
}

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

igraph_matrix_t* igraph_matrix_new(long int nrow, long int ncol)
{
  igraph_matrix_t* matrix = (igraph_matrix_t*) malloc (sizeof (igraph_matrix_t));
  igraph_matrix_init(matrix, nrow, ncol);
  return matrix;
}

igraph_t* igraph_new(igraph_integer_t n, igraph_bool_t directed)
{
  igraph_t* graph = (igraph_t*) malloc (sizeof (igraph_t));
  igraph_empty(graph, n, directed);
  return graph;
}

igraph_t* igraph_full_(igraph_integer_t n, igraph_bool_t directed, igraph_bool_t loops)
{
  igraph_t* graph = (igraph_t*) malloc (sizeof (igraph_t));
  igraph_full(graph, n, directed, loops);
  return graph;
}

void haskelligraph_init()
{
  /* attach attribute table */
  igraph_i_set_attribute_table(&igraph_cattribute_table);
}
