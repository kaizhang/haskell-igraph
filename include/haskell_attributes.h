#ifndef HASKELL_IGRAPH_ATTRIBUTE
#define HASKELL_IGRAPH_ATTRIBUTE

#include "igraph.h"
#include "bytestring.h"

#include <string.h>

igraph_bool_t igraph_haskell_attribute_find(const igraph_vector_ptr_t *ptrvec,
				       const char *name, long int *idx);

typedef struct igraph_haskell_attributes_t {
  igraph_vector_ptr_t gal;
  igraph_vector_ptr_t val;
  igraph_vector_ptr_t eal;
} igraph_haskell_attributes_t;

int igraph_haskell_attributes_copy_attribute_record(igraph_attribute_record_t **newrec,
					       const igraph_attribute_record_t *rec);


int igraph_haskell_attribute_init(igraph_t *graph, igraph_vector_ptr_t *attr);

void igraph_haskell_attribute_destroy(igraph_t *graph);

void igraph_haskell_attribute_copy_free(igraph_haskell_attributes_t *attr);

int igraph_haskell_attribute_copy(igraph_t *to, const igraph_t *from,
			     igraph_bool_t ga, igraph_bool_t va, igraph_bool_t ea);

int igraph_haskell_attribute_add_vertices(igraph_t *graph, long int nv,
				     igraph_vector_ptr_t *nattr);

void igraph_haskell_attribute_permute_free(igraph_vector_ptr_t *v);

int igraph_haskell_attribute_permute_vertices(const igraph_t *graph,
					 igraph_t *newgraph,
					 const igraph_vector_t *idx);

int igraph_haskell_attribute_combine_vertices(const igraph_t *graph,
			 igraph_t *newgraph,
			 const igraph_vector_ptr_t *merges,
			 const igraph_attribute_combination_t *comb);

int igraph_haskell_attribute_add_edges(igraph_t *graph, const igraph_vector_t *edges,
				 igraph_vector_ptr_t *nattr);

int igraph_haskell_attribute_permute_edges(const igraph_t *graph,
				      igraph_t *newgraph,
				      const igraph_vector_t *idx);

int igraph_haskell_attribute_combine_edges(const igraph_t *graph,
			 igraph_t *newgraph,
			 const igraph_vector_ptr_t *merges,
			 const igraph_attribute_combination_t *comb);

int igraph_haskell_attribute_get_info(const igraph_t *graph,
				 igraph_strvector_t *gnames,
				 igraph_vector_t *gtypes,
				 igraph_strvector_t *vnames,
				 igraph_vector_t *vtypes,
				 igraph_strvector_t *enames,
				 igraph_vector_t *etypes);

igraph_bool_t igraph_haskell_attribute_has_attr(const igraph_t *graph,
					 igraph_attribute_elemtype_t type,
					 const char *name);

int igraph_haskell_attribute_gettype(const igraph_t *graph,
			      igraph_attribute_type_t *type,
			      igraph_attribute_elemtype_t elemtype,
			      const char *name);

int igraph_haskell_attribute_get_numeric_graph_attr(const igraph_t *graph,
					      const char *name,
					      igraph_vector_t *value);

int igraph_haskell_attribute_get_bool_graph_attr(const igraph_t *graph,
					    const char *name,
					    igraph_vector_bool_t *value);

int igraph_haskell_attribute_get_string_graph_attr(const igraph_t *graph,
					     const char *name,
					     igraph_strvector_t *value_);

int igraph_haskell_attribute_get_numeric_vertex_attr(const igraph_t *graph,
					      const char *name,
					      igraph_vs_t vs,
					      igraph_vector_t *value);

int igraph_haskell_attribute_get_bool_vertex_attr(const igraph_t *graph,
					     const char *name,
					     igraph_vs_t vs,
					     igraph_vector_bool_t *value);

int igraph_haskell_attribute_get_string_vertex_attr(const igraph_t *graph,
					     const char *name,
					     igraph_vs_t vs,
					     igraph_strvector_t *value_);

int igraph_haskell_attribute_get_numeric_edge_attr(const igraph_t *graph,
					    const char *name,
					    igraph_es_t es,
					    igraph_vector_t *value);

int igraph_haskell_attribute_get_string_edge_attr(const igraph_t *graph,
					   const char *name,
					   igraph_es_t es,
					   igraph_strvector_t *value_);

int igraph_haskell_attribute_get_bool_edge_attr(const igraph_t *graph,
					   const char *name,
					   igraph_es_t es,
					   igraph_vector_bool_t *value);

igraph_real_t igraph_haskell_attribute_GAN(const igraph_t *graph, const char *name);

igraph_bool_t igraph_haskell_attribute_GAB(const igraph_t *graph, const char *name);

const bytestring_t* igraph_haskell_attribute_GAS(const igraph_t *graph, const char *name);

igraph_real_t igraph_haskell_attribute_VAN(const igraph_t *graph, const char *name,
				      igraph_integer_t vid);

igraph_bool_t igraph_haskell_attribute_VAB(const igraph_t *graph, const char *name,
				    igraph_integer_t vid);

const bytestring_t* igraph_haskell_attribute_VAS(const igraph_t *graph, const char *name,
				    igraph_integer_t vid);

igraph_real_t igraph_haskell_attribute_EAN(const igraph_t *graph, const char *name,
				      igraph_integer_t eid);

igraph_bool_t igraph_haskell_attribute_EAB(const igraph_t *graph, const char *name,
				    igraph_integer_t eid);

const bytestring_t* igraph_haskell_attribute_EAS(const igraph_t *graph, const char *name,
				    igraph_integer_t eid);

int igraph_haskell_attribute_VANV(const igraph_t *graph, const char *name,
			   igraph_vs_t vids, igraph_vector_t *result);

int igraph_haskell_attribute_VABV(const igraph_t *graph, const char *name,
			   igraph_vs_t vids, igraph_vector_bool_t *result);

int igraph_haskell_attribute_EANV(const igraph_t *graph, const char *name,
			   igraph_es_t eids, igraph_vector_t *result);

int igraph_haskell_attribute_EABV(const igraph_t *graph, const char *name,
			   igraph_es_t eids, igraph_vector_bool_t *result);

int igraph_haskell_attribute_VASV(const igraph_t *graph, const char *name,
			   igraph_vs_t vids, igraph_strvector_t *result);

int igraph_haskell_attribute_EASV(const igraph_t *graph, const char *name,
			   igraph_es_t eids, igraph_strvector_t *result);

int igraph_haskell_attribute_list(const igraph_t *graph,
			   igraph_strvector_t *gnames, igraph_vector_t *gtypes,
			   igraph_strvector_t *vnames, igraph_vector_t *vtypes,
			   igraph_strvector_t *enames, igraph_vector_t *etypes);

int igraph_haskell_attribute_GAN_set(igraph_t *graph, const char *name,
			      igraph_real_t value);

int igraph_haskell_attribute_GAB_set(igraph_t *graph, const char *name,
			      igraph_bool_t value);

int igraph_haskell_attribute_GAS_set(igraph_t *graph, const char *name,
			      const bytestring_t *value);

int igraph_haskell_attribute_VAN_set(igraph_t *graph, const char *name,
			      igraph_integer_t vid, igraph_real_t value);

int igraph_haskell_attribute_VAB_set(igraph_t *graph, const char *name,
			      igraph_integer_t vid, igraph_bool_t value);

int igraph_haskell_attribute_VAS_set(igraph_t *graph, const char *name,
			      igraph_integer_t vid, const bytestring_t *value);

int igraph_haskell_attribute_EAN_set(igraph_t *graph, const char *name,
			      igraph_integer_t eid, igraph_real_t value);

int igraph_haskell_attribute_EAB_set(igraph_t *graph, const char *name,
			      igraph_integer_t eid, igraph_bool_t value);

int igraph_haskell_attribute_EAS_set(igraph_t *graph, const char *name,
			      igraph_integer_t eid, const bytestring_t *value);

int igraph_haskell_attribute_VAN_setv(igraph_t *graph, const char *name,
			       const igraph_vector_t *v);

int igraph_haskell_attribute_VAB_setv(igraph_t *graph, const char *name,
			       const igraph_vector_bool_t *v);

int igraph_haskell_attribute_VAS_setv(igraph_t *graph, const char *name,
			       const bsvector_t *sv);

int igraph_haskell_attribute_EAN_setv(igraph_t *graph, const char *name,
			       const igraph_vector_t *v);

int igraph_haskell_attribute_EAB_setv(igraph_t *graph, const char *name,
			       const igraph_vector_bool_t *v);

int igraph_haskell_attribute_EAS_setv(igraph_t *graph, const char *name,
			       const bsvector_t *sv);

void igraph_haskell_attribute_free_rec(igraph_attribute_record_t *rec);

void igraph_haskell_attribute_remove_g(igraph_t *graph, const char *name);

void igraph_haskell_attribute_remove_v(igraph_t *graph, const char *name);

void igraph_haskell_attribute_remove_e(igraph_t *graph, const char *name);

void igraph_haskell_attribute_remove_all(igraph_t *graph, igraph_bool_t g,
				  igraph_bool_t v, igraph_bool_t e);
#endif
