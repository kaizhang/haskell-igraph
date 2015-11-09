#include "IGraph/Internal/Selector.chs.h"
igraph_vit_t * __c2hs_wrapped__igraph_vit_new(const igraph_t * graph,
                                              igraph_vs_t * vs)
{
    return igraph_vit_new(graph, *vs);
}
igraph_eit_t * __c2hs_wrapped__igraph_eit_new(const igraph_t * graph,
                                              igraph_es_t * es)
{
    return igraph_eit_new(graph, *es);
}
int __c2hs_wrapped__igraph_delete_vertices(igraph_t * graph,
                                           const igraph_vs_t * vertices)
{
    return igraph_delete_vertices(graph, *vertices);
}
int __c2hs_wrapped__igraph_delete_edges(igraph_t * graph,
                                        igraph_es_t * edges)
{
    return igraph_delete_edges(graph, *edges);
}
