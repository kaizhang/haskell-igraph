#include "IGraph/Internal/Structure.chs.h"
int __c2hs_wrapped__igraph_closeness(const igraph_t * graph,
                                     igraph_vector_t * res,
                                     const igraph_vs_t * vids,
                                     igraph_neimode_t mode,
                                     const igraph_vector_t * weights,
                                     igraph_bool_t normalized)
{
    return igraph_closeness(graph,
                            res,
                            *vids,
                            mode,
                            weights,
                            normalized);
}
int __c2hs_wrapped__igraph_betweenness(const igraph_t * graph,
                                       igraph_vector_t * res,
                                       const igraph_vs_t * vids,
                                       igraph_bool_t directed,
                                       const igraph_vector_t * weights,
                                       igraph_bool_t nobigint)
{
    return igraph_betweenness(graph,
                              res,
                              *vids,
                              directed,
                              weights,
                              nobigint);
}
