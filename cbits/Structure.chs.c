#include "IGraph/Internal/Structure.chs.h"
int __c2hs_wrapped__igraph_pagerank(const igraph_t * graph,
                                    igraph_pagerank_algo_t algo,
                                    igraph_vector_t * vector,
                                    igraph_real_t * value,
                                    const igraph_vs_t * vids,
                                    igraph_bool_t directed,
                                    igraph_real_t damping,
                                    const igraph_vector_t * weights,
                                    void * options)
{
    return igraph_pagerank(graph,
                           algo,
                           vector,
                           value,
                           *vids,
                           directed,
                           damping,
                           weights,
                           options);
}
int __c2hs_wrapped__igraph_induced_subgraph(const igraph_t * graph,
                                            igraph_t * res,
                                            const igraph_vs_t * vids,
                                            igraph_subgraph_implementation_t impl)
{
    return igraph_induced_subgraph(graph, res, *vids, impl);
}
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
