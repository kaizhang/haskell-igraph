{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Internal.Structure where

import Foreign
import Foreign.C.Types

{#import IGraph.Internal.Graph #}
{#import IGraph.Internal.Selector #}
{#import IGraph.Internal.Constants #}
{#import IGraph.Internal.Data #}
{#import IGraph.Internal.Arpack #}

#include "igraph/igraph.h"

{#fun igraph_induced_subgraph as ^ { `IGraph'
                                   , +160
                                   , %`IGraphVs'
                                   , `SubgraphImplementation' } -> `IGraph' #}

{#fun igraph_closeness as ^ { `IGraph'
                            , `Vector'
                            , %`IGraphVs'
                            , `Neimode'
                            , `Vector'
                            , `Bool' } -> `Int' #}

{#fun igraph_betweenness as ^ { `IGraph'
                              , `Vector'
                              , %`IGraphVs'
                              , `Bool'
                              , `Vector'
                              , `Bool' } -> `Int' #}

{#fun igraph_eigenvector_centrality as ^ { `IGraph'
                                         , `Vector'
                                         , id `Ptr CDouble'
                                         , `Bool'
                                         , `Bool'
                                         , `Vector'
                                         , `ArpackOpt' } -> `Int' #}

{#fun igraph_pagerank as ^ { `IGraph'
                           , `PagerankAlgo'
                           , `Vector'
                           , id `Ptr CDouble'
                           , %`IGraphVs'
                           , `Bool'
                           , `Double'
                           , `Vector'
                           , id `Ptr ()' } -> `Int' #}

{#fun igraph_personalized_pagerank as ^ { `IGraph'
                                        , `PagerankAlgo'
                                        , `Vector'
                                        , id `Ptr CDouble'
                                        , %`IGraphVs'
                                        , `Bool'
                                        , `Double'
                                        , `Vector'
                                        , `Vector'
                                        , id `Ptr ()' } -> `Int' #}
