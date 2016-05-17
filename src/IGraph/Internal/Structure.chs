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

{#fun igraph_induced_subgraph as ^ { `IGraphPtr'
                                   , id `Ptr (IGraphPtr)'
                                   , %`IGraphVsPtr'
                                   , `SubgraphImplementation' } -> `Int' #}

{#fun igraph_closeness as ^ { `IGraphPtr'
                            , `VectorPtr'
                            , %`IGraphVsPtr'
                            , `Neimode'
                            , `VectorPtr'
                            , `Bool' } -> `Int' #}

{#fun igraph_betweenness as ^ { `IGraphPtr'
                              , `VectorPtr'
                              , %`IGraphVsPtr'
                              , `Bool'
                              , `VectorPtr'
                              , `Bool' } -> `Int' #}

{#fun igraph_eigenvector_centrality as ^ { `IGraphPtr'
                                         , `VectorPtr'
                                         , id `Ptr CDouble'
                                         , `Bool'
                                         , `Bool'
                                         , `VectorPtr'
                                         , `ArpackOptPtr' } -> `Int' #}

{#fun igraph_pagerank as ^ { `IGraphPtr'
                           , `PagerankAlgo'
                           , `VectorPtr'
                           , id `Ptr CDouble'
                           , %`IGraphVsPtr'
                           , `Bool'
                           , `Double'
                           , `VectorPtr'
                           , id `Ptr ()' } -> `Int' #}

{#fun igraph_personalized_pagerank as ^ { `IGraphPtr'
                                        , `PagerankAlgo'
                                        , `VectorPtr'
                                        , id `Ptr CDouble'
                                        , %`IGraphVsPtr'
                                        , `Bool'
                                        , `Double'
                                        , `VectorPtr'
                                        , `VectorPtr'
                                        , id `Ptr ()' } -> `Int' #}
