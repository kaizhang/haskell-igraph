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
#include "cbits/igraph.c"

{#fun igraph_closeness as ^ { `IGraphPtr'
                            , `VectorPtr'
                            , %`IGraphVsPtr'
                            , `IGraphNeimode'
                            , `VectorPtr'
                            , `Bool' } -> `Int' #}

{#fun igraph_betweenness as ^ { `IGraphPtr'
                              , id `Ptr VectorPtr'
                              , %`IGraphVsPtr'
                              , `Bool' 
                              , id `Ptr VectorPtr'
                              , `Bool' } -> `Int' #}

{#fun igraph_eigenvector_centrality as ^ { `IGraphPtr'
                                         , id `Ptr VectorPtr'
                                         , id `Ptr CDouble'
                                         , `Bool'
                                         , `Bool'
                                         , id `Ptr VectorPtr'
                                         , `ArpackOptPtr' } -> `Int' #}
