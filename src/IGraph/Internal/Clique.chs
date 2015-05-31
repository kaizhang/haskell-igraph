{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Internal.Clique where

import Foreign
import Foreign.C.Types

{#import IGraph.Internal.Graph #}
{#import IGraph.Internal.Data #}

#include "cbits/haskelligraph.c"

{#fun igraph_cliques as ^ { `IGraphPtr', `VectorPPtr', `Int', `Int' } -> `Int' #}

{#fun igraph_maximal_cliques as ^ { `IGraphPtr', `VectorPPtr', `Int', `Int' } -> `Int' #}
