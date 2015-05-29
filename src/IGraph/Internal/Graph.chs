{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Internal.Graph where

import Foreign
import Foreign.C.Types

{# import IGraph.Internal.Initialization #}
{# import IGraph.Internal.Data #}

#include "igraph/igraph.h"
#include "cbits/igraph.c"

data IGraph
{#pointer *igraph_t as IGraphPtr -> IGraph #}

-- Graph Constructors and Destructors

igraphNew :: Int -> Bool -> HasInit -> IO IGraphPtr
igraphNew n directed _ = igraphNew' n directed

{#fun igraph_new as igraphNew' { `Int', `Bool' } -> `IGraphPtr' #}

{#fun igraph_destroy as ^ { `IGraphPtr' } -> `()' #}

-- Basic Query Operations

{#fun pure igraph_vcount as ^ { `IGraphPtr' } -> `Int' #}

{#fun pure igraph_ecount as ^ { `IGraphPtr' } -> `Int' #}

-- Adding and Deleting Vertices and Edges

{# fun igraph_add_edge as ^ { `IGraphPtr', `Int', `Int' } -> `()' #}

{# fun igraph_add_edges as ^ { `IGraphPtr', `VectorPtr', id `Ptr ()' } -> `()' #}
