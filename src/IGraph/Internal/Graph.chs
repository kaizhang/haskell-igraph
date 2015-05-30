{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Internal.Graph where

import Control.Monad
import Foreign
import Foreign.C.Types
import System.IO.Unsafe (unsafePerformIO)

{#import IGraph.Internal.Initialization #}
{#import IGraph.Internal.Data #}

#include "igraph/igraph.h"
#include "cbits/igraph.c"

{#pointer *igraph_t as IGraphPtr foreign finalizer igraph_destroy newtype#}

-- | create a igraph object and attach a finalizer
igraphNew :: Int -> Bool -> HasInit -> IO IGraphPtr
igraphNew n directed _ = igraphNew' n directed

-- Graph Constructors and Destructors

{#fun igraph_new as igraphNew' { `Int', `Bool' } -> `IGraphPtr' #}

-- Basic Query Operations

{#fun pure igraph_vcount as ^ { `IGraphPtr' } -> `Int' #}

{#fun pure igraph_ecount as ^ { `IGraphPtr' } -> `Int' #}

{#fun pure igraph_get_eid_ as igraphGetEid { `IGraphPtr', `Int', `Int', `Bool', `Bool' } -> `Int' #}

-- Adding and Deleting Vertices and Edges

{# fun igraph_add_vertices as ^ { `IGraphPtr', `Int', id `Ptr ()' } -> `()' #}

{# fun igraph_add_edge as ^ { `IGraphPtr', `Int', `Int' } -> `()' #}

{# fun igraph_add_edges as ^ { `IGraphPtr', `VectorPtr', id `Ptr ()' } -> `()' #}
