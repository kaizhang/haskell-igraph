{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Internal.Graph where

import Control.Monad
import Foreign
import Foreign.C.Types
import System.IO.Unsafe (unsafePerformIO)

{#import IGraph.Internal.Initialization #}
{#import IGraph.Internal.Data #}
{#import IGraph.Internal.Constants #}

#include "haskelligraph.h"

{#pointer *igraph_t as IGraphPtr foreign finalizer igraph_destroy newtype#}

-- | create a igraph object and attach a finalizer
igraphNew :: Int -> Bool -> HasInit -> IO IGraphPtr
igraphNew n directed _ = igraphNew' n directed

-- Graph Constructors and Destructors

{#fun igraph_empty as igraphNew' { +, `Int', `Bool' } -> `IGraphPtr' #}

{#fun igraph_copy as ^ { +, `IGraphPtr' } -> `IGraphPtr' #}

-- Basic Query Operations

{#fun pure igraph_vcount as ^ { `IGraphPtr' } -> `Int' #}

{#fun pure igraph_ecount as ^ { `IGraphPtr' } -> `Int' #}

{#fun pure igraph_get_eid_ as igraphGetEid { `IGraphPtr', `Int', `Int', `Bool', `Bool' } -> `Int' #}

{#fun igraph_edge as igraphEdge' { `IGraphPtr', `Int', id `Ptr CInt', id `Ptr CInt' } -> `Int' #}
igraphEdge :: IGraphPtr -> Int -> IO (Int, Int)
igraphEdge g i = alloca $ \fr -> alloca $ \to -> do
    igraphEdge' g i fr to
    fr' <- peek fr
    to' <- peek to
    return (fromIntegral fr', fromIntegral to')

-- Adding and Deleting Vertices and Edges

{# fun igraph_add_vertices as ^ { `IGraphPtr', `Int', id `Ptr ()' } -> `()' #}

{# fun igraph_add_edge as ^ { `IGraphPtr', `Int', `Int' } -> `()' #}

{# fun igraph_add_edges as ^ { `IGraphPtr', `VectorPtr', id `Ptr ()' } -> `()' #}


-- generators

{#fun igraph_full as ^ { +, `Int', `Bool', `Bool' } -> `IGraphPtr' #}

{#fun igraph_erdos_renyi_game as ^ {+, `ErdosRenyi', `Int', `Double', `Bool', `Bool'} -> `IGraphPtr' #}
