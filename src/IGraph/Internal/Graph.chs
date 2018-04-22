{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Internal.Graph where

import Control.Monad
import Foreign
import Foreign.C.Types
import System.IO.Unsafe (unsafePerformIO)

import IGraph.Internal.C2HS
{#import IGraph.Internal.Initialization #}
{#import IGraph.Internal.Data #}
{#import IGraph.Internal.Constants #}

#include "haskell_igraph.h"

--------------------------------------------------------------------------------
-- Graph Constructors and Destructors
--------------------------------------------------------------------------------

{#pointer *igraph_t as IGraph foreign finalizer igraph_destroy newtype#}

allocaIGraph :: (Ptr IGraph -> IO a) -> IO a
allocaIGraph f = mallocBytes {# sizeof igraph_t #} >>= f

addIGraphFinalizer :: Ptr IGraph -> IO IGraph
addIGraphFinalizer ptr = do
    vec <- newForeignPtr igraph_destroy ptr
    return $ IGraph vec

{#fun igraph_empty as igraphNew'
    { allocaIGraph- `IGraph' addIGraphFinalizer*
    , `Int', `Bool'
    } -> `CInt' void- #}

{#fun igraph_copy as ^
    { allocaIGraph- `IGraph' addIGraphFinalizer*
    , `IGraph'
    } -> `CInt' void- #}

-- | Create a igraph object and attach a finalizer
igraphNew :: Int -> Bool -> HasInit -> IO IGraph
igraphNew n directed _ = igraphNew' n directed

--------------------------------------------------------------------------------
-- Basic Query Operations
--------------------------------------------------------------------------------

{#fun igraph_vcount as ^ { `IGraph' } -> `Int' #}

{#fun igraph_ecount as ^ { `IGraph' } -> `Int' #}

{#fun igraph_get_eid as ^
    { `IGraph'
    , alloca- `Int' peekIntConv*
    , `Int'
    , `Int'
    , `Bool'
    , `Bool'
    } -> `CInt' void-#}

{#fun igraph_edge as ^
    { `IGraph'
    , `Int'
    , alloca- `Int' peekIntConv*
    , alloca- `Int' peekIntConv*
    } -> `CInt' void-#}

-- Adding and Deleting Vertices and Edges

{# fun igraph_add_vertices as ^ { `IGraph', `Int', id `Ptr ()' } -> `()' #}

{# fun igraph_add_edge as ^ { `IGraph', `Int', `Int' } -> `()' #}

-- | The edges are given in a vector, the first two elements define the first
-- edge (the order is from , to for directed graphs). The vector should
-- contain even number of integer numbers between zero and the number of
-- vertices in the graph minus one (inclusive). If you also want to add
-- new vertices, call igraph_add_vertices() first.
{# fun igraph_add_edges as ^
    { `IGraph'     -- ^ The graph to which the edges will be added.
    , `Vector'     -- ^ The edges themselves.
    , id `Ptr ()'  -- ^ The attributes of the new edges.
    } -> `()' #}


-- generators

{#fun igraph_full as ^ { +, `Int', `Bool', `Bool' } -> `IGraph' #}

{#fun igraph_erdos_renyi_game as ^ { +, `ErdosRenyi', `Int', `Double', `Bool'
    , `Bool' } -> `IGraph' #}

{#fun igraph_degree_sequence_game as ^ { +, `Vector', `Vector'
    , `Degseq' } -> `IGraph' #}

{#fun igraph_rewire as ^ { `IGraph', `Int', `Rewiring' } -> `Int' #}


{#fun igraph_isoclass_create as ^ { +, `Int', `Int', `Bool' } -> `IGraph' #}
