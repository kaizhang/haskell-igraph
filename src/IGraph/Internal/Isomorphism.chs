{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Internal.Isomorphism where

import Foreign
import Foreign.C.Types

{#import IGraph.Internal.Graph #}
{#import IGraph.Internal.Data #}

#include "igraph/igraph.h"

{#fun igraph_get_subisomorphisms_vf2 as ^ { `IGraph', `IGraph',
    id `Ptr ()', id `Ptr ()', id `Ptr ()', id `Ptr ()', `VectorPtr',
    id `FunPtr (Ptr IGraph -> Ptr IGraph -> CInt -> CInt -> Ptr () -> IO CInt)',
    id `FunPtr (Ptr IGraph -> Ptr IGraph -> CInt -> CInt -> Ptr () -> IO CInt)',
    id `Ptr ()'} -> `Int' #}

{#fun igraph_isomorphic as ^ { `IGraph', `IGraph', id `Ptr CInt' } -> `Int' #}
