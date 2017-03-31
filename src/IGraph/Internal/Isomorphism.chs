{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Internal.Isomorphism where

import Foreign
import Foreign.C.Types

{#import IGraph.Internal.Graph #}
{#import IGraph.Internal.Data #}

#include "igraph/igraph.h"

{#fun igraph_get_subisomorphisms_vf2 as ^ { `IGraphPtr', `IGraphPtr',
    id `Ptr ()', id `Ptr ()', id `Ptr ()', id `Ptr ()', `VectorPPtr',
    id `FunPtr (Ptr IGraphPtr -> Ptr IGraphPtr -> CInt -> CInt -> Ptr () -> IO CInt)',
    id `FunPtr (Ptr IGraphPtr -> Ptr IGraphPtr -> CInt -> CInt -> Ptr () -> IO CInt)',
    id `Ptr ()'} -> `Int' #}

{#fun igraph_isomorphic as ^ { `IGraphPtr', `IGraphPtr', id `Ptr CInt' } -> `Int' #}
