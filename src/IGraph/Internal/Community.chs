{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Internal.Community where

import Foreign
import Foreign.C.Types

{#import IGraph.Internal.Arpack #}
{#import IGraph.Internal.Graph #}
{#import IGraph.Internal.Data #}
{#import IGraph.Internal.Constants #}

#include "haskelligraph.h"

{#fun igraph_community_spinglass as ^
{ `IGraphPtr'
, `VectorPtr'
, id `Ptr CDouble'
, id `Ptr CDouble'
, `VectorPtr'
, id `Ptr VectorPtr'
, `Int'
, `Bool'
, `Double'
, `Double'
, `Double'
, `SpincommUpdate'
, `Double'
, `SpinglassImplementation'
, `Double'
} -> `Int' #}

{#fun igraph_community_leading_eigenvector as ^
{ `IGraphPtr'
, `VectorPtr'
, id `Ptr MatrixPtr'
, `VectorPtr'
, `Int'
, `ArpackOptPtr'
, id `Ptr CDouble'
, `Bool'
, id `Ptr VectorPtr'
, id `Ptr VectorPPtr'
, id `Ptr VectorPtr'
, id `T'
, id `Ptr ()'
} -> `Int' #}

type T = FunPtr ( Ptr VectorPtr
                -> CLong
                -> CDouble
                -> Ptr VectorPtr
                -> FunPtr (Ptr CDouble -> Ptr CDouble -> CInt -> Ptr () -> IO CInt)
                -> Ptr ()
                -> Ptr ()
                -> IO CInt)
