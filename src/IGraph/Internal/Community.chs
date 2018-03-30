{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Internal.Community where

import Foreign
import Foreign.C.Types

{#import IGraph.Internal.Arpack #}
{#import IGraph.Internal.Graph #}
{#import IGraph.Internal.Data #}
{#import IGraph.Internal.Constants #}

#include "igraph/igraph.h"

{#fun igraph_community_spinglass as ^
{ `IGraph'
, `Vector'
, id `Ptr CDouble'
, id `Ptr CDouble'
, `Vector'
, id `Ptr Vector'
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
{ `IGraph'
, `Vector'
, id `Ptr Matrix'
, `Vector'
, `Int'
, `ArpackOpt'
, id `Ptr CDouble'
, `Bool'
, id `Ptr Vector'
, id `Ptr VectorPtr'
, id `Ptr Vector'
, id `T'
, id `Ptr ()'
} -> `Int' #}

type T = FunPtr ( Ptr Vector
                -> CLong
                -> CDouble
                -> Ptr Vector
                -> FunPtr (Ptr CDouble -> Ptr CDouble -> CInt -> Ptr () -> IO CInt)
                -> Ptr ()
                -> Ptr ()
                -> IO CInt)
