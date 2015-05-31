{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Internal.Community where

import Foreign
import Foreign.C.Types

{#import IGraph.Internal.Arpack #}
{#import IGraph.Internal.Graph #}
{#import IGraph.Internal.Data #}

#include "cbits/haskelligraph.c"

{#fun igraph_community_leading_eigenvector as ^ { `IGraphPtr'
                                                , id `Ptr VectorPtr'
                                                , id `Ptr MatrixPtr'
                                                , id `Ptr VectorPtr'
                                                , `Int'
                                                , id `Ptr ArpackOptPtr'
                                                , id `Ptr CDouble'
                                                , `Bool'
                                                , id `Ptr VectorPtr'
                                                , `VectorPPtr'
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
