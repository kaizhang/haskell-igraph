{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Internal.Layout where

import qualified Foreign.Marshal.Utils as C2HSImp
import qualified Foreign.Ptr as C2HSImp

import Foreign
import Foreign.C.Types

{#import IGraph.Internal.Graph #}
{#import IGraph.Internal.Data #}

#include "igraph/igraph.h"

{#fun igraph_layout_kamada_kawai as ^ { `IGraphPtr'
, `MatrixPtr'
, `Int'
, `Double'
, `Double'
, `Double'
, `Double'
, `Bool'
, id `Ptr VectorPtr'
, id `Ptr VectorPtr'
, id `Ptr VectorPtr'
, id `Ptr VectorPtr'
} -> `Int' #}
