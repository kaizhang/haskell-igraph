{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Internal.Layout where

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
