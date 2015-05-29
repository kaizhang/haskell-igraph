{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Internal.Generator where

import Foreign
import Foreign.C.Types

{#import IGraph.Internal.Graph #}

#include "igraph/igraph.h"
#include "cbits/igraph.c"

-- Deterministic Graph Generators

{#fun igraph_full_ as igraphFull { `Int', `Bool', `Bool' } -> `IGraphPtr' #}
