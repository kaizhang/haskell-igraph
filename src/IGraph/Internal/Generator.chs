{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Internal.Generator where

import Control.Monad
import Foreign
import Foreign.C.Types

{#import IGraph.Internal.Graph #}

#include "cbits/haskelligraph.c"

-- Deterministic Graph Generators

{#fun igraph_full_ as igraphFull { `Int', `Bool', `Bool' } -> `IGraphPtr' #}
