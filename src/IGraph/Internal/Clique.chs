{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Internal.Clique where

import qualified Foreign.Marshal.Utils as C2HSImp
import qualified Foreign.Ptr as C2HSImp

import Foreign
import Foreign.C.Types

{#import IGraph.Internal.Graph #}
{#import IGraph.Internal.Data #}

#include "igraph/igraph.h"

{#fun igraph_cliques as ^ { `IGraph', `VectorPtr', `Int', `Int' } -> `Int' #}

{#fun igraph_maximal_cliques as ^ { `IGraph', `VectorPtr', `Int', `Int' } -> `Int' #}
