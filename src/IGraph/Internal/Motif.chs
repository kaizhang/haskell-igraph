{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Internal.Motif where

import qualified Foreign.Marshal.Utils as C2HSImp
import qualified Foreign.Ptr as C2HSImp
import Foreign
import Foreign.C.Types

{#import IGraph.Internal.Graph #}
{#import IGraph.Internal.Selector #}
{#import IGraph.Internal.Constants #}
{#import IGraph.Internal.Data #}

#include "igraph/igraph.h"

{#fun igraph_triad_census as ^ { `IGraphPtr'
                               , `VectorPtr' } -> `Int' #}
