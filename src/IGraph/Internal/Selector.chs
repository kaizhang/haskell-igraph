{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Internal.Selector where

import Control.Monad
import Foreign
import Foreign.C.Types
import System.IO.Unsafe (unsafePerformIO)

{#import IGraph.Internal.Constants #}
{#import IGraph.Internal.Data #}

#include "cbits/haskelligraph.c"

{#pointer *igraph_vs_t as IGraphVsPtr foreign finalizer igraph_vs_destroy newtype #}

{#fun igraph_vs_new as ^ { } -> `IGraphVsPtr' #}

{#fun igraph_vs_all as ^ { `IGraphVsPtr' } -> `Int' #}

{#fun igraph_vs_adj as ^ { `IGraphVsPtr', `Int', `Neimode' } -> `Int' #}

{#fun igraph_vs_vector as ^ { `IGraphVsPtr', `VectorPtr' } -> `Int' #}
