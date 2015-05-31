{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Internal.Arpack where

import Control.Monad
import Foreign
import Foreign.C.Types

#include "cbits/haskelligraph.c"

{#pointer *igraph_arpack_options_t as ArpackOptPtr foreign finalizer igraph_arpack_destroy newtype#}

{#fun igraph_arpack_new as ^ { } -> `ArpackOptPtr' #}
