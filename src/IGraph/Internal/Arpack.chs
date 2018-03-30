{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Internal.Arpack where

import Control.Monad
import Foreign
import Foreign.C.Types

#include "haskell_igraph.h"

{#pointer *igraph_arpack_options_t as ArpackOpt foreign newtype#}

{#fun igraph_arpack_options_init as igraphArpackNew
    { + } -> `ArpackOpt' #}
