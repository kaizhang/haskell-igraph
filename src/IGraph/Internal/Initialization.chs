{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Internal.Initialization where

#include "igraph/igraph.h"
#include "cbits/igraph.c"

data HasInit

igraphInit :: IO HasInit
igraphInit = do haskelligraphInit
                return undefined

{#fun haskelligraph_init as ^ {} -> `()' #}
