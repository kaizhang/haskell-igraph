{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Internal.Initialization where

#include "haskell_igraph.h"

data HasInit

igraphInit :: IO HasInit
igraphInit = do haskelligraphInit
                return undefined

{#fun haskelligraph_init as ^ {} -> `()' #}
