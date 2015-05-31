{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Internal.Initialization where

#include "cbits/haskelligraph.c"

data HasInit

igraphInit :: IO HasInit
igraphInit = do haskelligraphInit
                return undefined

{#fun haskelligraph_init as ^ {} -> `()' #}
