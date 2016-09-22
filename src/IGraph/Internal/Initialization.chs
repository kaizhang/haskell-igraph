{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Internal.Initialization where

#include "haskelligraph.h"

data HasInit

igraphInit :: IO HasInit
igraphInit = do haskelligraphInit
                return undefined

{#fun haskelligraph_init as ^ {} -> `()' #}
