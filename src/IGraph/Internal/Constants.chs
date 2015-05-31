{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Internal.Constants where

import Foreign

#include "cbits/haskelligraph.c"

{#enum igraph_neimode_t as Neimode {underscoreToCase} deriving (Show, Eq) #}
