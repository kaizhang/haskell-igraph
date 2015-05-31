{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Internal.Constants where

import Foreign

#include "igraph/igraph.h"
#include "cbits/igraph.c"

{#enum igraph_neimode_t as IGraphNeimode {underscoreToCase} deriving (Show, Eq) #}
