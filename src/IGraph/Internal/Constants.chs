{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Internal.Constants where

import Foreign

#include "cbits/haskelligraph.c"

{#enum igraph_neimode_t as Neimode {underscoreToCase} deriving (Show, Eq) #}

{#enum igraph_edgeorder_type_t as EdgeOrderType {underscoreToCase} deriving (Show, Eq) #}

{#enum igraph_spincomm_update_t as SpincommUpdate {underscoreToCase} deriving (Show, Eq) #}

{#enum igraph_spinglass_implementation_t as SpinglassImplementation {underscoreToCase} deriving (Show, Eq) #}
