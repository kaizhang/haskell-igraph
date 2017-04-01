{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Internal.Constants where

import Foreign

#include "igraph/igraph.h"

{#enum igraph_neimode_t as Neimode {underscoreToCase}
    deriving (Show, Eq) #}

{#enum igraph_edgeorder_type_t as EdgeOrderType {underscoreToCase}
    deriving (Show, Eq) #}

{#enum igraph_spincomm_update_t as SpincommUpdate {underscoreToCase}
    deriving (Show, Eq) #}

{#enum igraph_spinglass_implementation_t as SpinglassImplementation {underscoreToCase}
    deriving (Show, Eq) #}

{#enum igraph_attribute_elemtype_t as AttributeElemtype {underscoreToCase}
    deriving (Show, Eq) #}

{#enum igraph_subgraph_implementation_t as SubgraphImplementation {underscoreToCase}
    deriving (Show, Read, Eq) #}

{#enum igraph_pagerank_algo_t as PagerankAlgo {underscoreToCase}
    deriving (Show, Read, Eq) #}

{#enum igraph_erdos_renyi_t as ErdosRenyi {underscoreToCase}
    deriving (Show, Read, Eq) #}

{#enum igraph_rewiring_t as Rewiring {underscoreToCase}
    deriving (Show, Read, Eq) #}

{#enum igraph_degseq_t as Degseq {underscoreToCase}
    deriving (Show, Read, Eq) #}
