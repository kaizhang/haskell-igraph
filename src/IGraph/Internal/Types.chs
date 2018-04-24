{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Internal.Types where

#include "haskell_attributes.h"

{#pointer *igraph_t as IGraph foreign finalizer igraph_destroy newtype#}

{#pointer *igraph_vector_t as Vector foreign finalizer
    igraph_vector_destroy newtype#}

{#pointer *igraph_vector_ptr_t as VectorPtr foreign finalizer
    igraph_vector_ptr_destroy_all newtype#}

{#pointer *igraph_strvector_t as StrVector foreign finalizer
    igraph_strvector_destroy newtype#}

{#pointer *bsvector_t as BSVector foreign finalizer bsvector_destroy newtype#}

{#pointer *igraph_matrix_t as Matrix foreign finalizer
    igraph_matrix_destroy newtype#}
