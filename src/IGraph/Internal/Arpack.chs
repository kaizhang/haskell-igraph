{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Internal.Arpack where

import Control.Monad
import Foreign
import Foreign.C.Types

#include "cbits/haskelligraph.c"

{#pointer *igraph_arpack_options_t as ArpackOptPtr foreign finalizer igraph_arpack_destroy newtype#}

#c
igraph_arpack_options_t* igraph_arpack_new()
{
  igraph_arpack_options_t *arpack = (igraph_arpack_options_t*) malloc(sizeof(igraph_arpack_options_t));
  igraph_arpack_options_init(arpack);
  return arpack;
}

void igraph_arpack_destroy(igraph_arpack_options_t* arpack)
{
  if (arpack)
    free(arpack);
  arpack = NULL;
}
#endc

{#fun igraph_arpack_new as ^ { } -> `ArpackOptPtr' #}
