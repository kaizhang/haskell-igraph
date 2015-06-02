{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Internal.Selector where

import Control.Monad
import Foreign
import Foreign.C.Types
import System.IO.Unsafe (unsafePerformIO)

{#import IGraph.Internal.Constants #}
{#import IGraph.Internal.Graph #}
{#import IGraph.Internal.Data #}

#include "igraph/igraph.h"

{#pointer *igraph_vs_t as IGraphVsPtr foreign finalizer igraph_vs_destroy newtype #}

#c
igraph_vs_t* igraph_vs_new() {
  igraph_vs_t* vs = (igraph_vs_t*) malloc (sizeof (igraph_vs_t));
  return vs;
}
#endc

{#fun igraph_vs_new as ^ { } -> `IGraphVsPtr' #}

{#fun igraph_vs_all as ^ { `IGraphVsPtr' } -> `Int' #}

{#fun igraph_vs_adj as ^ { `IGraphVsPtr', `Int', `Neimode' } -> `Int' #}

{#fun igraph_vs_vector as ^ { `IGraphVsPtr', `VectorPtr' } -> `Int' #}


-- Vertex iterator

{#pointer *igraph_vit_t as IGraphVitPtr foreign finalizer igraph_vit_destroy newtype #}

#c
igraph_vit_t* igraph_vit_new(const igraph_t *graph, igraph_vs_t vs) {
  igraph_vit_t* vit = (igraph_vit_t*) malloc (sizeof (igraph_vit_t));
  igraph_vit_create(graph, vs, vit);
  return vit;
}

igraph_bool_t igraph_vit_end(igraph_vit_t *vit) {
  return IGRAPH_VIT_END(*vit);
}

void igraph_vit_next(igraph_vit_t *vit) {
  IGRAPH_VIT_NEXT(*vit);
}

igraph_integer_t igraph_vit_get(igraph_vit_t *vit) {
  return IGRAPH_VIT_GET(*vit);
}
#endc

{#fun igraph_vit_new as ^ { `IGraphPtr', %`IGraphVsPtr' } -> `IGraphVitPtr' #}

{#fun igraph_vit_end as ^ { `IGraphVitPtr' } -> `Bool' #}

{#fun igraph_vit_next as ^ { `IGraphVitPtr' } -> `()' #}

{#fun igraph_vit_get as ^ { `IGraphVitPtr' } -> `Int' #}
