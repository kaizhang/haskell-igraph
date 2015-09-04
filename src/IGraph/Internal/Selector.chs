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

{#fun igraph_vs_all as ^ { + } -> `IGraphVsPtr' #}

{#fun igraph_vs_adj as ^ { +, `Int', `Neimode' } -> `IGraphVsPtr' #}

{#fun igraph_vs_vector as ^ { +, `VectorPtr' } -> `IGraphVsPtr' #}


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

vitToList :: IGraphVitPtr -> IO [Int]
vitToList vit = do
    isEnd <- igraphVitEnd vit
    if isEnd
      then return []
      else do
        cur <- igraphVitGet vit
        igraphVitNext vit
        acc <- vitToList vit
        return $ cur : acc


-- Edge Selector

{#pointer *igraph_es_t as IGraphEsPtr foreign finalizer igraph_es_destroy newtype #}

{#fun igraph_es_all as ^ { +, `EdgeOrderType' } -> `IGraphEsPtr' #}

{# fun igraph_es_vector as ^ { +, `VectorPtr' } -> `IGraphEsPtr' #}


-- Edge iterator

{#pointer *igraph_eit_t as IGraphEitPtr foreign finalizer igraph_eit_destroy newtype #}

#c
igraph_eit_t* igraph_eit_new(const igraph_t *graph, igraph_es_t es) {
  igraph_eit_t* eit = (igraph_eit_t*) malloc (sizeof (igraph_eit_t));
  igraph_eit_create(graph, es, eit);
  return eit;
}

igraph_bool_t igraph_eit_end(igraph_eit_t *eit) {
  return IGRAPH_EIT_END(*eit);
}

void igraph_eit_next(igraph_eit_t *eit) {
  IGRAPH_EIT_NEXT(*eit);
}

igraph_integer_t igraph_eit_get(igraph_eit_t *eit) {
  return IGRAPH_EIT_GET(*eit);
}
#endc

{#fun igraph_eit_new as ^ { `IGraphPtr', %`IGraphEsPtr' } -> `IGraphEitPtr' #}

{#fun igraph_eit_end as ^ { `IGraphEitPtr' } -> `Bool' #}

{#fun igraph_eit_next as ^ { `IGraphEitPtr' } -> `()' #}

{#fun igraph_eit_get as ^ { `IGraphEitPtr' } -> `Int' #}

eitToList :: IGraphEitPtr -> IO [Int]
eitToList eit = do
    isEnd <- igraphEitEnd eit
    if isEnd
      then return []
      else do
        cur <- igraphEitGet eit
        igraphEitNext eit
        acc <- eitToList eit
        return $ cur : acc

-- delete vertices

{# fun igraph_delete_vertices as ^ { `IGraphPtr', %`IGraphVsPtr' } -> `Int' #}


-- delete edges

{# fun igraph_delete_edges as ^ { `IGraphPtr', %`IGraphEsPtr' } -> `Int' #}
