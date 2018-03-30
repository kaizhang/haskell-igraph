{-# LANGUAGE ForeignFunctionInterface #-}
module IGraph.Internal.Selector where

import Control.Monad
import Foreign
import Foreign.C.Types
import System.IO.Unsafe (unsafePerformIO)

{#import IGraph.Internal.Constants #}
{#import IGraph.Internal.Graph #}
{#import IGraph.Internal.Data #}

#include "haskell_igraph.h"

{#pointer *igraph_vs_t as IGraphVs foreign finalizer igraph_vs_destroy newtype #}

{#fun igraph_vs_all as ^ { + } -> `IGraphVs' #}

{#fun igraph_vs_adj as ^ { +, `Int', `Neimode' } -> `IGraphVs' #}

{#fun igraph_vs_vector as ^ { +, `Vector' } -> `IGraphVs' #}


-- Vertex iterator

{#pointer *igraph_vit_t as IGraphVit foreign finalizer igraph_vit_destroy newtype #}

#c
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

{#fun igraph_vit_create as igraphVitNew { `IGraph', %`IGraphVs', + } -> `IGraphVit' #}

{#fun igraph_vit_end as ^ { `IGraphVit' } -> `Bool' #}

{#fun igraph_vit_next as ^ { `IGraphVit' } -> `()' #}

{#fun igraph_vit_get as ^ { `IGraphVit' } -> `Int' #}

vitToList :: IGraphVit -> IO [Int]
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

{#pointer *igraph_es_t as IGraphEs foreign finalizer igraph_es_destroy newtype #}

{#fun igraph_es_all as ^ { +, `EdgeOrderType' } -> `IGraphEs' #}

{# fun igraph_es_vector as ^ { +, `Vector' } -> `IGraphEs' #}


-- Edge iterator

{#pointer *igraph_eit_t as IGraphEit foreign finalizer igraph_eit_destroy newtype #}

#c
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

{#fun igraph_eit_create as igraphEitNew { `IGraph', %`IGraphEs', + } -> `IGraphEit' #}

{#fun igraph_eit_end as ^ { `IGraphEit' } -> `Bool' #}

{#fun igraph_eit_next as ^ { `IGraphEit' } -> `()' #}

{#fun igraph_eit_get as ^ { `IGraphEit' } -> `Int' #}

eitToList :: IGraphEit -> IO [Int]
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

{# fun igraph_delete_vertices as ^ { `IGraph', %`IGraphVs' } -> `Int' #}


-- delete edges

{# fun igraph_delete_edges as ^ { `IGraph', %`IGraphEs' } -> `Int' #}
