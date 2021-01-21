#include "haskell_attributes.h"

/* An attribute is either a numeric vector (vector_t) or a string
   vector (strvector_t). The attribute itself is stored in a
   struct igraph_attribute_record_t, there is one such object for each
   attribute. The igraph_t has a pointer to an array of three
   vector_ptr_t's which contains pointers to
   igraph_haskell_attribute_t's. Graph attributes are first, then vertex
   and edge attributes. */

igraph_bool_t igraph_haskell_attribute_find(const igraph_vector_ptr_t *ptrvec,
				       const char *name, long int *idx) {
  long int i, n=igraph_vector_ptr_size(ptrvec);
  igraph_bool_t l=0;
  for (i=0; !l && i<n; i++) {
    igraph_attribute_record_t *rec=VECTOR(*ptrvec)[i];
    l= !strcmp(rec->name, name);
  }
  if (idx) { *idx=i-1; }
  return l;
}

int igraph_haskell_attributes_copy_attribute_record(igraph_attribute_record_t **newrec,
					       const igraph_attribute_record_t *rec) {
  bsvector_t *str, *newstr;

  *newrec=igraph_Calloc(1, igraph_attribute_record_t);
  if (!(*newrec)) { IGRAPH_ERROR("Cannot copy attributes", IGRAPH_ENOMEM); }
  IGRAPH_FINALLY(igraph_free, *newrec);
  (*newrec)->type=rec->type;
  (*newrec)->name=strdup(rec->name);
  if (!(*newrec)->name) { IGRAPH_ERROR("Cannot copy attributes", IGRAPH_ENOMEM); }
  IGRAPH_FINALLY(igraph_free, (void*)(*newrec)->name);

  if (rec->type == IGRAPH_ATTRIBUTE_STRING) {
    str=(bsvector_t*)rec->value;
    newstr=igraph_Calloc(1, bsvector_t);
    if (!newstr) {
      IGRAPH_ERROR("Cannot copy attributes", IGRAPH_ENOMEM);
    }
    IGRAPH_FINALLY(igraph_free, newstr);
    IGRAPH_CHECK(bsvector_copy(newstr, str));
    IGRAPH_FINALLY(bsvector_destroy, newstr);
    (*newrec)->value=newstr;
  } else {
    IGRAPH_ERROR("Wrong attribute type", IGRAPH_ENOMEM);
  }

  IGRAPH_FINALLY_CLEAN(4);
  return 0;
}


int igraph_haskell_attribute_init(igraph_t *graph, igraph_vector_ptr_t *attr) {
  igraph_attribute_record_t *attr_rec;
  long int i, n;
  igraph_haskell_attributes_t *nattr;

  n = attr ? igraph_vector_ptr_size(attr) : 0;

  nattr=igraph_Calloc(1, igraph_haskell_attributes_t);
  if (!nattr) {
    IGRAPH_ERROR("Can't init attributes", IGRAPH_ENOMEM);
  }
  IGRAPH_FINALLY(igraph_free, nattr);

  IGRAPH_CHECK(igraph_vector_ptr_init(&nattr->gal, n));
  IGRAPH_FINALLY(igraph_vector_ptr_destroy, &nattr->gal);
  IGRAPH_CHECK(igraph_vector_ptr_init(&nattr->val, 0));
  IGRAPH_FINALLY(igraph_vector_ptr_destroy, &nattr->val);
  IGRAPH_CHECK(igraph_vector_ptr_init(&nattr->eal, 0));
  IGRAPH_FINALLY_CLEAN(3);

  for (i=0; i<n; i++) {
    IGRAPH_CHECK(igraph_haskell_attributes_copy_attribute_record(
	  &attr_rec, VECTOR(*attr)[i]));
    VECTOR(nattr->gal)[i] = attr_rec;
  }

  graph->attr=nattr;

  return 0;
}

void igraph_haskell_attribute_destroy(igraph_t *graph) {
  igraph_haskell_attributes_t *attr=graph->attr;
  igraph_vector_ptr_t *als[3]= { &attr->gal, &attr->val, &attr->eal };
  long int i, n, a;
  bsvector_t *str;
  igraph_attribute_record_t *rec;
  for (a=0; a<3; a++) {
    n=igraph_vector_ptr_size(als[a]);
    for (i=0; i<n; i++) {
      rec=VECTOR(*als[a])[i];
      if (rec) {
				if (rec->type == IGRAPH_ATTRIBUTE_STRING) {
					str=(bsvector_t*)rec->value;
					bsvector_destroy(str);
					igraph_free(str);
				}
				igraph_free((char*)rec->name);
				igraph_free(rec);
			}
		}
  }
  igraph_vector_ptr_destroy(&attr->gal);
  igraph_vector_ptr_destroy(&attr->val);
  igraph_vector_ptr_destroy(&attr->eal);
  igraph_free(graph->attr);
  graph->attr=0;
}

/* Almost the same as destroy, but we might have null pointers */

void igraph_haskell_attribute_copy_free(igraph_haskell_attributes_t *attr) {
  igraph_vector_ptr_t *als[3] = { &attr->gal, &attr->val, &attr->eal };
  long int i, n, a;
  bsvector_t *str;
  igraph_attribute_record_t *rec;
  for (a=0; a<3; a++) {
    n=igraph_vector_ptr_size(als[a]);
    for (i=0; i<n; i++) {
      rec=VECTOR(*als[a])[i];
      if (!rec) { continue; }
			if (rec->type == IGRAPH_ATTRIBUTE_STRING) {
				str=(bsvector_t*)rec->value;
				bsvector_destroy(str);
				igraph_free(str);
			}
			igraph_free((char*)rec->name);
      igraph_free(rec);
    }
  }
}

/* No reference counting here. If you use attributes in C you should
   know what you're doing. */

int igraph_haskell_attribute_copy(igraph_t *to, const igraph_t *from,
			     igraph_bool_t ga, igraph_bool_t va, igraph_bool_t ea) {
  igraph_haskell_attributes_t *attrfrom=from->attr, *attrto;
  igraph_vector_ptr_t *alto[3], *alfrom[3]={ &attrfrom->gal, &attrfrom->val,
					     &attrfrom->eal };
  long int i, n, a;
  igraph_bool_t copy[3] = { ga, va, ea };
  to->attr=attrto=igraph_Calloc(1, igraph_haskell_attributes_t);
  if (!attrto) {
    IGRAPH_ERROR("Cannot copy attributes", IGRAPH_ENOMEM);
  }
  IGRAPH_FINALLY(igraph_free, attrto);
  IGRAPH_VECTOR_PTR_INIT_FINALLY(&attrto->gal, 0);
  IGRAPH_VECTOR_PTR_INIT_FINALLY(&attrto->val, 0);
  IGRAPH_VECTOR_PTR_INIT_FINALLY(&attrto->eal, 0);
  IGRAPH_FINALLY_CLEAN(3);
  IGRAPH_FINALLY(igraph_haskell_attribute_copy_free, attrto);

  alto[0]=&attrto->gal; alto[1]=&attrto->val; alto[2]=&attrto->eal;
  for (a=0; a<3; a++) {
    if (copy[a]) {
      n=igraph_vector_ptr_size(alfrom[a]);
      IGRAPH_CHECK(igraph_vector_ptr_resize(alto[a], n));
      igraph_vector_ptr_null(alto[a]);
      for (i=0; i<n; i++) {
	igraph_attribute_record_t *newrec;
	IGRAPH_CHECK(igraph_haskell_attributes_copy_attribute_record(&newrec,
								VECTOR(*alfrom[a])[i]));
	VECTOR(*alto[a])[i]=newrec;
      }
    }
  }

  IGRAPH_FINALLY_CLEAN(2);
  return 0;
}

int igraph_haskell_attribute_add_vertices(igraph_t *graph, long int nv,
				     igraph_vector_ptr_t *nattr) {
  igraph_haskell_attributes_t *attr=graph->attr;
  igraph_vector_ptr_t *val=&attr->val;
  long int length=igraph_vector_ptr_size(val);
  long int nattrno=nattr==NULL ? 0 : igraph_vector_ptr_size(nattr);
  long int origlen=igraph_vcount(graph)-nv;
  long int newattrs=0, i;
  igraph_vector_t news;

  /* First add the new attributes if any */
  newattrs=0;
  IGRAPH_VECTOR_INIT_FINALLY(&news, 0);
  for (i=0; i<nattrno; i++) {
    igraph_attribute_record_t *nattr_entry=VECTOR(*nattr)[i];
    const char *nname=nattr_entry->name;
    long int j;
    igraph_bool_t l=igraph_haskell_attribute_find(val, nname, &j);
    if (!l) {
      newattrs++;
      IGRAPH_CHECK(igraph_vector_push_back(&news, i));
    } else {
      /* check types */
      if (nattr_entry->type != ((igraph_attribute_record_t*)VECTOR(*val)[j])->type) {
				IGRAPH_ERROR("You cannot mix attribute types", IGRAPH_EINVAL);
			}
    }
  }

  /* Add NA/empty string vectors for the existing vertices */
  if (newattrs != 0) {
    for (i=0; i<newattrs; i++) {
      igraph_attribute_record_t *tmp=VECTOR(*nattr)[(long int)VECTOR(news)[i]];
      igraph_attribute_record_t *newrec=igraph_Calloc(1, igraph_attribute_record_t);
      igraph_attribute_type_t type=tmp->type;
      if (!newrec) {
				IGRAPH_ERROR("Cannot add attributes", IGRAPH_ENOMEM);
      }
      IGRAPH_FINALLY(igraph_free, newrec);
      newrec->type=type;
      newrec->name=strdup(tmp->name);
      if (!newrec->name) {
				IGRAPH_ERROR("Cannot add attributes", IGRAPH_ENOMEM);
      }
      IGRAPH_FINALLY(igraph_free, (char*)newrec->name);
			if (type==IGRAPH_ATTRIBUTE_STRING) {
				bsvector_t *newstr=igraph_Calloc(1, bsvector_t);
				if (!newstr) {
					IGRAPH_ERROR("Cannot add attributes", IGRAPH_ENOMEM);
				}
				IGRAPH_FINALLY(igraph_free, newstr);
				BSVECTOR_INIT_FINALLY(newstr, origlen);
				newrec->value=newstr;
      }
      IGRAPH_CHECK(igraph_vector_ptr_push_back(val, newrec));
      IGRAPH_FINALLY_CLEAN(4);
    }
    length=igraph_vector_ptr_size(val);
  }


  /* Now append the new values */
  for (i=0; i<length; i++) {
    igraph_attribute_record_t *oldrec=VECTOR(*val)[i];
    igraph_attribute_record_t *newrec=0;
    const char *name=oldrec->name;
    long int j;
    igraph_bool_t l=0;
    if (nattr) { l=igraph_haskell_attribute_find(nattr, name, &j); }
    if (l) {
      /* This attribute is present in nattr */
      bsvector_t *oldstr, *newstr;
      newrec=VECTOR(*nattr)[j];
      oldstr=(bsvector_t*)oldrec->value;
      newstr=(bsvector_t*)newrec->value;
      if (oldrec->type != newrec->type) {
				IGRAPH_ERROR("Attribute types do not match", IGRAPH_EINVAL);
      }
      if (oldrec->type == IGRAPH_ATTRIBUTE_STRING) {
				if (nv != bsvector_size(newstr)) {
					IGRAPH_ERROR("Invalid string attribute length", IGRAPH_EINVAL);
				}
				IGRAPH_CHECK(bsvector_append(oldstr, newstr));
			} else {
				IGRAPH_WARNING("Invalid attribute type");
			}
		} else {
			/* No such attribute, append NA's */
			bsvector_t *oldstr=(bsvector_t*)oldrec->value;
			if (oldrec->type == IGRAPH_ATTRIBUTE_STRING) {
				IGRAPH_CHECK(bsvector_resize(oldstr, origlen+nv));
			} else {
				IGRAPH_WARNING("Invalid attribute type");
			}
		}
	}

  igraph_vector_destroy(&news);
  IGRAPH_FINALLY_CLEAN(1);

  return 0;
}

void igraph_haskell_attribute_permute_free(igraph_vector_ptr_t *v) {
  long int i, n=igraph_vector_ptr_size(v);
  for (i=0; i<n; i++) {
    igraph_attribute_record_t *rec=VECTOR(*v)[i];
    igraph_Free(rec->name);
    if (rec->type == IGRAPH_ATTRIBUTE_STRING) {
      bsvector_t *strv= (bsvector_t*) rec->value;
      bsvector_destroy(strv);
      igraph_Free(strv);
		}
    igraph_Free(rec);
  }
  igraph_vector_ptr_clear(v);
}

int igraph_haskell_attribute_permute_vertices(const igraph_t *graph,
					 igraph_t *newgraph,
					 const igraph_vector_t *idx) {

  if (graph==newgraph) {

    igraph_haskell_attributes_t *attr=graph->attr;
    igraph_vector_ptr_t *val=&attr->val;
    long int valno=igraph_vector_ptr_size(val);
    long int i;

    for (i=0; i<valno; i++) {
      igraph_attribute_record_t *oldrec=VECTOR(*val)[i];
      igraph_attribute_type_t type=oldrec->type;
      bsvector_t *str, *newstr;
      if (type == IGRAPH_ATTRIBUTE_STRING) {
				str=(bsvector_t*)oldrec->value;
				newstr=igraph_Calloc(1, bsvector_t);
				if (!newstr) {
					IGRAPH_ERROR("Cannot permute vertex attributes", IGRAPH_ENOMEM);
				}
				IGRAPH_CHECK(bsvector_init(newstr, 0));
				IGRAPH_FINALLY(bsvector_destroy, newstr);
				bsvector_index(str, newstr, idx);
				oldrec->value=newstr;
				bsvector_destroy(str);
				igraph_Free(str);
				IGRAPH_FINALLY_CLEAN(1);
			} else {
				IGRAPH_WARNING("Unknown edge attribute ignored");
      }
    }
	} else {
    igraph_haskell_attributes_t *attr=graph->attr;
    igraph_vector_ptr_t *val=&attr->val;
    long int valno=igraph_vector_ptr_size(val);
    long int i;

    /* New vertex attributes */
    igraph_haskell_attributes_t *new_attr=newgraph->attr;
    igraph_vector_ptr_t *new_val=&new_attr->val;
    if (igraph_vector_ptr_size(new_val) != 0) {
      IGRAPH_ERROR("Vertex attributes were already copied",
		   IGRAPH_EATTRIBUTES);
    }
    IGRAPH_CHECK(igraph_vector_ptr_resize(new_val, valno));

    IGRAPH_FINALLY(igraph_haskell_attribute_permute_free, new_val);

    for (i=0; i<valno; i++) {
      igraph_attribute_record_t *oldrec=VECTOR(*val)[i];
      igraph_attribute_type_t type=oldrec->type;
      bsvector_t *str, *newstr;

      /* The record itself */
      igraph_attribute_record_t *new_rec=
	igraph_Calloc(1, igraph_attribute_record_t);
      if (!new_rec) {
	IGRAPH_ERROR("Cannot create vertex attributes", IGRAPH_ENOMEM);
      }
      new_rec->name = strdup(oldrec->name);
      new_rec->type = oldrec->type;
      VECTOR(*new_val)[i]=new_rec;

      /* The data */
      if (type == IGRAPH_ATTRIBUTE_STRING) {
				str=(bsvector_t*)oldrec->value;
				newstr=igraph_Calloc(1, bsvector_t);
				if (!newstr) {
					IGRAPH_ERROR("Cannot permute vertex attributes", IGRAPH_ENOMEM);
				}
				IGRAPH_CHECK(bsvector_init(newstr, 0));
				IGRAPH_FINALLY(bsvector_destroy, newstr);
				bsvector_index(str, newstr, idx);
				new_rec->value=newstr;
				IGRAPH_FINALLY_CLEAN(1);
			} else {
				IGRAPH_WARNING("Unknown vertex attribute ignored");
      }
    }
  }

  IGRAPH_FINALLY_CLEAN(1);
  return 0;
}

int igraph_haskell_attribute_combine_vertices(const igraph_t *graph,
			 igraph_t *newgraph,
			 const igraph_vector_ptr_t *merges,
			 const igraph_attribute_combination_t *comb) {
	IGRAPH_ERROR("Cannot combine vertex attributes", IGRAPH_ENOMEM);
  return 1;
}

int igraph_haskell_attribute_add_edges(igraph_t *graph, const igraph_vector_t *edges,
				 igraph_vector_ptr_t *nattr) {

  igraph_haskell_attributes_t *attr=graph->attr;
  igraph_vector_ptr_t *eal=&attr->eal;
  long int ealno=igraph_vector_ptr_size(eal);
  long int ne=igraph_vector_size(edges)/2;
  long int origlen=igraph_ecount(graph)-ne;
  long int nattrno= nattr == 0 ? 0 : igraph_vector_ptr_size(nattr);
  igraph_vector_t news;
  long int newattrs, i;

  /* First add the new attributes if any */
  newattrs=0;
  IGRAPH_VECTOR_INIT_FINALLY(&news, 0);
  for (i=0; i<nattrno; i++) {
    igraph_attribute_record_t *nattr_entry=VECTOR(*nattr)[i];
    const char *nname=nattr_entry->name;
    long int j;
    igraph_bool_t l=igraph_haskell_attribute_find(eal, nname, &j);
    if (!l) {
      newattrs++;
      IGRAPH_CHECK(igraph_vector_push_back(&news, i));
    } else {
      /* check types */
      if (nattr_entry->type != ((igraph_attribute_record_t*)VECTOR(*eal)[j])->type) {
				IGRAPH_ERROR("You cannot mix attribute types", IGRAPH_EINVAL);
			}
		}
  }

  /* Add NA/empty string vectors for the existing vertices */
  if (newattrs != 0) {
    for (i=0; i<newattrs; i++) {
      igraph_attribute_record_t *tmp=VECTOR(*nattr)[(long int)VECTOR(news)[i]];
      igraph_attribute_record_t *newrec=igraph_Calloc(1, igraph_attribute_record_t);
      igraph_attribute_type_t type=tmp->type;
      if (!newrec) {
				IGRAPH_ERROR("Cannot add attributes", IGRAPH_ENOMEM);
      }
      IGRAPH_FINALLY(igraph_free, newrec);
      newrec->type=type;
      newrec->name=strdup(tmp->name);
      if (!newrec->name) {
				IGRAPH_ERROR("Cannot add attributes", IGRAPH_ENOMEM);
      }
      IGRAPH_FINALLY(igraph_free, (char*)newrec->name);
      if (type==IGRAPH_ATTRIBUTE_STRING) {
				bsvector_t *newstr=igraph_Calloc(1, bsvector_t);
				if (!newstr) {
					IGRAPH_ERROR("Cannot add attributes", IGRAPH_ENOMEM);
				}
				IGRAPH_FINALLY(igraph_free, newstr);
				BSVECTOR_INIT_FINALLY(newstr, origlen);
				newrec->value=newstr;
      }
      IGRAPH_CHECK(igraph_vector_ptr_push_back(eal, newrec));
      IGRAPH_FINALLY_CLEAN(4);
    }
    ealno=igraph_vector_ptr_size(eal);
  }

  /* Now append the new values */
  for (i=0; i<ealno; i++) {
    igraph_attribute_record_t *oldrec=VECTOR(*eal)[i];
    igraph_attribute_record_t *newrec=0;
    const char *name=oldrec->name;
    long int j;
    igraph_bool_t l=0;
    if (nattr) { l=igraph_haskell_attribute_find(nattr, name, &j); }
    if (l) {
      /* This attribute is present in nattr */
      bsvector_t *oldstr, *newstr;
      newrec=VECTOR(*nattr)[j];
      oldstr=(bsvector_t*)oldrec->value;
      newstr=(bsvector_t*)newrec->value;
      if (oldrec->type != newrec->type) {
				IGRAPH_ERROR("Attribute types do not match", IGRAPH_EINVAL);
      }
      if (oldrec->type == IGRAPH_ATTRIBUTE_STRING) {
				if (ne != bsvector_size(newstr)) {
					IGRAPH_ERROR("Invalid string attribute length", IGRAPH_EINVAL);
				}
				IGRAPH_CHECK(bsvector_append(oldstr, newstr));
			} else {
				IGRAPH_WARNING("Invalid attribute type");
			}
    } else {
      /* No such attribute, append NA's */
      bsvector_t *oldstr=(bsvector_t*)oldrec->value;
      if (oldrec->type == IGRAPH_ATTRIBUTE_STRING) {
				IGRAPH_CHECK(bsvector_resize(oldstr, origlen+ne));
			} else {
				IGRAPH_WARNING("Invalid attribute type");
      }
    }
  }

  igraph_vector_destroy(&news);
  IGRAPH_FINALLY_CLEAN(1);

  return 0;
}

int igraph_haskell_attribute_permute_edges(const igraph_t *graph,
				      igraph_t *newgraph,
				      const igraph_vector_t *idx) {

  if (graph == newgraph) {

    igraph_haskell_attributes_t *attr=graph->attr;
    igraph_vector_ptr_t *eal=&attr->eal;
    long int ealno=igraph_vector_ptr_size(eal);
    long int i;

    for (i=0; i<ealno; i++) {
      igraph_attribute_record_t *oldrec=VECTOR(*eal)[i];
      igraph_attribute_type_t type=oldrec->type;
      bsvector_t *str, *newstr;
      if (type == IGRAPH_ATTRIBUTE_STRING) {
				str=(bsvector_t*)oldrec->value;
				newstr=igraph_Calloc(1, bsvector_t);
				if (!newstr) {
					IGRAPH_ERROR("Cannot permute edge attributes", IGRAPH_ENOMEM);
				}
				IGRAPH_CHECK(bsvector_init(newstr, 0));
				IGRAPH_FINALLY(bsvector_destroy, newstr);
				bsvector_index(str, newstr, idx);
				oldrec->value=newstr;
				bsvector_destroy(str);
				igraph_Free(str);
				IGRAPH_FINALLY_CLEAN(1);
			} else {
				IGRAPH_WARNING("Unknown edge attribute ignored");
      }
    }

  } else {

    igraph_haskell_attributes_t *attr=graph->attr;
    igraph_vector_ptr_t *eal=&attr->eal;
    long int ealno=igraph_vector_ptr_size(eal);
    long int i;

    /* New edge attributes */
    igraph_haskell_attributes_t *new_attr=newgraph->attr;
    igraph_vector_ptr_t *new_eal=&new_attr->eal;
    IGRAPH_CHECK(igraph_vector_ptr_resize(new_eal, ealno));

    IGRAPH_FINALLY(igraph_haskell_attribute_permute_free, new_eal);

    for (i=0; i<ealno; i++) {
      igraph_attribute_record_t *oldrec=VECTOR(*eal)[i];
      igraph_attribute_type_t type=oldrec->type;
      bsvector_t *str, *newstr;

      /* The record itself */
      igraph_attribute_record_t *new_rec= igraph_Calloc(1, igraph_attribute_record_t);
      if (!new_rec) {
				IGRAPH_ERROR("Cannot create edge attributes", IGRAPH_ENOMEM);
      }
      new_rec->name = strdup(oldrec->name);
      new_rec->type = oldrec->type;
      VECTOR(*new_eal)[i] = new_rec;

      if (type == IGRAPH_ATTRIBUTE_STRING) {
				str=(bsvector_t*)oldrec->value;
				newstr=igraph_Calloc(1, bsvector_t);
				if (!newstr) {
					IGRAPH_ERROR("Cannot permute edge attributes", IGRAPH_ENOMEM);
				}
				IGRAPH_CHECK(bsvector_init(newstr, 0));
				IGRAPH_FINALLY(bsvector_destroy, newstr);
				bsvector_index(str, newstr, idx);
				new_rec->value=newstr;
				IGRAPH_FINALLY_CLEAN(1);
			} else {
				IGRAPH_WARNING("Unknown edge attribute ignored");
      }
    }
    IGRAPH_FINALLY_CLEAN(1);
  }

  return 0;
}

int igraph_haskell_attribute_combine_edges(const igraph_t *graph,
			 igraph_t *newgraph,
			 const igraph_vector_ptr_t *merges,
			 const igraph_attribute_combination_t *comb) {

  IGRAPH_ERROR("Cannot combine edge attributes", IGRAPH_ENOMEM);
  return 1;
}

int igraph_haskell_attribute_get_info(const igraph_t *graph,
				 igraph_strvector_t *gnames,
				 igraph_vector_t *gtypes,
				 igraph_strvector_t *vnames,
				 igraph_vector_t *vtypes,
				 igraph_strvector_t *enames,
				 igraph_vector_t *etypes) {

  igraph_strvector_t *names[3] = { gnames, vnames, enames };
  igraph_vector_t *types[3] = { gtypes, vtypes, etypes };
  igraph_haskell_attributes_t *at=graph->attr;
  igraph_vector_ptr_t *attr[3]={ &at->gal, &at->val, &at->eal };
  long int i,j;

  for (i=0; i<3; i++) {
    igraph_strvector_t *n=names[i];
    igraph_vector_t *t=types[i];
    igraph_vector_ptr_t *al=attr[i];
    long int len=igraph_vector_ptr_size(al);

    if (n) {
      IGRAPH_CHECK(igraph_strvector_resize(n, len));
    }
    if (t) {
      IGRAPH_CHECK(igraph_vector_resize(t, len));
    }

    for (j=0; j<len; j++) {
      igraph_attribute_record_t *rec=VECTOR(*al)[j];
      const char *name=rec->name;
      igraph_attribute_type_t type=rec->type;
      if (n) {
	IGRAPH_CHECK(igraph_strvector_set(n, j, name));
      }
      if (t) {
	VECTOR(*t)[j]=type;
      }
    }
  }

  return 0;
}

igraph_bool_t igraph_haskell_attribute_has_attr(const igraph_t *graph,
					 igraph_attribute_elemtype_t type,
					 const char *name) {
  igraph_haskell_attributes_t *at=graph->attr;
  igraph_vector_ptr_t *attr[3]={ &at->gal, &at->val, &at->eal };
  long int attrnum;

  switch (type) {
  case IGRAPH_ATTRIBUTE_GRAPH:
    attrnum=0;
    break;
  case IGRAPH_ATTRIBUTE_VERTEX:
    attrnum=1;
    break;
  case IGRAPH_ATTRIBUTE_EDGE:
    attrnum=2;
    break;
  default:
    IGRAPH_ERROR("Unknown attribute element type", IGRAPH_EINVAL);
    break;
  }

  return igraph_haskell_attribute_find(attr[attrnum], name, 0);
}

int igraph_haskell_attribute_gettype(const igraph_t *graph,
			      igraph_attribute_type_t *type,
			      igraph_attribute_elemtype_t elemtype,
			      const char *name) {
  long int attrnum;
  igraph_attribute_record_t *rec;
  igraph_haskell_attributes_t *at=graph->attr;
  igraph_vector_ptr_t *attr[3]={ &at->gal, &at->val, &at->eal };
  igraph_vector_ptr_t *al;
  long int j;
  igraph_bool_t l=0;

  switch (elemtype) {
  case IGRAPH_ATTRIBUTE_GRAPH:
    attrnum=0;
    break;
  case IGRAPH_ATTRIBUTE_VERTEX:
    attrnum=1;
    break;
  case IGRAPH_ATTRIBUTE_EDGE:
    attrnum=2;
    break;
  default:
    IGRAPH_ERROR("Unknown attribute element type", IGRAPH_EINVAL);
    break;
  }

  al=attr[attrnum];
  l=igraph_haskell_attribute_find(al, name, &j);
  if (!l) {
    IGRAPH_ERROR("Unknown attribute", IGRAPH_EINVAL);
  }
  rec=VECTOR(*al)[j];
  *type=rec->type;

  return 0;
}

int igraph_haskell_attribute_get_numeric_graph_attr(const igraph_t *graph,
					      const char *name,
					      igraph_vector_t *value) {
  IGRAPH_ERROR("Not implemented", IGRAPH_ENOMEM);
  return 1;
}

int igraph_haskell_attribute_get_bool_graph_attr(const igraph_t *graph,
					    const char *name,
					    igraph_vector_bool_t *value) {
  IGRAPH_ERROR("Not implemented", IGRAPH_ENOMEM);
  return 1;
}

int igraph_haskell_attribute_get_string_graph_attr(const igraph_t *graph,
					     const char *name,
					     igraph_strvector_t *value_) {
  igraph_haskell_attributes_t *attr=graph->attr;
  igraph_vector_ptr_t *gal=&attr->gal;
  long int j;
  igraph_attribute_record_t *rec;
  bsvector_t *str;
  igraph_bool_t l=igraph_haskell_attribute_find(gal, name, &j);

  if (!l) {
    IGRAPH_ERROR("Unknown attribute", IGRAPH_EINVAL);
  }

  rec=VECTOR(*gal)[j];
  str=(bsvector_t*)rec->value;

	bsvector_t *value;
	bsvector_init(value, 1);

  IGRAPH_CHECK(bsvector_set(value, 0, BS(*str,0)));

	igraph_strvector_copy(value_, bsvector_to_strvector(value));

  return 0;
}

int igraph_haskell_attribute_get_numeric_vertex_attr(const igraph_t *graph,
					      const char *name,
					      igraph_vs_t vs,
					      igraph_vector_t *value) {
  IGRAPH_ERROR("Not implemented", IGRAPH_ENOMEM);
  return 1;
}

int igraph_haskell_attribute_get_bool_vertex_attr(const igraph_t *graph,
					     const char *name,
					     igraph_vs_t vs,
					     igraph_vector_bool_t *value) {
  IGRAPH_ERROR("Not implemented", IGRAPH_ENOMEM);
  return 1;
}

int igraph_haskell_attribute_get_string_vertex_attr(const igraph_t *graph,
					     const char *name,
					     igraph_vs_t vs,
					     igraph_strvector_t *value_) {
  igraph_haskell_attributes_t *attr=graph->attr;
  igraph_vector_ptr_t *val=&attr->val;
  long int j;
  igraph_attribute_record_t *rec;
  bsvector_t *str;
  igraph_bool_t l=igraph_haskell_attribute_find(val, name, &j);

  if (!l) {
    IGRAPH_ERROR("Unknown attribute", IGRAPH_EINVAL);
  }

	bsvector_t *value;
	bsvector_init(value, 0);

  rec=VECTOR(*val)[j];
  str=(bsvector_t*)rec->value;
  if (igraph_vs_is_all(&vs)) {
    bsvector_resize(value, 0);
    IGRAPH_CHECK(bsvector_append(value, str));
  } else {
    igraph_vit_t it;
    long int i=0;
    IGRAPH_CHECK(igraph_vit_create(graph, vs, &it));
    IGRAPH_FINALLY(igraph_vit_destroy, &it);
    IGRAPH_CHECK(bsvector_resize(value, IGRAPH_VIT_SIZE(it)));
    for (; !IGRAPH_VIT_END(it); IGRAPH_VIT_NEXT(it), i++) {
      long int v=IGRAPH_VIT_GET(it);
      bytestring_t *s;
      bsvector_get(str, v, &s);
      IGRAPH_CHECK(bsvector_set(value, i, s));
    }
    igraph_vit_destroy(&it);
    IGRAPH_FINALLY_CLEAN(1);
  }

	igraph_strvector_copy(value_, bsvector_to_strvector(value));
  return 0;
}

int igraph_haskell_attribute_get_numeric_edge_attr(const igraph_t *graph,
					    const char *name,
					    igraph_es_t es,
					    igraph_vector_t *value) {
  IGRAPH_ERROR("Not implemented", IGRAPH_ENOMEM);
  return 1;
}

int igraph_haskell_attribute_get_string_edge_attr(const igraph_t *graph,
					   const char *name,
					   igraph_es_t es,
					   igraph_strvector_t *value_) {
  igraph_haskell_attributes_t *attr=graph->attr;
  igraph_vector_ptr_t *eal=&attr->eal;
  long int j;
  igraph_attribute_record_t *rec;
  bsvector_t *str;
  igraph_bool_t l=igraph_haskell_attribute_find(eal, name, &j);

  if (!l) {
    IGRAPH_ERROR("Unknown attribute", IGRAPH_EINVAL);
  }

	bsvector_t *value;
	bsvector_init(value, 1);

  rec=VECTOR(*eal)[j];
  str=(bsvector_t*)rec->value;
  if (igraph_es_is_all(&es)) {
    bsvector_resize(value, 0);
    IGRAPH_CHECK(bsvector_append(value, str));
  } else {
    igraph_eit_t it;
    long int i=0;
    IGRAPH_CHECK(igraph_eit_create(graph, es, &it));
    IGRAPH_FINALLY(igraph_eit_destroy, &it);
    IGRAPH_CHECK(bsvector_resize(value, IGRAPH_EIT_SIZE(it)));
    for (; !IGRAPH_EIT_END(it); IGRAPH_EIT_NEXT(it), i++) {
      long int e=IGRAPH_EIT_GET(it);
      bytestring_t *s;
      bsvector_get(str, e, &s);
      IGRAPH_CHECK(bsvector_set(value, i, s));
    }
    igraph_eit_destroy(&it);
    IGRAPH_FINALLY_CLEAN(1);
  }

	igraph_strvector_copy(value_, bsvector_to_strvector(value));
  return 0;
}

int igraph_haskell_attribute_get_bool_edge_attr(const igraph_t *graph,
					   const char *name,
					   igraph_es_t es,
					   igraph_vector_bool_t *value) {
  IGRAPH_ERROR("Not implemented", IGRAPH_ENOMEM);
  return 1;
}

igraph_real_t igraph_haskell_attribute_GAN(const igraph_t *graph, const char *name) {
  IGRAPH_ERROR("Not implemented", IGRAPH_ENOMEM);
  return 1;
}

/**
 * \function igraph_haskell_attribute_GAB
 * Query a boolean graph attribute.
 *
 * Returns the value of the given numeric graph attribute.
 * The attribute must exist, otherwise an error is triggered.
 * \param graph The input graph.
 * \param name The name of the attribute to query.
 * \return The value of the attribute.
 *
 * \sa \ref GAB for a simpler interface.
 *
 * Time complexity: O(Ag), the number of graph attributes.
 */
igraph_bool_t igraph_haskell_attribute_GAB(const igraph_t *graph, const char *name) {
  IGRAPH_ERROR("Not implemented", IGRAPH_ENOMEM);
  return 1;
}

/**
 * \function igraph_haskell_attribute_GAS
 * Query a string graph attribute.
 *
 * Returns a <type>const</type> pointer to the string graph attribute
 * specified in \p name.
 * The attribute must exist, otherwise an error is triggered.
 * \param graph The input graph.
 * \param name The name of the attribute to query.
 * \return The value of the attribute.
 *
 * \sa \ref GAS for a simpler interface.
 *
 * Time complexity: O(Ag), the number of graph attributes.
 */
const bytestring_t* igraph_haskell_attribute_GAS(const igraph_t *graph, const char *name) {

  igraph_haskell_attributes_t *attr=graph->attr;
  igraph_vector_ptr_t *gal=&attr->gal;
  long int j;
  igraph_attribute_record_t *rec;
  bsvector_t *str;
  igraph_bool_t l=igraph_haskell_attribute_find(gal, name, &j);

  if (!l) {
    igraph_error("Unknown attribute", __FILE__, __LINE__, IGRAPH_EINVAL);
    return 0;
  }

  rec=VECTOR(*gal)[j];
  str=(bsvector_t*)rec->value;
  return BS(*str, 0);
}

/**
 * \function igraph_haskell_attribute_VAN
 * Query a numeric vertex attribute.
 *
 * The attribute must exist, otherwise an error is triggered.
 * \param graph The input graph.
 * \param name The name of the attribute.
 * \param vid The id of the queried vertex.
 * \return The value of the attribute.
 *
 * \sa \ref VAN macro for a simpler interface.
 *
 * Time complexity: O(Av), the number of vertex attributes.
 */
igraph_real_t igraph_haskell_attribute_VAN(const igraph_t *graph, const char *name,
				      igraph_integer_t vid) {
  IGRAPH_ERROR("Not implemented", IGRAPH_ENOMEM);
  return 1;
}

/**
 * \function igraph_haskell_attribute_VAB
 * Query a boolean vertex attribute.
 *
 * The attribute must exist, otherwise an error is triggered.
 * \param graph The input graph.
 * \param name The name of the attribute.
 * \param vid The id of the queried vertex.
 * \return The value of the attribute.
 *
 * \sa \ref VAB macro for a simpler interface.
 *
 * Time complexity: O(Av), the number of vertex attributes.
 */
igraph_bool_t igraph_haskell_attribute_VAB(const igraph_t *graph, const char *name,
				    igraph_integer_t vid) {
  IGRAPH_ERROR("Not implemented", IGRAPH_ENOMEM);
  return 1;
}

/**
 * \function igraph_haskell_attribute_VAS
 * Query a string vertex attribute.
 *
 * The attribute must exist, otherwise an error is triggered.
 * \param graph The input graph.
 * \param name The name of the attribute.
 * \param vid The id of the queried vertex.
 * \return The value of the attribute.
 *
 * \sa The macro \ref VAS for a simpler interface.
 *
 * Time complexity: O(Av), the number of vertex attributes.
 */
const bytestring_t* igraph_haskell_attribute_VAS(const igraph_t *graph, const char *name,
				    igraph_integer_t vid) {
  igraph_haskell_attributes_t *attr=graph->attr;
  igraph_vector_ptr_t *val=&attr->val;
  long int j;
  igraph_attribute_record_t *rec;
  bsvector_t *str;
  igraph_bool_t l=igraph_haskell_attribute_find(val, name, &j);

  if (!l) {
    igraph_error("Unknown attribute", __FILE__, __LINE__, IGRAPH_EINVAL);
    return 0;
  }

  rec=VECTOR(*val)[j];
  str=(bsvector_t*)rec->value;
  return BS(*str, (long int)vid);
}

/**
 * \function igraph_haskell_attribute_EAN
 * Query a numeric edge attribute.
 *
 * The attribute must exist, otherwise an error is triggered.
 * \param graph The input graph.
 * \param name The name of the attribute.
 * \param eid The id of the queried edge.
 * \return The value of the attribute.
 *
 * \sa \ref EAN for an easier interface.
 *
 * Time complexity: O(Ae), the number of edge attributes.
 */
igraph_real_t igraph_haskell_attribute_EAN(const igraph_t *graph, const char *name,
				      igraph_integer_t eid) {
  IGRAPH_ERROR("Not implemented", IGRAPH_ENOMEM);
  return 1;
}

/**
 * \function igraph_haskell_attribute_EAB
 * Query a boolean edge attribute.
 *
 * The attribute must exist, otherwise an error is triggered.
 * \param graph The input graph.
 * \param name The name of the attribute.
 * \param eid The id of the queried edge.
 * \return The value of the attribute.
 *
 * \sa \ref EAB for an easier interface.
 *
 * Time complexity: O(Ae), the number of edge attributes.
 */
igraph_bool_t igraph_haskell_attribute_EAB(const igraph_t *graph, const char *name,
				    igraph_integer_t eid) {
  IGRAPH_ERROR("Not implemented", IGRAPH_ENOMEM);
  return 1;
}

/**
 * \function igraph_haskell_attribute_EAS
 * Query a string edge attribute.
 *
 * The attribute must exist, otherwise an error is triggered.
 * \param graph The input graph.
 * \param name The name of the attribute.
 * \param eid The id of the queried edge.
 * \return The value of the attribute.
 *
 * \se \ref EAS if you want to type less.
 *
 * Time complexity: O(Ae), the number of edge attributes.
 */
const bytestring_t* igraph_haskell_attribute_EAS(const igraph_t *graph, const char *name,
				    igraph_integer_t eid) {
  igraph_haskell_attributes_t *attr=graph->attr;
  igraph_vector_ptr_t *eal=&attr->eal;
  long int j;
  igraph_attribute_record_t *rec;
  bsvector_t *str;
  igraph_bool_t l=igraph_haskell_attribute_find(eal, name, &j);

  if (!l) {
    igraph_error("Unknown attribute", __FILE__, __LINE__, IGRAPH_EINVAL);
    return 0;
  }

  rec=VECTOR(*eal)[j];
  str=(bsvector_t*)rec->value;
  return BS(*str, (long int)eid);
}

/**
 * \function igraph_haskell_attribute_VANV
 * Query a numeric vertex attribute for many vertices
 *
 * \param graph The input graph.
 * \param name The name of the attribute.
 * \param vids The vertices to query.
 * \param result Pointer to an initialized vector, the result is
 *    stored here. It will be resized, if needed.
 * \return Error code.
 *
 * Time complexity: O(v), where v is the number of vertices in 'vids'.
 */

int igraph_haskell_attribute_VANV(const igraph_t *graph, const char *name,
			   igraph_vs_t vids, igraph_vector_t *result) {

  return igraph_haskell_attribute_get_numeric_vertex_attr(graph, name, vids,
						     result);
}

/**
 * \function igraph_haskell_attribute_VABV
 * Query a boolean vertex attribute for many vertices
 *
 * \param graph The input graph.
 * \param name The name of the attribute.
 * \param vids The vertices to query.
 * \param result Pointer to an initialized boolean vector, the result is
 *    stored here. It will be resized, if needed.
 * \return Error code.
 *
 * Time complexity: O(v), where v is the number of vertices in 'vids'.
 */

int igraph_haskell_attribute_VABV(const igraph_t *graph, const char *name,
			   igraph_vs_t vids, igraph_vector_bool_t *result) {

  return igraph_haskell_attribute_get_bool_vertex_attr(graph, name, vids,
						  result);
}

/**
 * \function igraph_haskell_attribute_EANV
 * Query a numeric edge attribute for many edges
 *
 * \param graph The input graph.
 * \param name The name of the attribute.
 * \param eids The edges to query.
 * \param result Pointer to an initialized vector, the result is
 *    stored here. It will be resized, if needed.
 * \return Error code.
 *
 * Time complexity: O(e), where e is the number of edges in 'eids'.
 */

int igraph_haskell_attribute_EANV(const igraph_t *graph, const char *name,
			   igraph_es_t eids, igraph_vector_t *result) {

  return igraph_haskell_attribute_get_numeric_edge_attr(graph, name, eids,
						   result);
}

/**
 * \function igraph_haskell_attribute_EABV
 * Query a boolean edge attribute for many edges
 *
 * \param graph The input graph.
 * \param name The name of the attribute.
 * \param eids The edges to query.
 * \param result Pointer to an initialized boolean vector, the result is
 *    stored here. It will be resized, if needed.
 * \return Error code.
 *
 * Time complexity: O(e), where e is the number of edges in 'eids'.
 */

int igraph_haskell_attribute_EABV(const igraph_t *graph, const char *name,
			   igraph_es_t eids, igraph_vector_bool_t *result) {

  return igraph_haskell_attribute_get_bool_edge_attr(graph, name, eids,
						result);
}

/**
 * \function igraph_haskell_attribute_VASV
 * Query a string vertex attribute for many vertices
 *
 * \param graph The input graph.
 * \param name The name of the attribute.
 * \param vids The vertices to query.
 * \param result Pointer to an initialized string vector, the result
 *     is stored here. It will be resized, if needed.
 * \return Error code.
 *
 * Time complexity: O(v), where v is the number of vertices in 'vids'.
 * (We assume that the string attributes have a bounded length.)
 */

int igraph_haskell_attribute_VASV(const igraph_t *graph, const char *name,
			   igraph_vs_t vids, igraph_strvector_t *result) {

  return igraph_haskell_attribute_get_string_vertex_attr(graph, name, vids,
						    result);
}

/**
 * \function igraph_haskell_attribute_EASV
 * Query a string edge attribute for many edges
 *
 * \param graph The input graph.
 * \param name The name of the attribute.
 * \param vids The edges to query.
 * \param result Pointer to an initialized string vector, the result
 *     is stored here. It will be resized, if needed.
 * \return Error code.
 *
 * Time complexity: O(e), where e is the number of edges in
 * 'eids'. (We assume that the string attributes have a bounded length.)
 */

int igraph_haskell_attribute_EASV(const igraph_t *graph, const char *name,
			   igraph_es_t eids, igraph_strvector_t *result) {

  return igraph_haskell_attribute_get_string_edge_attr(graph, name, eids,
						   result);
}

/**
 * \function igraph_haskell_attribute_list
 * List all attributes
 *
 * See \ref igraph_attribute_type_t for the various attribute types.
 * \param graph The input graph.
 * \param gnames String vector, the names of the graph attributes.
 * \param gtypes Numeric vector, the types of the graph attributes.
 * \param vnames String vector, the names of the vertex attributes.
 * \param vtypes Numeric vector, the types of the vertex attributes.
 * \param enames String vector, the names of the edge attributes.
 * \param etypes Numeric vector, the types of the edge attributes.
 * \return Error code.
 *
 * Naturally, the string vector with the attribute names and the
 * numeric vector with the attribute types are in the right order,
 * i.e. the first name corresponds to the first type, etc.
 *
 * Time complexity: O(Ag+Av+Ae), the number of all attributes.
 */
int igraph_haskell_attribute_list(const igraph_t *graph,
			   igraph_strvector_t *gnames, igraph_vector_t *gtypes,
			   igraph_strvector_t *vnames, igraph_vector_t *vtypes,
			   igraph_strvector_t *enames, igraph_vector_t *etypes) {
  return igraph_haskell_attribute_get_info(graph, gnames, gtypes, vnames, vtypes,
				      enames, etypes);
}

/**
 * \function igraph_haskell_attribute_has_attr
 * Checks whether a (graph, vertex or edge) attribute exists
 *
 * \param graph The graph.
 * \param type The type of the attribute, \c IGRAPH_ATTRIBUTE_GRAPH,
 *        \c IGRAPH_ATTRIBUTE_VERTEX or \c IGRAPH_ATTRIBUTE_EDGE.
 * \param name Character constant, the name of the attribute.
 * \return Logical value, TRUE if the attribute exists, FALSE otherwise.
 *
 * Time complexity: O(A), the number of (graph, vertex or edge)
 * attributes, assuming attribute names are not too long.
igraph_bool_t igraph_haskell_attribute_has_attr(const igraph_t *graph,
					 igraph_attribute_elemtype_t type,
					 const char *name) {
  return igraph_haskell_attribute_has_attr(graph, type, name);
}
 */

/**
 * \function igraph_haskell_attribute_GAN_set
 * Set a numeric graph attribute
 *
 * \param graph The graph.
 * \param name Name of the graph attribute. If there is no such
 *   attribute yet, then it will be added.
 * \param value The (new) value of the graph attribute.
 * \return Error code.
 *
 * \se \ref SETGAN if you want to type less.
 *
 * Time complexity: O(1).
 */
int igraph_haskell_attribute_GAN_set(igraph_t *graph, const char *name,
			      igraph_real_t value) {
  IGRAPH_ERROR("Not implemented", IGRAPH_ENOMEM);
  return 1;
}

/**
 * \function igraph_haskell_attribute_GAB_set
 * Set a boolean graph attribute
 *
 * \param graph The graph.
 * \param name Name of the graph attribute. If there is no such
 *   attribute yet, then it will be added.
 * \param value The (new) value of the graph attribute.
 * \return Error code.
 *
 * \se \ref SETGAN if you want to type less.
 *
 * Time complexity: O(1).
 */
int igraph_haskell_attribute_GAB_set(igraph_t *graph, const char *name,
			      igraph_bool_t value) {
  IGRAPH_ERROR("Not implemented", IGRAPH_ENOMEM);
  return 1;
}

/**
 * \function igraph_haskell_attribute_GAS_set
 * Set a string graph attribute.
 *
 * \param graph The graph.
 * \param name Name of the graph attribute. If there is no such
 *   attribute yet, then it will be added.
 * \param value The (new) value of the graph attribute. It will be
 *   copied.
 * \return Error code.
 *
 * \se \ref SETGAS if you want to type less.
 *
 * Time complexity: O(1).
 */
int igraph_haskell_attribute_GAS_set(igraph_t *graph, const char *name,
			      const bytestring_t *value) {

  igraph_haskell_attributes_t *attr=graph->attr;
  igraph_vector_ptr_t *gal=&attr->gal;
  long int j;
  igraph_bool_t l=igraph_haskell_attribute_find(gal, name, &j);

  if (l) {
    igraph_attribute_record_t *rec=VECTOR(*gal)[j];
    if (rec->type != IGRAPH_ATTRIBUTE_STRING) {
      IGRAPH_ERROR("Invalid attribute type", IGRAPH_EINVAL);
    } else {
      bsvector_t *str=(bsvector_t*)rec->value;
      IGRAPH_CHECK(bsvector_set(str, 0, value));
    }
  } else {
    igraph_attribute_record_t *rec=igraph_Calloc(1, igraph_attribute_record_t);
    bsvector_t *str;
    if (!rec) {
      IGRAPH_ERROR("Cannot add graph attribute", IGRAPH_ENOMEM);
    }
    IGRAPH_FINALLY(igraph_free, rec);
    rec->name=strdup(name);
    if (!rec->name) {
      IGRAPH_ERROR("Cannot add graph attribute", IGRAPH_ENOMEM);
    }
    IGRAPH_FINALLY(igraph_free, (char*)rec->name);
    rec->type=IGRAPH_ATTRIBUTE_STRING;
    str=igraph_Calloc(1, bsvector_t);
    if (!str) {
      IGRAPH_ERROR("Cannot add graph attribute", IGRAPH_ENOMEM);
    }
    IGRAPH_FINALLY(igraph_free, str);
    BSVECTOR_INIT_FINALLY(str, 1);
    IGRAPH_CHECK(bsvector_set(str, 0, value));
    rec->value=str;
    IGRAPH_CHECK(igraph_vector_ptr_push_back(gal, rec));
    IGRAPH_FINALLY_CLEAN(4);
  }

  return 0;
}

/**
 * \function igraph_haskell_attribute_VAN_set
 * Set a numeric vertex attribute
 *
 * The attribute will be added if not present already. If present it
 * will be overwritten. The same \p value is set for all vertices
 * included in \p vid.
 * \param graph The graph.
 * \param name Name of the attribute.
 * \param vid Vertices for which to set the attribute.
 * \param value The (new) value of the attribute.
 * \return Error code.
 *
 * \sa \ref SETVAN for a simpler way.
 *
 * Time complexity: O(n), the number of vertices if the attribute is
 * new, O(|vid|) otherwise.
 */
int igraph_haskell_attribute_VAN_set(igraph_t *graph, const char *name,
			      igraph_integer_t vid, igraph_real_t value) {
  IGRAPH_ERROR("Not implemented", IGRAPH_ENOMEM);
  return 1;
}

/**
 * \function igraph_haskell_attribute_VAB_set
 * Set a boolean vertex attribute
 *
 * The attribute will be added if not present already. If present it
 * will be overwritten. The same \p value is set for all vertices
 * included in \p vid.
 * \param graph The graph.
 * \param name Name of the attribute.
 * \param vid Vertices for which to set the attribute.
 * \param value The (new) value of the attribute.
 * \return Error code.
 *
 * \sa \ref SETVAB for a simpler way.
 *
 * Time complexity: O(n), the number of vertices if the attribute is
 * new, O(|vid|) otherwise.
 */
int igraph_haskell_attribute_VAB_set(igraph_t *graph, const char *name,
			      igraph_integer_t vid, igraph_bool_t value) {
  IGRAPH_ERROR("Not implemented", IGRAPH_ENOMEM);
  return 1;
}

/**
 * \function igraph_haskell_attribute_VAS_set
 * Set a string vertex attribute
 *
 * The attribute will be added if not present already. If present it
 * will be overwritten. The same \p value is set for all vertices
 * included in \p vid.
 * \param graph The graph.
 * \param name Name of the attribute.
 * \param vid Vertices for which to set the attribute.
 * \param value The (new) value of the attribute.
 * \return Error code.
 *
 * \sa \ref SETVAS for a simpler way.
 *
 * Time complexity: O(n*l), n is the number of vertices, l is the
 * length of the string to set. If the attribute if not new then only
 * O(|vid|*l).
 */
int igraph_haskell_attribute_VAS_set(igraph_t *graph, const char *name,
			      igraph_integer_t vid, const bytestring_t *value) {

  igraph_haskell_attributes_t *attr=graph->attr;
  igraph_vector_ptr_t *val=&attr->val;
  long int j;
  igraph_bool_t l=igraph_haskell_attribute_find(val, name, &j);

  if (l) {
    igraph_attribute_record_t *rec=VECTOR(*val)[j];
    if (rec->type != IGRAPH_ATTRIBUTE_STRING) {
      IGRAPH_ERROR("Invalid attribute type", IGRAPH_EINVAL);
    } else {
      bsvector_t *str=(bsvector_t*)rec->value;
      IGRAPH_CHECK(bsvector_set(str, vid, value));
    }
  } else {
    igraph_attribute_record_t *rec=igraph_Calloc(1, igraph_attribute_record_t);
    bsvector_t *str;
    if (!rec) {
      IGRAPH_ERROR("Cannot add vertex attribute", IGRAPH_ENOMEM);
    }
    IGRAPH_FINALLY(igraph_free, rec);
    rec->name=strdup(name);
    if (!rec->name) {
      IGRAPH_ERROR("Cannot add vertex attribute", IGRAPH_ENOMEM);
    }
    IGRAPH_FINALLY(igraph_free, (char*)rec->name);
    rec->type=IGRAPH_ATTRIBUTE_STRING;
    str=igraph_Calloc(1, bsvector_t);
    if (!str) {
      IGRAPH_ERROR("Cannot add vertex attribute", IGRAPH_ENOMEM);
    }
    IGRAPH_FINALLY(igraph_free, str);
    BSVECTOR_INIT_FINALLY(str, igraph_vcount(graph));
    IGRAPH_CHECK(bsvector_set(str, vid, value));
    rec->value=str;
    IGRAPH_CHECK(igraph_vector_ptr_push_back(val, rec));
    IGRAPH_FINALLY_CLEAN(4);
  }

  return 0;
}

/**
 * \function igraph_haskell_attribute_EAN_set
 * Set a numeric edge attribute
 *
 * The attribute will be added if not present already. If present it
 * will be overwritten. The same \p value is set for all edges
 * included in \p vid.
 * \param graph The graph.
 * \param name Name of the attribute.
 * \param eid Edges for which to set the attribute.
 * \param value The (new) value of the attribute.
 * \return Error code.
 *
 * \sa \ref SETEAN for a simpler way.
 *
 * Time complexity: O(e), the number of edges if the attribute is
 * new, O(|eid|) otherwise.
 */
int igraph_haskell_attribute_EAN_set(igraph_t *graph, const char *name,
			      igraph_integer_t eid, igraph_real_t value) {
  IGRAPH_ERROR("Not implemented", IGRAPH_ENOMEM);
  return 1;
}

/**
 * \function igraph_haskell_attribute_EAB_set
 * Set a boolean edge attribute
 *
 * The attribute will be added if not present already. If present it
 * will be overwritten. The same \p value is set for all edges
 * included in \p vid.
 * \param graph The graph.
 * \param name Name of the attribute.
 * \param eid Edges for which to set the attribute.
 * \param value The (new) value of the attribute.
 * \return Error code.
 *
 * \sa \ref SETEAB for a simpler way.
 *
 * Time complexity: O(e), the number of edges if the attribute is
 * new, O(|eid|) otherwise.
 */
int igraph_haskell_attribute_EAB_set(igraph_t *graph, const char *name,
			      igraph_integer_t eid, igraph_bool_t value) {
  IGRAPH_ERROR("Not implemented", IGRAPH_ENOMEM);
  return 1;
}

/**
 * \function igraph_haskell_attribute_EAS_set
 * Set a string edge attribute
 *
 * The attribute will be added if not present already. If present it
 * will be overwritten. The same \p value is set for all edges
 * included in \p vid.
 * \param graph The graph.
 * \param name Name of the attribute.
 * \param eid Edges for which to set the attribute.
 * \param value The (new) value of the attribute.
 * \return Error code.
 *
 * \sa \ref SETEAS for a simpler way.
 *
 * Time complexity: O(e*l), n is the number of edges, l is the
 * length of the string to set. If the attribute if not new then only
 * O(|eid|*l).
 */
int igraph_haskell_attribute_EAS_set(igraph_t *graph, const char *name,
			      igraph_integer_t eid, const bytestring_t *value) {

  igraph_haskell_attributes_t *attr=graph->attr;
  igraph_vector_ptr_t *eal=&attr->eal;
  long int j;
  igraph_bool_t l=igraph_haskell_attribute_find(eal, name, &j);

  if (l) {
    igraph_attribute_record_t *rec=VECTOR(*eal)[j];
    if (rec->type != IGRAPH_ATTRIBUTE_STRING) {
      IGRAPH_ERROR("Invalid attribute type", IGRAPH_EINVAL);
    } else {
      bsvector_t *str=(bsvector_t*)rec->value;
      IGRAPH_CHECK(bsvector_set(str, eid, value));
    }
  } else {
    igraph_attribute_record_t *rec=igraph_Calloc(1, igraph_attribute_record_t);
    bsvector_t *str;
    if (!rec) {
      IGRAPH_ERROR("Cannot add edge attribute", IGRAPH_ENOMEM);
    }
    IGRAPH_FINALLY(igraph_free, rec);
    rec->name=strdup(name);
    if (!rec->name) {
      IGRAPH_ERROR("Cannot add edge attribute", IGRAPH_ENOMEM);
    }
    IGRAPH_FINALLY(igraph_free, (char*)rec->name);
    rec->type=IGRAPH_ATTRIBUTE_STRING;
    str=igraph_Calloc(1, bsvector_t);
    if (!str) {
      IGRAPH_ERROR("Cannot add edge attribute", IGRAPH_ENOMEM);
    }
    IGRAPH_FINALLY(igraph_free, str);
    BSVECTOR_INIT_FINALLY(str, igraph_ecount(graph));
    IGRAPH_CHECK(bsvector_set(str, eid, value));
    rec->value=str;
    IGRAPH_CHECK(igraph_vector_ptr_push_back(eal, rec));
    IGRAPH_FINALLY_CLEAN(4);
  }

  return 0;
}

/**
 * \function igraph_haskell_attribute_VAN_setv
 * Set a numeric vertex attribute for all vertices.
 *
 * The attribute will be added if not present yet.
 * \param graph The graph.
 * \param name Name of the attribute.
 * \param v The new attribute values. The length of this vector must
 *   match the number of vertices.
 * \return Error code.
 *
 * \sa \ref SETVANV for a simpler way.
 *
 * Time complexity: O(n), the number of vertices.
 */

int igraph_haskell_attribute_VAN_setv(igraph_t *graph, const char *name,
			       const igraph_vector_t *v) {
  IGRAPH_ERROR("Not implemented", IGRAPH_ENOMEM);
  return 1;
}
/**
 * \function igraph_haskell_attribute_VAB_setv
 * Set a boolean vertex attribute for all vertices.
 *
 * The attribute will be added if not present yet.
 * \param graph The graph.
 * \param name Name of the attribute.
 * \param v The new attribute values. The length of this boolean vector must
 *   match the number of vertices.
 * \return Error code.
 *
 * \sa \ref SETVANV for a simpler way.
 *
 * Time complexity: O(n), the number of vertices.
 */

int igraph_haskell_attribute_VAB_setv(igraph_t *graph, const char *name,
			       const igraph_vector_bool_t *v) {
  IGRAPH_ERROR("Not implemented", IGRAPH_ENOMEM);
  return 1;
}

/**
 * \function igraph_haskell_attribute_VAS_setv
 * Set a string vertex attribute for all vertices.
 *
 * The attribute will be added if not present yet.
 * \param graph The graph.
 * \param name Name of the attribute.
 * \param sv String vector, the new attribute values. The length of this vector must
 *   match the number of vertices.
 * \return Error code.
 *
 * \sa \ref SETVASV for a simpler way.
 *
 * Time complexity: O(n+l), n is the number of vertices, l is the
 * total length of the strings.
 */
int igraph_haskell_attribute_VAS_setv(igraph_t *graph, const char *name,
			       const bsvector_t *sv) {

  igraph_haskell_attributes_t *attr=graph->attr;
  igraph_vector_ptr_t *val=&attr->val;
  long int j;
  igraph_bool_t l=igraph_haskell_attribute_find(val, name, &j);

  /* Check length first */
  if (bsvector_size(sv) != igraph_vcount(graph)) {
    IGRAPH_ERROR("Invalid vertex attribute vector length", IGRAPH_EINVAL);
  }

  if (l) {
    /* Already present, check type */
    igraph_attribute_record_t *rec=VECTOR(*val)[j];
    bsvector_t *str=(bsvector_t *)rec->value;
    if (rec->type != IGRAPH_ATTRIBUTE_STRING) {
      IGRAPH_ERROR("Attribute type mismatch", IGRAPH_EINVAL);
    }
    bsvector_clear(str);
    IGRAPH_CHECK(bsvector_append(str, sv));
  } else {
    /* Add it */
    igraph_attribute_record_t *rec=igraph_Calloc(1, igraph_attribute_record_t);
    bsvector_t *str;
    if (!rec) {
      IGRAPH_ERROR("Cannot add vertex attribute", IGRAPH_ENOMEM);
    }
    IGRAPH_FINALLY(igraph_free, rec);
    rec->type=IGRAPH_ATTRIBUTE_STRING;
    rec->name=strdup(name);
    if (!rec->name) {
      IGRAPH_ERROR("Cannot add vertex attribute", IGRAPH_ENOMEM);
    }
    IGRAPH_FINALLY(igraph_free, (char*)rec->name);
    str=igraph_Calloc(1, bsvector_t);
    if (!str) {
      IGRAPH_ERROR("Cannot add vertex attribute", IGRAPH_ENOMEM);
    }
    IGRAPH_FINALLY(igraph_free, str);
    rec->value=str;
    IGRAPH_CHECK(bsvector_copy(str, sv));
    IGRAPH_FINALLY(bsvector_destroy, str);
    IGRAPH_CHECK(igraph_vector_ptr_push_back(val, rec));
    IGRAPH_FINALLY_CLEAN(4);
  }

  return 0;
}

/**
 * \function igraph_haskell_attribute_EAN_setv
 * Set a numeric edge attribute for all vertices.
 *
 * The attribute will be added if not present yet.
 * \param graph The graph.
 * \param name Name of the attribute.
 * \param v The new attribute values. The length of this vector must
 *   match the number of edges.
 * \return Error code.
 *
 * \sa \ref SETEANV for a simpler way.
 *
 * Time complexity: O(e), the number of edges.
 */
int igraph_haskell_attribute_EAN_setv(igraph_t *graph, const char *name,
			       const igraph_vector_t *v) {
  IGRAPH_ERROR("Not implemented", IGRAPH_ENOMEM);
  return 1;
}

/**
 * \function igraph_haskell_attribute_EAB_setv
 * Set a boolean edge attribute for all vertices.
 *
 * The attribute will be added if not present yet.
 * \param graph The graph.
 * \param name Name of the attribute.
 * \param v The new attribute values. The length of this vector must
 *   match the number of edges.
 * \return Error code.
 *
 * \sa \ref SETEABV for a simpler way.
 *
 * Time complexity: O(e), the number of edges.
 */
int igraph_haskell_attribute_EAB_setv(igraph_t *graph, const char *name,
			       const igraph_vector_bool_t *v) {
  IGRAPH_ERROR("Not implemented", IGRAPH_ENOMEM);
  return 1;
}

/**
 * \function igraph_haskell_attribute_EAS_setv
 * Set a string edge attribute for all vertices.
 *
 * The attribute will be added if not present yet.
 * \param graph The graph.
 * \param name Name of the attribute.
 * \param sv String vector, the new attribute values. The length of this vector must
 *   match the number of edges.
 * \return Error code.
 *
 * \sa \ref SETEASV for a simpler way.
 *
 * Time complexity: O(e+l), e is the number of edges, l is the
 * total length of the strings.
 */
int igraph_haskell_attribute_EAS_setv(igraph_t *graph, const char *name,
			       const bsvector_t *sv) {

  igraph_haskell_attributes_t *attr=graph->attr;
  igraph_vector_ptr_t *eal=&attr->eal;
  long int j;
  igraph_bool_t l=igraph_haskell_attribute_find(eal, name, &j);

  /* Check length first */
  if (bsvector_size(sv) != igraph_ecount(graph)) {
    IGRAPH_ERROR("Invalid edge attribute vector length", IGRAPH_EINVAL);
  }

  if (l) {
    /* Already present, check type */
    igraph_attribute_record_t *rec=VECTOR(*eal)[j];
    bsvector_t *str=(bsvector_t *)rec->value;
    if (rec->type != IGRAPH_ATTRIBUTE_STRING) {
      IGRAPH_ERROR("Attribute type mismatch", IGRAPH_EINVAL);
    }
    bsvector_clear(str);
    IGRAPH_CHECK(bsvector_append(str, sv));
  } else {
    /* Add it */
    igraph_attribute_record_t *rec=igraph_Calloc(1, igraph_attribute_record_t);
    bsvector_t *str;
    if (!rec) {
      IGRAPH_ERROR("Cannot add vertex attribute", IGRAPH_ENOMEM);
    }
    IGRAPH_FINALLY(igraph_free, rec);
    rec->type=IGRAPH_ATTRIBUTE_STRING;
    rec->name=strdup(name);
    if (!rec->name) {
      IGRAPH_ERROR("Cannot add vertex attribute", IGRAPH_ENOMEM);
    }
    IGRAPH_FINALLY(igraph_free, (char*)rec->name);
    str=igraph_Calloc(1, bsvector_t);
    if (!str) {
      IGRAPH_ERROR("Cannot add vertex attribute", IGRAPH_ENOMEM);
    }
    IGRAPH_FINALLY(igraph_free, str);
    rec->value=str;
    IGRAPH_CHECK(bsvector_copy(str, sv));
    IGRAPH_FINALLY(bsvector_destroy, str);
    IGRAPH_CHECK(igraph_vector_ptr_push_back(eal, rec));
    IGRAPH_FINALLY_CLEAN(4);
  }

  return 0;
}

void igraph_haskell_attribute_free_rec(igraph_attribute_record_t *rec) {

  if (rec->type==IGRAPH_ATTRIBUTE_NUMERIC) {
    igraph_vector_t *num=(igraph_vector_t*)rec->value;
    igraph_vector_destroy(num);
  } else if (rec->type==IGRAPH_ATTRIBUTE_STRING) {
    bsvector_t *str=(bsvector_t*)rec->value;
    bsvector_destroy(str);
  } else if (rec->type==IGRAPH_ATTRIBUTE_BOOLEAN) {
    igraph_vector_bool_t *boolvec=(igraph_vector_bool_t*)rec->value;
    igraph_vector_bool_destroy(boolvec);
  }
  igraph_Free(rec->name);
  igraph_Free(rec->value);
  igraph_Free(rec);
}

/**
 * \function igraph_haskell_attribute_remove_g
 * Remove a graph attribute
 *
 * \param graph The graph object.
 * \param name Name of the graph attribute to remove.
 *
 * \sa \ref DELGA for a simpler way.
 *
 */
void igraph_haskell_attribute_remove_g(igraph_t *graph, const char *name) {

  igraph_haskell_attributes_t *attr=graph->attr;
  igraph_vector_ptr_t *gal=&attr->gal;
  long int j;
  igraph_bool_t l=igraph_haskell_attribute_find(gal, name, &j);

  if (l) {
    igraph_haskell_attribute_free_rec(VECTOR(*gal)[j]);
    igraph_vector_ptr_remove(gal, j);
  } else {
    IGRAPH_WARNING("Cannot remove non-existent graph attribute");
  }
}

/**
 * \function igraph_haskell_attribute_remove_v
 * Remove a vertex attribute
 *
 * \param graph The graph object.
 * \param name Name of the vertex attribute to remove.
 *
 * \sa \ref DELVA for a simpler way.
 *
 */
void igraph_haskell_attribute_remove_v(igraph_t *graph, const char *name) {

  igraph_haskell_attributes_t *attr=graph->attr;
  igraph_vector_ptr_t *val=&attr->val;
  long int j;
  igraph_bool_t l=igraph_haskell_attribute_find(val, name, &j);

  if (l) {
    igraph_haskell_attribute_free_rec(VECTOR(*val)[j]);
    igraph_vector_ptr_remove(val, j);
  } else {
    IGRAPH_WARNING("Cannot remove non-existent graph attribute");
  }
}

/**
 * \function igraph_haskell_attribute_remove_e
 * Remove an edge attribute
 *
 * \param graph The graph object.
 * \param name Name of the edge attribute to remove.
 *
 * \sa \ref DELEA for a simpler way.
 *
 */
void igraph_haskell_attribute_remove_e(igraph_t *graph, const char *name) {

  igraph_haskell_attributes_t *attr=graph->attr;
  igraph_vector_ptr_t *eal=&attr->eal;
  long int j;
  igraph_bool_t l=igraph_haskell_attribute_find(eal, name, &j);

  if (l) {
    igraph_haskell_attribute_free_rec(VECTOR(*eal)[j]);
    igraph_vector_ptr_remove(eal, j);
  } else {
    IGRAPH_WARNING("Cannot remove non-existent graph attribute");
  }
}

/**
 * \function igraph_haskell_attribute_remove_all
 * Remove all graph/vertex/edge attributes
 *
 * \param graph The graph object.
 * \param g Boolean, whether to remove graph attributes.
 * \param v Boolean, whether to remove vertex attributes.
 * \param e Boolean, whether to remove edge attributes.
 *
 * \sa \ref DELGAS, \ref DELVAS, \ref DELEAS, \ref DELALL for simpler
 * ways.
 */
void igraph_haskell_attribute_remove_all(igraph_t *graph, igraph_bool_t g,
				  igraph_bool_t v, igraph_bool_t e) {

  igraph_haskell_attributes_t *attr=graph->attr;

  if (g) {
    igraph_vector_ptr_t *gal=&attr->gal;
    long int i, n=igraph_vector_ptr_size(gal);
    for (i=0;i<n;i++) {
      igraph_haskell_attribute_free_rec(VECTOR(*gal)[i]);
    }
    igraph_vector_ptr_clear(gal);
  }
  if (v) {
    igraph_vector_ptr_t *val=&attr->val;
    long int i, n=igraph_vector_ptr_size(val);
    for (i=0;i<n;i++) {
      igraph_haskell_attribute_free_rec(VECTOR(*val)[i]);
    }
    igraph_vector_ptr_clear(val);
  }
  if (e) {
    igraph_vector_ptr_t *eal=&attr->eal;
    long int i, n=igraph_vector_ptr_size(eal);
    for (i=0;i<n;i++) {
      igraph_haskell_attribute_free_rec(VECTOR(*eal)[i]);
    }
    igraph_vector_ptr_clear(eal);
  }
}
