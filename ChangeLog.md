Revision history for haskell-igraph
===================================

v0.4.0 -- 2018-04-20
-------------------

* A new attribute interface written in C. The graph attributes are now directly serialized into bytestring using "cereal" (before we used the `Show` instance).
