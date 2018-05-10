Revision history for haskell-igraph
===================================

v0.6.0 -- 2018-05-10
--------------------

* Breaking change: Drop `Graph` type class. Change `LGraph` and `MLGraph` to
`Graph` and `MGraph`. The new `Graph` and `MGraph` types are now dependently typed.

v0.5.0 -- 2018-04-25
--------------------

* Fix memory leaks.
* Interface change: `mapNodes`, `mapEdges`, `filterNodes`, `filterEdges` become
`nmap`, `emap`, `nfilter`, `efilter`.


v0.4.0 -- 2018-04-20
--------------------

* A new attribute interface written in C. The graph attributes are now directly serialized into bytestring using "cereal" (before we used the `Show` instance).
