# Active Config Data Structure - package aconr

The acon class provided by this package implements a hierachical datastructure
containing key-value pairs at each level of the hierarchy, with
each value defined as R expressions that can reference keys from both the current
and upper levels, with the expression recomputed at each access to the key using
current values of other keys.
