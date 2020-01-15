#' Active Config Data Structure
#'
#' This package provides the \code{\link{acon}} class that implements a hierachical datastructure
#' containing key-value pairs at each level of the hierarchy, with
#' each value defined as R expressions that can reference keys from both the current
#' and upper levels, with the expression recomputed at each access to the key using
#' current values of other keys.
#'
#' The main entry point is the constructor of the \code{\link{acon}}
#' class.
#'
"_PACKAGE"
