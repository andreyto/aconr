# Turns nested environments into nested lists A nested environment is a tree of environments where the tree
# relationship is defined by a parent.env() results.
as_list_nested_env <- function(x) {
    done_env_hash = new.env()

    as_list_nested_env_inner <- function(x) {
        stopifnot(is.environment(x) || is.list(x))
        y = list()
        x_names = names(x)  #for list elements w/o names, returns ''
        for (i_el in seq_along(x_names)) {
            if (is.environment(x))
                val = x[[x_names[[i_el]]]] else val = x[[i_el]]
            if (is.environment(val)) {
                val_key = data.table::address(val)
                if (is.null(done_env_hash[[val_key]])) {
                  done_env_hash[[val_key]] = T
                  val = as_list_nested_env_inner(val)
                }
            } else if (is.list(val)) {
                val = as_list_nested_env_inner(val)
            }
            y[[i_el]] = val
        }
        names(y) = names(x)
        return(y)
    }
    return(as_list_nested_env_inner(x))
}

# apply ls.str() to a nested environment structure
ls_str_nested_env <- function(x) {
    utils::ls.str(as_list_nested_env(x))
}

# print nested environment structure for debugging purposes by applying ls.str()
print_nested_env <- function(x, max.level = 100, ...) {
    print(ls_str_nested_env(x), max.level = max.level, ...)
}


#' Create a copy of a nested environment structure
#'
#' A nested environment is a tree of environments where the tree
#' relationship is defined by a predicate \code{identical(parent,parent.env(child))}.
#'
#' With the default arguments, this function creates a deep copy of
#' the input nested environment where the parent-child relationsip is
#' replicated between the newly created environments. Environments of
#' functions assigned to names in the environment, including active
#' bindings are also reset, but only if the original function's
#' environment is the same as the environment that holds the name.
#'
#' @param x Top environment in a nested environment structure
#' @param deep Create a deep copy
#' @param parent Place new top environment under this parent environment
#' @param func.update.parent Update environment of functions, including
#' functions executed by active bindings. Updating is only performed if
#' the function's environment is the environment in which the function is
#' stored.
#' @param deep.update.parent For each environment assigned to a name in
#' the original environment, change parent to a newly created copy of the
#' original parent if the parent was the environment holding the name
#' (in other words, reset parents in the environment tree).
#' @param into.env Instead of creating new top environment, copy all elements
#' from the original top environment into the existing environment pointed to
#' by this argument.
#' @param ... Passed to \code{\link[base]{list2env}} when the new environment
#' is created.
#'
#' @return Newly created environment
copy_env <- function(x, deep = F, parent = parent.env(x), func.update.parent = T, deep.update.parent = T, into.env = NULL,
    ...) {
    ## as.list copies active binding as functions; this allows us recreating them as active bindings in the new
    ## environment
    y = list2env(as.list.environment(x, all.names = TRUE), parent = parent, envir = into.env, ...)
    for (name in names(y)) {
        if (deep) {

            if (is.environment(y[[name]])) {
                y_sub = y[[name]]
                y_sub_par = parent.env(y_sub)
                if (deep.update.parent && identical(y_sub_par, x)) {
                  y_sub_par = y
                }
                y[[name]] = copy_env(y_sub, deep = deep, parent = y_sub_par, func.update.parent = func.update.parent,
                  deep.update.parent = deep.update.parent, ...)
            }
        }
        ## both plain functions and active bindings become functions in y
        if (is.function(y[[name]])) {
            y_sub = y[[name]]
            y_sub_par = environment(y_sub)
            ## update function environment if the original one points to the enclosing object
            if (func.update.parent && identical(y_sub_par, x)) {
                y_sub_par = y
                environment(y_sub) <- y_sub_par
            }
            ## additionally, if the original value was active binding, create active binding for the copied value
            if (bindingIsActive(name, x)) {
                rm(list = name, envir = y)
                makeActiveBinding(name, y_sub, y)
            } else {
                y[[name]] = y_sub
            }
        }
    }
    return(y)
}

