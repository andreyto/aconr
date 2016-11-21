# Converts objects of several kinds into an environment (possibly by reference).  Copied from package pryr.
to_env <- function(x, quiet = FALSE) {
    if (is.environment(x)) {
        x
    } else if (is.list(x)) {
        list2env(x)
    } else if (is.function(x)) {
        environment(x)
    } else if (length(x) == 1 && is.character(x)) {
        if (!quiet) 
            message("Using environment ", x)
        as.environment(x)
    } else if (length(x) == 1 && is.numeric(x) && x > 0) {
        if (!quiet) 
            message("Using environment ", search()[x])
        as.environment(x)
    } else {
        stop("Input can not be coerced to an environment", call. = FALSE)
    }
}

# Test if all elements of x are named. Copied from package pryr.
all_named <- function(x) {
    if (length(x) == 0) 
        return(TRUE)
    !is.null(names(x)) && all(names(x) != "")
}

# Constructs a function object from a body expression and a pairlist of arguments. Copied from the package
# pryr.  Examples: make_function(alist(x =), x**2, env) will create (function(x) x**2) and environment() of
# this function will be env.  make_function(alist(), x**2, env) will create (function() x**2), so that when
# the function is called, the value of x will be search in env and up the enclosing chain of environments.
make_function <- function(args, body, env = parent.frame()) {
    args <- as.pairlist(args)
    # stopifnot( all_named(args), is.language(body))
    env <- to_env(env)
    
    eval(call("function", args, body), env)
}

# remove the name from environment is the name exists
rm_if_exists <- function(name, env) {
    for (nm in name) {
        if (exists(nm, envir = env, inherits = FALSE)) {
            rm(list = nm, envir = env)
        }
    }
}
