#' Create an instance of an \emph{Active Config} \code{acon} class.
#'
#' The \code{acon} class represents a hierachical datastructure
#' containing key-value pairs at each level of the hierarchy, with
#' each value defined as R expressions that can reference keys from both the current
#' and upper levels, with the expression recomputed at each access to the key using
#' current values of other keys.
#'
#' The returned object is similar to a nested named list data structure
#' that can be used, for example, to describe configuration parameters
#' for a software. Such nested list configuration structures are often
#' represented by JSON files containing a single top-level JSON object
#' which in turn contains other JSON objects.
#'
#' However, the \strong{active} semantics of the \code{acon} class means that a value
#' assigned to any key in the config can be a R expression that is evaluated
#' every time this key is accessed. This is similar to the behavior
#' of the recusivley expanded variables in the
#' \href{https://www.gnu.org/software/make/manual/html_node/Flavors.html#Flavors}{Make utility}
#' Additionally, when another key symbol is used in the expression defining the
#' value, the key will be looked up in the contaiment hierarchy of the enclosing
#' config objects when the expression is evaluated.
#'
#' For example, if we create \code{conf <- acon(a=1,b=acon(c=a*2,d=acon(x=c*3))}, then
#' \code{conf$b$d$x} will return \code{6}. If we then create a modified copy of
#' \code{conf} as \code{conf1 <- acon(conf,b=acon(c=-2))}, then
#' \code{conf1$b$d$x} will return \code{-6}.
#'
#' Above in \code{b=acon(c=-2)}, we relied on one additional aspect of the default behaviour of the
#' \code{acon} constructor.
#' Specifically, because the name \code{b} already existed in the enclosing \code{acon} object and was already
#' assigned itself to an \code{acon} object, the new assignment extended the existing object
#' by modifying the value of \code{c} key instead of creating new \code{acon} instance. Therefore,
#' the resulting \code{acon} object assigned to \code{b} still contained unchanged copy of the \code{d}
#' key, in addition to a modified copy of \code{c}.
#'
#' The above example leads to the \strong{major intended use case} for the \code{acon} class: a software
#' pipeline can define
#' the default instance of a (possibly large and deeply nested) config, where values deeper in the
#' config hierarchy are defined as expressions computed from the values of the keys located higher up in the
#' hierarchy. The user, applying the pattern from the example above, can derive another instance of
#' the entire config by selectively modifying values at arbitrary levels of the hierarchy with
#' a minimal amount of code that only dereferences the keys to be modified. The config is then passed to
#' the pipeline that uses it to obtain the parameters. Other values which depend on the modified
#' values will be appropriately recomputed.
#'
#' The \code{acon} class is derived from the \code{\link[base]{environment}} and uses active bindings
#' as values for the config keys. The \code{acon} constructor always makes a deep copy when it extends
#' the existing \code{acon} object. For performance and portability reasons, we recommend that functions
#' consuming the resulting config data structure are to be written as expecting a hierarchy of standard
#' named lists. You should use \code{acon} calls to create the original active config and/or a modified
#' version of the active config,
#' and then "freeze" the final result with a call to \code{as.list} that has the S3 class method
#' \code{\link{as.list.acon}}. This method creates a hirarchical named list with the same structure
#' as a given active config, and with the values computed from the active config. Pass this list
#' to your consumer function.
#'
#' @param .with Existing \code{acon} object. Start constructing new object from a deep copy of this
#' object. If missing, start from an empty new object.
#' @param .update If TRUE, for any key that already exists and is \code{acon} itself,
#' and the new value is also \code{acon} call, update a copy of the existing object.
#' Otherwise, construct new \code{acon} object for the value.
#' @param .under Environment that will be set as a parent environment for the returned object.
#' @param ... Named arguments which will be interpreted as key-value pairs to populate the
#' return value (similar to a list constructor).
#'
#' @return \code{acon} object
#' @export
#'
#' @examples
#' z = acon(v=4,w=acon(x=v*2))
#' x = acon(s=1:4,
#'          f=function(x) sprintf("x is %s",x),
#'          t=acon(x=s*2,
#'                 z=acon(k=x*4)
#'          ),
#'          p = z
#' )
#' print(x$t$z$k)
#' y = acon(x,
#'          s=1:16,
#'          f=function(x) sprintf("y is %s",x),
#'          t=acon(l=10))
#' print(y$f(y$t$z$k))
acon <- function(.with=NULL,
                 .update=T,
                 .under=parent.frame(),...) {
  ## How this is designed:
  ## The general idea is to overload the `=` assignment operator.
  ## Since it is impossible in R, we instead emulate it by
  ## designing a special processing of the `=` when it is used
  ## to specify values for named arguments in a function call.
  ## this acon() constructor performs NSE of its argument list and
  ## returns a new environment object (S3 subclassed as `acon`).
  ## Any value expression that is itself a call to acon() is generated with a recursive call
  ## to acon(), but with the .under argument updated to point to the currently built
  ## environment (current acon object).
  ## Any other value expression is first evaluated anc checked if it is of type acon. If yes,
  ## the evaluated value is copied and placed under the current acon. If no, the name of
  ## the argument is made an active binding bound to a function that is constructed from the
  ## value expression.
  ## Thus, the returned result is a nested set of environments (acon objects), wit leaf values
  ## all being active bindings. When any of the active binding attributes is accessed, the
  ## corresponding expression will be evaluated, with names in the expression automatically being
  ## looked up the chain of nested environments until found.
  this_call = match.call()
  if(is.null(.with)) {
    env = new.env(parent=.under)
  }
  else {
    ## always make a deep copy of the .with environment, thus providing a "copy
    ## constructor" semantics
    env = copy_env(.with,deep = T,parent = .under)
  }
  class(env) <- append(class(env),"acon",0)
  #   print("DEBUG START")
  #   print(paste0(".parent= ",as.list(.under)," address_parent=",data.table::address(.under),
  #                " this_call=",paste(this_call,collapse = ",")," address_env=",data.table::address(env),
  #                " sys.calls=",paste(sys.calls(),collapse = "->")))
  #   print("DEBUG END")

  for(i in 2:length(this_call)) {
    var_name = names(this_call)[[i]]
    if(substring(var_name,1,1)!=".") {
      var_name_exists = exists(var_name, envir = env, inherits = FALSE)
      var_rhs = this_call[[i]]
      if(is.language(var_rhs) && (!is.symbol(var_rhs)) && var_rhs[[1]]=="acon") {
        ## Missing .update arg means T (the default, but default is not set in
        ## the unevaluated expression yet)
        update_ev = (is.null(var_rhs$.update) || eval(var_rhs$.update,env))
        ## if rhs is acon ctor expression, and the lhs already exists in the
        ## current object (inherited from .with), then the lhs is assumed to be
        ## the environment and is used as the .with argument to call the rhs ev ctor.
        if(!var_name_exists) {
          update_ev = F
        }
        if(update_ev) {
          var_rhs[[".with"]] = eval(parse(text=var_name),env)
        }
        var_rhs[[".under"]] = env
        ## simply evaluate the acon ctor call and assign
        val = eval(var_rhs,envir = env)
        rm_if_exists(var_name,env)
        env[[var_name]] = val
      }
      else {
        ## we have to eval in order to check the class of the resulting value
        val = eval(var_rhs,env)
        ## if rhs is acon object, it is copied and attached to the current object
        if(inherits(val,"acon")) {
          val = copy_env(val,deep = T,parent = env)
          rm_if_exists(var_name,env)
          env[[var_name]] = val
        }
        ## if not acon object or acon ctor expression, assign as active binding
        else {
          func_rhs = make_function(alist(), var_rhs, env) #.x =
          rm_if_exists(var_name,env)
          makeActiveBinding(var_name, func_rhs, env)
        }
      }
    }
  }
  env
}

#' Create a hierarchical named list with the same structure
#' as the provided \emph{Active Config} \code{\link{acon}} object,
#' and with the values computed from the active config.
#'
#' \strong{Note}: Active bindings are evaluated and replaced with the returned
#' values. Values that are functions will currently keep their original environment,
#' even when their environment is the \code{acon} object that is being converted here.
#'
#' @param x \code{\link{acon}} object
#'
#' @export
#'
#' @examples
#' z_acon = acon(v=4,w=acon(x=v*2))
#' z_acon
#' z_list = as.list(z_acon)
#' z_list
#' z_list$w$x == 8
as.list.acon <- function(x) {
  as_list_nested_env(x)
}

#' Print \code{\link{acon}} object
#'
#' @param x \code{\link{acon}} object
#' @param max.level maximum hierarchy level to print
#' @param ... other parameters passed to the print function
#'
#' @export
#'
print.acon <- function(x,max.level=100,...) {
  print_nested_env(x,max.level=max.level,...)
}

