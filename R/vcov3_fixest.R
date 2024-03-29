vcov_CR3J.fixest <- function(
    obj,
    cluster,
    type = "CRV3",
    return_all = FALSE,
    absorb_cluster_fixef = TRUE,
    ...
){

  #' Compute CRV3 covariance matrices via a cluster
  #' jackknife as described in MacKinnon, Nielsen & Webb
  #' (2022) for objects of type `fixest`
  #'
  #' @references
  #' MacKinnon, James G., Morten Ørregaard Nielsen, and Matthew D. Webb.
  #' "Leverage, influence, and the jackknife in clustered regression models:
  #' Reliable inference using summclust."
  #' arXiv preprint arXiv:2205.03288 (2022).
  #'
  #' @param obj An object of type fixest
  #' @param cluster A clustering vector
  #' @param absorb_cluster_fixef TRUE by default. Should the cluster fixed
  #'        effects be projected out? This increases numerical stability.
  #' @param type "CRV3" or "CRV3J" following MacKinnon, Nielsen & Webb.
  #' CRV3 by default
  #' @param return_all Logical scalar, FALSE by default. Should only
  #' the vcov be returned (FALSE) or additional results (TRUE)
  #' @param ... other function arguments passed to 'vcov'
  #' @method vcov_CR3J fixest
  #' @importFrom stats coef weights coefficients model.matrix
  #' @importFrom dreamerr check_arg
  #' @importFrom MASS ginv
  #' @importFrom stats expand.model.frame formula model.frame model.response na.pass pt qt reformulate
  #' @export
  #'
  #' @examples
  #'
  #' library(summclust)
  #' library(fixest)
  #' data(mtcars)
  #' mtcars
  #'
  #' fit <- feols(mpg ~ cyl + disp + hp, data = mtcars)
  #' summ <- vcov_CR3J(fit, cluster = ~carb)
  #'
  #'@return An object of class \code{vcov_CR3J}


  check_arg(return_all, "logical scalar")
  check_arg(cluster, "character scalar | formula")
  check_arg(type, "character scalar")
  check_arg(absorb_cluster_fixef, "logical scalar")


  if(obj$method != "feols"){
    cli::cli_abort(
      "'summclust' currently only works with estimation method 'feols'."
    )
  }

  call_env <- obj$call_env

  X <- model.matrix(obj, type = "rhs")
  y <- model.matrix(obj, type = "lhs")

  N <- nrow(X)
  # k: see below
  w <- weights(obj)

  # get the clustering variable

  if(!inherits(cluster, "formula")){
    cluster <- reformulate(cluster)
  }

  cluster_df <- get_cluster(
    object = obj,
    cluster = cluster,
    N = N,
    call_env = call_env
  )$cluster_df

  k <- obj$nparams

  # preprocess fixed effects
  has_fe <- length(obj$fixef_vars) > 0

  if(inherits(cluster, "formula")){
    cluster_char <- attr(terms(cluster), "term.labels")
  } else {
    cluster_char <- cluster
  }

  # add all fixed effects variables as dummies
  cluster_fixef_outprojected <- FALSE

  if(has_fe){

    fixef_vars <- obj$fixef_vars
    fe <- model_matrix.fixest(obj, type = "fixef")

    # if the clustering variable is a cluster fixed effect & if absorb_cluster_fixef == TRUE,
    # then demean X and y by the cluster fixed effect


    if(absorb_cluster_fixef && cluster_char %in% fixef_vars){

      cluster_fixef_outprojected <- TRUE

      fixef_vars_minus_cluster <- fixef_vars[cluster_char != fixef_vars]
      add_fe <- fe[,fixef_vars_minus_cluster, drop = FALSE]

      if(length(fixef_vars_minus_cluster) > 0){
        fml_fe <- reformulate(fixef_vars_minus_cluster, response = NULL)
        add_fe_dummies <- model.matrix(fml_fe, model.frame(fml_fe , data = as.data.frame(add_fe)))
        # drop the intercept
        add_fe_dummies <- add_fe_dummies[, -which(colnames(add_fe_dummies) =="(Intercept)")]
        X <- as.matrix(collapse::add_vars(as.data.frame(X), add_fe_dummies))
      }

      g <- collapse::GRP(cluster_df, call = FALSE)
      X <- collapse::fwithin(X, g, w = w)
      y <- collapse::fwithin(y, g, w = w)

    } else {

      add_fe <- fe[, fixef_vars, drop = FALSE]
      fml_fe <- reformulate(fixef_vars, response = NULL)
      add_fe_dummies <- model.matrix(fml_fe, model.frame(fml_fe , data = as.data.frame(add_fe)))
      # drop the intercept
      X <- as.matrix(collapse::add_vars(as.data.frame(X), add_fe_dummies))

    }

  }

  if(!is.null(w)){
    X <- sqrt(w) * X
    y <- sqrt(w) * y
  }


  if(cluster_fixef_outprojected){
    k <- ncol(X)
  } else {
    k <- obj$nparams
  }

  res <-
    cluster_jackknife(
      y = y,
      X = X,
      cluster_df = cluster_df,
      type = type
    )


  if(return_all == TRUE){
    res[["X"]] <- X
    res[["y"]] <- y
    res[["N"]] <- N
    res[["k"]] <- k
    res[["cluster_df"]] <- cluster_df
  } else {
    res <- res$vcov
  }

  class(res) <- "vcov_CR3J"


  res
}

