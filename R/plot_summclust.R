plot.summclust <- function(x, ...) {

  #' Plotting method for objects of type `summclust`
  #'
  #' Plots residual leverage, partial leverage and the
  #' leave-one-cluster-out regression coefficients
  #'
  #' Note that the function requires `ggplot2` to be installed.
  #'
  #' @param x An object of type `summclust`
  #' @param ... other optional function arguments
  #' @export
  #' @method plot summclust
  #' @importFrom utils stack
  #'
  #' @examples
  #'
  #' library(summclust)
  #' data(mtcars)
  #' mtcars
  #'
  #' fit <- lm(mpg ~ cyl + disp + hp, data = mtcars)
  #' summ <- summclust(fit, params = ~cyl + disp, cluster = ~carb)
  #' summary(summ)
  #' tidy(summ)
  #' plot(summ)
  #'
  #'@references
  #' MacKinnon, James G., Morten Ørregaard Nielsen, and Matthew D. Webb.
  #' "Leverage, influence, and the jackknife in clustered regression models:
  #' Reliable inference using summclust."
  #' arXiv preprint arXiv:2205.03288 (2022).
  #'
  #' @return A list containing
  #' \item{residual_leverage}{A `ggplot` of the residual leverages}
  #' \item{coef_leverage}{A `ggplot` of the coefficient leverages}
  #' \item{coef_beta}{A `ggplot` of the leave-one-out cluster jackknife
  #' regression coefficients}



  if (requireNamespace("ggplot2")) {
    ggplot_installed <- TRUE
    if (requireNamespace("latex2exp")) {
      latex2exp_installed <- TRUE
    } else {
      cli::cli_abort(
        "Please install the 'latex2exp' package to use this function")
    }
  } else {
    cli::cli_abort(
      "Please install the 'ggplot2' package to use this function")
  }

  param <- x$params

  df <- data.frame(
    cluster = x$cluster,
    cluster_leverage = unlist(x$leverage_g) / x$leverage_avg
  )

  # plot residual leverage:

  residual_leverage <-
    ggplot2::ggplot(
      data = df,
      ggplot2::aes(
        x = cluster,
        y = cluster_leverage
      )
    ) +
    ggplot2::geom_hline(
      yintercept = 1,
      color = "red",
      linetype = "dotted"
    ) +
    ggplot2::theme_bw() +
    ggplot2::geom_point() +
    ggplot2::ylab(latex2exp::TeX(r'($L_{g} / mean(L_{g})$)')) +
    ggplot2::ggtitle("Residual Leverage")


  # coef leverage

  df <- as.data.frame(x$partial_leverage)

  df_long <- stack(df, select = colnames(df))
  names(df_long) <- c("values", "cluster")

  df_long$variable <- rep(rownames(df), length(param))

  G <- length(x$cluster)
  df_long$G <- G

  coef_leverage <-
    ggplot2::ggplot(
      data = df_long,
      ggplot2::aes(
        x = cluster,
        y = values
      )
    ) +
    ggplot2::facet_wrap(~variable) +
    ggplot2::ylab(latex2exp::TeX(r'($\hat{\L}_{j}^{g}$)')) +
    ggplot2::theme_bw() +
    ggplot2::geom_point() +
    ggplot2::geom_hline(
      data = df_long,
      ggplot2::aes(
        yintercept = 1 / G
      ),
      color = "red",
      linetype = "dotted"
    )


  # plot jackknife'd coefs

  df <- as.data.frame(x$beta_jack)

  if (!is.null(param)) {
    df <- df[rownames(df) %in% param, ]
  }

  df_long <- stack(df, select = colnames(df))
  names(df_long) <- c("values", "cluster")

  df_long$variable <- rep(rownames(df), length(param))
  for(var_name in unique(df_long$variable)){
    df_long[
      which(df_long$variable == var_name),
      "coef_estimate"
    ] <- x$coef_estimates[var_name]
  }

  coef_beta <-
    ggplot2::ggplot(
      data = df_long,
      ggplot2::aes(
        x = cluster,
        y = values
      )
    ) +
    ggplot2::facet_wrap(~variable) +
    ggplot2::ylab(
      latex2exp::TeX(r'($\hat{\beta}_{j}^{g}$)')
    ) +
    ggplot2::theme_bw() +
    ggplot2::geom_point() +
    ggplot2::geom_hline(
      data = df_long,
      ggplot2::aes(
        yintercept = coef_estimate
      ),
      color = "red",
      linetype = "dotted"
    )



  res <- list(
    residual_leverage = residual_leverage,
    coef_leverage = coef_leverage,
    coef_beta = coef_beta
  )

  res
}
