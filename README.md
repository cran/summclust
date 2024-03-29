
<!-- README.md is generated from README.Rmd. Please edit that file -->

# summclust

<!-- badges: start -->

[![R-CMD-check](https://github.com/s3alfisc/summclust/workflows/R-CMD-check/badge.svg)](https://github.com/s3alfisc/summclust/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/summclust)](https://CRAN.R-project.org/package=summclust)
<!-- [![pkgcheck](https://github.com/s3alfisc/summclust/workflows/pkgcheck/badge.svg)](https://github.com/s3alfisc/summclust/actions?query=workflow%3Apkgcheck) -->
![runiverse-package](https://s3alfisc.r-universe.dev/badges/summclust)
[![](http://cranlogs.r-pkg.org/badges/grand-total/summclust?color=blue)](https://cran.r-project.org/package=summclust)
[![](http://cranlogs.r-pkg.org/badges/last-month/summclust?color=green)](https://cran.r-project.org/package=summclust)
[![Codecov test
coverage](https://codecov.io/gh/s3alfisc/summclust/branch/main/graph/badge.svg)](https://app.codecov.io/gh/s3alfisc/summclust?branch=main)
<!-- badges: end -->

`{summclust}` is an R module for cluster level measures of leverage and
influence, and further implements CRV3 and CRV3J cluster robust variance
estimators.

For an introduction to the package, take a look at its
[vignette](https://s3alfisc.github.io/summclust/articles/summclust.html).

For a quick overview of different CRV estimators, take a look at the
[cluster robust variance estimation
vignette](https://s3alfisc.github.io/summclust/articles/Cluster-Robust-Variance-Estimators-CRV-1-3.html).

For a very detailed description of the implemented methods, in
particular a discussion of the different leverage and influence metrics,
see:

[MacKinnon, J.G., Nielsen, M.Ø., Webb, M.D., 2022. Leverage, influence,
and the jackknife in clustered regression models: Reliable inference
using summclust](https://arxiv.org/abs/2205.03288). QED Working Paper
1483. Queen’s University.

For the Stata version of the package, see
[here](https://github.com/mattdwebb/summclust).

## Installation

You can install the development version of summclust from CRAN,
[GitHub](https://github.com/) and [r-universe](https://r-universe.dev/)
with:

``` r
# install from CRAN
install.packages('summclust')

# from r-universe (windows & mac, compiled R > 4.0 required)
install.packages('summclust', repos ='https://s3alfisc.r-universe.dev')

# install.packages("devtools")
devtools::install_github("s3alfisc/summclust")
```

<!-- ## Citation  -->
<!-- If you are in `R`, you can simply run the following command to get the BibTeX citation for `{summclust}`: -->
<!-- ```{r, warning = FALSE, message = FALSE} -->
<!-- citation("summclust") -->
<!-- ``` -->
<!-- Alternatively, you can cite the paper by MacKinnon, Nielsen and Webb (2022). In this case, I would be super happy if you mentioned (e.g. in a footnote) that you are using the r-version of `{summclust}`. -->
