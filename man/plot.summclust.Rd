% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_summclust.R
\name{plot.summclust}
\alias{plot.summclust}
\title{Plotting method for objects of type \code{summclust}}
\usage{
\method{plot}{summclust}(x, ...)
}
\arguments{
\item{x}{An object of type \code{summclust}}

\item{...}{other optional function arguments}
}
\value{
A list containing
\item{residual_leverage}{A \code{ggplot} of the residual leverages}
\item{coef_leverage}{A \code{ggplot} of the coefficient leverages}
\item{coef_beta}{A \code{ggplot} of the leave-one-out cluster jackknife
regression coefficients}
}
\description{
Plots residual leverage, partial leverage and the
leave-one-cluster-out regression coefficients
}
\details{
Note that the function requires \code{ggplot2} to be installed.
}
\examples{

\donttest{
if(requireNamespace("summclust") && requireNamespace("haven")){

library(summclust)
library(haven)

nlswork <- read_dta("http://www.stata-press.com/data/r9/nlswork.dta")
# drop NAs at the moment
nlswork <- nlswork[, c("ln_wage", "grade", "age", "birth_yr", "union", "race", "msp", "ind_code")]
nlswork <- na.omit(nlswork)

lm_fit <- lm(
  ln_wage ~ union +  race + msp + as.factor(birth_yr) + as.factor(age) + as.factor(grade),
  data = nlswork)

res <- summclust(
   obj = lm_fit,
   params = c("msp", "union"),
   cluster = ~ind_code,
 )

 plot(res)
}
}

}
\references{
MacKinnon, James G., Morten Ørregaard Nielsen, and Matthew D. Webb.
"Leverage, influence, and the jackknife in clustered regression models:
Reliable inference using summclust."
arXiv preprint arXiv:2205.03288 (2022).
}