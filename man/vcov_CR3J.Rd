% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vcov3.R
\name{vcov_CR3J}
\alias{vcov_CR3J}
\title{Compute CRV3 covariance matrices via a cluster
jackknife as described in MacKinnon, Nielsen & Webb
(2022)}
\usage{
vcov_CR3J(obj, ...)
}
\arguments{
\item{obj}{An object of class \code{lm} or \code{fixest}
computed?}

\item{...}{misc function argument}
}
\value{
An object of type 'vcov_CR3J'
}
\description{
Compute CRV3 covariance matrices via a cluster
jackknife as described in MacKinnon, Nielsen & Webb
(2022)
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

# CRV3 standard errors
vcov <- vcov_CR3J(
   obj = lm_fit,
   cluster = ~ind_code,
   type = "CRV3"
)

# CRV3 standard errors
vcovJN <- vcov_CR3J(
   obj = lm_fit,
   cluster = ~ind_code,
   type = "CRV3J",
)
}
}
}
\references{
MacKinnon, James G., Morten Ørregaard Nielsen, and Matthew D. Webb.
"Leverage, influence, and the jackknife in clustered regression models:
Reliable inference using summclust."
arXiv preprint arXiv:2205.03288 (2022).
}
\seealso{
\link[summclust]{vcov_CR3J.lm},
\link[summclust]{vcov_CR3J.fixest}
}
