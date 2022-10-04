## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(summclust)

## ----example, warning = FALSE, message = FALSE, out.width="50%", out.height="50%"----
library(summclust)
library(lmtest)
library(haven)

nlswork <- read_dta("http://www.stata-press.com/data/r9/nlswork.dta")
# drop NAs at the moment
nlswork <- nlswork[, c("ln_wage", "grade", "age", "birth_yr", "union", "race", "msp", "ind_code")]
nlswork <- na.omit(nlswork)

lm_fit <- lm(
  ln_wage ~ as.factor(grade) + as.factor(age) + as.factor(birth_yr) + union +  race + msp,
  data = nlswork)

summclust_res <- summclust(
  obj = lm_fit,
  cluster = ~ind_code,
  params = c("msp", "union")
)

# CRV3-based inference - exactly matches output of summclust-stata
tidy(summclust_res)

summary(summclust_res)

## ---- warning = FALSE, message = FALSE, out.width="50%"-----------------------
plot(summclust_res)

## ---- warning = FALSE, message=FALSE------------------------------------------
library(lmtest)
library(fixest)

vcov3J <- 
vcov_CR3J(
  lm_fit, 
  cluster = ~ ind_code
)


all.equal(
  vcov3J, 
  summclust_res$vcov
)

df <- length(summclust_res$cluster) - 1

# with lmtest
CRV1 <- coeftest(lm_fit, sandwich::vcovCL(lm_fit, ~ind_code), df = df)
CRV3 <- coeftest(lm_fit, vcov3J, df = df)

CRV1[c("union", "race", "msp"),]
CRV3[c("union", "race", "msp"),]

confint(CRV1)[c("union", "race", "msp"),]
confint(CRV3)[c("union", "race", "msp"),]

# with fixest
feols_fit <- feols(
  ln_wage ~ as.factor(grade) + as.factor(age) + as.factor(birth_yr) + union +  race + msp,
  data = nlswork)

fixest::coeftable(
  feols_fit,
  vcov = summclust_res$vcov,
  ssc = ssc(adj = FALSE, cluster.adj = FALSE)
)[c("msp", "union", "race"),]


