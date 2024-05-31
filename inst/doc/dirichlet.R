## ----setup, include=FALSE-----------------------------------------------------
set.seed(0)
library("hyper2")
options(rmarkdown.html_vignette.check_title = FALSE)
knitr::opts_chunk$set(echo = TRUE)
knit_print.function <- function(x, ...){dput(x)}
registerS3method(
  "knit_print", "function", knit_print.function,
  envir = asNamespace("knitr")
)

## ----out.width='20%', out.extra='style="float:right; padding:10px"',echo=FALSE----
knitr::include_graphics(system.file("help/figures/hyper2.png", package = "hyper2"))

## ----label=showdirichlet,comment=""-------------------------------------------
dirichlet

## ----dirichletcompleteFALSE---------------------------------------------------
dirichlet(c(a=4,b=5,c=3))

## ----dirichletabc-------------------------------------------------------------
(L1 <- dirichlet(c(a=4,b=5,c=3)))

## ----dirichletbcd-------------------------------------------------------------
(L2 <- dirichlet(c(b=1,c=1,d=8)))

## ----sumofL1andL2,cache=TRUE--------------------------------------------------
L1+L2
maxp(L1+L2)

## ----fourwayobs---------------------------------------------------------------
(L3 <- dirichlet(c(a=4,b=3,c=2,d=6)))
L1+L2+L3

## -----------------------------------------------------------------------------
(L4 <- dirichlet(c(a=5,b=3,c=0,d=3)))

## ----label=L4testspec,cache=TRUE----------------------------------------------
maxp(L4)

## ----label=testcquarter,cache=TRUE--------------------------------------------
specificp.test(L4,'c',0.25)

## ----label=testequal,cache=TRUE-----------------------------------------------
equalp.test(L4)

