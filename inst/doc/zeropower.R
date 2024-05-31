## ----setup, include=FALSE-----------------------------------------------------
set.seed(0)
library("hyper2")
library("magrittr")
options(rmarkdown.html_vignette.check_title = FALSE)
knitr::opts_chunk$set(echo = TRUE)
knit_print.function <- function(x, ...){dput(x)}
registerS3method(
  "knit_print", "function", knit_print.function,
  envir = asNamespace("knitr")
)

## ----out.width='20%', out.extra='style="float:right; padding:10px"',echo=FALSE----
knitr::include_graphics(system.file("help/figures/hyper2.png", package = "hyper2"))

## ----appidiom-----------------------------------------------------------------
chess <- hyper2()
chess["Topalov"] <- 30
chess["Anand"  ] <- 36
chess["Karpov" ] <- 22
chess[c("Topalov","Anand" )] <- 35  # bug!  should  be -35
chess[c("Anand","Karpov"  )] <- 35  # bug!  should  be -35
chess[c("Karpov","Topalov")] <- 18  # bug!  should  be -18

## ----showwarningunbalanced----------------------------------------------------
chess

## ----correctid----------------------------------------------------------------
chess[c("Topalov","Anand" )] <- -35
chess[c("Anand","Karpov"  )] <- -35
chess[c("Karpov","Topalov")] <- -18
chess
loglik(equalp(chess),chess)

## ----newinfo------------------------------------------------------------------
chess["Anand" ] %<>% inc
chess["Karpov"] %<>% inc
chess[c("Anand,Karpov")]  %<>% dec(2)
chess

## -----------------------------------------------------------------------------
H <- hyper2()
H['a'] %<>% inc(5)
H['b'] %<>% inc(2)
H['c'] %<>% inc(9)
H %<>% balance
H

