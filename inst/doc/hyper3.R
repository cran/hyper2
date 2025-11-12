## ----setup, include=FALSE-----------------------------------------------------
library("hyper2",quietly=TRUE)
set.seed(0)
knitr::opts_chunk$set(echo = TRUE)

## ----label="hexsticker",out.width='20%', out.extra='style="float:right; padding:10px"',echo=FALSE----
knitr::include_graphics(system.file("help/figures/hyper2.png", package = "hyper2"))

## ----showLL-------------------------------------------------------------------
LL <- hyper3()
LL[c(a = 1)] <- 1
LL[c(a = 3, b = 2, c = 1)] <- -1
LL

## ----loglikabc----------------------------------------------------------------
loglik(c(a = 0.01, b = 0.01, c = 0.98), LL)
loglik(c(a = 0.90, b = 0.05, c = 0.05), LL)

## ----orderacbaab--------------------------------------------------------------
(H <- ordervec2supp3(c("a", "c", "b", "a", "a", "b")))

## ----mhmaxp-------------------------------------------------------------------
(mH <- maxp(H))

## ----testequality-------------------------------------------------------------
equalp.test(H)

## ----maxpaba------------------------------------------------------------------
maxp(ordervec2supp3(c("a", "b", "a")))

## ----ahalf--------------------------------------------------------------------
a <- 1/2 # null
(S_delta <- log(a * (1 - a)/(1 + a)) - log(3 - 2 * sqrt(2)))

## ----usechisq-----------------------------------------------------------------
pchisq(-2 * S_delta, df = 1, lower.tail = FALSE)

## ----fig.width=7, fig.height=7, fig.cap="A support function for a>b>a"--------
a <- seq(from = 0, by = 0.005, to = 1)
S <- function(a){log(a * (1 - a) / ((1 + a) * (3 - 2 * sqrt(2))))}
plot(a, S(a), type = 'b',xlab=expression(p[a]),ylab="support")
abline(h = c(0, -2))
abline(v = c(0.02438102, 0.9524271), col = 'red')
abline(v = sqrt(2) - 1)

## ----fig.width=7, fig.height=7, fig.cap="Support functions for observations a>a>b, a>b>a and b>a>a.  Horizontal dotted line represents two units of support"----
f_aab <- function(a){a^2 / (1 + a)}
f_aba <- function(a){a * (1 - a) / (1 + a)}
f_baa <- function(a){(1 - a) / (1 + a)}
p <- function(f, ...){
  a <- seq(from = 0, by = 0.005, to = 1)
  points(a, f(a) / max(f(a)), ...)
  }
plot(0:1, 0:1, xlab = expression(p[a]), ylab = "Likelihood", type = "n")
p(f_aab, type = "l", col = "black")
p(f_aba, type = "l", col = "red")
p(f_baa, type = "l", col = "blue")
text(0.8,0.8,"AAB")
text(0.8,0.4,"ABA",col="red")
text(0.8,0.05,"BAA",col="blue")
abline(h = exp(-2), lty = 2)

## ----maxoab-------------------------------------------------------------------
maxp(ordervec2supp3(c("a", "b"), nonfinishers=c("a", "b")))

## ----define_xy_wilcox---------------------------------------------------------
x <- c(0.80, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46)
y <- c(1.15, 0.88, 0.90, 0.74, 1.21)

## ----hyper3osdef--------------------------------------------------------------
names(x) <- rep("x", length(x))
names(y) <- rep("y", length(y))
(os <- names(sort(c(x, y))))

## ----hyper3xytest-------------------------------------------------------------
Hxy <- ordervec2supp3(os)
equalp.test(Hxy)

## ----fig.width=7, fig.height=7, fig.cap="A support function for the Bradley-Terry strength of permeability at term.  The evaluate of 0.24 is shown; and the two-units-of support credible interval, which does not exclude the null of strength 0.5 (dotted line), is also shown"----
a <- seq(from = 0.02, to = 0.8, len = 40)
L <- sapply(a, function(p){loglik(p, Hxy)})
plot(a, L - max(L), type = 'b',xlab=expression(p[a]),ylab="likelihood")
abline(h = c(0, -2))
abline(v = c(0.24))
abline(v=c(0.5), lty=2)

## ----javtab-------------------------------------------------------------------
javelin_table

## ----converttosupp3---------------------------------------
javelin_vector <- attemptstable2supp3(javelin_table,
       decreasing = TRUE, give.supp = FALSE)
options(width = 60)
javelin_vector

## ----javorder---------------------------------------------
javelin <- ordervec2supp3(v = names(javelin_vector)[!is.na(javelin_vector)])

## ----setoptsdig3,echo=FALSE-------------------------------
options(digits = 3)

## ----fig.width=7, fig.height=7, fig.cap="Maximum  likelihood estimate for javelin throwers' Bradley-Terry strengths"----
(mj <- maxp(javelin))
dotchart(mj, pch = 16,xlab="Estimated Bradley-Terry strength")

## ----echo=FALSE, fig.width=7, fig.height=7, fig.cap="Profile likelihood for log-contrast as per the text. Null indicated with a dotted line, and two-units-of-support limit indicated with horizontal lines; thus the null is not rejected"----
M <-  structure(c(3.13462808462437, -3.49547133821576, 2.31958387909791, 
 -1.453478109001, 1.85404664879129, -0.591727993171389, 1.47038547592281, 
 -0.172268427320532, 1.17564017651567, -0.00849666385090586, 0.927885079194978, 
 0, 0.706501034186756, -0.0998759637247275, 0.536848604955139, 
 -0.281432246390324, 0.33818759910768, -0.521602929477154, 0.167613563612845, 
 -0.816172716636203, 0.0827382775966942, -1.20676576256501, -0.138220714577699, 
 -1.5361019338357, -0.273432638006251, -1.95101772253715, -0.409370691327909, 
 -2.40043826949947, -0.560827492984493, -2.88462938724336, -0.666618197447362, 
 -3.39664587496794), dim = c(2L, 16L), dimnames = list(c("logcontrast", 
 "support"), NULL))
plot(t(M), type = "b")
abline(h = c(0, -2))
abline(v = 0, lty = 2)
abline(v = log(0.32062833 / 0.11402735)) # these from mp

## ----cons2021---------------------------------------------
constructor_2021_table[, 1:9]

## ----cheatmaxp2, echo=TRUE, eval=FALSE--------------------
# const2020 <- ordertable2supp3(constructor_2020_table)
# const2021 <- ordertable2supp3(constructor_2021_table)
# options(digits=4)

## ----defcomblike------------------------------------------
H <- (
      psubs(constructor_2020, "Merc", "Merc2020") +
      psubs(constructor_2021, "Merc", "Merc2021")
     )

## ----usesameptest,eval=FALSE------------------------------
# options(digits = 4)
# samep.test(H, c("Merc2020", "Merc2021"))

