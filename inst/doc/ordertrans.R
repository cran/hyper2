## ----setup, include=FALSE-----------------------------------------------------
set.seed(0)
library("hyper2")
options(rmarkdown.html_vignette.check_title = FALSE)
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(fig.width=6, fig.height=6)
knit_print.function <- function(x, ...){dput(x)}
registerS3method(
  "knit_print", "function", knit_print.function,
  envir = asNamespace("knitr")
)

## ----out.width='20%', out.extra='style="float:right; padding:10px"',echo=FALSE----
knitr::include_graphics(system.file("help/figures/hyper2.png", package = "hyper2"))

## ----label=showordertrans-----------------------------------------------------
ordertrans

## ----label=simpexample--------------------------------------------------------
x <- c(d=2,a=3,b=1,c=4)
x

## -----------------------------------------------------------------------------
sort(x)

## ----label=useordertrans------------------------------------------------------
o <- ordertrans(x) # by default, sorts names() into alphabetical order
o

## ----label=equalrearrangement-------------------------------------------------
identical(x, o[c(4,1,2,3)])
identical(o, x[c(2,3,4,1)])

## ----showordervec-------------------------------------------------------------
(Sx <- ordervec2supp(x))
(So <- ordervec2supp(o))

## -----------------------------------------------------------------------------
Sx==So

## -----------------------------------------------------------------------------
(x <- c(d=2, a=3, c=4, b=3, e=6))
(y <- c(e=3, c=2, a=4, b=5, d=1))
x+y

## -----------------------------------------------------------------------------
ordertrans(x) + ordertrans(y)

## -----------------------------------------------------------------------------
(z <- c(f=3, g=2, h=4, a=5, b=1))  # names NOT letters[1:5]
ordertrans(x) + ordertrans(z)  # arguably not well-defined

## ----label=lookatskating------------------------------------------------------
skating_table

## ----makej1j2-----------------------------------------------------------------
j1 <- skating_table[,1]  # column 1 is judge number 1
names(j1) <- rownames(skating_table)
j2 <- skating_table[,2]  # column 2 is judge number 2
names(j2) <- rownames(skating_table)
j1
j2
cbind(j1,j2)

## ----j1vsj2,fig.cap="Judge 1 vs judge 2"--------------------------------------
par(pty='s')  # forces plot to be square
plot(j1,j2,asp=1,pty='s',xlim=c(0,25),ylim=c(0,25),pch=16,xlab='judge 1',ylab='judge 2')
abline(0,1)  # diagonal line
for(i in seq_along(j1)){text(j1[i],j2[i],names(j1)[i],pos=4,col='gray',cex=0.7)}

## ----label=maxlikeskating,cache=TRUE------------------------------------------
mL <- skating_maxp  # predefined; use maxp(skating) to calculate ab initio
mL

## ----label=strengthstoranks---------------------------------------------------
mL[] <- rank(-mL)  # minus because ranks orders from weak to strong
mL

## ----label=pointsskating,cache=TRUE-------------------------------------------
mP <- rowSums(skating_table)  # 'P' for Points
mP[] <- rank(mP,ties='first') # positive sign here
mP

## ----label=ordertransexample--------------------------------------------------
ordertrans(mP,names(mL))  

## ----label=crapplot,fig.cap="points-based ranks vs likelihood ranks"----------
plot(mL,ordertrans(mP,names(mL)))

## ----label=showoffordertransplot,fig.cap="points=based rank vs likelihood rank using `ordertransplot()`"----
ordertransplot(mL,mP,xlab="likelihood rank",ylab="Borda rank")

## ----transplotjudge1,fig.cap="Likelihood rank vs rank according to Judge 1"----
ordertransplot(mL,j1,xlab="likelihood rank",ylab="Judge 1 rank")

