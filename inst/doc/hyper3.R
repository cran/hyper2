## -----------------------------------------------------------------------------
library("hyper2",quietly=TRUE)

## ----label=usenamedvectorsforH------------------------------------------------
A <- 6 ; B <- 2 ; C <- 7 ; D <- 1
H <- hyper3()
H[c(p1=1.3)]      %<>% inc(A) 
H[c(p2=1)]        %<>% inc(B) 
H[c(p1=1.3,p2=1)] %<>% dec(A+B)
H[c(p1=1)]        %<>% inc(C) 
H[c(p2=1.3)]      %<>% inc(D) 
H[c(p1=1,p2=1.3)] %<>% dec(C+D)
H

## ----label=findtheevaluate,cache=TRUE-----------------------------------------
maxp(H)

## ----label=testequality,cache=TRUE--------------------------------------------
equalp.test(H)

## ----label=workouta-----------------------------------------------------------
a <- hyper3()
a[c(R=1)] %<>% inc
a[c(R=2,M=2,F=2)] %<>% dec
a[c(M=1)] %<>% inc
a[c(R=1,M=2,F=2)] %<>% dec
a[c(F=1)] %<>% inc
a[c(R=1,M=1,F=2)] %<>% dec
a[c(F=1)] %<>% inc
a[c(R=1,M=1,F=1)] %<>% dec
a[c(R=1)] %<>% inc
a[c(R=1,M=1    )] %<>% dec
a

## ----label=usebetteridiomforb-------------------------------------------------
b <- hyper3()
b["R"] %<>% inc ; b[c("R","M","F","F","R","M")] %<>% dec
b["M"] %<>% inc ; b[c(    "M","F","F","R","M")] %<>% dec
b["F"] %<>% inc ; b[c(        "F","F","R","M")] %<>% dec
b["F"] %<>% inc ; b[c(            "F","R","M")] %<>% dec
b["R"] %<>% inc ; b[c(                "R","M")] %<>% dec

a==b

## ---- label=maxpa,cache=TRUE--------------------------------------------------
ma <- maxp(a)
ma

## ----aretheydifferent,cache=TRUE----------------------------------------------
equalp.test(a)

## ----label=showgradissmall----------------------------------------------------
gradient(a,ma)  # should be small at the evaluate

