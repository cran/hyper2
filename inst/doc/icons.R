## ----setup, include=FALSE-----------------------------------------------------
library("hyper2",quietly=TRUE)
set.seed(0)
knitr::opts_chunk$set(echo = TRUE)

## ----label="hexsticker",out.width='20%', out.extra='style="float:right; padding:10px"',echo=FALSE----
knitr::include_graphics(system.file("help/figures/hyper2.png", package = "hyper2"))

## ----label=loadlib------------------------------------------------------------
library("hyper2",quietly=TRUE)
M <- icons_table # saves typing
M

## ----showicons----------------------------------------------------------------
icons
icons == saffy(icons_table)  # should be TRUE

## ----estmaxlike,cache=TRUE----------------------------------------------------
options("digits" = 4)
(mic <- maxp(icons))
dotchart(mic,pch=16)

## ----calclog------------------------------------------------------------------
L1 <- loglik(indep(mic),icons)
options(digits=9)
L1

## ----doequalp,cache=TRUE------------------------------------------------------
equalp.test(icons)

## ----dospecificp,cache=TRUE---------------------------------------------------
specificp.test(icons,1)

## ----largestoffive,cache=TRUE-------------------------------------------------
o <- function(Ul,Cl,startp,give=FALSE){
    small <- 1e-4  #  ensure start at an interior point
    if(missing(startp)){startp <- small*(1:5)+rep(0.1,5)}			
    out <- maxp(icons, startp=small*(1:5)+rep(0.1,5), give=TRUE, fcm=Ul,fcv=Cl)
    if(give){
        return(out)
    }else{
        return(out$value)
    }
}

p2max <- o(c(-1, 1, 0, 0, 0), 0)  # p1 <= p2
p3max <- o(c(-1, 0, 1, 0, 0), 0)  # p1 <= p3
p4max <- o(c(-1, 0, 0, 1, 0), 0)  # p1 <= p4
p5max <- o(c(-1, 0, 0, 0, 1), 0)  # p1 <= p5
p6max <- o(c(-2,-1,-1,-1,-1),-1)  # p1 <= p6 (fillup)

## ----pnmax--------------------------------------------------------------------
likes <- c(p2max,p3max,p4max,p5max,p6max)
likes
ml <- max(likes) 
ml

## ----extralike----------------------------------------------------------------
L1-ml

## ----evalworth,cache=TRUE-----------------------------------------------------
o2 <- function(Ul,Cl){
  jj <-o(Ul,Cl,give=TRUE)
  out <- c(jj[[1]],1-sum(jj[[1]]),jj[[2]])
  names(out) <- c("p1","p2","p3","p4","p5","p6","support")
  return(out)
}
rbind(
o2(c(-1, 1, 0, 0, 0), 0),  # p1 <= p2
o2(c(-1, 0, 1, 0, 0), 0),  # p1 <= p3
o2(c(-1, 0, 0, 1, 0), 0),  # p1 <= p4
o2(c(-1, 0, 0, 0, 1), 0),  # p1 <= p5
o2(c(-2,-1,-1,-1,-1),-1)   # p1 <= p6
)

## ----lowfreq,cache=TRUE-------------------------------------------------------
jj <- o(c(-1,-1,-1,-1,0) , -2/3, give=TRUE,start=indep((1:6)/21))$value
jj

## ----extralowfreq-------------------------------------------------------------
L1-jj

## ----ofcourse,cache=TRUE------------------------------------------------------
small <- 1e-4
start <- indep(c(small,small,small,small,0.5-2*small,0.5-2*small))
jj <- c(
   o(c(-1, 0, 0, 0, 1), 0,start=start),  # p1 >= p5
   o(c( 0,-1, 0, 0, 1), 0,start=start),  # p2 >= p5
   o(c( 0, 0,-1, 0, 1), 0,start=start),  # p3 >= p5
   o(c( 0, 0, 0,-1, 1), 0,start=start),  # p4 >= p5

   o(c(-2,-1,-1,-1,-1),-1,start=start),  # p1 >= p6
   o(c(-1,-2,-1,-1,-1),-1,start=start),  # p2 >= p6
   o(c(-1,-1,-2,-1,-1),-1,start=start),  # p3 >= p6
   o(c(-1,-1,-1,-2,-1),-1,start=start)   # p4 >= p6
   )
jj

## ----maxjj--------------------------------------------------------------------
max(jj)

## ----extraofcourse------------------------------------------------------------
L1-max(jj)

## ----lookatmax,cache=TRUE-----------------------------------------------------
o(c( 0, 0, 0,-1, 1), 0,give=TRUE,start=start)

