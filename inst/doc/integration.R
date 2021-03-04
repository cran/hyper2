### R code from vignette source 'integration.Rnw'

###################################################
### code chunk number 1: integration.Rnw:85-87
###################################################
ignore <- require(hyper2,quietly=TRUE)
ignore <- require(magrittr,quietly=TRUE)


###################################################
### code chunk number 2: chess_show
###################################################
data(chess)
chess


###################################################
### code chunk number 3: chess_normalizing
###################################################
B(chess)


###################################################
### code chunk number 4: integration.Rnw:137-139
###################################################
f <- function(p){loglik(indep(p),chess) > loglik(c(1,1)/3,chess)}
probability(chess, disallowed=f,tol=0.01)


###################################################
### code chunk number 5: integration.Rnw:147-149
###################################################
T.lt.A <- function(p){p[1]<p[2]}
probability(chess, disallowed=T.lt.A,tol=0.001)


###################################################
### code chunk number 6: integration.Rnw:192-195
###################################################
x <- c(a=1,b=2,c=3,d=4)  # needs a named vector
B(dirichlet(x))
prod(gamma(1:4))/gamma(sum(1:4))


###################################################
### code chunk number 7: integration.Rnw:202-205
###################################################
f <- function(p){p[1]<p[2]}
H <- dirichlet(alpha=c(a=3,b=3,c=3,d=3))
probability(H,f,tol=0.1)


###################################################
### code chunk number 8: integration.Rnw:210-212
###################################################
g <- function(p){(p[1]<p[2]) & (p[2]<p[3])}
1-probability(H,disallowed=g,tol=0.1)


###################################################
### code chunk number 9: integration.Rnw:221-224
###################################################
data("oneill")                    # load the dataset
icons
maxp(icons)


###################################################
### code chunk number 10: integration.Rnw:262-266
###################################################
f1 <- function(p){p[1] > 1/6}
f2 <- function(p){p[1] > max(fillup(p)[-1])}
f3 <- function(p){sum(fillup(p)[5:6]) > 1/3}
f4 <- function(p){max(fillup(p)[1:2]) > min(fillup(p)[3:6])}


###################################################
### code chunk number 11: integration.Rnw:275-276
###################################################
probability(icons, disallowed=function(p){p[1] > 1/6}, tol=0.1)


###################################################
### code chunk number 12: integration.Rnw:285-286
###################################################
pchisq(2*2.608,df=1,lower.tail=FALSE)


###################################################
### code chunk number 13: integration.Rnw:341-351
###################################################
H <- hyper2()
H["t00"] <- 18
H["t10"] <- 01
H["t01"] <- 05
H["t11"] <- 26
H[c("t11","t10")] <- 2
H[c("t01","t00")] <- 9
H[c("t11","t01")] <- 4
H[c("t10","t00")] <- 4
H


###################################################
### code chunk number 14: integration.Rnw:359-364
###################################################
free <- maxp(H,give=TRUE)
m <- fillup(free$par)
names(m) <- pnames(H)
m
free$value


###################################################
### code chunk number 15: integration.Rnw:369-373
###################################################
obj <- function(p){-loglik(p,H)}   # objective func
gr  <- function(p){-gradient(H,p)} # gradient, needed for speed
UI <- rbind(diag(3),-1)           # UI and CI specify constraints
CI <- c(rep(0,3),-1)              # p_i >= 0 and sum p_i <= 1


