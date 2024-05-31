### R code from vignette source 'integration.Rnw'

###################################################
### code chunk number 1: integration.Rnw:86-88
###################################################
ignore <- require(hyper2,quietly=TRUE)
ignore <- require(magrittr,quietly=TRUE)


###################################################
### code chunk number 2: chess_show
###################################################
chess


###################################################
### code chunk number 3: integration.Rnw:208-212
###################################################
x <- c(a=1, b=2, c=3, d=4)  # needs a named vector
ans1 <- B(dirichlet(alpha = x),tol=0.1)
ans2 <- prod(gamma(x))/gamma(sum(x)) 
c(numerical=ans1,theoretical=ans2)   # should agree


###################################################
### code chunk number 4: integration.Rnw:219-222
###################################################
f <- function(p){p[1]<p[2]}
H <- dirichlet(alpha=c(a=3,b=3,c=3,d=3))
probability(H,f,tol=0.1)


###################################################
### code chunk number 5: integration.Rnw:231-233
###################################################
g <- function(p){(p[1]<p[2]) & (p[2]<p[3])}
1-probability(H,disallowed=g,tol=0.1)


###################################################
### code chunk number 6: integration.Rnw:245-247
###################################################
icons
maxp(icons)


###################################################
### code chunk number 7: integration.Rnw:285-289
###################################################
f1 <- function(p){p[1] > 1/6}
f2 <- function(p){p[1] > max(fillup(p)[-1])}
f3 <- function(p){sum(fillup(p)[5:6]) > 1/3}
f4 <- function(p){max(fillup(p)[1:2]) > min(fillup(p)[3:6])}


###################################################
### code chunk number 8: integration.Rnw:311-312
###################################################
pchisq(2*2.608,df=1,lower.tail=FALSE)


###################################################
### code chunk number 9: integration.Rnw:367-378
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
H <- balance(H)
H


###################################################
### code chunk number 10: integration.Rnw:385-390
###################################################
free <- maxp(H,give=TRUE)
m <- fillup(free$par)
names(m) <- pnames(H)
m
free$value


###################################################
### code chunk number 11: integration.Rnw:395-399
###################################################
obj <- function(p){-loglik(p,H)}   # objective func
gr  <- function(p){-gradient(H,p)} # gradient, needed for speed
UI <- rbind(diag(3),-1)           # UI and CI specify constraints
CI <- c(rep(0,3),-1)              # p_i >= 0 and sum p_i <= 1


