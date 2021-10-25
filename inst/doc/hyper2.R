### R code from vignette source 'hyper2.Rnw'

###################################################
### code chunk number 1: set_seed_chunk
###################################################
library("hyper2")
set.seed(0)


###################################################
### code chunk number 2: time_saver
###################################################
calc_from_scratch <- FALSE


###################################################
### code chunk number 3: hyper2.Rnw:111-112
###################################################
ignore <- require(magrittr,quietly=TRUE)


###################################################
### code chunk number 4: chess_setup
###################################################
chess <- hyper2()
chess["Topalov"] <- 30
chess["Anand"  ] <- 36
chess["Karpov" ] <- 22
chess[c("Topalov","Anand" )] <- 35
chess[c("Anand","Karpov"  )] <- 35
chess[c("Karpov","Topalov")] <- 18
chess


###################################################
### code chunk number 5: use_chess
###################################################
chess[c("Karpov","Topalov")] 
chess[c("Topalov","Karpov")] 



###################################################
### code chunk number 6: loglikelihoodchess
###################################################
loglik(c(1/3,1/3),chess)


###################################################
### code chunk number 7: gradientloglikelihoodchess
###################################################
gradient(chess,c(1/3,1/3))


###################################################
### code chunk number 8: maxlikechess
###################################################
maxp(chess)


###################################################
### code chunk number 9: 2016_olympics_heat1_first
###################################################
heat1 <- c("fournier", "cabrera", "bhokanal", "saensuk", "kelmelis", "teilemb")
H <- rankvec_likelihood(heat1)
H


###################################################
### code chunk number 10: 2016_olympics_heat1_first
###################################################
heat2 <- c("drysdale", "molnar", "esquivel", "garcia", "khafaji", "monasterio")
H <- H + rankvec_likelihood(heat2)
head(H)


###################################################
### code chunk number 11: rowers
###################################################
head(rowing)   # see rowing.Rd


###################################################
### code chunk number 12: rowing_maxp
###################################################
dotchart(rowing_maxp)


###################################################
### code chunk number 13: setupteams
###################################################
team_red <- c("Jamie","Tracy","Ben","Amy","Renae","Georgia")
team_blue <- c("Brent","Laura","Emelia","Colin","Kira","Tash")


###################################################
### code chunk number 14: masterchef_example
###################################################
H <- hyper2(pnames = c(
                "Amy", "Ben", "Brent", "Colin", "Emelia",
                "Georgia", "Jamie", "Kira", "Laura", "Renae",
                "Sarah", "Tash", "Tracy"))
H


###################################################
### code chunk number 15: redteamwins
###################################################
H[team_red] <- +1
H[c(team_red,team_blue)] <- -1
H


###################################################
### code chunk number 16: teamchallenge
###################################################
blue   <- c("Laura","Jamie")   # first
yellow <- c("Emelia","Amy")    # second
green  <- c("Brent","Tracy")   # third
red    <- c("Ben","Renae")     # fourth

H[blue] <- 1
H[c(blue,yellow,green,red)] <- -1
H[yellow] <- 1
H[c(yellow,green,red)] <- -1
H[green] <- 1
H[c(green,red)] <- -1
H


###################################################
### code chunk number 17: hyper2.Rnw:593-597
###################################################
L <- ggrl(H, 
          winner     = "Laura",
          btm4       = c("Brent", "Tracy","Ben"),
          eliminated = "Renae")


###################################################
### code chunk number 18: like_series1
###################################################
data("masterchef")
n <- 13   # 13 players
equal_strengths <- rep(1/n,n-1)
like_series(equal_strengths, masterchef)  # see masterchef.Rd


###################################################
### code chunk number 19: unconstrained_optimization (eval = FALSE)
###################################################
## UI <- rbind(diag(n-1),-1)  # p_i >= 0
## CI <- c(rep(0,n-1),-1)     # p_1+...+p_{n-1} <= 1
## 
## constrOptim(  # maxp() does not work for masterchef_series6
##     theta = equal_strengths,  # starting point for optimization
##     f = function(p){-like_series(p,masterchef)}, # see masterchef.Rd
##     ui=UI, ci=CI,
##     grad=NULL)


###################################################
### code chunk number 20: masterchef_maxp
###################################################
masterchef_maxp
dotchart(masterchef_maxp)


###################################################
### code chunk number 21: like_series2
###################################################
like_series(indep(masterchef_maxp), masterchef)


###################################################
### code chunk number 22: pvalcalcequalstrengths
###################################################
pchisq(2*(78.7-66.2),df=12,lower.tail=FALSE)


###################################################
### code chunk number 23: brent.gt.laura (eval = FALSE)
###################################################
## UI <- rbind(UI,c(0,0,1,0,0,0,0,0,-1,0,0,0,0))  # Brent >= Laura
## CI <- c(CI,0)
## ans2 <-
## constrOptim(  # maxp() does not work for masterchef_series6
##     theta = equal_strengths,
##     f = function(p){-like_series(p,masterchef_series6)},  # see masterchef.Rd
##     grad=NULL,
##     ui = UI, ci=CI)


###################################################
### code chunk number 24: hyper2.Rnw:745-746
###################################################
like_series(indep(masterchef_constrained_maxp), masterchef)


