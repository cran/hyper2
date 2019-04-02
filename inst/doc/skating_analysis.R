## ------------------------------------------------------------------------
library("hyper2",quietly=TRUE)
data("skating")
skating_table

## ------------------------------------------------------------------------
skating_incorrect <- order_likelihood(t(skating_table))

## ------------------------------------------------------------------------
skating_rank <- apply(skating_table,2,order)
head(skating_rank)

## ------------------------------------------------------------------------
skating <- order_likelihood(t(skating_rank))
pnames(skating) <- rownames(skating_table)
head(skating)

## ----dpi=72--------------------------------------------------------------
dotchart(maxp(skating))
dotchart(log(maxp(skating)))

## ----dpi=72--------------------------------------------------------------
par(pty='s')
lik <- order(maxp(skating),decreasing=TRUE)
plot(1:23,lik,asp=1,pch=16,xlab='official order',ylab='likelihood order')
text(1:23,lik,asp=1,pch=16,pos=c(rep(4,19),rep(2,4)),rownames(skating_table))
abline(0,1)

## ------------------------------------------------------------------------
jj <-
    matrix(c(
        -1, 0, 0,   1,0,  # Hughes   < Cohen
         0,-1, 0,   1,0,  # Slutskya < Cohen
         0, 0,-1,   1,0,  # Kwan     < Cohen
        
        -1, 0, 0,   0,1,  # Hughes   < Suguri
         0,-1, 0,   0,1,  # Slutskya < Suguri
         0, 0,-1,   0,1), # Kwan     < Suguri
        byrow=TRUE,ncol=5)

problem_constraints <-  # fill with zeros for other competitors
    cbind(jj,matrix(0,nrow(jj),size(skating)-ncol(jj)-1))

small <-1e-3  # need a sensible start value satisfying the constraints
start <- c(rep(2*small,3),rep(3*small,2),rep(small,17))

out <- rep(0,nrow(problem_constraints))
fullout <- list()

for(i in seq_len(nrow(problem_constraints))){
	jj <- maxp(skating, startp=start, give=TRUE,fcm=problem_constraints[i,], fcv=0)
   fullout[[i]] <- jj
   out[i] <- jj$value
}
out
maxp(skating,give=TRUE)$value  - max(out)

## ------------------------------------------------------------------------
maxp(skating,give=TRUE)$value  - max(out[4:5])

## ------------------------------------------------------------------------
plot(out,ylab="log likelihood",axes=FALSE)
axis(2)
axis(side=1,at=1:6,srt=45,labels=c(
"Hug<Coh", "Slu<Coh", "Kwa<Coh",
"Hug<Sug", "Slu<Sug", "Kwa<Sug"
))

