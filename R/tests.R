`equalp.test` <- function(H,...){
    n <- size(H)
    m_alternative <- maxp(H,give=TRUE,...)
    alternative_support <- m_alternative$value
    jj <- fillup(m_alternative$par)
    names(jj) <- pnames(H)
    alternative_estimate <- jj
    
    df <- size(H)-1

    null_support <- loglik(indep(equalp(H)),H)
    jj <- equalp(H)
    names(jj) <- pnames(H)
    null_estimate <- jj

    support_difference <- alternative_support-null_support

    if(identical(pnames(H),NA)){
        jj <- paste(paste("p_",seq_len(n)," = ",sep=""),collapse="")
        null_hypothesis <- substr(jj,1,nchar(jj)-3)
    } else {
        null_hypothesis = paste(pnames(H),collapse=" = ")
    }

    rval <- list(
        statistic = support_difference,
        p.value = pchisq(2*support_difference,df=df,lower.tail=FALSE),
        df = df,
        null_hypothesis = null_hypothesis,
        null_estimate = null_estimate,
        null_support = null_support,

        alternative_hypothesis = "sum p_i=1",
        alternative_estimate = alternative_estimate,
        alternative_support = alternative_support,
        method = "Constrained support maximization",
        data.name = deparse(substitute(H))
        )
    class(rval) <- "hyper2test"
    return(rval)
}

`specificp.test` <- function(H, i, specificp=1/size(H), alternative = c("two.sided","less","greater"), ...){
    alternative <- match.arg(alternative)

    return(switch(alternative,
                  "two.sided" = specificp.ne.test(H=H, i=i, specificp, ...),
                  "less"      = specificp.lt.test(H=H, i=i, specificp, ...),
                  "greater"   = specificp.gt.test(H=H, i=i, specificp, ...)
                  )
           )
}

`specificp.ne.test` <- function(H, i, specificp=1/size(H), ...){
    rsp <- round(specificp,getOption("digits"))  # for printing purposes
    if(is.character(i)){
      null_hypothesis <- paste("sum p_i=1, ",i, " = ", rsp, sep="")
      i <- which(pnames(H)==i)
    } else {
      null_hypothesis <- paste("sum p_i=1, p_",i, " = ", rsp, sep="")
    }
    delta <- 1e-5
    specificp <- max(delta,specificp)
    
    n <- size(H)
    # Do the null (restricted optimization) first:

    if(i<size(H)){  # regular, non-fillup value
        UI <- rep(0,n-1)
        UI[i] <- 1
        CI <- specificp
        start_min <- rep((1-specificp-delta)/(n-1),n-1)
        start_min[i] <- specificp+delta ## so p > specificp
        start_max <-  rep((1-specificp+delta)/(n-1),n-1)
        start_max[i] <- specificp-delta ## so p < specificp
    } else {   # fillup tested
        UI <- rep(-1,size(H)-1)
        CI <- specificp-1
        start_min <- rep(delta/n,n-1)         # all regular values small, fillup 1-delta
        start_max <- rep((1-delta)/(n-1),n-1) # all regular values big, fillup delta
    }

    m_min <- maxp(H,startp=start_min, fcm=+UI, fcv=+CI, ..., give=TRUE) # p_i >= specificp
    m_max <- maxp(H,startp=start_max, fcm=-UI, fcv=-CI, ..., give=TRUE) # p_i <= specificp
    ## in the above, 'm_min' interprets specificp as a minimum (that
    ## is, a lower bound) and 'm_max' interprets specificp as a
    ## maximum (that is, an upper bound).

    if(m_min$value < m_max$value){
        a <- m_min
    } else {
        a <- m_max
    }

    jj <- fillup(a$par)
    names(jj) <- pnames(H)
    null_estimate <- jj
    null_support <- a$value
        
    ## Now the alternative, free optimization
    m_alternative <- maxp(H, ..., give=TRUE)  # free optimization
    alternative_support <- m_alternative$value
    jj <- fillup(m_alternative$par)
    names(jj) <- pnames(H)
    alternative_estimate <- jj

    ## For debugging, this is a good place to uncomment the following line
    ## browser()

    df <- 1
    support_difference <- alternative_support - null_support

    rval <- list(
        statistic = support_difference,
        p.value = pchisq(2*support_difference,df=df,lower.tail=FALSE),
        df = df,
        null_hypothesis = null_hypothesis,
        null_estimate = null_estimate,
        null_support = null_support,
        alternative_hypothesis = "sum p_i=1",
        alternative_estimate = alternative_estimate,
        alternative_support = alternative_support,
        method = "Constrained support maximization",
        sidedness = "(two sided)",
        data.name = deparse(substitute(H))
        )
    class(rval) <- "hyper2test"
    return(rval)
}

`specificp.gt.test` <- function(H, i, specificp=1/size(H), delta=1e-5, ...){  # alternative = "greater"
    ## NB here we treat specificp as a *lower* bound for the
    ## optimization, the constraint being 'specificp <= p_i' (or,
    ## operationally, p_i <= specificp); the sense of 'greater'---as
    ## in, why the function is ".gt." rather than ".lt."---is that we
    ## suspect p_i is _greater than_ specificp and want to find
    ## evidence for this; so, operationally, we try to reject the
    ## hypothesis that p_i is less than specificp (and infer that p_i
    ## is indeed greater than specificp).

    rsp <- round(specificp,getOption("digits"))  # for printing purposes
    if(is.character(i)){
      null_hypothesis <- paste("sum p_i=1, ", i, " <= ", rsp, sep="")
      i <- which(pnames(H)==i)
    } else {
      null_hypothesis <- paste("sum p_i=1, p_",i, " = ", rsp, sep="")
    }
    if(specificp==0){null_hypothesis <- c(null_hypothesis, " (notional)")}
    n <- size(H)
  
    # Do the null first: (restricted optimization, p <= specificp
    if(i<size(H)){  # regular, non-fillup value
        UI <- rep(0,n-1)
        UI[i] <- 1
        start_max <- rep((1-specificp+delta)/(n-1),n-1)
        if(specificp==0){
            CI <- delta
            start_max[i] <- delta/2 ## so p < specificp
        } else {
            CI <- specificp
            start_max[i] <- specificp-delta 
        }
    } else {   # fillup tested
        if(specificp==0){
        UI <- rep(-1,size(H)-1)
        CI <- delta-1
        start_max <- rep((1-delta*(n-1)/n)/(n-1),n-1)
        } else {
        UI <- rep(-1,size(H)-1)
        CI <- specificp-1
        start_max <- rep((1-specificp+delta)/(n-1),n-1)
        }
    }

    a <- maxp(H,startp=start_max, fcm=-UI, fcv=-CI, ..., give=TRUE) # p_i <= specificp
    ## in the above, maxp() interprets specificp as a
    ## maximum (that is, an upper bound).

    jj <- fillup(a$par)
    if(!identical(pnames(H),NA)){names(jj) <- pnames(H)}
    null_estimate <- jj
    null_support <- a$value
        
    ## Now the alternative, free optimization
    m_alternative <- maxp(H, ..., give=TRUE)  # free optimization
    alternative_support <- m_alternative$value
    jj <- fillup(m_alternative$par)
    if(!identical(pnames(H),NA)){names(jj) <- pnames(H)}
    alternative_estimate <- jj
  
    alternative_hypothesis <- "sum p_i=1"
    ## For debugging, this is a good place to uncomment the following line
    ## browser()

    df <- 1
    support_difference <- alternative_support - null_support

    rval <- list(
        statistic = support_difference,
        p.value = pchisq(2*support_difference,df=df,lower.tail=FALSE),
        df = df,
        null_hypothesis = null_hypothesis,
        null_estimate = null_estimate,
        null_support = null_support,
        alternative_estimate = alternative_estimate,
        alternative_support = alternative_support,
        alternative_hypothesis = alternative_hypothesis,
        method = "Constrained support maximization",
        sidedness = "(one-sided)",
        data.name = deparse(substitute(H))
        )
    class(rval) <- "hyper2test"
    return(rval)
}

`specificp.lt.test` <- function(H, i, specificp=1/size(H), ...){  # alternative = "less"
    ## NB here we treat specificp as an *upper* bound for the
    ## optimization, the constraint being 'specificp >= p_i' (or,
    ## operationally, p_i >= specificp); the sense of 'less'---as
    ## in, why the function is ".lt." rather than ".gt."---is that we
    ## suspect p_i is _less than_ specificp and want to find
    ## evidence for this; so, operationally, we try to reject the
    ## hypothesis that p_i is greater than specificp (and infer that p_i
    ## is indeed less than specificp).


    rsp <- round(specificp,getOption("digits"))  # for printing purposes
    if(is.character(i)){
      null_hypothesis <- paste("sum p_i=1, ",i, " >= ", rsp, sep="")
      i <- which(pnames(H)==i)
    } else {
      null_hypothesis <- paste("sum p_i=1, p_",i, " = ", rsp, sep="")
    }

    delta <- 1e-4
    specificp <- min(max(delta,specificp),1-delta)
    n <- size(H)
  
    # Do the null first: (restricted optimization, p >= specificp
    if(i<size(H)){  # regular, non-fillup value
        UI <- rep(0,n-1)
        UI[i] <- 1
        CI <- specificp
        start_min <- rep((1-specificp-delta)/(n-1),n-1)
        start_min[i] <- specificp+delta ## so p > specificp
    } else {   # fillup tested
        UI <- rep(-1,size(H)-1)
        CI <- specificp-1
        start_min <- rep((1-specificp-delta)/(n-1),n-1) 
    }

    a <- maxp(H,startp=start_min, fcm=+UI, fcv=+CI, ..., give=TRUE) # p_i >= specificp
    ## in the above, maxp() interprets specificp as a
    ## minimum (that is, a lower bound).

    jj <- fillup(a$par)
    if(!identical(pnames(H),NA)){names(jj) <- pnames(H)}
    null_estimate <- jj
    null_support <- a$value
        
    ## Now the alternative, free optimization
    m_alternative <- maxp(H, ..., give=TRUE)  # free optimization
    alternative_support <- m_alternative$value
    jj <- fillup(m_alternative$par)
    if(!identical(pnames(H),NA)){names(jj) <- pnames(H)}
    alternative_estimate <- jj
  
    alternative_hypothesis <- "sum p_i=1"

    ## For debugging, this is a good place to uncomment the following line
    ## browser()

    df <- 1
    support_difference <- alternative_support - null_support

    rval <- list(
        statistic = support_difference,
        p.value = pchisq(2*support_difference,df=df,lower.tail=FALSE),
        df = df,
        null_hypothesis = null_hypothesis,
        null_estimate = null_estimate,
        null_support = null_support,
        alternative_estimate = alternative_estimate,
        alternative_support = alternative_support,
        alternative_hypothesis = alternative_hypothesis,
        method = "Constrained support maximization",
        sidedness = "(one-sided)",
        data.name = deparse(substitute(H))
        )
    class(rval) <- "hyper2test"
    return(rval)
}

`samep.test` <- function(H, i, give=FALSE, ...){
    n <- size(H)
    stopifnot(all(table(i)==1))
    stopifnot(length(i) > 1)
    stopifnot(length(i) < n)
    
    if(is.character(i)){
        stopifnot(all(i %in% pnames(H)))
        jj <- paste(paste(i," = ",sep=""),collapse="")
        null_hypothesis <- substr(jj,1,nchar(jj)-3)
        i <- which(pnames(H) %in% i)
    } else {  # numeric
        stopifnot(all(i<=n))
        jj <- paste(paste("p_",i," = ",sep=""),collapse="")
        null_hypothesis <- substr(jj,1,nchar(jj)-3)
    }
    SMALL <- 1e-3

    m_alternative <- maxp(H, ..., give=TRUE)  # free optimization
    alternative_support <- m_alternative$value
    jj <- fillup(m_alternative$par)
    names(jj) <- pnames(H)
    alternative_estimate <- jj
    
    o <- n-length(i) 

    if(any(i==n)){
        `q_to_p` <- function(qq){   # NB 'i' in scope
            ii <- i[i<n]  # ii is one shorter than i
            out <- rep(NA,n-1)
            out[ii] <- qq[1]
            r <- max(which(!(seq_len(n-1) %in% ii)))
            out[r] <- 1-qq[1]*length(i)-sum(qq[-1])  # ensures fillup correct
            out[-c(i,r)] <- qq[-1]
            return(out)
        }
    } else {
        `q_to_p` <- function(qq){   # NB 'i' in scope
            out <- rep(0,n-1)
            out[i] <- qq[1]
            out[-i] <- qq[-1]
            return(out)
        }
    } # if(i==n) closes

    
    `objective` <- function(jj){ ## jj == c(v,p[-i])  (NB p[i]==v)
        -loglik(q_to_p(jj),H)
    }
    
    ## optimization proceeds over R^(n-length(i))
    startq <- rep(-SMALL+1/n,n-length(i))

    ## constraints on q:
    UI <- rbind(diag(nrow = o),-c(length(i),rep(1,o-1)))
    CI <- c(rep(SMALL,o),SMALL-1)
    
    constrained_opt <-
        constrOptim(theta = startq, f = objective, grad = NULL, ui = UI, ci = CI, ...)
    null_support <- -constrained_opt$value
    null_estimate <- fillup(q_to_p(constrained_opt$par))
    names(null_estimate) <- pnames(H)
    support_difference <- alternative_support - null_support
    df <- length(i)-1

    rval <- list(
        statistic = support_difference,
        p.value = pchisq(2*support_difference,df=df,lower.tail=FALSE),
        df = df,
        null_hypothesis = null_hypothesis,
        null_estimate = null_estimate,
        null_support = null_support,
        alternative_hypothesis = "sum p_i=1",
        alternative_estimate = alternative_estimate,
        alternative_support = alternative_support,
        method = "Constrained support maximization",
        sidedness = "",
        data.name = deparse(substitute(H))
        )
    class(rval) <- "hyper2test"
    return(rval)
}

`print.hyper2test` <- function(x,...){
    cat("\n")
    cat(strwrap(x$method, prefix = "\t"), sep = "\n")
    cat("\n")
    cat("data:  ", x$data.name, "\n", sep = "")

    cat("null hypothesis: ", x$null_hypothesis, "\n", sep="")
    cat("null estimate:\n")
    print(x$null_estimate)
    cat("(argmax, constrained optimization)\n")
    cat("Support for null: ",x$null_support, "+ K\n\n")

    cat("alternative hypothesis: ",x$alternative_hypothesis, "\n")
    cat("alternative estimate:\n")
    print(x$alternative_estimate)
    cat("(argmax, free optimization)\n")
    cat("Support for alternative: ",x$alternative_support, "+ K\n\n")

    cat("degrees of freedom: ", x$df, "\n", sep = "")
    cat("support difference = ", x$statistic, "\n",sep="")
    cat("p-value: ", x$p.value, " ",x$sidedness, "\n", sep = "")
    cat("\n")
    return(invisible(x))
}