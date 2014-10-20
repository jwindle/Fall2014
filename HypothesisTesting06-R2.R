

################################################################################
                                ## Functions ##
################################################################################

random.process.A <- function(nsamples)
{
    ## x = rexp(nsamples, 4)
    ## x = rt(nsamples, df=10)
    ## x = rnorm(nsamples, 2, 3)
    
    x = rgamma(nsamples, 10, 5)
    x
}

randomization.test <- function(x1, x2, sig.level=0.05, alternative=c("both", "left", "right"), niter=1000)
{

    n1 = length(x1)
    n2 = length(x2)
    n  = n1 + n2

    n.min = min(n1, n2)
    T     = rep(0, niter)
    x     = c(x1, x2)

    ## smaller.grp = 1:n1
    ## if (n1 > n2) smaller.grp = 1:n2 + n1
    
    for (i in 1:niter) {
        p    = sample(x, size=n, replace=FALSE)  ## Shuffle
        T[i] = mean(p[1:n1]) - mean(p[1:n2+n1])  ## Test statistic
    }

    T.obs = mean(x[1:n1]) - mean(x[1:n2+n1])

    decision = "accept"

    if (alternative[1]=="both") {
        
        half.sig.level = 0.5 * sig.level
        cutoff         = quantile(T, c(half.sig.level, 1-half.sig.level))
        
        if (T.obs < cutoff[1] || T.obs > cutoff[2])
            decision = "reject"
        
        p.obs = ( sum(T < -abs(T.obs)) + sum(T > abs(T.obs)) ) / niter
        
    } else if (alternative[1]=="left") {
        
        cutoff = quantile(T, sig.level)
        
        if (T.obs < cutoff)
            decision = "reject"
        
        p.obs = sum(T < T.obs ) / niter
                 
    } else if (alternative[1]=="right") {
        
        cutoff = quantile(T, 1-sig.level)
        
        if (T.obs > cutoff)
            decision = "reject"
        
        p.obs = sum(T > T.obs ) / niter
                 
    } else {
        cat("Unrecognized alternative.\n")
        return(NA)
    }

    list("simulation"=T, "niter"=niter, "T.obs"=T.obs, "cutoff"=cutoff, "alt"=alternative[1], "p.obs"=p.obs, "sig.level"=sig.level, "decision"=decision)
    
}

################################################################################
                ## Doing a randomization test once under null ##
################################################################################

desired.error.rate = 0.05 # The type I error rate we want.

n1 = 7                    # The number of subjects in group 1.
n2 = 6                    # The number of subjects in group 2.
n =n1+n2                  # The total number of subjects.

## randomly assign gropus
grp.1 = sample(1:n, size=n1, replace=TRUE)
grp.2 = (1:n)[-grp.1]

x1 = random.process.A(n1)  # random.process.A is not a built-in R command.
x2 = random.process.A(n2)  # It is a function defined above.

## randomization.test is not a built-in command either.
test = randomization.test(x1, x2, sig.level=desired.error.rate)
    
decision = test$decision
    
################################################################################
         ## Repeating the fake experiment many times under the null ##
################################################################################

desired.error.rate = 0.05 # The type I error rate we want.

m = 1000                  # The number of times to repeat the fake experiment.

decisions = rep(0, m)     # We need to set aside space for all of the decisions.

n1 = 7                    # The number of subjects in group 1.
n2 = 6                    # The number of subjects in group 2.
n =n1+n2                  # The total number of subjects.

## Repeat the experiment many times under the null...

for (j in 1:m) {
    if (j %% 50 == 0) cat("j =", j, "\n") 

    ## randomly assign gropus
    grp.1 = sample(1:n, size=n1, replace=TRUE)
    grp.2 = (1:n)[-grp.1]

    ## generate data from same random process
    x1 = random.process.A(n1)  
    x2 = random.process.A(n2)  

    ## do randomization test
    test = randomization.test(x1, x2, sig.level=desired.error.rate)
    
    decision[j] = test$decision
    
}

## The approximate type I error rate.
mean(decision=="reject")
