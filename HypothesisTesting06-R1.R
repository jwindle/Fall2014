


################################################################################
                   ## Asch Conformity Experiments Example ##
################################################################################

## Type I error
sum( dbinom(4:10, size=10, prob=0.1) )

## Power
sum( dbinom(4:10, size=10, prob=0.4) )

## For easy recall:
type.I.error = sum( dbinom(4:10, size=10, prob=0.1) )
power        = sum( dbinom(4:10, size=10, prob=0.4) )

## Computing the type I error under the null p=0.1.
m = 10

reject = rep(0, m) # Set aside space for the decisions

for (j in 1:m) {
    k = rbinom(1, size=10, prob=0.1) # Generate data
    T = k                            # Compute test statistic
    reject[j] = (T >= 4)             # Make decision
}

sum(reject) / m

mean(reject)

## Type I error for p=0.0, 0.1
sum( dbinom(4:10, size=10, prob=0.0) )
sum( dbinom(4:10, size=10, prob=0.1) )

## Power for p=0.2, 0.3, etc
sum( dbinom(4:10, size=10, prob=0.2) )
sum( dbinom(4:10, size=10, prob=0.3) )
sum( dbinom(4:10, size=10, prob=0.4) )
sum( dbinom(4:10, size=10, prob=0.5) )
sum( dbinom(4:10, size=10, prob=0.6) )
sum( dbinom(4:10, size=10, prob=0.7) )
sum( dbinom(4:10, size=10, prob=0.8) )
sum( dbinom(4:10, size=10, prob=0.9) )
sum( dbinom(4:10, size=10, prob=1.0) )


################################################################################
                        ## Why does this work again? ##
################################################################################

## The true probability of the test statistic under the null p = 0.1.
probs = dbinom(0:10, size=10, prob=0.1)

m    = 10000

T    = rep(0, m) # Set aside space for the test statistics.

for (j in 1:m) {
    k    = rbinom(1, size=10, prob=0.1) # Generate data
    T[j] = k                            # Compute test statistic
}

## Plot the truth and plot the approximation --- DON'T WORRY ABOUT THE DETAILS HERE.
plot(0:10, probs, type="h", ylim=c(0, 0.4), main="Exact and approximate distribution of test statistic", xlab="k")
points(0:10, probs, pch=19)
h = hist(samp, breaks=seq(-0.5,10.5,by=1), plot=FALSE)
lines(0:10+0.1, h$density, col=4, typ="h")
points(0:10+0.1, h$density, pch=19, col=4)
legend("topright", legend=c("Exact", "Using sample"), pch=c(19,19), col=c(1,4))

