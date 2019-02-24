### NOTES FOR HOEMWORK 2 ###



# What is Standard Error???
# How far a mean collected from a sample is
# likely to be from the population mean
# SD of a sample is used to estimate the SE
# SE is what is used to put confidence intervals in
library(tidyverse)
library(curl)
library(manipulate)
library(ggplot2)

outcomes <- c(1, 2, 3, 4, 5, 6)
manipulate(hist(sample(x = outcomes, size = n, 
                       replace = TRUE), breaks = c(0.5, 
                                                   1.5, 2.5, 3.5, 4.5, 5.5, 6.5), probability = TRUE,
                main = paste("Histogram of Outcomes of ", 
                             n, " Die Rolls", sep = ""), xlab = "roll",
                ylab = "probability"), n = slider(0, 
                                                  10000, initial = 100, step = 100))
n, " Die Rolls", sep = ""), xlab = "roll",
ylab = "probability"), n = slider(0, 
                                  10000, initial = 100, step = 100))

# Manipulate and these other commands give you a 
# histogram with a slider tool to "Manipulate" the data
# You have to specify variables to be adjusted by the slider

n<-1000
dice <- function(x) {sample(x = 1:6, size = x, replace = T)}
twodice <-(dice(n)+dice(n))
hist(twodice, breaks = c(1.5:12.5))

# this sets up a function to simulate rolling a die n number of times
# Then adding a second die and plotting the distribution of values


## Probability Mass Functions (PMF's)
# Involve discrete outcomes and their probabilities

outcomes <- c("heads", "tails")
prob <- c(1/2, 1/2)
barplot(prob, ylim = c(0, 0.6), names.arg = outcomes, space = 0.1, xlab = "outcome", 
        ylab = "Pr(X = outcome)", main = "Probability Mass Function")

outcomes <- c(1, 2, 3, 4, 5, 6)
prob <- c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)
barplot(prob, ylim = c(0, 0.5), names.arg = outcomes, space = 0.1, xlab = "outcome", 
        ylab = "Pr(X = outcome)", main = "Probability Mass Function")

## PMF's are different from cumulative probablitites, but related

outcomes <- c("heads", "tails")
prob <- c(1/2, 1/2)
cumprob <- cumsum(prob)
barplot(cumprob, names.arg = outcomes, space = 0.1, xlab = "outcome", ylab = "Cumulative Pr(X)", 
        main = "Cumulative Probability")

outcomes <- c(1, 2, 3, 4, 5, 6)
prob <- c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)
cumprob <- cumsum(prob)
barplot(cumprob, names.arg = outcomes, space = 0.1, xlab = "outcome", ylab = "Cumulative Pr(X)", 
        main = "Cumulative Probability")






#### PROBABILITY DISTRIBUTIONS ####


## The following code simulates the BETA Distribution
#  Cumulative probability is what falls under the curve
#  up to the X-axis value (X = thepr)

# Beta Distruibution is equal to:  f(x)  = x^(α−1)*(1−x)^(β−1)


manipulate(ggplot(data = data.frame(x = c(0, 1)), aes(x)) +
            stat_function(fun = dbeta, args = list(shape1 = alpha,
            shape2 = beta), n = 1000) + xlab("x") + ylab("f(x)") + 
            labs(title = "Exploring the Beta Distribution",
            subtitle = paste0("Cumulative Probability = ",
            round(pbeta(x, alpha, beta), 2))) +
            stat_function(fun = dbeta, xlim = c(0, x),
            args = list(shape1 = alpha, shape2 = beta),
            n = 1000, geom = "area"), alpha = slider(0, 10,
            initial = 2, step = 0.1), beta = slider(0, 10,
            initial = 1, step = 0.1), x = slider(0, 1,
            initial = 0, step = 0.01))


# pbeta() gives the cumulative probability for an x value
# with the respective alpha and beta values

pbeta(0.75, 2, 1)  # cumulative probability for x ≤ 0.75
pbeta(0.5, 2, 1)  # cumulative probability for x ≤ 0.50


#### Can check this with the Manupilate Beta Distributuion code ####

# qbeta is essentially the inverse of pbeta and is a quantile measure

pbeta(0.7, 2, 1)  # yields 0.49 -- x values ≤ 0.7 comprise 49% of the CDF
qbeta(0.49, 2, 1)  # yields 0.7 -- 49% of the CDF falls for x ≤ 0.7



# Expected mean of DISCRETE random variables like a DICE

m <- sum(seq(1:6)*(1/6))
m

## Now the EXPECTED variance of DISCRETE random variables the same
vary <- sum((seq(1:6) - mean(seq(1:6)))^2 * (1/6))
vary


#### PROBABILITY MASS FUNCTIONS ####





### Binomial Diartribution ###
# Used to model the X number of success out of N number of trials

n <- 6  # number of trials
k <- 6  # exact number of successes
p <- 1/6
prob <- (factorial(n)/(factorial(k) * factorial(n - k))) *
      (p^k) * (1 - p)^(n - k)
prob


k <- 3  # exact number of successes
prob <- (factorial(n)/(factorial(k) * factorial(n - k))) *
      (p^k) * (1 - p)^(n - k)
prob





### Bernoiuli Distribution ###
# A special case of the binomial distribution with binary variables



n <- 6  # number of trials
k <- 6  # exact number of successes
p <- 1/6
prob <- (factorial(n)/(factorial(k) * factorial(n - k))) * (p^k) * (1 - p)^(n - 
                                                                              k)
prob


## Can solve directly with dbinom ##
## x = # of successes; size = # of trials;
## probability = probability of single occurrence
dbinom(x = k, size = n, prob = p)




probset <- dbinom(x = 0:6, size = 6, prob = 1/6)  # x is number of successes, size is number of trials
barplot(probset, names.arg = 0:6, space = 0, xlab = "outcome", ylab = "Pr(X = outcome)", 
        main = "Probability Mass Function")

cumprob = cumsum(probset)
barplot(cumprob, names.arg = 0:6, space = 0.1, xlab = "outcome", ylab = "Cumulative Pr(X)", 
        main = "Cumulative Probability")

## prob of getting exactly 3 rolls of "1" in 6 total rolls
dbinom(x = 3, size = 6, prob = 1/6)


## prob of getting UP to and also 3 rolls of "1" in 6 total rolls
pbinom(q = 3, size = 6, prob = 1/6)
sum(dbinom(x = 0:3, size = 6, prob = 1/6))  # this sums the probabilities of 0, 1, 2, and 3 successes


# prob of getting more than 3 rolls of "1" in 6 total rolls
1- pbinom(q = 3, size = 6, prob = 1/6)
pbinom(q = 3, size = 6, prob = 1/6, lower.tail = FALSE)


# prob of getting 3 or more
1 - pbinom(q = 2, size = 6, prob = 1/6)  # note here that the q argument is '2'
pbinom(q = 2, size = 6, prob = 1/6, lower.tail = FALSE)






#### POISSON DISTRIBUTION ####
# for modeling open ended independent events over a period time
# in POISSON DISTRIBUTION:
# MEAN and VARIANCE are the SAME: λ
# λ = THE MEAN # OF OCCURRENCES OF THE EVENT IN THE GIVEN INTERVAL



x <- 0:10
l = 3.5
probset <- dpois(x = x, lambda = l)
barplot(probset, names.arg = x, space = 0, xlab = "x",
      ylab = "Pr(X = x)", main = paste0("Probability Mass Function\nlambda = ", l))



x <- 0:50
l = 20
probset <- dpois(x = x, lambda = l)
barplot(probset, names.arg = x, space = 0, xlab = "x", ylab = "Pr(X = x)",
        main = paste0("Probability Mass Function\nlambda = ", l))


## THE CUMULATIVE PROBABILLITY OF POISSON DISTRIBUTION
x <- 0:10
l <- 3.5
barplot(ppois(q = x, lambda = l), ylim = 0:1, space = 0,
        names.arg = x, xlab = "x", ylab = "Pr(X ≤ x)",
        main = paste0("Cumulative Probability\nlambda = ", l))


x <- 0:50
l <- 20
barplot(ppois(q = x, lambda = l), ylim = 0:1, space = 0, names.arg = x,
        xlab = "x", ylab = "Pr(X ≤ x)",
        main = paste0("Cumulative Probability\nlambda = ", l))






##



##
##

















################## HOMEWORK #########################




## 1.a. What is the probability that she will hear more
## than 8 calls during any given session?
ppois(q = 8, lambda = 15, lower.tail = FALSE)  


## 1.b. What is the probability that she will hear no calls
## in a session?
ppois(q = 0, lambda = 15, lower.tail = T)
dpois(x = 0, lambda = 15)  


## 1.c. What is the probability that she will hear exactly
## 3 calls in a session?
dpois(x = 3, lambda = 15)


## 1.d. Plot the relevant Poisson mass function over the
## values in range 0 ≤ x ≤ 30.
x <- 0:30
l = 15
poistiti <- dpois(x = 0:30, lambda = 15)
barplot(poistiti, names.arg = x, space = 0, xlab = "x",
        ylab = "Pr(X = x)", main = paste0("Probability Mass Function\nlambda = ", l),
        xlim = c(0,30))


## 1.e. Simulate 104 results from this distribution
## (i.e., 2 years of Saturday monitoring sessions)
rpois(n = 104, lambda = 15)

## 1.f. Plot the simulated results using hist() and use xlim()
## to set the horizontal limits to be from 0 to 30. How does
## your histogram compare to the shape of the probability mass
## function you plotted above?
randopois <- rpois(n = 104, lambda = 15)
hist(x = randopois, xlim = c(0,30))



