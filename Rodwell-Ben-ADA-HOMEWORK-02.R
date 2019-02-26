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




#### SECTION ONE OF HOMEWORK COMPLETE ####





# Uniform density function

a <- 4
b <- 8
x <- seq(from = a - (b - a), to = b + (b - a), by = 0.01)
fx <- dunif(x, min = a, max = b)  # dunif() evaluates the density at each x
plot(x, fx, ylim = c(0, max(fx) + 0.1), type = "l", xlab = "x", ylab = "f(x)", 
     main = "Probability Density Function")

# punif() is the cumulative probability density up to a given x
plot(x, punif(q = x, min = a, max = b), ylim = c(0, 1.1), type = "l", xlab = "x", 
     ylab = "Pr(X ≤ x)", main = "Cumulative Probability")  




randounif <- runif(n = 10000, min = 6, max = 8)
mean(randounif)
var(randounif)

a <- 6
b <- 8
x <- seq(from = a - (b - a), to = b + (b - a), by = 0.01)
fx <- dunif(x, min = a, max = b)  # dunif() evaluates the density at each x
plot(x, fx, ylim = c(0, max(fx) + 0.1), type = "l", xlab = "x", ylab = "f(x)", 
     main = "Probability Density Function")
mean(fx)
var(fx)



#### NORMAL DISTRIBUTION ####


manipulate(ggplot(data = data.frame(x = c(mu - 4 * sigma, mu + 4 * sigma)), 
    aes(x)) + stat_function(fun = dnorm, args = list(mean = mu, sd = sigma),
    n = 1000) + xlab("x") + ylab("f(x)") + labs(title = "Exploring the Normal Distribution", 
    subtitle = paste0("Cumulative Probability Under Curve = ",
    round(pnorm(mu + nsigma * sigma, mu, sigma) - pnorm(mu - nsigma * sigma, mu, sigma), 
    3))) + stat_function(fun = dnorm, xlim = c(mu - nsigma * sigma, mu + 
    nsigma * sigma), args = list(mean = mu, sd = sigma), n = 1000, geom = "area",
    fill = "red", alpha = 0.5, color = "red"), mu = slider(-100, 100, initial = 0, 
    step = 10), sigma = slider(0, 30, initial = 5, step = 1), nsigma = slider(0, 
    4, initial = 0, step = 0.25))


# plots the cumulative distribution function
manipulate(plot(seq(from = (mu - 4 * sigma), to = (mu + 4 * sigma), length.out = 1000), 
      pnorm(seq(from = (mu - 4 * sigma), to = (mu + 4 * sigma),
      length.out = 1000), mean = mu, sd = sigma), type = "l",
      xlim = c(mu - 4 * sigma, mu + 4 * sigma), xlab = "x", ylab = "f(x)",
      main = "Cumulative Probability"), mu = slider(-100, 100, initial = 0,
      step = 10), sigma = slider(0, 30, initial = 5,step = 1))


# prob of a value falling within an interval based on a certain MEAN and SD
p <- pnorm(8, mean = 6, sd = 2) - pnorm(7, mean = 6, sd = 2)
p


# prob of a value falling within 2 SD of a normal distribution
# with a certain MEAN

## 2 SD
mu <- 0
sigma <- 1
p <- pnorm(mu + 2 * sigma, mean = mu, sd = sigma) - pnorm(mu - 2 * sigma,
      mean = mu, sd = sigma)
p

## 1 SD
p <- pnorm(mu + 1 * sigma, mean = mu, sd = sigma) - pnorm(mu - 1 * sigma,
      mean = mu, sd = sigma)
p

# With Confidence Intervals
manipulate(ggplot(data = data.frame(x = c(mu - 4 * sigma, mu + 4 * sigma)), 
      aes(x)) + stat_function(fun = dnorm, args = list(mean = mu, sd = sigma),
      n = 1000) + xlab("x") + ylab("f(x)") + labs(title = "Exploring the Normal Distribution",
      subtitle = paste0("Confidence Interval = ", round(CI, 2))) + geom_vline(xintercept = qnorm((1 - 
      CI)/2, mean = mu, sd = sigma), color = "red", linetype = "dashed") + geom_vline(xintercept = qnorm(1 - 
      (1 - CI)/2, mean = mu, sd = sigma), color = "red", linetype = "dashed"), 
      mu = slider(-100, 100, initial = 0, step = 10),
      sigma = slider(0, 30, initial = 5,
      step = 1), CI = slider(0, 1, initial = 0.5, step = 0.05))



normdist <- rnorm(n = 1000, mean = 3.5, sd = 4)
mean(normdist)
var(normdist)
sd(normdist)
hist(normdist)
hist(normdist, breaks = seq(from = -15, to = 20, by = 0.5), probability = TRUE)
hist(normdist, breaks = seq(from = -15, to = 20, by = 0.5), probability = F)


# Q-Q Plot
qqnorm(normdist, main = "Normal QQ plot random normal variables")
qqline(normdist, col = "gray")

# Q-Q Plot Alternative
p <- ppoints(length(normdist))
head(p)
tail(p)
theoretical_q <- qnorm(p)
observed_q <- quantile(normdist, ppoints(normdist))
plot(theoretical_q, observed_q, main = "QQ plot - Random Normal variable",
      xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")

# Simulating differentamount data in Q-Q plots - less samples = worse fit to line
n <- 100
mu <- 3.5
sigma <- 4
v <- rnorm(n, mu, sigma)
qqnorm(v, main = "QQ plot - Random Normal variable")
qqline(v, col = "gray")

# diff kind of distribution doesn't fit the line
<- 1000
v <- rbeta(n, shape1 = 1.3, shape2 = 2)
qqnorm(v, main = "QQ plot Random Beta variable")
qqline(v, col = "gray")



# The "Standard Normal Distribution - Standardizing a normal distribution"
x <- rnorm(10000, mean = 5, sd = 8)  # simulate from a normal distribution with mean 5 and SD 8
hist(x)

mean(x)  # really close to 5
sd(x)  # really close to 8

# Standardize it! - also known a "Z-Scores"
z <- (x - mean(x))/sd(x)  # standardized!
hist(z)

#### SAMPLE VS. POPULATION DISTRIBUTIONS ####

set.seed(1)
x <- rnorm(1e+06, 25, 5)
hist(x, probability = TRUE)

# Get the Population SD - sd(x) would give the SAMPLE SD
mu <- mean(x)
mu
sigma <- sqrt(sum((x - mean(x))^2)/length(x))
sigma

sd(x)


# creating a sampling distribution
k <- 5000  # number of samples
n <- 10  # size of each sample
s <- NULL  # dummy variable to hold each sample
for (i in 1:k) {
  s[[i]] <- sample(x, size = n, replace = FALSE)
}
head(s)

# And comparing against the population
m <- NULL
for (i in 1:k) {
  m[i] <- mean(s[[i]])
}
mean(m)  # the mean of our sample means, i.e., the mean of the sampling distribution is almost equal to...
mu  # the population mean


(p <- ggplot(data = as.data.frame(m), aes(x = m)) + geom_histogram(binwidth = function(x) (max(m) - 
      min(m))/20, alpha = 0.75) + labs(title = paste0("Sampling Distribution\nMeans of ", 
      k, " samples of size ", n)) + xlab("Mean") + ylab("Frequency") + geom_vline(xintercept = mu, 
      color = "red", linetype = 2, size = 1) + annotate(geom = "text", x = mu, 
      y = k * 0.06, label = "Population Mean\n", color = "red", angle = 90, size = 6))


# Standard Error
pop_se <- sqrt(sigma^2/n)
pop_se  # SE calculated from population variance
pop_se <- sigma/sqrt(n)
pop_se  # SE calculated from population standard deviation
samp_dist_v <- sum((m - mean(m))^2)/length(m)  # variance of the sampling distribution
samp_dist_se <- sqrt(samp_dist_v)  # SD of the sampling distribution = SE estimated based on the sampling distribution for k samples of size n
samp_dist_se  # very close to pop_se!


# function for Standard Error
se <- function(x) {
  sd(x)/sqrt(length(x))
}
se(x)

# SE is also available as a function in the package: SCIPLOT
library(sciplot)
se(x)

sample_sd <- NULL
for (i in 1:k) {
  sample_sd[i] <- sd(s[[i]])  # a vector of SDs for each sample
}
sample_se <- sample_sd/sqrt(n)  # a vector of SEs estimated from each sample
(p <- ggplot(data = as.data.frame(sample_se), aes(x = sample_se)) + geom_histogram(binwidth = function(x) (max(sample_se) - 
      min(sample_se))/20, alpha = 0.75) + labs(title = paste0("Distribution of SEs Estimated from\n", 
      k, " samples of size ", n)) + xlab("Estimated SE") + ylab("Frequency") + 
      geom_vline(xintercept = mean(sample_se), color = "red", linetype = 2, size = 1) + 
      annotate(geom = "text", x = mean(sample_se), y = k * 0.075, label = "Mean Estimated SE\n", 
      color = "red", angle = 90, size = 4) + geom_vline(xintercept = pop_se, 
      color = "blue", linetype = 2, size = 1) + annotate(geom = "text", x = pop_se, 
      y = k * 0.075, label = "\n\nSE calculated from known population variance", 
      color = "blue", angle = 90, size = 4) + geom_vline(xintercept = samp_dist_se, 
      color = "green", linetype = 2, size = 1) + annotate(geom = "text", x = samp_dist_se, 
      y = k * 0.075, label = "\nSE estimated as SD of sampling distribution", 
      color = "green", angle = 90, size = 4))




#### HOMEWORK 2 PART 2 ####

# Load in the dataset “zombies.csv” from my GitHub repository at
# https://github.com/difiore/ADA-2019. This data includes the first
# and last name and gender of the entire population of 1000 people who
# have survived the zombie apocalypse and are now ekeing out an existence
# somewhere on the East Coast, along with several other variables
#(height, weight, age, number of years of education, number of zombies
# they have killed, and college major see here for info on important
# post-zombie apocalypse majors

# load in the zombies.csv dataset

library(curl)
f <- f <- curl("https://raw.githubusercontent.com/difiore/ADA-2019/master/zombies.csv")
d <- read.csv(f, header = TRUE, sep = ",", stringsAsFactors = FALSE)
head(d)


# 2.a. Calculate the population mean and standard deviation for each
# quantitative random variable (height, weight, age, number of zombies
# killed, and years of education).

# NOTE: You will not want to use the built in var() and sd() commands as
# those are for samples.


# First create functions for population variance and standard deviation
## A function to calculate population variance
popvar <- function(x) {
  sum((x - mean(x))^2)/(length(x))}
popvar(x)

## A function to calculate polulation standard deviation
popsd <- function(x) {
  sqrt(popvar(x))}
popsd(x)

# Calculate Mean for each variable
mean(d$height)
mean(d$weight)
mean(d$age)
mean(d$zombies_killed)
mean(d$years_of_education)

# calculate SD for each variable
popsd(d$height)
popsd(d$weight)
popsd(d$age)
popsd(d$zombies_killed)
popsd(d$years_of_education)

# 2.b. Use {ggplot} and make boxplots of each of these variable by gender.
library(ggplot2)
heightbox <- ggplot(d, aes(x = gender, y = height)) + 
  geom_boxplot()
heightbox

weightbox <- ggplot(d, aes(x = gender, y = weight)) + 
  geom_boxplot()
weightbox

agebox <- ggplot(d, aes( x = gender, y = age)) + 
  geom_boxplot()
agebox

zombieskilledbox <- ggplot(d, aes(x = gender, y = zombies_killed)) + 
  geom_boxplot()
zombieskilledbox

yearsofeducationbox <- ggplot( d, aes(x = gender, y = years_of_education)) +
  geom_boxplot()
yearsofeducationbox



# 2.c. Use {ggplot} and make scatterplots of height and weight in relation
# to age. Do these variables seem to be related? In what way?

heightscatter <- ggplot(d, aes(x= age, y= height)) + geom_point()
heightscatter


weightscatter <- ggplot(d, aes(x= age, y= weight)) + geom_point()
weightscatter


## Both of the height and weight variables seem to be correlated in that they both
##increase with age, but there seems to be a considerable amount of variability.
## There seems to be less of a correlation in the relationship between age and weight.




# 2.d. Using histograms and Q-Q plots, check whether the quantitative
# variables seem to be drawn from a normal distribution. Which seem
# to be and which do not?
# HINT: Not all are drawn from a normal distribution!
# For those that are not, can you determine what common distribution
# they are drawn from?

## Height
qqnorm(d$height, main = "Normal QQ plot Height")
qqline(d$height, col = "gray")

## Weight
qqnorm(d$weight, main = "Normal QQ plot Weight")
qqline(d$weight, col = "gray")

## Age
qqnorm(d$age, main = "Normal QQ plot Age")
qqline(d$age, col = "gray")

## Zombies killed
qqnorm(d$zombies_killed, main = "Normal QQ plot # of Zombies Killed")
qqline(d$zombies_killed, col = "gray")

## Years of Education
qqnorm(d$years_of_education, main = "Normal QQ plot Years of education")
qqline(d$years_of_education, col = "gray")


### Both of the variables # of zombies killed AND,
### years of education are not normally distributed 

### # of zombies killes
mean(d$zombies_killed)
popsd(d$zombies_killed)
length(d$zombies_killed)
n <- 1000
v <- rbeta(n, shape1 = min(d$zombies_killed), shape2 = max(d$zombies_killed))
qqnorm(d$zombies_killed, main = "QQ plot Random Beta variable")
qqline(v, col = "gray")

n <- 1000
v <- rpois(n, lambda)
qqnorm(d$zombies_killed, main = "QQ plot Random Beta variable")
qqline(v, col = "gray")



mean(d$years_of_education)
popsd(d$years_of_education)
length(d$years_of_education)








# 2.e. Now use the sample() function to sample ONE subset of 30
# zombies (without replacement) from this population and calculate
# the mean and sample standard deviation for each variable. 
# Also, # estimate the standard error for each variable and construct the 95%
# confidence interval for each mean. Note that for the variables that
# are not drawn from the normal distribution, you will need to base
# your estimate of the CIs on some different distribution!

## First sample the population
dsample<- d[sample(nrow(d), 30), ]
head(dsample)

## calculate mean for each variable
mean(dsample$height)
mean(dsample$weight)
mean(dsample$age)
mean(dsample$zombies_killed)
mean(dsample$years_of_education)

## Calcualte Standard Deviation for each variable
sd(dsample$height)
sd(dsample$weight)
sd(dsample$age)
sd(dsample$zombies_killed)
sd(dsample$years_of_education)

## Estimate Standard error for each variable
### Create function to estimate Standard Error
se <- function(x) {
  sd(x)/sqrt(length(x))
}
se(dsample$height)
se(dsample$weight)
se(dsample$age)
se(dsample$zombies_killed)
se(dsample$years_of_education)

## Construct 95% confidence intervals for each variable




# 2.f. Now draw 99 more random samples of 30 zombies out and calculate
# the mean for each of the these samples. Together with the first
# sample you drew out, you now have a set of 100 means for each
# variable (each based on 30 observations), which constitutes a
# sampling distribution for each variable. What are the means and
# standard deviations of the sampling distribution for each variable?
# How do the standard deviations compare to the standard errors estimated
# in [2.e.]? What do these sampling distributions look like?
# Are they normally distributed? What about for those variables
# that you concluded were not originally drawn from a normal distribution?







