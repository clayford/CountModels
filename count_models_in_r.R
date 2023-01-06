#' ---
#' title: "Modeling Count Data with R"
#' author: "Clay Ford, UVA Library StatLab"
#' date: "Spring 2020"
#' ---
#' 


# To submit a line of code, place your cursor in the line and hit Ctrl + Enter
# (Win/Linux) or Cmd + Enter (Mac)

# To insert an assignment arrow ( <- ): 
# Alt + - (Win/Linux) or Option + - (Mac)

# To insert a pipe ( %>% ): 
# Ctrl + Shift + M (Win/Linux) or Cmd + Shift + M (Mac)

# Packages ----------------------------------------------------------------

# install.packages("countreg", repos="http://R-Forge.R-project.org")
library(countreg)

library(pscl)
library(AER)
library(tidyverse)
library(ggeffects)



# Linear Model Review -----------------------------------------------------

# Let's say we have the following data
x <- 1:25
y <- 10 + 5*x
plot(x, y)

# 10 is the intercept, 5 is the slope
# y is completely determined by x

# Let's add some "noise" to our data by adding random draws from a Normal
# distribution with mean = 0 and a standard deviation = 10.

# set.seed(1) ensures we all get the same "random" data
set.seed(1)
noise <- rnorm(n = 25, mean = 0, sd = 10)

# Add the noise to 10 + 5*x and re-draw plot
y <- 10 + 5*x + noise
plot(x, y)

# This data is the combination of two parts:

# 1. 10 + 5*x
# 2. rnorm(n = 25, mean = 0, sd = 10)

# IMPORTANT FOR TODAY:
# Notice we could also generate the data this way:
x <- 1:25
set.seed(1)
y <- rnorm(n = 25, mean = 10 + 5*x, sd = 10)
plot(x, y)

# Each y is drawn from a Normal distribution with mean dependent on x.

# What if we were given this data and told to determine the process that
# generated it? In other words, fill in the blanks:

# 1. ________________
# 2. rnorm(n = 25, mean = 0, sd = ____)

# That's basically what linear modeling is.

# Traditional linear modeling assumes the following (among others):

# 1) the formula is a weighted sum of predictors (eg, 10 + 5*x)
# 2) the noise is a random draw from a Normal distribution with mean = 0
# 3) the standard deviation of the Normal distribution is constant

# Linear modeling tries to recover the weights (or coefficients) in the first
# assumption (10 and 5) and the standard deviation in the 3rd assumption (10).

# To do this we use lm()
# "y ~ x" means we think Part 1 of the model is "y = intercept + slope*x".
mod <- lm(y ~ x)
summary(mod)

# The model returns the following estimates:

# intercept = 11.135
# slope = 5.042
# sd = 9.7 (Residual standard error)

# We can use our model to generate data and see if it looks similar to our
# original data.
y_sim <- simulate(mod, nsim = 20)
plot(density(y))
for(i in 1:20)lines(density(y_sim[[i]]), col = "grey80")

# See appendix for a tidyverse version of the same plot
  
# Hence this is linear modeling:
# 1) propose and fit model(s)
# 2) determine if the model is good
# 3) use the model to explain relationships or make predictions


# Today we look at count models where we relax the assumptions that each
# response value is drawn from a Normal distribution with constant variance.



# Distributions for counts ------------------------------------------------

# Poisson distribution

# Use the `rpois` function to generate random data from a Poisson distribution
# with a specified mean. Notice the data are positive integers greater than or
# equal to 0.

# lambda is the mean; the variance and mean are equal in Poisson distributions

# Sample data from a Poisson distribution with a mean of 3. Note that lambda can
# take decimal values.
set.seed(1)
y1 <- rpois(n = 1000, lambda = 3)

# counts of the distinct values
table(y1)

# visual of the distribution of counts
table(y1) %>% plot()

# visual of the proportion of counts
table(y1) %>% prop.table() %>% plot()

# the mean and variance are roughly equal
c(mean(y1), var(y1)) 


# Negative binomial distribution

# Use the `rnbinom` function to generate random data from a negative binomial
# distribution with a specified mean and dispersion parameter (theta).

# mu is the mean. The size argument is the dispersion parameter (theta). The
# negative binomial allows the variance to be greater than the mean.
# Var = mu + mu^2/theta
set.seed(2)

# Sample data from a negative binomial distribution with a mean = 3 and theta = 2. 
y2 <- rnbinom(n = 1000, mu = 3, size = 2)

# counts of the distinct values
table(y2)

# visual of the distribution of counts
table(y2) %>% plot()

# visual of the proportion of counts
table(y2) %>% prop.table() %>% plot()

# the mean and variance are not equal. The variance is larger
c(mean(y2), var(y2)) 


# Simulating count data with conditional mean -----------------------------

# Above we plugged in a single mean. But perhaps the mean depends on some other
# variable. Perhaps the mean is lower for an "untreated" group than the mean for
# a "treated" group.

# Simulate a bunch of 0s and 1s to indicate untreated and treated
set.seed(3)
trt <- sample(0:1, size = 1000, replace = T)

# sample from a Poisson distribution with a mean of exp(1.2 + 1.8*trt)
y1 <- rpois(n = 1000, lambda = exp(1.2 + 1.8*trt))

# Why use exp()? That ensures we get a positive mean. Lambda has to be positive.
# Here it's not necessary but as we'll see this transformation is built into
# count models.

# expected count when trt = 0
exp(1.2) # about 3
# expected count when trt = 1
exp(1.2 + 1.8) # about 20

# plot of y1 distribution
table(y1) %>% plot()


# Let's do the same for a negative binomial distribution

# Sample from a negative binomial distribution with a mean of exp(1.2 + 1.8*trt)
# and a dispersion of 1.2.
set.seed(4)
y2 <- rnbinom(n = 1000, mu = exp(1.2 + 1.8*trt), size = 1.2)

# plot of y2 distribution
table(y2) %>% plot()



# Count modeling with simulated data --------------------------------------

# Let's pretend we don't know the formula used to generate the counts: 
# 1.2 + 1.8*trt. 

# How to recover those "true" values? This is essentially count modeling.

# Fitting a Poisson count model

# We use the glm() function to fit a poisson count model. Use the same way as
# lm(), but include the family argument: `family = poisson` The `link = "log"`
# argument is assumed by default but is included here for completeness.

# Let's fit a model to our simulated data. We generated the data so we know the
# correct model to specify!
# y1 <- rpois(n = 1000, lambda = exp(1.2 + 1.8*trt))

# y1 ~ trt is shorthand for "y1 = (Intercept) + slope*trt"
m1 <- glm(y1 ~ trt, family = poisson(link = "log"))
summary(m1)
coef(m1)

# The coefficients are very close to the "true" values of 1.2 and 1.8.

# Notice the coefficients are on the log scale. We need to use exp() to get the
# coefficients on the original scale.

# When trt == 0 the expected mean count is about 3.3
coef(m1)[1] %>% exp()

# When trt == 1 (sum both coefficients) the expected mean count is about 20.2
sum(coef(m1)) %>% exp()

# exponentiating the second coefficient tells us the multiplicative effect of
# trt == 1. Expected count of y when trt = 1 is about 6.1 times the expected
# count when trt = 0.
coef(m1)[2] %>% exp()

# Notice these are equal
exp(coef(m1)[1]) * 6.109849
sum(coef(m1)) %>% exp()


# Let's use our model to simulate some data and see if it looks similar to our
# oberved data set.
sim_y1 <- rpois(n = 1000, lambda = exp(1.198277 + 1.809902*trt))
plot(table(y1))
plot(table(sim_y1))

# We could also plot smooth density curves that approximate the distribution,
# though some may argue this is not appropriate for discrete data. 
sim_y1 <- simulate(m1, nsim = 50)
plot(density(y1, from = 0))
for(i in 1:50)lines(density(sim_y1[[i]], from = 0), 
                    col = "grey80", lty = 3)


# Fitting a Negative binomial count model

# Use the glm.nb() function from the MASS package to fit a negative binomial
# model. No need to specify the family argument. The `link = log` argument is
# assumed by default but is included here for completeness.

# Let's fit a model to our simulated data. We generated the data so we know the
# correct model to specify!
# y2 <- rnbinom(n = 1000, mu = exp(1.2 + 1.8*trt), size = 1.2)

m2 <- glm.nb(y2 ~ trt, link = log)
summary(m2)
coef(m2)
m2$theta

# Again, the coefficients are very close to the "true" values of 1.2, 1.8 and
# 1.2

# And again we need to use exp() to get our expected counts on the original
# scale.

# When trt == 0 the expected mean count is about 3.4
coef(m2)[1] %>% exp()

# When trt == 1 (sum both coefficients) the expected mean count is about 20.4
sum(coef(m2)) %>% exp()

# The multiplicative effect of trt==1 is about 6.1.
coef(m2)[2] %>% exp()


# Let's use our model to simulate some data and see if it looks similar to our
# oberved data set.
sim_y2 <- rnbinom(n = 1000, mu = exp(coef(m2)[1] + coef(m2)[2]*trt), 
                  size = m2$theta)
plot(table(y2))
plot(table(sim_y2))

# We could also plot smooth density curves that approximate the distribution
sim_y2 <- simulate(m2, nsim = 50)
plot(density(y2, from = 0))
for(i in 1:50)lines(density(sim_y2[[i]], from = 0), 
                    col = "grey80", lty = 3)


# YOUR TURN #1 ------------------------------------------------------------

# Run the following lines of code to generate y
set.seed(4)
grp <- sample(c("a","b"), size = 300, replace = T)
x1 <- runif(n = 300, min = 1, max = 10)
y <- rpois(n = 300, lambda = exp(1.8 + 1.5*(grp=="b") + -0.6*x1))

# (1) Fit a model that attempts to "recover" the true values in the model
#     specified above.


# (2) Simulate data from the model and compare to the original observed data.



# Deviance ----------------------------------------------------------------

# Let's look again at the summary of m1, the poisson model
summary(m1)

# Notice the lines for Null deviance and Residual deviance. 

#     Null deviance: 7715.13  on 999  degrees of freedom
# Residual deviance:  977.78  on 998  degrees of freedom

# Residuals represent the difference between what we observed and what our model
# predicts. There are several types of residuals for GLM models. The Null and
# Residual deviance are the sum of squares of the deviance residuals.

# Residual deviance "by hand"
residuals(m1, type = "deviance")^2 %>% sum()

# Null deviance "by hand"
# The null deviance is the sum of squared deviance residuals for a model with no
# predictors, just an intercept: y1 ~ 1
null_m1 <- glm(y1 ~ 1, family = poisson)
residuals(null_m1, type = "deviance")^2 %>% sum()

# Three things to remember about deviance...

# (1) Deviance is a measure of error, lower means better

# (2) If a predictor is just noise and has no explanatory power, we expect
#     deviance to decrease by 1 on average.

# (3) When a useful predictor is added to a model, we expect deviance to
#     decrease by more than 1.

# We see a huge decrease in deviance in m1. 



# Poisson count modeling with real data -----------------------------------

# Let's load some real data to explore and model. A data set giving the number
# of publications by doctoral candidates in biochemistry in relation to various
# predictors.

# Source: "Discrete Data Analysis with R" by Michael Friendly and David Meyer.

data("PhdPubs", package = "vcdExtra")
str(PhdPubs)

# articles: number of articles published in final three years of PhD studies
# female: dummy variable for gender, 1 = female
# married: dummy variable for marital status, 1 = married
# kid5: number of young children, age 5 and under
# phdprestige: prestige of the PhD department
# mentor: number of publications by the mentor in the preceding three years

summary(PhdPubs)

# Make female and married Factors
PhdPubs <- PhdPubs %>% 
  mutate(female = factor(female, labels = c("no", "yes")),
         married = factor(married, labels = c("no", "yes")))

# Plot the response variable: number of published articles
table(PhdPubs$articles) %>% plot()


# The number of published articles has quite a bit of variability. Can we model
# the counts using the other variables?

# Let's fit a Poisson model using female and mentor. 
phd.pois <- glm(articles ~ female + mentor, 
                data = PhdPubs, 
                family = poisson)
summary(phd.pois)

# Naive interpretation of coefficients.

# The coefficients represent the increase/decrease in the log of expected number
# of articles. 
coef(phd.pois)

# If we exponentiate, we get the multiplicative effect (holding other predictors
# constant.)
exp(coef(phd.pois))

# If we exponentiate, multiply by 100 and subtract 1, we get the percent
# increase or decrease.
100 * (exp(coef(phd.pois)) - 1)

# All together in a matrix using cbind(), rounded to 3 decimal places
round(
  cbind(beta = coef(phd.pois),
        exp_beta = exp(coef(phd.pois)),
        pct = 100 * (exp(coef(phd.pois)) - 1)),
  3)

# The expected number of publications for female candidates is about 0.83
# times that of male candidates, or about a 17% decrease

# Each additional mentor publication increases expected count of published
# articles by about 2.5%.

# Using both predictors seem to be better than using just an intercept.


# Confidence intervals

# Confidence intervals give us a sense of how certain these estimated effects
# are. Since we exponentiate, overlapping 1 (instead of 0) indicates an
# uncertain direction in magnitude.
confint(phd.pois) %>% exp()

# The effect of female is estimated to decrease expected published article count anywhere from 25% to 8%.

# We should check the fit of the model before putting too much stock into these
# interpretations. We could do something like the following:

sim_articles <- simulate(phd.pois)
plot(density(PhdPubs$articles))
lines(density(sim_articles$sim_1), col = "red")

# This doesn't look good. We'll explore another model fit visualization below
# called rootograms.



# YOUR TURN #2 ------------------------------------------------------------

# (1) Add kid5 to the following model:
# phd.pois <- glm(articles ~ female + mentor, data = PhdPubs, family = poisson)


# (2) Interpret the coefficient for kid5



# Evaluating model fit with rootograms ------------------------------------

# A good fitting model should generate counts that look similar to the original
# data.

# In the previous exercise we proposed the following model:
# articles = (Intercept) + female + married + kid5
phd.pois2 <- glm(articles ~ female + mentor + kid5,
                 data = PhdPubs, family = poisson)

# The result of the model was an intercept and coefficients for the 3 predictors
coef(phd.pois2)

# Previously we simulated data and tried comparing smooth densities of the
# observed and predicted distributions, but this doesn't always work well due to
# the discrete values our model generates.
sim2 <- simulate(phd.pois2)
density(PhdPubs$articles) %>% plot()
density(sim2$sim_1) %>% lines(col = "red")

# Another way to visualize model fit for count models is through rootograms.

# The countreg package provides the rootogram() function to quickly generate
# "hanging" rootograms to visualize goodness of fit.
countreg::rootogram(phd.pois2)

# The red dots and connector lines visualize the model's expected number of
# articles.

# The length of the bars are the observed counts. Notice the counts have been
# transformed with a square root transformation. That's to help smaller counts
# not get squashed by larger counts.

# Bars that dip below 0 are underfit; bars that hang above 0 are overfit.

# Use the max argument to increase or decrease the count displayed
countreg::rootogram(phd.pois, max = 15)

# There are two other styles available: standing and suspended
countreg::rootogram(phd.pois2, style = "standing")
countreg::rootogram(phd.pois2, style = "suspended")

# ggplot2 version
autoplot(rootogram(phd.pois))

# This is not a good fitting model. We are underfitting 0 counts, overfitting
# 1, 2, and 3 counts, then underfitting counts 4 - 7.

# Recall our earlier toy model for which we fit the "correct" model. Notice
# we're not consistently over or underfitting counts. The counts seem to vary
# around 0 randomly.
countreg::rootogram(m1)

# See Appendix at conclusion of script for how to make rootograms "by hand"




# YOUR TURN #3 ------------------------------------------------------------


# (1) Add married and phdprestige to the following model and save as a new model
# object.
# phd.pois <- glm(articles ~ female + mentor + kid5, data = PhdPubs, family = poisson)



# (2) Create a rootogram of the model




# Negative Binomial count modeling with real data -------------------------

# Recall the negative binomial model accommodates unequal mean and variance.
# This allows us to model "overdispersion". That is, when we have counts with
# larger variance than their mean.

# The PhdPubs data appears to exhibit some overdispersion
mean(PhdPubs$articles)
var(PhdPubs$articles)

# The AER package provides a formal test of overdispersion with the
# `dispersiontest()` function. The null is the mean and variance are equal.
# Rejecting the null with a small p-value provides evidence of overdispersion.

phd.pois3 <- glm(articles ~ female + mentor + kid5 + married + phdprestige,
                 data = PhdPubs, family = poisson)

# The first argument is a fitted Poisson model.
dispersiontest(phd.pois3)

# It appears we have overdispersion and that a Poisson model may not be
# suitable, which is also what we saw with the rootogram. In this case a
# negative binomial model may be more suitable since it accommodates
# overdispersion.
 
# The glm.nb function in the MASS package fits a negative binomial model
phd.nb <- glm.nb(articles ~ ., data = PhdPubs)
summary(phd.nb)

# The interpretation of the coefficients is the same as a Poisson model.
coef(phd.nb) %>% exp()
 
# Is this model better than the Poisson model?

# Can use AIC/BIC. Lower is better.
AIC(phd.pois3, phd.nb)
BIC(phd.pois3, phd.nb)

# The rootogram allows us to asses how well the model fits the observed data.
countreg::rootogram(phd.nb)

# This certainly seems to fit better.


# Zero-inflated count models ----------------------------------------------


# It's not unusual for real life count data to have more zeroes than would be
# expected of a Poisson or negative binomial model. When this happens we say we
# have "zero inflation".

# A common approach to modeling such data is to create a mixture of two models:
# one governing the occurrence of zeroes, and one governing the counts (which
# can also produce zeroes).

# Two sources of zeroes:
# - zeroes because they could be nothing else
# - zeroes due to chance; they could have been 1, 2, ...

# Example: number of peanuts eaten on a Friday night by UVa students.
# - Some have peanut allergies and will eat 0
# - Some can eat peanuts but don't

# Let's simulate data with these qualities.
 
# create a predictor variable
trt <- rep(0:1, each = 150)

# two means: 0.05 when trt = 0; 0.85 when trt = 1
mu <- 0.05 + 0.8*trt 

# simulate zeroes and ones using rbinom (random draws from a binomial
# distribution). A binomial distribution has two parameters: 

# - size (number of "trials") 
# - probability (chance of "success"). 

# Below we generate 300 draws from a binomial distribution with size = 1 and
# prob = 0.7. This says 70% of the time we will draw a 1, which implies 30% of
# the time we will draw 0.
set.seed(10)
zi <- rbinom(300, size = 1, prob = 0.7)

# Now simulate counts conditional on our draws from the binomial distribution.
# If it's a 0, keep it. Otherwise take the result from a Poisson distribution
# with mean = exp(0.05 + 0.8*trt). These creates extra zeroes. NOTE: we could
# also use the negative binomial distribution

y <- ifelse(zi == 0, 0, rpois(300, lambda=exp(mu)))  
dat <- data.frame(y, trt)
table(dat$y) %>% plot()

# without zero inflation
rpois(300, lambda=exp(mu)) %>% table() %>% plot()

# Fit a model to our data that does not accommodate excess zeroes
mod1 <- glm(y ~ trt, data = dat, family = poisson)
summary(mod1)
countreg::rootogram(mod1)

# underfitting zeroes and overfitting counts of 1 and 2 is characteristic of
# zero-inflated data.
 
# Fit a model that accommodates excess zeroes using the zeroinfl() function
# from the pscl package. This fits two models, separated by the |
# - a Poisson count model: y ~ trt
# - a binomial model: 1 (intercept only model)
 
mod2 <- pscl::zeroinfl(y ~ trt | 1, data = dat, dist = "poisson")
summary(mod2)
countreg::rootogram(mod2)
coef(mod2)

# Use the plogis() function to take the inverse logit of the binomial model
# coefficient to get the estimated probability of being a zero count with no
# chance of being anything greater than 0. Notice we come close to the "true"
# value of 0.3.
 
plogis(coef(mod2)["zero_(Intercept)"])

# Let's do the same for negative binomial using the same mu.

# two means: 0.6 when trt = 0; 2.4 when trt = 1
trt <- rep(0:1, each = 500)
mu <- 0.6 + 1.8*trt 
set.seed(15)
zi <- rbinom(1000, size = 1, prob = 0.7)
# Now simulate negative binomial counts with theta = 9
y2 <- ifelse(zi == 0, 0, rnbinom(1000, mu=exp(mu), size = 9))  
dat2 <- data.frame(y2, trt)

# Notice the zero inflation
table(dat2$y2) %>% plot()

# without zero inflation
rnbinom(1000, mu=exp(mu), size = 9) %>% table() %>% plot()

# Fit a model to our data that does not accommodate excess zeroes
mod3 <- glm.nb(y2 ~ trt, data = dat2)
summary(mod3)
countreg::rootogram(mod3)

# underfitting zeroes and overfitting counts of 1 and 2 is characteristic of
# zero-inflated data.

# Fit a model that accommodates excess zeroes using the zeroinfl() function
# from the pscl package. This fits two models, separated by the |
# - a negative binomial count model: y2 ~ trt
# - a binomial model: 1 (intercept only model)

mod4 <- pscl::zeroinfl(y2 ~ trt | 1, data = dat2, dist = "negbin")
summary(mod4)
countreg::rootogram(mod4)
coef(mod4)

plogis(coef(mod4)["zero_(Intercept)"])

# If we don't specify anything after the |, the zeroinfl() function assumes you
# want to use all predictors in both models.
mod5 <- pscl::zeroinfl(y2 ~ trt, data = dat2, dist = "negbin")
summary(mod5)

# Notice trt is included in the Zero-inflation model. And it doesn't appear to
# be useful.


# YOUR TURN #4 ------------------------------------------------------------


# Recall the distribution of PhD articles. Might there be 0 inflation? Maybe
# some candidates are trying to publish but haven't just yet. And there may be
# others who have no intention or desire to publish. Two sources of zeroes.
table(PhdPubs$articles) %>% plot()

# (1) Fit a zero-inflated negative binomial model using all predictors. Call it
# phd.zinb. 



# (2) Compare the zero-inflated negative binomial model to the negative binomial
# model we fit earlier (phd.nb) using AIC and BIC.


# (3) Compare rootograms for the two models



# Hurdle models -----------------------------------------------------------

# Another type of model for excess zeroes is the hurdle model. In this case we
# assume there is one and only one process for the zeroes, and a separate
# process for counts greater than 0.

# Example: number of cigarettes smoked in one month. 
# - Zero counts are non-smokers
# - non-zero counts are smokers (ie, zero truncated)

# We can simulate data as before using the rztpois() function from the countreg
# package, which simulates data from a zero-truncated poisson distribution.

# two means: 0.9 when trt = 0; 2.4 when trt = 1
trt <- rep(0:1, each = 300)
mu <- 0.9 + 1.5*trt 
set.seed(20)
zi <- rbinom(600, size = 1, prob = 0.4) # ~60% zeroes

# Now simulate counts conditional on our draws from the binomial distribution.
# If it's a 0, keep it. Otherwise take the result from a zero-truncated Poisson
# distribution with mean = exp(0.9 + 1.5*trt). NOTE: we could also use the
# negative binomial distribution

y <- ifelse(zi == 0, 0, rztpois(600, mean=exp(mu)))  
dat <- data.frame(y, trt)
table(dat$y) %>% plot()

# Fit a model to our data that does not accommodate excess zeroes
mod1 <- glm(y ~ trt, data = dat, family = poisson)
summary(mod1)
countreg::rootogram(mod1)


# Fit a model that accommodates excess zeroes using the hurdle() function
# from the pscl package. This fits two models, separated by the |
# - a zero-truncated Poisson count model: y ~ trt
# - a binomial model: 1 (intercept only model)
mod2 <- pscl::hurdle(y ~ trt | 1, data = dat, dist = "poisson")
summary(mod2)
countreg::rootogram(mod2)
coef(mod2)

# Use the plogis() function to take the inverse logit of the binomial model
# coefficient to get the estimated probability of clearing the "hurdle" (ie,
# having a count greater than 0)
plogis(coef(mod2)["zero_(Intercept)"])

# In this case, this is simply the proportion of counts NOT 0
mean(dat$y != 0)

# The binomial part of the hurdle model is the "Hurdle" to clear before the
# count model process kicks in. In this simple model, we might say we estimate
# about 40% of the subjects to clear the hurdle, which is what we specified when
# we generated the data.





# Interactions and Effect plots -------------------------------------------


# Effect plots help us visualize our models. They allow us to see the effects of
# certain predictors holding others at a common value such as a mean, median or
# mode. We essentially make predictions with our model using various values of
# a "focal predictor" with all other predictors set to a fixed value.

# Effect plots are very useful for visualizing interactions and non-linear
# effects.


# Interaction example
phd.nb2 <- glm.nb(articles ~ female + married + kid5 + phdprestige + mentor +
                   phdprestige:mentor, data = PhdPubs)
summary(phd.nb2)


mentor_eff <- ggpredict(model = phd.nb2, terms = c("mentor", "phdprestige"))
plot(mentor_eff)

mentor_eff <- ggpredict(model = phd.nb2, terms = c("mentor", "phdprestige[1,3,5]"))
plot(mentor_eff)

mentor_eff <- ggpredict(model = phd.nb2, terms = c("phdprestige","mentor[3,6,9,12]"))
plot(mentor_eff)

# Non-linear effect example
library(splines)
phd.nb3 <- glm.nb(articles ~ female + married + kid5 + phdprestige + ns(mentor, df = 3), 
                  data = PhdPubs)
summary(phd.nb3)

mentor_eff <- ggpredict(model = phd.nb3, terms = "mentor")
plot(mentor_eff)

mentor_eff <- ggpredict(model = phd.nb3, terms = "mentor[10,20,30,40]")
plot(mentor_eff)



# Rate models -------------------------------------------------------------

# Not all counts are created equal. For example two intersections may have
# similar counts of accidents per month. But if one intersection handles much
# less traffic than the other and has a similar number of accidents, then
# clearly it seems more dangerous.

# Let's simulate some data
set.seed(4) 

# Exposure = number of opportunities for something to happen
exposure <- round(runif(n = 200, min = 100, max = 20000))

# some sort of grouping or treatment condition
trt <- sample(0:1, 200, replace = TRUE)

# generate counts where the mean is exp(3) if trt == 0 and exp(5) if trt == 1.
count <- rpois(n = 200, lambda = exp(3 + 2*trt))

rate_data <- data.frame(count, trt, exposure, 
                        rate = count/exposure)
head(rate_data)

# Notice the counts for observations 2 and 5 are similar, but the exposures,
# and thus the rates, are very different.
 
# To model this data appropriately we should include the exposure. Log transform
# the "exposure" and use either the offset argument or include in the model
# formula using the `offset` function

rate.mod1 <- glm(count ~ trt, offset = log(exposure), 
                 data = rate_data, family = poisson)
# same thing
rate.mod2 <- glm(count ~ trt + offset(log(exposure)), 
                 data = rate_data, family = poisson)
all.equal(rate.mod1$coefficients, rate.mod2$coefficients)

# The interpretation is in terms of the _rate_ rather than the mean
summary(rate.mod1)

# Notice the intercept is nowhere close to the true value of 3. It is the
# estimated rate when all other explanatory variables are set to 0. In this
# case that's the estimated rate when trt = 0.
exp(coef(rate.mod1)["(Intercept)"])

# The estimated rate when trt = 1 is exp(1.927) = 6.9 times that of the rate
# when trt = 0.
exp(coef(rate.mod1)["trt"])

# The estimated rate when trt = 1 is exp(-6.19872 + 1.92684)
exp(sum(coef(rate.mod1)))

# To use the model to make predictions, we need to include an exposure. Below
# we get predicted values for the two levels of trt with exposure set to
# 10,000. (Notice we don't need to log transform the exposure.) The default
# prediction type is "link". In this case the link is the log transform. Notice
# the result closely matches the "true" values we used to simulate the data.
predict(rate.mod1, type = "link",
        newdata = data.frame(trt = c(0,1), exposure = c(10000, 10000)))

# To get the predicted values as counts, we need to specify the type as
# "response". Below we get predicted counts per 10,000
predict(rate.mod1, type = "response",
        newdata = data.frame(trt = c(0,1), exposure = c(10000, 10000)))

# To get the predicted values as rates, we need to specify the type as
# "response" and divide by the exposure. 
p <- predict(rate.mod1, type = "response",
        newdata = data.frame(trt = c(0,1), exposure = c(10000, 10000)))
data.frame(p, rate = p/10000)

# It doesn't matter what exposure you pick, the rate will be the same:
p <- predict(rate.mod1, type = "response",
             newdata = data.frame(trt = c(0,1), exposure = c(12345, 12345)))
data.frame(p, rate = p/12345)


# Notice the rates match what we got above by exponentiating the coefficients.


# We can create effect plots using the ggeffects package, but there are some
# modifications we need to make to plot rates:

# - need to use ggeffect() instead of ggpredict().

# - ggeffects expects the model to have been fit with the offset argument
#   specified (as opposed to being included in the model formula)

# - We need to add rates to the data frame created by ggeffect()

# The following plots the expected counts per 10,000, not the incidence rates
eff_out <- ggeffect(rate.mod1, terms = "trt", offset = log(10000))
plot(eff_out)

# Notice the fit is a line that treats trt as if it's a continuous number.
# Let's refit the model with trt as a factor.
 
rate_data$trtF <- factor(rate_data$trt)
rate.mod <- glm(count ~ trtF, offset = log(exposure), 
                 data = rate_data, family = poisson)
eff_out <- ggeffect(rate.mod, terms = "trtF", offset = log(10000))
plot(eff_out)

# To plot expected rates, we need to divide the fit and CI lower and upper
# bounds by 10,000. We do this below with mutate_at
eff_out2 <- eff_out %>% 
  mutate_at(c("predicted", "conf.low", "conf.high"), 
            .funs = function(y)y/10000)

# Once we do that, we have to create the plot ourselves:
ggplot(eff_out2, aes(x, predicted)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1)

# Instead of per 10,000, we could do per 1,000, but notice the plot does not change:
eff_out <- ggeffect(rate.mod, terms = "trtF", offset = log(1000))
eff_out2 <- eff_out %>% 
  mutate_at(c("predicted", "conf.low", "conf.high"), 
            .funs = function(y)y/1000)
ggplot(eff_out2, aes(x, predicted)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1)

# Changing the exposure modifies expected counts but not expected rates.



# Appendix: tidyverse simulation plot -------------------------------------

# tidyverse method of plotting simulated model data.

# simulate data, fit a model and generate simulations from the model
x <- 1:25
set.seed(1)
y <- rnorm(n = 25, mean = 10 + 5*x, sd = 10)
mod <- lm(y ~ x)
y_sim <- simulate(mod, nsim = 20)

# Requires a data frame and that the simulations data set be in "long" form.
d <- data.frame(x, y)
ggplot(d, aes(x = y)) +
  geom_line(aes(x = y, group = sim), 
            pivot_longer(y_sim, everything(), 
                         names_to = "sim", 
                         values_to = "y"),
            color = "lightblue",
            stat = "density") +
  geom_line(stat = "density")



# Appendix: rootograms "by hand" ------------------------------------------

# Recall this model and associate rootogram
summary(phd.pois)
rootogram(phd.pois)

# Here's how we can make the rootogram "by hand"

# Get expected mean count at each observation. The model returns the predicted
# mean count at each observation.
mu <- predict(phd.pois, type = "response")

# Next we create an empty matrix. Using the expected mean count for each
# observation, we calculate the probability of a 0 count, a 1 count,.., and a 9
# count. The `dpois` function returns the expected probability of a count for a
# given mean (lambda).
p <- matrix(NA, nrow = length(mu), ncol = length(0:9))
for (i in 0:9) p[, i + 1L] <- dpois(i, lambda = mu)

# Then we sum the columns (the probabilities) to get an expected count of 0,
# 1,...9. We also take the square root to put all the expected counts on a
# similar scale.
expctd <- sqrt(colSums(p))
expctd

# Now get the observed counts and take the square roor
obs <- sqrt(as.vector(table(PhdPubs$articles)[1:10]))

# Put everything into a data frame including the difference between expected and
# observed counts. This is our data frame to create the rootogram.
d <- data.frame(count = 0:9,
                obs,
                expctd,
                diff = expctd - obs)

# Make the rootogram. It's a little different than the one produced by
# `rootogram` but it works.
ggplot(d) +
  geom_segment(aes(x = count, y = diff, 
                   xend = count, yend = expctd),
               size = 6, alpha = 1/4) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point(aes(x = count, y = expctd), col = "red") +
  geom_line(aes(x = count, y = expctd), col = "red") +
  scale_x_continuous(breaks = 0:9, minor_breaks = NULL)




# Appendix: glm residuals -------------------------------------------------

# How response, pearson and deviance residuals are calculated

# obs 2
trt[2]
y1[2]

# obs 2 is in the trt=1 group, so they're predicted value is simply this:
mu_hat <- exp(sum(coef(m1)))

# response residual for obs 2
residuals(m1, type = "response")[2]
y1[2] - mu_hat

# pearson residual for obs 2
residuals(m1, type = "pearson")[2]
(y1[2] - mu_hat)/sqrt(mu_hat)

# deviance residual for obs 2
residuals(m1, type = "deviance")[2]
sign(y1[2] - mu_hat)* sqrt(2*(y1[2]*log(y1[2]/mu_hat) - (y1[2] - mu_hat)))


