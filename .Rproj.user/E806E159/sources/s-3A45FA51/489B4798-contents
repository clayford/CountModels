# ch 11 - DDA with R

library(ggplot2)
library(vcdExtra)
library(effects)
library(countreg)

data("PhdPubs")
with(PhdPubs, c(mean = mean(articles), var = var(articles),
                ratio = var(articles)/mean(articles)))

table(PhdPubs$articles)
# to include 0 counts
art.fac <- factor(PhdPubs$articles, levels = 0:19)
art.tab <- table(art.fac)
art.tab

# Fig 11.1
barplot(art.tab)
abline(v = mean(PhdPubs$articles))
barplot(art.tab + 1, log = "y")

# Fig 11.2
boxplot(articles + 1 ~ married, data = PhdPubs, log = "y", cex.lab = 1.25)
plot(jitter(articles + 1) ~ mentor, data = PhdPubs, log = "y")
lines(lowess(PhdPubs$mentor, PhdPubs$articles + 1))

# ggplot version of Fig 11.2 (right)
ggplot(PhdPubs, aes(mentor, articles + 1)) +
  geom_jitter(position = position_jitter(h = 0.05)) +
  stat_smooth(method = "loess", size = 2, fill = "blue", alpha = 0.25) +
  stat_smooth(method = "lm", color = "red", se = F) +
  scale_y_log10(breaks = c(1,2,5,10,20))

# fit a poisson model using all predictors
PhdPubs <- within(PhdPubs, {
  female <- factor(female)
  married <- factor(married)
})

phd.pois <- glm(articles ~ ., data = PhdPubs, family = poisson)
summary(phd.pois)

round(cbind(beta = coef(phd.pois),
            expbeta = exp(coef(phd.pois)),
            pct = 100 * (exp(coef(phd.pois)) - 1)), 3)

plot(allEffects(phd.pois), rug = F)
# make plots strictly comparable
plot(allEffects(phd.pois), rug = F, ylim = c(0, log(10)))

# Example 11.2
data("CrabSatellites")
str(CrabSatellites)

ggplot(CrabSatellites, aes(width, satellites)) +
  geom_point(position = position_jitter(height = 0.2, width = 0)) +
  geom_smooth(se = F)

ggplot(CrabSatellites, aes(weight, satellites)) +
  geom_point(position = position_jitter(height = 0.2, width = 0)) +
  geom_smooth(se = F)

ggplot(CrabSatellites, aes(vcdExtra::cutfac(width), satellites)) +
  geom_boxplot()
ggplot(CrabSatellites, aes(vcdExtra::cutfac(weight), satellites)) +
  geom_boxplot()

crabs.pois <- glm(satellites ~ ., data = CrabSatellites, family = poisson)
summary(crabs.pois)
plot(allEffects(crabs.pois))

# set color numeric
CrabSatellites1 <- transform(CrabSatellites, color = as.numeric(color))
crabs.pois1 <- glm(satellites ~ weight + color, data = CrabSatellites1, family = poisson)
summary(crabs.pois1)
LRstats(crabs.pois, crabs.pois1)

library(faraway)
data("gala")
gala <- gala[,-2]
modp <- glm(Species ~ ., data = gala, family = poisson)
summary(modp)
LRstats(modp)


data(bliss)
mod1 <- glm(cbind(dead, alive) ~ conc, family = binomial, data = bliss)
LRstats(mod1)
deviance(mod1)
curve(dchisq(x, df = 3), from = 0, to = 10)
abline(v = deviance(mod1))

# Example 11.3
# estimate dispersion parameter
with(phd.pois, deviance/df.residual)
sum(residuals(phd.pois, type = "pearson")^2) / phd.pois$df.residual
phd.qpois <- glm(articles ~ ., data = PhdPubs, family = quasipoisson)
summary(phd.qpois)

# Example 11.4
# negative binomial
crabs.nbin <- glm.nb(satellites ~ weight + color, data = CrabSatellites)
phd.nbin <- glm.nb(articles ~ ., data = PhdPubs)

# Example 11.5
fit.pois <- fitted(phd.pois, type = "response")
fit.nbin <- fitted(phd.nbin, type = "response")

cutq <- function(x, q = 10){
  quantile <- cut(x, breaks = quantile(x, probs = (0:q)/q),
                  include.lowest = TRUE, labels = 1: q)
}

group <- cutq(fit.nbin, q = 10)
qdat <- aggregate(PhdPubs$articles, 
                  list(group), 
                  FUN = function(x) c(mean = mean(x), var = var(x)))
qdat <- data.frame(qdat$x)
qdat <- qdat[order(qdat$mean),]


# Example 11.6
# The connected dots are the expected counts
# The bars are the observed counts
library(countreg)
countreg::rootogram(phd.pois, max = 9)
countreg::rootogram(phd.nbin, max = 9)


# Testing overdispersion
library(AER)
dispersiontest(phd.pois)
dispersiontest(phd.pois, 2)

# NB1: quasi-poisson
# NB2: negative binomial

# Example 11.8

library(VGAM)
set.seed(1234)
data1 <- rzipois(200, 3, 0)
data2 <- rzipois(200, 3, 0.3)

tdata1 <- table(data1)
barplot(tdata1)
tdata2 <- table(data2)
barplot(tdata2)

# simulate zero inflation
trt <- rep(0:1, each = 150)
mu <- 0.05 + 0.8*trt 
pz <- 0.3  # Zero-inflation parameter
zi <- rbinom(300, size = 1, prob = pz)
y <- ifelse(zi, 0, rpois(300, lambda=exp(mu)))  
dat <- data.frame(y, trt)
barplot(table(dat$y))
# without zero inflation
barplot(table(rpois(300, lambda=exp(mu))))


mod1 <- glm(y ~ trt, data = dat, family = poisson)
summary(mod1)

countreg::rootogram(mod1)

mod2 <- pscl::zeroinfl(y ~ trt | 1, data = dat, dist = "poisson")
summary(mod2)
countreg::rootogram(mod2)

LRstats(mod1, mod2)


# Simulate negative binomial model (intercept only)
y <- rnegbin(n = 100, mu = exp(1.6), theta = 1.4)
barplot(table(factor(y, 0:21)))
mod3 <- glm.nb(y ~ 1)
summary(mod3)

# as theta gets large, negative binomial approaches a poisson
y <- rnegbin(n = 100, mu = exp(1.6), theta = 100)
barplot(table(factor(y, 0:21)))
mod4 <- glm.nb(y ~ 1)
mod5 <- glm(y ~ 1, family = poisson)
summary(mod4)
summary(mod5)

countreg::rootogram(mod4, max = 12)
countreg::rootogram(mod5, max = 12)


# Basic simulations

# Poisson
# lambda is the mean
# as mean gets bigger, variance stays the same
set.seed(1)
d <- rpois(n = 1000, lambda = 5)
table(d)
plot(table(d))
c(mean(d), var(d)) # about the same

m1 <- glm(d ~ 1, family = poisson)
summary(m1)
exp(coef(m1)) # recover the mean

trt <- sample(0:1, size = 1000, replace = TRUE)
d <- rpois(n = 1000, lambda = exp(3 + 2*trt))
plot(table(d))
m2 <- glm(d ~ trt, family = poisson)
summary(m2)
exp(coef(m2)) # recover the mean

# simulate data from model
sim.out <- simulate(m2)
plot(table(sim.out$sim_1))

# negative binomial
# As mu gets bigger, variance gets even bigger
d <- MASS::rnegbin(n = 500, mu = 10, theta = 1)
plot(table(d))
c(mean(d), var(d))


# observed
barplot(table(PhdPubs$articles))

# expected (sort of)
barplot(table(rpois(n = nrow(PhdPubs), lambda = fitted(phd.pois))))

# real expected
mu <- predict(phd.pois, type = "response")
p <- matrix(NA, length(mu), length(0:9))
for (i in 0:9) p[, i + 1] <- dpois(i, lambda = mu)
expctd <- colSums(p)
barplot(expctd)

# CF: Expected values
summary(PhdPubs)
lambda <- predict(phd.pois, newdata = data.frame(female = factor(0, levels = 0:1), 
                                                 married = factor(1, levels = 0:1), 
                                                 kid5 = 0, phdprestige = 3, 
                                                 mentor = 6))
p.out <- rpois(n = 100, lambda = lambda)
table(p.out)

p.out <- fitted(phd.pois)
exp.out <- dpois(x = 0:9, lambda = mean(fitted(phd.pois)))

p.out <- rpois(n = 915, lambda = fitted(phd.pois))
barplot(table(p.out))
barplot(table(PhdPubs$articles))
root.out <- countreg::rootogram(phd.pois)
# ggplot version
autoplot(root.out)

root.out$expected

# getting expected counts "by hand"
# from rootogram.glm
mu <- predict(phd.pois, type = "response", na.action = na.omit)
w <- model.weights(phd.pois)
p <- matrix(NA, length(mu), length(0:9))
for (i in 0:9) p[, i + 1L] <- dpois(i, lambda = mu)
expctd <- colSums(p)
expctd


data("NMES1988", package = "AER")
nmes <- NMES1988[, c("visits", "gender", "school", "insurance")]
m <- glm(visits ~ ., data = nmes, family = poisson)
predict(m, newdata = data.frame(gender = "male", 
                                school = 10, 
                                insurance = "yes"), 
        type = "response")


p <- rpois(1000, lambda = 5.546819)
table(p)

library(dplyr)
nmes %>% filter(gender == "male", 
                school == 10, 
                insurance == "yes") %>% 
  count(visits) %>% 
  summarize(mean(n), var(n))
