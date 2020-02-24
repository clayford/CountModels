Anova(m3)
# Load the NMES1988 data from the AER package. Demand for medical care in the US
# National Medical Expenditure Survey (NMES) conducted in 1987.
data("NMES1988", package = "AER")

# extract a few columns of interest
nmes <- NMES1988[, c(1, 6:8, 13, 15, 18)]
names(nmes)

# visits: Number of physician office visits
# hospital: Number of hospital stays
# health:  self-perceived health status, "poor", "average", "excellent".
# chronic: Number of chronic conditions.
# gender: Factor indicating gender
# school: Number of years of education
# insurance: Is the individual covered by private insurance?

summary(nmes)
plot(table(nmes$visits))

# (1) Model the number of visits as a function of all predictors using a Poisson
# count model. Hint: use visits ~ . as the formula
nmes.pois <- glm(visits ~ ., data = nmes, family = poisson)
summary(nmes.pois)

# (2) How does the expected number of visits change for Males (gendermale)?
exp(confint(nmes.pois))
100 * (exp(coef(nmes.pois)["gendermale"]) - 1)


# We can use those coefficients and proposed model to simulate counts and see if
# they are similar to the original data.

# The "by hand" matrix algebra way is to multiply the matrix of predictors by
# the vector of coffecients:

# create a matrix of predictors using model.matrix()
X <- model.matrix(~ female + married + kid5, data = PhdPubs)

# simulate the data as before using rpois; %*% means matrix multiplication 
sim1 <- rpois(n = nrow(PhdPubs), lambda = exp(X %*% coef(phd.pois2)))


# Compare
plot(table(sim1), main = "simulated")
plot(table(PhdPubs$articles), main = "original")

# Forunately there's a simulate() function to help make this easier.


# Create a rootogram of the nmes Poisson model. What do you think?
# nmes.pois <- glm(visits ~ ., data = nmes, family = poisson)
rootogram(nmes.pois)



# YOUR TURN #4 ------------------------------------------------------------

# Fit a negative binomial model to the nmes data using all predictors, then
# create a rootogram to assess model fit. Compare the AIC between the Poisson
# and negative binomial models.
nmes.nb <- glm.nb(visits ~ ., data = nmes)
summary(nmes.nb)
rootogram(nmes.nb)
rootogram(nmes.nb, max = 10)
AIC(nmes.pois, nmes.nb)

