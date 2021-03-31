library(bbmle)
# 1. Load the datasets using this function: load("datasets.RData"). 
# Notice they now appear in the environment tab.
load("datasets.RData")

# 2. Identify which dataset corresponds to your group. The topic that you'll focus on is the
# breakout group number you're randomly assinged to. I've not labelled columns in a helpful way at all. 
# Use exploratory plots and functions to determine which dataset is for your group. 
# Once you've done this, make a plot with an altered y label that describes the outcome variable.
# Topic 1 - Lognormal - d1 "hist(d1$y[d1$restored==0])"
# Topic 2 - Bernoulli - d3 "d3$y: 0 or 1"
# Topic 3 - Binomial - d4 "hist(d1$y[d1$restored==0])" max(d1$y[d1$restored==0]) close to 10
# Topic 4 - Poisson - d2 "hist(d2$y[d2$restored==0])" max(d2$y[d2$restored==0]) does not close to 10

# 3. Address your study question using the p-value approach. Convert the study question into
# a statistical hypothesis. Write out the null and alternative hypotheses. 
# Design a single model that uses 'R speak' (y ~ x) to test this model, and run this model. 
# Write down what the p-value tells you about the null hypothesis.
# H0: d2$y does not relate to d2$restored
m2a <- glm(d2$y ~ d2$restored, family = "poisson")
summary(m2a)         # d2$restored Pr(>|z|) <2e-16

# 4. Next, use the model selection approach to test whether restoration affects your outcome 
# variable by creating a second, intercept only model. Use AIC to compare the models. 
# Write down your interpretation. Do you have the same interpretation as in step 3?
m2b <- glm(d2$y ~ 1, family = "poisson")
summary(m2b)
AICtab(m2a, m2b, base = TRUE, weights = TRUE)
TRUE              # Rejects H0

# 5. Use the parameter estimates from the model in step 3 and the appropriate link function to 
# plot the expected (mean) value for the outcome variable in the control and restoration areas.
a <- coef(m2a)[[1]]
b <- coef(m2a)[[2]]
curve(a + b * x)

# 6. Use the relevant random number generators for your distribution and the parameters fit in 
# the model in step 3 to simulate the same data. Create a plot of these simulated data.
y <- c(rpois(length(d2$y) / 2, a), rpois(length(d2$y) / 2, a + b))
x <- rep(c(0,1), c(500, 500))
sim <- data.frame(y = y, restored = x)

# 7. Re-do steps 3-4 but with the mle2() function, which involves writing the model in 'math speak'.
m2c <- mle2( d2$y ~ dpois(lambda = a + b * d2$restored), start = list(a = mean(d2$y), b = 0), data = d2)
summary(m2c)
m2d <- mle2( d2$y ~ dpois(lambda = a), start = list(a = mean(d2$y)), data = d2)
summary(m2d)
AICtab(m2c, m2d, base = TRUE, weights = TRUE)
