

install.packages(c("sensemakr", "mediation"))

# OLS and Control Variables: Simulation

set.seed(2323) # makes simulation reproducible

u <- rbinom(n = 1000, size = 1, prob = 0.5) # binary u
x <- u + runif(n = 1000, min = 0, max = 2) # x is a function of u and uniform noise
y <- -0.25*x + u + rnorm(n = 1000, sd = 0.75) # y is a function of x, u, and Gaussian noise

# lm is used to fit linear model
summary(lm(y ~ x))
summary(lm(y ~ x + u))

display(summary(lm(y ~ x)))

plot(x, y)
abline(lm(y ~ x))

plot(x, y, col = u + 1)
legend("bottomright", legend = c("U = 0", "U = 1"), fill = c("black", "red"))

abline(lm(y[u == 0] ~ x[u == 0]))
abline(lm(y[u == 1] ~ x[u == 1]), col = "red")

# Simulation for d-separation

set.seed(2983)
D <- rnorm(1000)
M <- 0.4*D + rnorm(1000)
Y <- -0.6*M + rnorm(1000)
lm(Y ~ D)
lm(Y ~ D + M)

# Simulation for Post-Treatment Bias

set.seed(2983)
D <- rnorm(1000)
U <- rnorm(1000)
M <- 0.6*D + 0.6*U + rnorm(1000)
Y <- 0.6*U + rnorm(1000)
lm(Y ~ D)
lm(Y ~ D + M)

# Sensitivity

library(sensemakr)

# loads dataset
data("darfur")

model.1 <- lm(peacefactor ~ directlyharmed, data = darfur)

# runs sensemakr for sensitivity analysis
sensitivity.1 <- sensemakr(model.1, treatment = "directlyharmed",
                           kd = 1:3)

# plot bias contour of point estimate
plot(sensitivity.1)

# plot bias contour of t-value
plot(sensitivity.1, sensitivity.of = "t-value")

# Run model with gender and village as controls
# Plot sensitivity analysis with gender as benchmark covariate

model.2 <- lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
                pastvoted + hhsize_darfur + female + village, data = darfur)

# runs sensemakr for sensitivity analysis
sensitivity.2 <- sensemakr(model.2, treatment = "directlyharmed",
                           benchmark_covariates = "female",
                           kd = 1:3)

# plot bias contour of point estimate
plot(sensitivity.2)

# Mediation Analysis

library(mediation)

data("framing")

# generate treatment variable
framing$tone_eth <- ifelse(framing$tone == 1 & framing$eth == 1, 1, 0)

# average causal effect of treatment
summary(lm(immigr ~ tone_eth, data = framing))

# Mediator model:
model.m <- lm(emo ~ tone_eth, data = framing)

# Outcome model:
model.y <- lm(immigr ~ emo + tone_eth , data = framing)

out.1 <- mediate(model.m, model.y, sims=1000, 
                 treat="tone_eth", mediator="emo", boot=FALSE)

plot(out.1)

summary(out.1)

sensout.1 <- medsens(out.1, sims=10000, rho.by=.01)
summary(sensout.1)
plot(sensout.1)
plot(sensout.1, sens.par="R2")