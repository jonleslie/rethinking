#########################################################################
# Project: Statistical Rethinking
# Script purpose: Chapter 3
# Date: 25/4/2019
# Author: Jon Leslie
#########################################################################

PrPV <- 0.95
PrPM <- 0.01
PrV <- 0.001
PrP <- PrPV * PrV + PrPM * (1 - PrV)
(PrVP <- PrPV * PrV / PrP)

PrPT <- 0.95
PrT <- 0.01
PrPF <- 0.05
PrP <- PrPT * PrT + PrPF * (1 - PrT)
(PrTP <- PrPT * PrT / PrP)

# 3.2 ---------------------------------------------------------------------

p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(6, 9, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior/sum(posterior)
plot(p_grid, posterior)

samples <- sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)
plot(samples)
library(rethinking)
dens(samples)

# 3.6 ---------------------------------------------------------------------

# Adds up posterior probability where p < 0.5
sum(posterior[p_grid < 0.5])
sum(samples < 0.5) / 1e4
sum(samples > 0.5 & samples < 0.75) / 1e4

quantile(samples, 0.8)
quantile(samples, c(0.1, 0.9))

# 3.11 --------------------------------------------------------------------

p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(3, size = 3, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior/sum(posterior)
samples <- sample(p_grid, size = 1e4, replace = TRUE, prob = posterior)

PI(samples = samples, prob = 0.5)
plot(p_grid, posterior)
HPDI(samples, prob = 0.50)

# 3.14 --------------------------------------------------------------------

p_grid[which.max(posterior)]
chainmode(samples, adj=0.01)
mean(samples)
median(samples)

# 3.17 --------------------------------------------------------------------

sum(posterior * abs(0.5 - p_grid))
loss <- sapply(p_grid, 
               function(d) sum(posterior * abs(d - p_grid)))
p_grid[which.min(loss)]

# 3.20 --------------------------------------------------------------------

dbinom(0:2, size = 2, prob = 0.7)
rbinom(1, size = 2, prob = 0.7)
rbinom(10, size = 2, prob = 0.7)
dummy_w <- rbinom(1e5, size = 2, prob = 0.7)
table(dummy_w)/1e5

dummy_w <- rbinom(1e5, size = 9, prob = 0.7)
table(dummy_w)/1e5
simplehist(dummy_w, xlab="dummy water count")

# Model Checking (section 3.3.2) ------------------------------------------

w <- rbinom(1e4, size = 9, prob = 0.6)
simplehist(w)

w <- rbinom(1e4, size = 9, prob = samples)
simplehist(w)


# Practice -----------------------------------------------------------------

p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(6, size = 9, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior/sum(posterior)
set.seed(100)
samples <- sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)

# 3E1 ---------------------------------------------------------------------

mean(samples < 0.2)
sum(samples < 0.2) / 1e4

# 3E2 ---------------------------------------------------------------------

mean(samples > 0.8)

# 3E3 ---------------------------------------------------------------------

mean(samples > 0.2 & samples < 0.8)

# 3E4 ---------------------------------------------------------------------

quantile(samples, probs = 0.2)

# 3E5 ---------------------------------------------------------------------

quantile(samples, probs = 0.8)

# 3E6 ---------------------------------------------------------------------

HPDI(samples, 0.66)

# 3E7 ---------------------------------------------------------------------

PI(samples, 0.66)



# 3M1 ---------------------------------------------------------------------

p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1e3)
likelihood <- dbinom(8, size = 15, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior/sum(posterior)
plot(p_grid, posterior)

# 3M2 ---------------------------------------------------------------------

samples <- sample(p_grid, size = 1e4, replace = TRUE, prob = posterior)
HPDI(samples = samples, prob = 0.9)

# 3M3 ---------------------------------------------------------------------

w <- rbinom(1e4, size = 15, prob = samples)
simplehist(w)
mean(w == 8)

# 3M4 ---------------------------------------------------------------------

w <- rbinom(1e4, size = 9, prob = samples)
simplehist(w)
mean(w == 6)

# 3M5 ---------------------------------------------------------------------

# 1
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- ifelse(p_grid < 0.5, 0, 1)
likelihood <- dbinom(8, size = 15, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior/sum(posterior)
plot(p_grid, posterior)

# 2
samples <- sample(p_grid, size = 1e4, prob = posterior, replace = TRUE)
HPDI(samples, prob = 0.9)

# 3
w <- rbinom(1e4, size = 15, prob = samples)
simplehist(w)
mean(w == 8)

# 4
w <- rbinom(1e4, size = 9, prob = samples)
simplehist(w)
mean(w == 6)

# 3H1 ---------------------------------------------------------------------

data(homeworkch3)
sum(birth1) + sum(birth2)

p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(110, 200, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior/sum(posterior)
plot(p_grid, posterior)
p_grid[which.max(posterior)]


# 3H2 ---------------------------------------------------------------------

samples <- sample(p_grid, size = 1e4, replace = TRUE, prob = posterior)
dens(samples)
HPDI(samples, prob = 0.5)
HPDI(samples, prob = 0.89)
HPDI(samples, prob = 0.97)
HPDI(samples = samples, prob = c(0.5, 0.89, 0.97))


# 3H3 ---------------------------------------------------------------------


w <- rbinom(n = 1e4, size = 200, prob = samples)
dens(w)
abline(v = 110, col = "red")

# 3H4 ---------------------------------------------------------------------

sum(birth1)
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(sum(birth1), size = length(birth1), prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior/sum(posterior)
samples <- sample(p_grid, size = 1e4, replace = TRUE, prob = posterior)
dens(samples)
abline(v = mean(birth1), col = "red")

w <- rbinom(1e4, size = 100, prob = samples)
dens(w)
abline(v = sum(birth1), col = "red")

# 3H5 ---------------------------------------------------------------------

# birth3 will be second births in which the first birth was a female
birth3 <- birth2[birth1 == 0]
sum(birth3)
mean(birth3)
w <- rbinom(1e4, size = length(birth3), prob = samples)
dens(w)
abline(v = sum(birth3), col = "red")
