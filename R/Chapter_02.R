#########################################################################
# Project: Statistical Rethinking
# Script purpose: Chapter 2
# Date: 10 April 2019
# Author: Jon Leslie
#########################################################################

ways <- c( 0 , 3 , 8 , 9 , 0 )
ways/sum(ways)

dbinom(6, size = 9, prob = 0.5)
dbinom(6, size = 10, prob = 0.6)


# 2.3 Grid appoximation----------------------------------------------------

# define grid
p_grid <- seq(from = 0, to = 1, length.out = 100)

# definte prior
prior <- rep(1, 20)

# compute likelihood at each value in grid
likelihood <- dbinom(6, size = 9, prob = p_grid)

# compute product of likelihood and prior
unstd.posterior <- likelihood * prior

# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)


plot(p_grid, posterior, type = "b", 
     xlab = "probability of water",
     ylab = "posterior probability")
mtext("20 points")


prior <- ifelse(p_grid < 0.5, 0, 1)
prior <- exp( -5 * abs(p_grid - 0.5))


# 2.6 Quadraditic approximation -------------------------------------------

library(rethinking)
globe.qa <- map(
  alist(
    w ~ dbinom(9, p), # binomial likelihood
    p ~ dunif(0, 1) # uniform prior
  ),
  data = list(w = 6)
)


# display summary of quadratic approximation
precis( globe.qa )

# analytical calculation
w <- 6
n <- 9
curve(dbeta(x, w+1, n-w+1), from = 0, to = 1)
# quadratic approximation
curve(dnorm(x, 0.67, 0.16), lty = 2, add = TRUE)


# 2M1 ---------------------------------------------------------------------

compute_grid <- function(w, n){
  # define grid
  p_grid <- seq(from = 0, to = 1, length.out = 20)
  
  # definte prior
  prior <- rep(1, 20)
  
  # compute likelihood at each value in grid
  likelihood <- dbinom(w, size = n, prob = p_grid)
  
  # compute product of likelihood and prior
  unstd.posterior <- likelihood * prior
  
  # standardize the posterior, so it sums to 1
  posterior <- unstd.posterior / sum(unstd.posterior)
  
  plot(p_grid, posterior, type = "b", 
       xlab = "probability of water",
       ylab = "posterior probability")
  
}

compute_grid(3, 3)
compute_grid(3, 4)
compute_grid(5, 7)


# 2M2 ---------------------------------------------------------------------

compute_grid_2M2 <- function(w, n){
  # define grid
  p_grid <- seq(from = 0, to = 1, length.out = 100)
  
  # definte prior
  prior <- ifelse(p_grid < 0.5, 0, 1)
  
  # compute likelihood at each value in grid
  likelihood <- dbinom(w, size = n, prob = p_grid)
  
  # compute product of likelihood and prior
  unstd.posterior <- likelihood * prior
  
  # standardize the posterior, so it sums to 1
  posterior <- unstd.posterior / sum(unstd.posterior)
  
  plot(p_grid, posterior, type = "l", 
       xlab = "probability of water",
       ylab = "posterior probability")
  
}

compute_grid_2M2(3, 3)
compute_grid_2M2(3, 4)
compute_grid_2M2(5, 7)


# 2M3 ---------------------------------------------------------------------

# p(Earth|land) = p(land|Earth) * p(Earth)/p(land)

# probabilities of land given Earth or Mars
p_land_Earth <- 0.3
p_land_Mars <- 1

# probabilities of tossing Earth or Mars
p_Earth <- 0.5
p_Mars <- 0.5

# average likelihood of land
p_land <- p_land_Earth * p_Earth + p_land_Mars * p_Mars

# probability of Earth given land
p_land_Earth*p_Earth/p_land


# 2M4 ---------------------------------------------------------------------

# using the counting approach

# possible outcomes |First side black | Second side black |
# b1 -> b2          |         1     ` |       1           |
# b2 -> b1          |         1       |       1           |
# b1 -> w1          |         1       |       0           |
# w1 -> b1          |         0       |       0           |
# w1 -> w2          |         0       |       0           |
# w2 -> w1          |         0       |       0           |

# Total outcomes where the first card has a black side-up is 3
# Total outcomes where the first card had black side-up, and 
# the second side is also black is 2.
# So, the probability that the second side is black given that
# the first side was black is 2/3

# p(2_sides_black|1_side_black) = p(1_side_black|2_sides_black) * p(2_sides_black)/p(1_side_black)
p_1_b <- 1/2
p_2_b <- 1/3
p_1_given_2_b <- 1
p_1_given_2_b * p_2_b / p_1_b


# 2M5 ---------------------------------------------------------------------

# possible outcomes |First side black | Second side black |
# b1 -> b2          |         1     ` |       1           |
# b2 -> b1          |         1       |       1           |
# b1 -> b2          |         1     ` |       1           |
# b2 -> b1          |         1       |       1           |
# b1 -> w1          |         1       |       0           |
# w1 -> b1          |         0       |       0           |
# w1 -> w2          |         0       |       0           |
# w2 -> w1          |         0       |       0           |

4/5

p_1_b <- 5/8
p_2_b <- 1/2
p_1_given_2_b <- 1
p_1_given_2_b * p_2_b/p_1_b


# 2M6 ---------------------------------------------------------------------


# possible outcomes |First side black | Second side black |
# b1 -> b2          |         1     ` |       1           |
# b2 -> b1          |         1       |       1           |
# b1 -> w1          |         1       |       0           |
# w1 -> b1          |         0       |       0           |
# b1 -> w1          |         1       |       0           |
# w1 -> b1          |         0       |       0           |
# w1 -> w2          |         0       |       0           |
# w2 -> w1          |         0       |       0           |
# w1 -> w2          |         0       |       0           |
# w2 -> w1          |         0       |       0           |
# w1 -> w2          |         0       |       0           |
# w2 -> w1          |         0       |       0           |

2/4

p_1_b <- 4/12
p_2_b <- 2/12
p_1_given_2_b <- 1
p_1_given_2_b * p_2_b/p_1_b


# 2M7 ---------------------------------------------------------------------

# ~valid, ~correct, ~first.card.(back.side), ~second.card.(back.side),
#      1,        1,                "b1(b2)",                 "w1(b1)",
#      0,        0,                "b1(b2)",                 "b1(w1)",
#      1,        1,                "b1(b2)",                 "w1(w2)",
#      1,        1,                "b1(b2)",                 "w2(w1)",
#      1,        1,                "b2(b1)",                 "w1(b1)",
#      0,        0,                "b2(b1)",                 "b1(w1)",
#      1,        1,                "b2(b1)",                 "w1(w2)",
#      1,        1,                "b2(b1)",                 "w2(w1)",
#      0,        0,                "b1(w1)",                 "b1(b2)",
#      0,        0,                "b1(w1)",                 "b2(b1)",
#      1,        0,                "b1(w1)",                 "w1(w2)",
#      1,        0,                "b1(w1)",                 "w2(w1)",
#      8,        6,                      NA,                       NA

6/8


# 2H1 ---------------------------------------------------------------------

p_A = 1/2 # prior probability of species A
p_B = 1/2 # prior probability of species B

p_twins_A = 0.1 # P(Twins|Species = A)
p_twins_B = 0.2 # P(Twins|Species = B)

# Given first birth are twins, what is the probability that
# second birth are also twins


# Given first birth are twins, what is the probability that
# second birth are also twins


# Given a species, births should be independent. 
# But, because we are unsure about the species, we need to 
# average out the species to estimate the probability of
# next birth being twins, given that the first birth 
# were twins.

# In other words,if I knew the species, knowing that the current 
# birth resulted in twins, would not change my expectation about 
# twins for the following birth.

# Thus, in principle:
# P(twins) = P(twins|Species = A)P(A) + P(twins|Species = B)P(B)

# However, knowing about a single twin birth, does change 
# the relative plausibilities of the two species. So, P(A) and
# P(B), are actually P(A|twins) and P(B|twins).

# P(A|twins) = P(twins|A)P(A)/P(twins)
# P(B|twins) = P(twins|B)P(B)/P(twins)
# P(twins) = P(twins|A)P(A) + P(twins|B)P(B)
p_twins = 0.1 * 0.5 + 0.2 * 0.5

p_A_twins = (0.1 * 0.5)/p_twins
p_B_twins = (0.2 * 0.5)/p_twins
p_A_twins
p_B_twins

p_twins_twins = 0.1 * p_A_twins + 0.2 * p_B_twins
p_twins_twins

# Another approach:
## 2H1

# find posterior for plausibility of each pandas species following the first birth of twins
species.1.likelihood <- .1
species.2.likelihood <- .2
likelihood <- c(species.1.likelihood, species.2.likelihood)
prior <- c(0.5, 0.5) # was written c(1, 1) on the webpage
unstandardized.posterior <- likelihood * prior
posterior <- unstandardized.posterior / sum(unstandardized.posterior)

# probability next birth is set of twins
posterior[1] * .1 + posterior[2] * .2


# 2H2 ---------------------------------------------------------------------

# P(Species = A|Twins)?
# P(Species = A |Twins) = P(Twins |Species = A)P(A)/P(Twins)

#P(Twins) = P(Twins|Species = A)P(A) + P(Twins|Species = B)P(B)

p_A_twins = ( 0.1 * 0.5 ) / (( 0.1 * 0.5 ) + ( 0.2 * 0.5 ))
p_A_twins
(0.1 * 0.5)/p_twins

# Another approach:
## 2H2
species.1.likelihood <- .1
species.2.likelihood <- .2
likelihood <- c(species.1.likelihood, species.2.likelihood)
prior <- c(0.5, 0.5)
unstandardized.posterior <- likelihood * prior
posterior <- unstandardized.posterior / sum(unstandardized.posterior)

# probability pandas is from species 1
posterior[1]
