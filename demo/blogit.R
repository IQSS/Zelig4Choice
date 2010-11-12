# library

#library(Zelig)
library(bivariate.zelig)


# data
data(sanction)

# zelig
## z.out1 <- zelig(cbind(import, export) ~ coop + cost + target,
##                 model = "blogit",
##                 data = sanction
##                 )

## # setx
## x.low <- setx(z.out1, cost = 1)
## x.high <- setx(z.out1, cost = 4)

## # Simulate fitted values and first differences:  

## s.out1 <- sim(z.out1, x = x.low, x1 = x.high)



## print(summary(s.out1))

## # Plot the s.out

## plot(s.out1)

##### Example 2: Joint Estimation of a Model with        #####
#####            Different Sets of Explanatory Variables #####

# Estimate the statistical model, with import a function of coop
# in the first equation and export a function of cost and target
# in the second equation, by using the zeros argument:
z.out2 <- zelig(list(mu1=import~coop,mu2=export~cost+target),
                model = "blogit",
                data = sanction
                )

# Set the explanatory variables to their default values:
x.out2 <- setx(z.out2)

# Simulate draws from the posterior distribution:

s.out2 <- sim(z.out2, x = x.out2)

print(summary(s.out2))

# Plotting marginal densities:

plot(s.out2)

##### Example 3: Joint Estimation of a Parametrically #####
##### and Stochastically Dependent Model              #####

# A bivariate model is parametrically dependent if Y1 and Y2 share
# some or all explanatory variables, {\it and} the effects of the shared
# explanatory variables are jointly estimated.  For example,

z.out3 <- zelig(cbind(import, export) ~ coop + cost + target, 
                constrain = list("1" = c("coop", "cost", "target"),
                                 "2" = c("coop", "cost", "target")),
                model = "blogit", data = sanction)

print(summary(z.out3))

# Note that this model only returns one parameter estimate for each of
# coop, cost, and target.  Contrast this to Example 1 which returns two
# parameter estimates for each of the explanatory variables.

# Set values for the explanatory variables:
stop("everything ended well")
x.out3 <- setx(z.out3, cost = 1:4)

# Draw simulated expected values:  

s.out3 <- sim(z.out3, x = x.out3)

print(summary(s.out3))




