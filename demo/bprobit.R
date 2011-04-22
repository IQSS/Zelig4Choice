# bprobit1
# Matt Owen
# Unit test based on a demo by Kosuke Imai and Olivia Lau

library(bivariate.zelig)

data(sanction)

z.out <- zelig(
               cbind(import, export) ~ coop + cost + target, 
               model = "bprobit",
               data = sanction
               )


x.low <- setx(z.out, cost = 1)
x.high <- setx(z.out, cost = 4)


s.out <- sim(z.out, x = x.low, x1 = x.high)


#
plot(s.out)
summary(z.out)
vcov(z.out)
coef(z.out)

#
x.low
x.high
