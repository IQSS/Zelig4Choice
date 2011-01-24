library(bivariate.zelig)

data(sanction)

# TEST 1
z.out1 <- zelig(
                cbind(import, export) ~ coop + cost,
                model = "blogit",
                data = sanction
                )


x.low <- setx(z.out1, cost = 1)
x.high <- setx(z.out1, cost = 4)
x.low
x.high


s.out1 <- sim(z.out1, x = x.low, x1 = x.high)


# TEST 2
z.out2 <- zelig(
                list(
                     mu1=import~coop,
                     mu2=export~cost+target
                     ),
                model = "blogit",
                data = sanction
                )

x.out2 <- setx(z.out2)
x.out2


s.out2 <- sim(z.out2, x = x.out2)

plot(s.out2)


# TEST 3
z.out3 <- zelig(
                cbind(import, export) ~ coop + cost + target, 
                constrain = list(
                  "1" = c("coop", "cost", "target"),
                  "2" = c("coop", "cost", "target")
                  ),
                model = "blogit", data = sanction
                )
