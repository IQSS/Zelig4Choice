library(bivariate.zelig)

data(sanction)

# TEST 1
z.out1 <- zelig(
                cbind(import, export) ~ coop + cost + target, 
                model = "bprobit",
                data = sanction
                )


z.out1


x.low <- setx(z.out1, cost = 1)
x.high <- setx(z.out1, cost = 4)
x.low
x.high


s.out1 <- sim(z.out1, x = x.low, x1 = x.high)

plot(s.out1)


# TEST 2
z.out2 <- zelig(list(
                     mu1 = import ~ coop,
                     mu2 = export ~ cost + target
                     ), 
                model = "bprobit",
                data = sanction
                )

summary(z.out2)

x.out2 <- setx(z.out2)
x.out2

s.out2 <- sim(z.out2, x = x.out2)

plot(s.out2)



# TEST 3
data(sanction)

z.out3 <- zelig(list(
                     mu1 = import ~ tag(coop, "coop") + tag(cost, "cost") + tag (target, "target"),
                     mu2 = export ~ tag(coop, "coop") + tag(cost, "cost") + tag (target, "target")
                     ), 
                model = "bprobit",
                data = sanction
                )

x.out3 <- setx(z.out3)

s.out3 <- sim(z.out3, x = x.out3)



