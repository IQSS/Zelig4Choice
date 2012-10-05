# blogit1
# Matt Owen
# Unit test based on a demo by Kosuke Imai and Olivia Lau

library(ZeligChoice)

data(sanction)

z.out <- zelig(
               cbind(import, export) ~ coop + cost,
               model = "blogit",
               data = sanction[1:10, ]
               )

x.low <- setx(z.out, cost = 1)
x.high <- setx(z.out, cost = 4)

s.out <- sim(z.out, x = x.low, x1 = x.high, num = 100)



#
plot(s.out)
summary(z.out)
coef(z.out)
vcov(z.out)

x.low
x.high
z.out <- zelig(
               list(
                    mu1=import~coop,
                    mu2=export~cost+target
                    ),
               model = "blogit",
               data = sanction
               )

x.out <- setx(z.out)

s.out <- sim(z.out, x = x.out)

plot(s.out)

z.out <- zelig(
               cbind(import, export) ~ coop + cost + target, 
               constraint = list(
                 "1" = c("coop", "cost", "target"),
                 "2" = c("coop", "cost", "target")
                 ),
               model = "blogit",
               data = sanction[1:10, ]
               )



z.out <- zelig(
               cbind(import, export) ~ coop + cost + target, 
               model = "bprobit",
               data = sanction
               )


x.low <- setx(z.out, cost = 1)
x.high <- setx(z.out, cost = 4)


s.out <- sim(z.out, x = x.low, x1 = x.high, num = 100)
#
x.low
x.high

z.out <- zelig(list(
                    mu1 = import ~ coop,
                    mu2 = export ~ cost + target
                    ), 
               model = "bprobit",
               data = sanction[1:10, ]
               )

x.out <- setx(z.out)

s.out <- sim(z.out, x = x.out, num = 100)

summary(s.out)

# zelig results
z.out <- zelig(list(
                    mu1 = import ~ tag(coop, "coop") + tag(cost, "cost") + tag (target, "target"),
                    mu2 = export ~ tag(coop, "coop") + tag(cost, "cost") + tag (target, "target")
                    ), 
               model = "bprobit",
               data = sanction[1:10, ]
               )
x.out <- setx(z.out)

s.out <- sim(z.out, x = x.out, num = 100)

#
data(mexico)

z.out1 <- zelig(as.factor(vote88) ~ pristr + othcok + othsocok, model = "mlogit", 
               data = mexico)


x.weak <- setx(z.out1, pristr = 1)
x.strong <- setx(z.out1, pristr = 3)


s.out1 <- sim(z.out1, x = x.strong, x1 = x.weak, num = 100)

z.out2 <- zelig(list(id(vote88,"1")~pristr + othcok, id(vote88,"2")~othsocok), model = "mlogit", 
               data = mexico)


x.weak <- setx(z.out2, pristr = 1)
x.strong <- setx(z.out2, pristr = 3)

s.out2 <- sim(z.out2, x = x.strong, x1 = x.weak)

#
z.out1 <- zelig(
                cost ~ mil + coop,
                model = "ologit", 
                data = sanction
                )

x.low <- setx(z.out1, coop = 1)
x.high <- setx(z.out1, coop = 4)

s.out1 <- sim(z.out1, x = x.low, x1 = x.high)

summary(z.out1)

sanction$ncost <- factor(
                         sanction$ncost,
                         ordered = TRUE,
                         levels = c("net gain", "little effect", 
                                    "modest loss", "major loss")
                         )

z.out2 <- zelig(ncost ~ mil + coop, model = "ologit", data = sanction)

x.out2 <- setx(z.out2, fn = NULL)

s.out2 <- sim(z.out2, x = x.out2)

summary(z.out2)
plot(s.out2)

z.out1 <- zelig(as.factor(cost) ~ mil + coop, model = "oprobit", 
                    data = sanction)

x.low <- setx(z.out1, mil = 0)
x.high <- setx(z.out1, mil = 1)

s.out1 <- sim(z.out1, x = x.low, x1 = x.high)

summary(z.out1)
plot(s.out1)

sanction$ncost <- factor(sanction$ncost, ordered = TRUE,
                         levels = c("net gain", "little effect", 
                         "modest loss", "major loss"))

z.out2 <- zelig(ncost ~ mil + coop, model = "oprobit", data = sanction)

x.out2 <- setx(z.out2, fn = NULL)

s.out2 <- sim(z.out2, x = x.out2)

summary(z.out2)

plot(s.out2)
