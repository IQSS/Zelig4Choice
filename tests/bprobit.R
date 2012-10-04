# bprobit1
# Matt Owen
# Unit test based on a demo by Kosuke Imai and Olivia Lau

library(ZeligChoice)

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

z.out <- zelig(list(
                    mu1 = import ~ coop,
                    mu2 = export ~ cost + target
                    ), 
               model = "bprobit",
               data = sanction
               )

x.out <- setx(z.out)

s.out <- sim(z.out, x = x.out)


# zelig results
plot(s.out)
summary(z.out)
vcov(z.out)
coef(z.out)

# setx results
x.out

z.out <- zelig(list(
                    mu1 = import ~ tag(coop, "coop") + tag(cost, "cost") + tag (target, "target"),
                    mu2 = export ~ tag(coop, "coop") + tag(cost, "cost") + tag (target, "target")
                    ), 
               model = "bprobit",
               data = sanction
               )

x.out <- setx(z.out)

s.out <- sim(z.out, x = x.out)


#
plot(s.out)
summary(z.out)
vcov(z.out)
coef(z.out)

# setx results
x.out
