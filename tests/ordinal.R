#
library(ZeligChoice)

data(sanction)

z.out1 <- zelig(
                cost ~ mil + coop,
                model = "ologit", 
                data = sanction
                )

x.low <- setx(z.out1, coop = 1)
x.high <- setx(z.out1, coop = 4)

s.out1 <- sim(z.out1, x = x.low, x1 = x.high)

summary(z.out1)
plot(s.out1)

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
