data(sanction)

z.out <- zelig(
               cbind(import, export) ~ coop + cost + target, 
               model = "bprobit",
               data = sanction
               )

x.low <- setx(z.out, cost = 1)

x.high <- setx(z.out, cost = 4)

s.out <- sim(z.out, x = x.low, x1 = x.high)

summary(s.out)

plot(s.out)
