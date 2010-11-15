## library(bivariate.zelig)
## data(sanction)

## z.out1 <- zelig(cbind(import, export) ~ coop + cost + target, 
##                 model = "bprobit",
##                 data = sanction
##                 )


## print(summary(z.out1))


## x.low <- setx(z.out1, cost = 1)
## x.high <- setx(z.out1, cost = 4)


## s.out1 <- sim(z.out1, x = x.low, x1 = x.high)
## print(summary(s.out1))


## plot(s.out1)



## z.out2 <- zelig(list(mu1 = import ~ coop, mu2 = export ~ cost + target), 
##                 model = "bprobit",
##                 data = sanction
##                 )


## print(summary(z.out2))


## x.out2 <- setx(z.out2)


## s.out2 <- sim(z.out2, x = x.out2)
## print(summary(s.out2))


## plot(s.out2)


library(bivariate.zelig)
data(sanction)
z.out3 <- zelig(list(
                     mu1 = import ~ tag(coop, "coop") + tag(cost, "cost") + tag (target, "target"),
                     mu2 = export ~ tag(coop, "coop") + tag(cost, "cost") + tag (target, "target")
                     ), 
                model = "bprobit",
                data = sanction
                )

print(summary(z.out3))

message("!!!!!!!")
x.out3 <- setx(z.out3, cost = 1:4)

s.out3 <- sim(z.out3, x = x.out3)

print(summary(s.out3))



