# bprobit2
# Matt Owen
# Unit test based on a demo by Kosuke Imai and Olivia Lau

library(ZeligMultivariate)

data(sanction)

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
