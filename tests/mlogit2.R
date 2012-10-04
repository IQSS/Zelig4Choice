#
library(ZeligMultinomial)
data(mexico)
z.out2 <- zelig(list(id(vote88,"1")~pristr + othcok, id(vote88,"2")~othsocok), model = "mlogit", 
               data = mexico)


x.weak <- setx(z.out2, pristr = 1)
x.strong <- setx(z.out2, pristr = 3)

s.out2 <- sim(z.out2, x = x.strong, x1 = x.weak)

#
summary(z.out2)
vcov(z.out2)
coef(z.out2)
x.weak
x.strong
