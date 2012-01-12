# blogit3
# Matt Owen
# Unit test based on a demo by Kosuke Imai and Olivia Lau

library(ZeligMultivariate)

data(sanction)

z.out <- zelig(
               cbind(import, export) ~ coop + cost + target, 
               constraint = list(
                 "1" = c("coop", "cost", "target"),
                 "2" = c("coop", "cost", "target")
                 ),
               model = "blogit", data = sanction
               )


summary(z.out)
