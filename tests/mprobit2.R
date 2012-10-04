# TEST2
library(ZeligMultinomial)

data(detergent)

z.out <- zelig(
               choice ~ 1,
               data = detergent,
               choiceX = list(
                              Surf = SurfPrice,
                              Tide = TidePrice,
                              Wisk = WiskPrice,
                              EraPlus = EraPlusPrice,
                              Solo = SoloPrice,
                              All = AllPrice
                              ),
               cXnames = "price",
               n.draws = 500,
               burnin = 100,
               thin = 3,
               verbose = TRUE,
               model = "mprobit"
               )

x.out <- setx(z.out)
s.out <- sim(z.out, x = x.out)
