# This is an example of Genetic Algorithms using permutation algorithm.
# To learn more about it, read:
#  - http://www.obitko.com/tutorials/genetic-algorithms
#  - https://www.r-bloggers.com/genetic-algorithms-a-simple-r-example


# If the package GA is not installed, uncomment the line below
# install.packages("GA")

library(GA)

mdat <- matrix(c(0, 10, 8, 9, 25, 10, 0, 10, 21, 12, 8, 10, 0, 10, 13, 9, 21, 10, 0, 14, 25, 12, 13, 14, 0), nrow = 5, ncol = 5, byrow = TRUE,
               dimnames = list(c("SaoPaulo", "Mairipora", "Braganca", "Atibaia", "Campinas"),
                               c("SaoPaulo", "Mairipora", "Braganca", "Atibaia", "Campinas")))



tourLength <- function(tour, distMatrix) {
   tour <- c(tour, tour[1])
   route <- embed(tour, 2)[,2:1]
   sum(distMatrix[route])
}

tspFitness <- function(tour, mdat) 1/tourLength(tour, mdat)

result <- ga(type = "permutation", fitness = tspFitness, distMatrix = D,
          min = 1, max = attr(eurodist, "Size"), popSize = 50, maxiter = 5000,
          run = 500, pmutation = 0.2)



# result = ga("permutation", fitness = analysis, nBits = 7, popSize = 10, maxiter = 20,  names = c("pocketknife", "beans", "potatoes", "flashlight", "sleeping_bag", "rope", "compass"))

summary(result)$solution

plot(result)
