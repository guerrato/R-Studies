# This is an example of Genetic Algorithms using permutation algorithm.
# To learn more about it, read:
#  - http://www.obitko.com/tutorials/genetic-algorithms
#  - https://simple.wikipedia.org/wiki/Travelling_salesman_problem
#  - https://www.rdocumentation.org/packages/stats/versions/3.5.0/topics/embed
#  - https://www.rdocumentation.org/packages/cshapes/versions/0.6/topics/distmatrix
#  - https://www.rdocumentation.org/packages/cshapes/versions/0.6/topics/distmatrix
#  - https://en.wikipedia.org/wiki/Ramer%E2%80%93Douglas%E2%80%93Peucker_algorithm
#
# Special thanks to: Karthik (https://rstudio-pubs-static.s3.amazonaws.com/132872_620c10f340f348b88453d75ec99960ff.html)
#
# If the package GA is not installed, uncomment the line below
# install.packages("GA")

library(GA)

data <- data.frame(c(0, 10, 8, 9, 25),
                   c(10, 0, 10, 21, 12),
                   c(8, 10, 0, 10, 13),
                   c(9, 21, 10, 0, 14),
                   c(25, 12, 13, 14, 0))

rownames(data) <- c("SaoPaulo", "Mairipora", "Braganca", "Atibaia", "Campinas")
colnames(data) <- c("SaoPaulo", "Mairipora", "Braganca", "Atibaia", "Campinas")

data <- data.matrix(data)

tourLength <- function(tour, distMatrix) {
  tour <- c(tour, tour[1])
  route <- embed(tour, 2)[,2:1]
  sum(distMatrix[route])
}

tspFitness <- function(tour, ...) {
  1/ tourLength(tour, ...) 
}

result <- ga(type = "permutation", fitness = tspFitness, distMatrix = data, lower = 1, upper = 5, popSize = 50, maxiter = 10000, run = 500, 
             pmutation = 0.2,  
             names = c("SaoPaulo", "Mairipora", "Braganca", "Atibaia", "Campinas"))

summary(result)$solution


plot(result)