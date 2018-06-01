# This is an example of Genetic Algorithms applied to Knapsack Problem.
# To learn more about it, read:
#  - https://en.wikipedia.org/wiki/Knapsack_problem
#  - http://www.obitko.com/tutorials/genetic-algorithms
#  - https://www.r-bloggers.com/genetic-algorithms-a-simple-r-example/


# If the package GA is not installed, uncomment the line below
# install.packages("GA")

library(GA)

dataset = data.frame(
    items = c("pocketknife", "beans", "potatoes", "flashlight", "sleeping_bag", "rope", "compass"), 
    survivalpoints = c(10, 20, 15, 2, 30, 10, 30), 
    weight = c(1, 5, 10, 1, 7, 5, 1)
)

analysis = function(x)
{
  weightlimit = 15
  selectedpoints = 0
  selectedweight = 0
  
  for (i in 1:nrow(dataset)) 
  {
    if(x[i] != 0) {
      selectedpoints = selectedpoints + dataset[i, 2]
      selectedweight = selectedweight + dataset[i, 3]
    }  
  }

  if (selectedweight > weightlimit) {
    return(0)
  }
  
  return(selectedpoints)
}

result = ga("binary", fitness = analysis, nBits = 7, popSize = 10, maxiter = 20,  names = c("pocketknife", "beans", "potatoes", "flashlight", "sleeping_bag", "rope", "compass"))

summary(result)$solution

plot(result)
