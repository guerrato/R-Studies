# This is an example of Simulated Annealing applied to Rosenbrock function.
# I implemented this example as a class exercise of Fernando Amaral's course 
# called "Super Formação Inteligência Artificial e Machine Learning"

# If the package GenSA is not installed, uncomment the line below
#install.packages('GenSA');

library(GenSA)

rosenbrok = function(x, y) {
  return ((1 - x) ^ 2 ) + 100 * (y  - (x ^ 2)) ^ 2 
}

tolerance = 1e-13 # It defines the limit of tries
lower = rep(0, 2)
upper = c(9, 9)
out = GenSA(lower = lower, upper = upper, fn = rosenbrok, control=list(max.call=tolerance, verbose=TRUE))
out[c("value","par","counts")]