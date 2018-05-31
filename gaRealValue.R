# This is an example of Genetic Algorithms using real value function.
# I implemented this example as a class exercise of Fernando Amaral's course 
# called "Super Formação Inteligência Artificial e Machine Learning"

# Goal:
# Given the equation 2 * x + 5 = 20, find the x value.

# If the package GA is not installed, uncomment the line below
# install.packages("GA")

library(GA)

analysis = function(x)
{
    total = 2 * x + 5

    if (total > 20) {
        return (20 - total)
    } else {
        return (total - 20)
    }
}

result = ga("real-value", fitness = analysis, lower = c(-20), upper = c(20), popSize = 10, maxiter = 20, monitor = T,  names = c("a"))

summary(result)$solution

plot(result)