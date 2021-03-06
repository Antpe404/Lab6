% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/greedy_knapsack.R
\name{knapsack_greedy}
\alias{knapsack_greedy}
\title{A greedy solution to the knapsack_problem}
\usage{
knapsack_greedy(x, W)
}
\value{
It returns an aproximate maximum value and what elements this value is based of.
}
\description{
This function implements a greedy aproximation algorithm to the knapsack problem. The function 
takes a max weight W and a data.frame x that contains a column v (value) for  and column w (weight).
}
\examples{

## Generate data

set.seed(42)
n <- 2000 
knapsack_objects <- data.frame( w=sample(1:4000, size = n, replace = TRUE), 
                               v=runif(n = n, 0, 10000) )

knapsack_greedy(x = knapsack_objects[1:800,], W = 3500)


knapsack_greedy(x = knapsack_objects[1:1200,], W = 2000)


}
\references{
\url{https://en.wikipedia.org/wiki/Knapsack_problem#Greedy_approximation_algorithm}
}

