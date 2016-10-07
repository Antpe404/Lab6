## ---- echo=TRUE, message = FALSE, warning = FALSE------------------------
library(Lab6)

##Building a dataset to test on
set.seed(42)
n <- 2000 

knapsack_objects <- data.frame( w=sample(1:4000, size = n, replace = TRUE), 
                                v=runif(n = n, 0, 10000) )

##Use the the first 16 rows of data, and W=2000
knapsack_brute_force(x = knapsack_objects[1:12,], W=2000) 

##You might wonder how heavy brute force is. Let's see!
system.time(knapsack_brute_force(x = knapsack_objects[1:16,], W=2000))


## ---- echo=TRUE, message = FALSE, warning = FALSE------------------------
##Use the the first 12 rows of data, and W=2000
knapsack_dynamic(x=knapsack_objects[1:12,], W=2000)



## How long time does it takes to run the algorithm for n = 500 objects!
system.time(knapsack_dynamic(x=knapsack_objects[1:500,], W=2000))


## ---- echo=TRUE, message = FALSE, warning = FALSE------------------------
##Use the the first 800 rows of data, and W=3500
knapsack_greedy(x = knapsack_objects[1:800,], W = 3500)


## How long time does it takes to run the algorithm for n = 1000000 objects?
set.seed(42)
n <- 1000000 

knapsack_objects <- data.frame( w=sample(1:4000, size = n, replace = TRUE), 
                                v=runif(n = n, 0, 10000) )
system.time(knapsack_greedy(x = knapsack_objects[1:1000000,], W = 3500))


