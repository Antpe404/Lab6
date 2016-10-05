#' A greedy solution to the knapsack_problem
#'
#'This function implements a greedy aproximation algorithm to the knapsack problem. The function 
#'takes a max weight W and a data.frame x that contains a column v (value) for  and column w (weight). 
#'
#'@return It returns an aproximate maximum value and what elements this value is based of. 
#'
#'@references
#'\url{https://en.wikipedia.org/wiki/Knapsack_problem#Greedy_approximation_algorithm}
#'
#'@examples
#'
#'## Generate data
#'
#'set.seed(42)
#'n <- 2000 
#'knapsack_objects <- data.frame( w=sample(1:4000, size = n, replace = TRUE), 
#'                                v=runif(n = n, 0, 10000) )
#'
#'greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
#'
#'
#'greedy_knapsack(x = knapsack_objects[1:1200,], W = 2000)
#'
#'
#'@export

greedy_knapsack <- function(x,W){ 
  
  if(!is.data.frame(x) || !all(colnames(x)==c("w","v")) || !all(x>0) || !W>0 || !length(W)==1) {
    warning("Your inputs are not correct")} 
  
  
  x$rat<-x$v / x$w
  x<-x[order(-x$rat),]
  
  i <- 1
  kp<-c(0)
  
  for (i in 1:length(x$w)){
    if (W <= (sum(kp) + x$w[i])) {break}
    kp[i] <- x$w[i]
    
    
    
  }
  
  
  overlistad <- list(value = sum(x[1:sum(kp %in% x$w),"v"]),
                     elements = as.numeric(rownames(x[1:sum(kp %in% x$w),])) )
  
  return(overlistad)
  
}

#greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)

#greedy_knapsack(x = knapsack_objects[1:1200,], W = 2000)


