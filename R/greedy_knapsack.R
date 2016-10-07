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
#'knapsack_greedy(x = knapsack_objects[1:800,], W = 3500)
#'
#'
#'knapsack_greedy(x = knapsack_objects[1:1200,], W = 2000)
#'
#'
#'@export

knapsack_greedy <- function(x,W){ 
  
  if(!is.data.frame(x)){warning("Your data frame is not in a correct format. Supply a data frame with columns v and w.")}
  
  if( !all(colnames(x)==c("w","v")) ){warning("Your data frame is not in a correct format. Supply a data frame with columns v and w.")}
  
  if( !all(x>0) ){warning("All elements in the data frame must be greater then zero")}
    
  if((W < 1) || (length(W) != 1) || (!is.numeric(W))){warning("The wheight must be a single number larger or equal to 1 ")}

  #if 
  
  #if(!is.data.frame(x) || !all(colnames(x)==c("w","v")) || !all(x>0) || !W>0 || !length(W)==1 || !is.numeric(W)) {
    #warning("Your inputs are not correct")} 
  
  
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

#knapsack_greedy(x = knapsack_objects[1:800,], W = 3500)

#knapsack_greedy(x = knapsack_objects[1:1200,], W = 2000)


