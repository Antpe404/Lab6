

greedy_knapsack <- function(x,W){ 
 
 

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

greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)

greedy_knapsack(x = knapsack_objects[1:1200,], W = 2000)





