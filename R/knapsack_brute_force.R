#'Brute force solution to the knapsack problem
#'
#'A solution to the knapsack problem that employs a bruteforce apporach to the problem. 
#'The function generates and test for all possible combinations. This generates a huge
#'amount of calculations for larger data-sets. Users are not recomended to give the 
#'function a data material with more than 20 rows. 
#'
#'@return 
#'
#'The function returns the maximum value and all possible wheight combinations of this solution. 
#'
#'@references
#'
#'\link{https://en.wikipedia.org/wiki/Knapsack_problem}
#'
#'@examples 
#'
#'set.seed(42)
#'n <- 2000
#'knapsack_objects <-
#'  data.frame(
#'    w=sample(1:4000, size = n, replace = TRUE),
#'    v=runif(n = n, 0, 10000)
#'  )
#'
#'brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
#'
#'brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)
#'
#'
#'
#'@export



knapsack_brute_force<-function(x, W){
  
  if(!is.data.frame(x)){warning("Your data frame is not in a correct format. Supply a data frame with columns v and w.")}
  
  if( !all(colnames(x)==c("w","v")) ){warning("Your data frame is not in a correct format. Supply a data frame with columns v and w.")}
  
  if( !all(x>0) ){warning("All elements in the data frame must be greater then zero")}
  
  if((W < 1) || (length(W) != 1) || (!is.numeric(W))){warning("The wheight must be a single number larger or equal to 1 ")}

  #if(!is.data.frame(x) || !all(colnames(x)==c("w","v")) || !all(x>0) || !W>0 || !length(W)==1 || !is.numeric(W)) {
    #warning("Your inputs are not correct")} 
  
  element<-0
  Values<-0
  spara<-1
  for (index in 2:length(x[,1])) {
    weight<-as.data.frame(combn(x[,1], index))#What innehåller alla komb av w i liten.
    value<-as.data.frame(combn(x[,2], index))
    
    vikt<-colSums(weight)
    
    spara<-which(vikt<=W)
    if(length(spara)>0){ 
      possible<-colSums(value)[spara]
      Values<-max(possible)
      
      #ja<-which((colSums(value)[spara])==max(colSums(value)[spara]))
      ja<- which((possible)==(max(Values)))
      spara2<-spara[ja]
      Leta2<-weight[, spara2]
      
      for (j in 1:index){
        element[j]<-which(x[,1]==Leta2[j])
        #element[1]<-which(x[,1]==Leta2[1])
        #element2<-which(x[,1]==Leta2[2])
        #element<-c(element1, element2)
      }
      
    }
    else {
      break
    }
  }
  #MaxValue<-max(Values)
  #which(Values==MaxValue)
  return(list(value=Values, element=element))
}
