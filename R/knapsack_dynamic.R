#'Dynamic programming solution to the knapsack problem.
#'
#'The knapsack_dynamic function applies a dynamic programing solution to the knapsack
#' problem. The function builds a matrix to store the calculations. 
#' The maximum value and the used itemsare returned by the function.
#'
#'
#'@param x    A data frame with two variables, w and v.
#'@param W   An integer which represent the weight limit for the knapsack problem.
#'
#'@return A list with the maximun value for a collection of items that fits under the weight
#'limit.
#'
#'
#'@references 
#'More about the knapsack problem can be read here:
#'\url{https://en.wikipedia.org/wiki/Knapsack_problem}
#'
#'@examples
#'##Get a solution for the knapsack problem on the data frame 
#'knapsack_objects, with the knapsack size of 2000.
#'knapsack_dynamic(x=knapsack_objects,W=2000)
#'
#'@export
knapsack_dynamic<-function(x, W){
  w<-c(0,x[,1]) #Lägger till nollor i inledningen för att få matrisen att starta med nollor.
  v<-c(0,x[,2]) #Det krävs för att få koden till skapandet av matrisen att fungera.
  n<-length(v)-1
  Weight<-W
  
  m<-matrix(ncol=Weight+1, nrow=n+1) #Skapar en matris
  
  for (j in 0:n+1){ #Lägger nollor i första kolumnen
    m[j,1]<-0
  }
  
  for (j in 0:Weight+1){ #Och nollor i första rader
    m[1,j]<-0
  }
  
  colnames(m)<-paste(0:Weight) #Sätter colnamnen till 0:Weight, för att lättare förstå 
  #youtubevideon. Behövs eg inte.
  
  #################Nedan görs själva matrisen
  for (i in 2:c(n+1)){
    for (j in 1:c(Weight+1)){
      if (w[i]>j-1){
        m[i,j]<-m[i-1, j]
      }
      else{
        m[i,j]<-max(m[i-1, j], m[i-1,(j-w[i])]+v[i])
      }
    }
  }
  value<-max(m) #Detta är värdet som ska returneras.
  
  ################Nedan tar jag fram vilka element som ska tas fram.
  Pos<-which(m ==max(m), arr.ind=T) #Tar fram var maxvärdet ligger.
  Rad<-Pos[nrow(Pos),1] #sparar dess radnummer
  Kol<-Pos[nrow(Pos),2] #och dess kol-nummer
  element<-NULL
  
  
  while(Rad>1 && Kol>1){
    
    if (m[Rad, Kol]==m[Rad-1, Kol]){
      Rad<-Rad-1
    }
    else if (m[Rad, Kol]>m[Rad-1, Kol]){
      element[length(element)+1]<-Rad - 1 #Eftersom jag har en nollrad i början.
      #Diff<-m[Rad, Kol]-m[Rad-1, Kol]
      Kol<-Kol-w[Rad] #Eftersom jag lagt till en nolla i w
      Rad<-Rad-1
    }
  }
  
  return(list(value=value, element=rev(element)))
}
