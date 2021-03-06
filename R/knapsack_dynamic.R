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
  
  if(!is.data.frame(x)){warning("Your data frame is not in a correct format. Supply a data frame with columns v and w.")}
  
  if( !all(colnames(x)==c("w","v")) ){warning("Your data frame is not in a correct format. Supply a data frame with columns v and w.")}
  
  if( !all(x>0) ){warning("All elements in the data frame must be greater then zero")}
  
  if((W < 1) || (length(W) != 1) || (!is.numeric(W))){warning("The wheight must be a single number larger or equal to 1 ")}
  
  #if(!is.data.frame(x) || !all(colnames(x)==c("w","v")) || !all(x>0) || !W>0 || !length(W)==1 || !is.numeric(W)) {
    #warning("Your inputs are not correct")} 
  
  w<-c(0,x$w) #L�gger till nollor i inledningen f�r att f� matrisen att starta med nollor.
  v<-c(0,x$v) #Det kr�vs f�r att f� koden till skapandet av matrisen att fungera.
  n<-length(v)-1
  Weight<-W
  
  m<-matrix(ncol=Weight+1, nrow=n+1) #Skapar en matris
  
  for (j in 0:n+1){ #L�gger nollor i f�rsta kolumnen
    m[j,1]<-0
  }
  
  for (j in 0:Weight+1){ #Och nollor i f�rsta rader
    m[1,j]<-0
  }
  
  colnames(m)<-paste(0:Weight) #S�tter colnamnen till 0:Weight, f�r att l�ttare f�rst� 
  #youtubevideon. Beh�vs eg inte.
  
  #################Nedan g�rs sj�lva matrisen
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
  value<-max(m) #Detta �r v�rdet som ska returneras.
  
  ################Nedan tar jag fram vilka element som ska tas fram.
  Pos<-which(m ==max(m), arr.ind=T) #Tar fram var maxv�rdet ligger.
  Rad<-Pos[nrow(Pos),1] #sparar dess radnummer
  Kol<-Pos[nrow(Pos),2] #och dess kol-nummer
  element<-NULL
  
  
  while(Rad>1 && Kol>1){
    
    if (m[Rad, Kol]==m[Rad-1, Kol]){
      Rad<-Rad-1
    }
    else if (m[Rad, Kol]>m[Rad-1, Kol]){
      element[length(element)+1]<-Rad - 1 #Eftersom jag har en nollrad i b�rjan.
      #Diff<-m[Rad, Kol]-m[Rad-1, Kol]
      Kol<-Kol-w[Rad] #Eftersom jag lagt till en nolla i w
      Rad<-Rad-1
    }
  }
  
  return(list(value=value, element=rev(element)))
}
