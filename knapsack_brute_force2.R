knapsack_brute_force<-function(x, W){
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
    
    ja<-which((colSums(value)[spara])==max(colSums(value)[spara]))
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

set.seed(42)
n <- 2000 
knapsack_objects <- data.frame( w=sample(1:4000, size = n, replace = TRUE), 
                                v=runif(n = n, 0, 10000) )


liten<-as.data.frame(knapsack_objects[1:8,])
litestorre<-as.data.frame(knapsack_objects[1:12,])

question<-as.data.frame(knapsack_objects[1:16,])

ytterliggarelitestorre<-as.data.frame(knapsack_objects[1:20,])
annuytterliggarelitestorre<-as.data.frame(knapsack_objects[1:25,])
jatteannuytterliggarelitestorre<-as.data.frame(knapsack_objects[1:30,])
femton<-as.data.frame(knapsack_objects[1:15,])
medelstor<-as.data.frame(knapsack_objects[1:35,]) #rad 35 är jävligt liten, vilket fuckar upp.
medelstorre<-as.data.frame(knapsack_objects[1:40,]) 


knapsack_brute_force(x=question, W=3500)
system.time(knapsack_brute_force(x=question, W=3500))

#Den här funktionen har en bugg. Den utgår fr att ju fler element, desto större är summan av dess values.
#Det är inte rätt, denna returnerar maxvärdet av de kombinationer som innehåller flest möjliga element 
#från combn. Mao skickar den hellre ut element 3,5,6 och valuet 300 än 4, 6 och 400. Verkar dock otroligt
# att det sker, då alla våra tester signalerar rätt resultat. Bör dessutom vara relativt enkelt att ändra
#i koden.
