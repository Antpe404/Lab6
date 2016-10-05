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

liten<-knapsack_objects[1:8,]
mellan<-knapsack_objects[1:12,]

knapsack_dynamic(x=mellan, W=2000)




#######################MATRISFRAMSTÄLLNING##############
w<-c(0,1,3,4,5)
v<-c(0,1,4,5,7)
n<-length(v)-1
Weight<-7

m<-matrix(ncol=Weight+1, nrow=n+1)
#colnames(m)<-c(paste(0:w))

for (j in 0:n+1){
  m[j,1]<-0
}

for (j in 0:Weight+1){
  m[1,j]<-0
}


colnames(m)<-paste(0:Weight)

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

m#YAY. Looks the same as in the example.
value<-max(m)

#########################Elementframtagning
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
      Kol<-Kol-w[Rad]
      Rad<-Rad-1
    }
  }

element
rev(element)

#############################För test på "liten"-datan.
#Liten är första 8 raderna ur knapsack_object i labinstruktionen
w<-c(0,liten[,1])
v<-c(0,liten[,2])
n<-length(v)-1
Weight<-2000

m<-matrix(ncol=Weight+1, nrow=n+1)
#colnames(m)<-c(paste(0:w))

for (j in 0:n+1){
  m[j,1]<-0
}

for (j in 0:Weight+1){
  m[1,j]<-0
}


colnames(m)<-paste(0:Weight)

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

m#YAY. Looks the same as in the example.
value<-max(m)

#########################Elementframtagning
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
    Kol<-Kol-w[Rad]
    Rad<-Rad-1
  }
}

element
rev(element)

