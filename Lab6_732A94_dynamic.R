w<-c(0,1,3,4,5)
v<-c(0,1,4,5,7)
n<-4
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

#######################Nedan är försök att hitta vilka element

value<-max(m)

which(m ==max(m), arr.ind=T) #Detta plockar ut pos för det sökta värdet, i detta fall max.

Pos<-which(m ==max(m), arr.ind=T)
Rad<-Pos[nrow(Pos),][1]
Kol<-Pos[nrow(Pos),][2]
m[Rad, Kol]
m[Rad, Kol-1] #Den åt sidan
m[Rad-1, Kol]#Den uppåt

m[Rad-1, Kol]>m[Rad, Kol-1] #Den ovanför är större.

m[Rad-2, Kol]>m[Rad-1, Kol-1]

