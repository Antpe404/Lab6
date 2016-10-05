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

#######################Nedan är försök att hitta vilka element
#http://www.micsymposium.org/mics_2005/papers/paper102.pdf
#c i deras ex=w i mitt.
which(m ==max(m), arr.ind=T) #Detta plockar ut pos för det sökta värdet, i detta fall max.

Pos<-which(m ==max(m), arr.ind=T)
Rad<-Pos[nrow(Pos),1]
Kol<-Pos[nrow(Pos),2]

m[Rad, Kol]
remainingCap<-1
while(remainingWeight>0 && )
  
  
  Pos<-which(m ==max(m), arr.ind=T)
  Rad<-Pos[nrow(Pos),1]
  Kol<-Pos[nrow(Pos),2]
   
  
  if(m[Rad, Kol]==m[Rad-1, Kol]){
    Rad<-Rad-1
   }
  else if (m[Rad, Kol]>m[Rad-1, Kol]){
    element<-Rad - 1 #Eftersom jag har en nollrad i början.
  }
  
##############################HÄRIFRÅN  
  Pos<-which(m ==max(m), arr.ind=T)
  Rad<-Pos[nrow(Pos),1]
  Kol<-Pos[nrow(Pos),2]
  
  while(m[Rad, Kol]==m[Rad-1, Kol]){
    Rad<-Rad-1
  }
  if (m[Rad, Kol]>m[Rad-1, Kol]){
    element<-Rad - 1 #Eftersom jag har en nollrad i början.
    Diff<-m[Rad, Kol]-m[Rad-1, Kol]
  }
element
Diff #Då vill jag bege mig till kolumn nummer Kol - Diff
NyKol<-Kol-Diff
Rad

while(m[Rad, NyKol]==m[Rad-1, NyKol]){
  Rad<-Rad-1
}
if (m[Rad, NyKol]>m[Rad-1, NyKol]){
  element2<-Rad - 1 #Eftersom jag har en nollrad i början.
  Diff<-m[Rad, NyKol]-m[Rad-1, NyKol]
}
element2
Diff
NyKol-Diff
NyKol2<-NyKol-Diff
Rad
 #Hit fram är det rätt, nedanstående kör ju bara på eftersom logiska i while alltid är sant.
#Om jag lägger till typ while Rad >1 så ska det funka tror jag. Måste ju generalisera dock.
while(m[Rad, NyKol2]==m[Rad-1, NyKol2]){
  Rad<-Rad-1
}
if (m[Rad, NyKol2]>m[Rad-1, NyKol2]){
  element3<-Rad - 1 #Eftersom jag har en nollrad i början.
  Diff<-m[Rad, NyKol2]-m[Rad-1, NyKol2]
}

#########################

value<-max(m)

which(m ==max(m), arr.ind=T) #Detta plockar ut pos för det sökta värdet, i detta fall max.

Pos<-which(m ==max(m), arr.ind=T)
Rad<-Pos[nrow(Pos),1]
Kol<-Pos[nrow(Pos),2]
m[Rad, Kol]

if (m[Rad-1, Kol]>m[Rad, Kol-1]){ #If den ovanför är större..
  if(m[Rad-2, Kol]>m[Rad-1, Kol-1]){ #Så gå till raden ovan. If den ovan är större..
    if(m[Rad-3, Kol]>m[Rad-2, Kol-1]){ #så gå till raden ovan.
      if(m[Rad-4, Kol]>m[Rad-3, Kol-1]){
        element<-Rad-4
      }
else{
  element<-Rad-4
}
    }
    else {
      element<-Rad-3}
  }
  else{
    element<-Rad-2
  }
}
else{
  element<-Rad-1
}
  



m[Rad, Kol-1] #Den åt sidan
m[Rad-1, Kol]#Den uppåt

m[Rad-1, Kol]>m[Rad, Kol-1] #Den ovanför är större.

m[Rad-2, Kol]>m[Rad-1, Kol-1]

