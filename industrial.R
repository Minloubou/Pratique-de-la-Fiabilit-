 library(readxl)
 library(zoo)
 library(evd)
 library(triangle)
setwd("/Users/David/Downloads/Stats Indutrielles")
w<- read_excel("Donnees-Projet-Pratique-Fiabilit é-ISUP-ISDS_E-Remy_2021-2022.xlsx",sheet=1)


names(w)[2]<-"debit"
names(w)[3]<-"hauteur"

ind <- which(is.na(w$hauteur))

#w$hauteur<-na.approx(w$hauteur,w$debit,na.rm=F)
w1<-na.omit(w)

# Avant impuation des données manquantes, et en les supprimation on a un coeff de corr ~0.964

model<-lm(hauteur~debit,data=w1)
w[ind,3]<-predict(model,w[ind,2])

# Apres imputation des données manquantes on obtient un coeff de corrélation > 0.97
set.seed(123)

n=10000

Zb<- 55.5
L<- 5000
B<- 300

Q <- rgumbel(n,loc=1013,scale=558)
Ks<-rnorm(n,mean=30,sd=7.5)
Zv <- rtriangle(n,a=49,b=51,c=50)
Zm <- rtriangle(n,a=54,b=56,c=55)

computeH<-function(L,B,Q,Ks,Zv,Zm){
  res<-numeric(n)
  b<-numeric(n)
  
  for(i in 1:n){
    b[i] <- Ks[i]* sqrt((Zm[i]-Zv[i])/L)*B
    res[i] <- (Q[i]/b[i])**(3/5)
  }
  return(res)
}

H<-computeH(L,B,Q,Ks,Zv,Zm)
X<- Zv+H-Zb
Y<-na.omit(X)

m<- -2.955621584
s<-1.080812491


fit<-density(Y)
x.new<-rnorm(9982,sample(Y,size=9982,replace=T),fit$bw)
lines(density(x.new),col="blue")


montecarlo<-function(X,x){
  res<-0
  for(i in 1:length(X)){
    res<- res + 1*(X[i]>x)
  }
  return (res/length(X))
}

create<-function(data,hd){
  data[which(data$S<0),2]<-0
  data[which(data$S>1.5),2]<-2000
  data[which(between(data$S,0,0.5)),2] <- mean(c(0,150))
  data[which(between(data$S,0.5,1)),2] <- mean(c(1500,150))
  data[which(between(data$S,1,1.5)),2] <- mean(c(1500,2000))
  
  data[which(data$S< -0.1),3]<-0
  data[which(between(data$S,-0.1,0)),3] <- mean(c(0,0.1))
  data[which(between(data$S,0,0.5)),3] <- mean(c(0.1,0.5))
  data[which(between(data$S,0.5,1)),3] <- mean(c(0.5,1))
  data[which(between(data$S,1,1.5)),3] <- 1
  data[which(data$S>2),3] <- 1
  
  
  data$Cs <- data$Cs*1e6
  data$Cg <- data$Cg*data2[which(data2$h==hd),4]*1000
  return(data)
}
