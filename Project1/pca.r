library(tidyverse)
library(datasets)
library(stringr)
library(stats)
library(FactoMineR)
library(factoextra)
library(ggplot2)

# PCA analysis for numerical variables

r <- read.csv("/Users/laiabonilla/Desktop/data.csv")
numeriques<-which(sapply(r,is.numeric))
dnum<-r[,numeriques]
view(r)

# Variable correlation plot (it shows the relationships between all variables)
pca <- PCA(X=dnum,scale.unit=TRUE,ncp=7,graph=TRUE)
print(pca)

# a. Scree plot. Specify how many principal components are selected
# We selected 4 components
pc <- prcomp(dnum, scale=TRUE)
inerProj<- pc$sdev^2 
totalIner<- sum(inerProj)
pinerEix<- 100*inerProj/totalIner
#pinerEix
#sum(pinerEix[1:4])
barplot(pinerEix,main="Scree plot",xlab="Components",ylab="Total Inertia in subespaces(%)")
qplot(c(1:7),pinerEix[1:7]) + geom_line() + 
  xlab("Component") + ylab("Total Inertia in subespaces(%)") + 
  ggtitle("Scree Plot") + ylim(0, 40)

fviz_screeplot(pc)

# b. Factorial map visualisation: For each factorial map provide. Be sure you use a single landscape pager
# for each single map in order to guarantee visibility of materials to the readers

# STORAGE OF THE EIGENVALUES, EIGENVECTORS AND PROJECTIONS IN THE nd DIMENSIONS
#View(pc$x)

nd = 4
dim(pc$x)
dim(dnum)
dnum[2000,]
pc$x[2000,]

Psi = pc$x[,1:nd]
dim(Psi)
Psi[2000,]

# STORAGE OF LABELS FOR INDIVIDUALS AND VARIABLES
iden = row.names(dnum)
etiq = names(dnum)
ze = rep(0,length(etiq)) 

# PLOT OF INDIVIDUALS
#select your axis
eje1<-1
eje2<-3
Psi

plot(Psi[,eje1],Psi[,eje2],xlab="PC1", ylab="PC3")
text(Psi[,eje1],Psi[,eje2],cex=0.5)
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")

plot(Psi[,eje1],Psi[,eje2], type="n", xlab="PC1", ylab="PC3")
text(Psi[,eje1],Psi[,eje2],labels=iden, cex=0.5)
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")

#Projection of variables
Phi = cor(dnum,Psi)
#View(Phi)

#select your axis
X<-Phi[,eje1]
Y<-Phi[,eje2]

plot(Psi[,eje1],Psi[,eje2],type="n",xlim=c(-1,0.2), ylim=c(-1,1),xlab="PC1", ylab="PC3")
axis(side=1, pos= 0, labels = F)
axis(side=3, pos= 0, labels = F)
axis(side=2, pos= 0, labels = F)
axis(side=4, pos= 0, labels = F)
arrows(ze, ze, X, Y, length = 0.07,col="blue")
text(X,Y,labels=etiq,col="darkblue", cex=0.7)

#MCA

# PROJECTION OF ILLUSTRATIVE qualitative variables on individuals' map
# PROJECTION OF INDIVIDUALS DIFFERENTIATING THE Sex, Race, Marst, Empstat and Ftype

#Sex
varcat=factor(r[,6])
plot(Psi[,eje1],Psi[,eje2],col=varcat,xlab="PC1", ylab="PC3")
axis(side=1, pos= 0, labels = F, col="darkgray")
axis(side=3, pos= 0, labels = F, col="darkgray")
axis(side=2, pos= 0, labels = F, col="darkgray")
axis(side=4, pos= 0, labels = F, col="darkgray")
legend("bottomleft",levels(factor(varcat)),pch=1,col=c(1,2), cex=0.6)

#Race
varcat=factor(r[,7])
plot(Psi[,eje1],Psi[,eje2],col=varcat,xlab="PC1", ylab="PC3")
axis(side=1, pos= 0, labels = F, col="darkgray")
axis(side=3, pos= 0, labels = F, col="darkgray")
axis(side=2, pos= 0, labels = F, col="darkgray")
axis(side=4, pos= 0, labels = F, col="darkgray")
legend("bottomleft",levels(factor(varcat)),pch=1,col=c(1,2,3,4), cex=0.6)

#Marst
varcat=factor(r[,8])
plot(Psi[,eje1],Psi[,eje2],col=varcat,xlab="PC1", ylab="PC3")
axis(side=1, pos= 0, labels = F, col="darkgray")
axis(side=3, pos= 0, labels = F, col="darkgray")
axis(side=2, pos= 0, labels = F, col="darkgray")
axis(side=4, pos= 0, labels = F, col="darkgray")
legend("bottomleft",levels(factor(varcat)),pch=1,col=c(1,2,3,4,5,6), cex=0.6)

#Empstat
varcat=factor(r[,12])
plot(Psi[,eje1],Psi[,eje2],col=varcat,xlab="PC1", ylab="PC3")
axis(side=1, pos= 0, labels = F, col="darkgray")
axis(side=3, pos= 0, labels = F, col="darkgray")
axis(side=2, pos= 0, labels = F, col="darkgray")
axis(side=4, pos= 0, labels = F, col="darkgray")
legend("bottomleft",levels(factor(varcat)),pch=1,col=c(1,2), cex=0.6)

#Ftype
varcat=factor(r[,17])
plot(Psi[,eje1],Psi[,eje2],col=varcat,xlab="PC1", ylab="PC3")
axis(side=1, pos= 0, labels = F, col="darkgray")
axis(side=3, pos= 0, labels = F, col="darkgray")
axis(side=2, pos= 0, labels = F, col="darkgray")
axis(side=4, pos= 0, labels = F, col="darkgray")
legend("bottomleft",levels(factor(varcat)),pch=1,col=c(1,2,3,4,5), cex=0.6)

# Levels of the qualitative variables:
# 2 = 9 levels
# 3 = 5 levels
# 4 = 11 levels
# 6 = 2 levels
# 7 = 4 levels
# 8 = 6 levels
# 9 = 108 levels
# 10 = 12 levels
# 11 = 15 levels
# 12 = 2 levels
# 13 = 4 levels
# 17 = 5 levels
# 20 = 219 levels
# 21 = 13 levels

#all qualitative together
plot(Psi[,eje1],Psi[,eje2],type="n",xlim=c(-0.4,0.6), ylim=c(-0.5,1),xlab="PC1", ylab="PC3")
axis(side=1, pos= 0, labels = F, col="darkgray")
axis(side=3, pos= 0, labels = F, col="darkgray")
axis(side=2, pos= 0, labels = F, col="darkgray")
axis(side=4, pos= 0, labels = F, col="darkgray")

#divide categoricals in several graphs if joint representation saturates

#dcat<-c(2,3,4)
dcat<-c(6,7,8)
#dcat<-c(12,13,17)

colors<-rainbow(length(dcat))

c<-1
for(k in dcat){
  seguentColor<-colors[c]
  fdic1 = tapply(Psi[,eje1],r[,k],mean)
  fdic2 = tapply(Psi[,eje2],r[,k],mean) 
  
  text(fdic1,fdic2,labels=levels(factor(r[,k])),col=seguentColor, cex=0.6)
  c<-c+1
}
legend("bottomleft",names(r)[dcat],pch=1,col=colors, cex=0.6)

#Represent numerical variables in background

plot(Psi[,eje1],Psi[,eje2],type="n",xlim=c(-1.5,1), ylim=c(-1,1),xlab="PC1", ylab="PC3")
#plot(X,Y,type="none",xlim=c(min(X,0),max(X,0)))
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")
#add projections of numerical variables in background
arrows(ze, ze, X, Y, length = 0.07,col="grey")
text(X,Y,labels=etiq,col="grey", cex=0.7)

#add centroids
c<-1
for(k in dcat){
  seguentColor<-colors[c]
  
  fdic1 = tapply(Psi[,eje1],r[,k],mean)
  fdic2 = tapply(Psi[,eje2],r[,k],mean) 
  
  #points(fdic1,fdic2,pch=16,col=seguentColor, labels=levels(r))
  text(fdic1,fdic2,labels=levels(factor(r[,k])),col=seguentColor, cex=0.6)
  c<-c+1
}
legend("bottomleft",names(r)[dcat],pch=1,col=colors, cex=0.6)

#add ordinal qualitative variables. Ensure ordering is the correct

dordi<-c(11) #educ99

levels(factor(r[,dordi[1]]))
#reorder modalities: when required
r[,dordi[1]] <- factor(r[,dordi[1]], ordered=TRUE,  
                       levels= c("No school","1st-4th grades","5th-8th grades","9th grade","10th grade", 
                                 "11th grade","12th grade","High school","Some collage","Associate's degree 1",
                                 "Associate's degree 2","Bachelor's degree","Master's degree",
                                 "Professional degree","Doctoral degree"))
levels(r[,dordi[1]])

c<-1
col<-1

for(k in dordi){
  seguentColor<-colors[col]
  fdic1 = tapply(Psi[,eje1],r[,k],mean)
  fdic2 = tapply(Psi[,eje2],r[,k],mean) 
  #points(fdic1,fdic2,pch=16,col=seguentColor, labels=levels(r[,k]))
  #connect modalities of qualitative variables
  lines(fdic1,fdic2,pch=16,col="orange")
  text(fdic1,fdic2,labels=levels(r[,k]),col="orange", cex=0.6)
  c<-c+1
  #col<-col+1
}
legend("topleft",names(r)[dordi],pch=1,col="orange", cex=0.6)


