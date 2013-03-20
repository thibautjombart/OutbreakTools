library(ape)
library(sna)
library(network)

#' Simulate an epidemic following a SIR model
#' @param N Size of the population
#' @param D Duration of simulation
#' @param beta Rate of infection
#' @param nu Rate of recovery
#' @return simulated epidemic as an obkData object
#' @author Xavier Didelot
simuEpi <- function (N=1000,D=10,beta=0.001,nu=0.1,showPlots=FALSE) {
	S<-matrix(0,D,3)
	T<-matrix(0,N,3)
	dates<-matrix("",N,1)
	S[1,1]=N-1;S[1,2]=1;S[1,3]=0
	T[1,1]=1;T[1,2]=NA;T[1,3]=0
	dates[1,1]=as.character(as.Date(0,origin="2000-01-01"));
	curinf=1;
	ninf=1;
	for (i in 2:D) {
		inf=rbinom(1,S[i-1,1],1-(1-beta)^S[i-1,2])
		rec=rbinom(1,S[i-1,2],nu)
		if (inf>0) for (j in 1:inf) {
			T[ninf+1,1]=ninf+1;
			T[ninf+1,2]=sample(curinf,1);
			T[ninf+1,3]=i;
			dates[ninf+1,1]=as.character(as.Date(i-1,origin="2000-01-01"));
			ninf=ninf+1;
		}
		curinf=sample(curinf,length(curinf)-rec)
		if (inf>0) curinf=c(curinf,(ninf-inf+1):ninf)
		S[i,1]=S[i-1,1]-inf
		S[i,2]=S[i-1,2]+inf-rec
		S[i,3]=S[i-1,3]+rec
	}
	T=T[1:ninf,]
	dates=dates[1:ninf,]
	if (showPlots) {plotEpi(S);plot(infectorTableToNetwork(T));}
	samp=data.frame("sampleID"=1:ninf,"individualID"=1:ninf,"date"=dates)
	ret<-new("obkData",individuals=data.frame("individualID"=1:ninf),sample=samp)
	return(ret)
}

#' Plot the number of susceptible, infected and recovered as a function of time
#' @param S Matrix containing the numbers to be plotted
#' @author Xavier Didelot
plotEpi <- function(S) {
	plot(c(0,dim(S)[1]),c(0,sum(S[1,])),type='n',xlab='Days',ylab='Individuals')
	lines(S[,1],col='black')
	lines(S[,2],col='red')
	lines(S[,3],col='blue')
	legend('right',lty=c(1,1),col=c('black','red','blue'),c('Susceptible','Infected','Recovered'))
}

#' Convert transmission tree to a network
#' @param transmissiontreeData Matrix of who infected whom
#' @return Network of who infected whom
#' @author Caroline Colijn
infectorTableToNetwork <- function (transmissiontreeData)
{
uniqueIDs <- sort(c(unique(as.character(transmissiontreeData[,1]),as.character(transmissiontreeData[,2]))))
nUniqueIDs <- length(uniqueIDs)
edgeList <- na.omit(transmissiontreeData[,1:2])
numEdges <- dim(edgeList)[1]
y <- network.initialize(nUniqueIDs)
network.vertex.names(y) <- uniqueIDs
for(i in 1:numEdges){
v1 <- match(as.character(edgeList[i,1]),uniqueIDs)
v2 <- match(as.character(edgeList[i,2]),uniqueIDs)

if (!is.na(v2)) add.edges(y,v2,v1)
}
return(y)
}

#' Create phylogenetic tree from transmission tree
#' @param transmissiontreeData Matrix of who infected whom
#' @return phylogenetic tree representing how samples of the infectious agents may be related
#' @author Caroline Colijn
phylofromtranstree <- function(transmissiontreeData){

# use the transmission tree data to create an *undirected* network

uniqueIDs <- sort(c(unique(as.character(transmissiontreeData[,1]),as.character(transmissiontreeData[,2]))))
nUniqueIDs <- length(uniqueIDs)
edgeList <- na.omit(transmissiontreeData[,1:2])

numEdges <- dim(edgeList)[1]
y <- network.initialize(nUniqueIDs)
network.vertex.names(y) <- uniqueIDs
for(i in 1:numEdges){
v1 <- match(as.character(edgeList[i,1]),uniqueIDs)
v2 <- match(as.character(edgeList[i,2]),uniqueIDs)

if (!is.na(v2) & !is.na(v1)) add.edges(y,v2,v1)
if (!is.na(v1) & !is.na(v2)) add.edges(y,v1,v2)
}
mynet<-y

# get pairwise shortest path distances in this network and use them to make a phylogeny in one of 2 very simple ways

phylotree=nj(geodist(mynet)$gdist)

return(phylotree)
}

