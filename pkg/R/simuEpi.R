library(ape)
library(sna)
library(network)
library(grImport)

#' Simulate an epidemic following a SIRS model
#' @param N Size of the population
#' @param D Duration of simulation
#' @param beta Rate of infection
#' @param nu Rate of recovery
#' @param f Rate of loss of immunity
#' @return simulated epidemic
simuEpi <- function (N=1000,D=50,beta=0.2,nu=0.1,f=0.5) {
	S<-matrix(0,D,3)
	T<-matrix(0,N,3)
	S[1,1]=N-1;S[1,2]=1;S[1,3]=0
	T[1,1]=1;T[1,2]=NA;T[1,3]=0
	curinf=1;
	ninf=1;
	for (i in 2:D) {
		inf=rbinom(1,S[i-1,1],beta*S[i-1,2]/N)
		rec=rbinom(1,S[i-1,2],nu)
		los=rbinom(1,S[i-1,3],f)
		if (inf>0) for (j in 1:inf) {
			T[ninf+1,1]=ninf+1;
			T[ninf+1,2]=sample(curinf,1);
			T[ninf+1,3]=i;
			ninf=ninf+1;
		}
		curinf=sample(curinf,length(curinf)-rec)
		if (inf>0) curinf=c(curinf,(ninf-inf+1):ninf)
		S[i,1]=S[i-1,1]-inf+los
		S[i,2]=S[i-1,2]+inf-rec
		S[i,3]=S[i-1,3]+rec-los
	}
	ret=list()
	ret$S=S
	ret$T=T[1:ninf,]
	return(ret)
}

###Plot the epidemic
plotEpi <- function(S) {
	plot(c(0,dim(S)[1]),c(0,sum(S[1,])),type='n',xlab='Days',ylab='Individuals')
	lines(S[,1],col='black')
	lines(S[,2],col='red')
	lines(S[,3],col='blue')
	legend('right',lty=c(1,1),col=c('black','red','blue'),c('Susceptible','Infected','Recovered'))
}

###Convert transmission tree to a network
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

### Create phylo tree in very simple way from transmission tree
phylofromtranstree <- function(transmissiontreeData,method='nj'){

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

if (method=='upgma')
phylotree=upgma(geodist(mynet)$gdist)
if (method=='nj')
phylotree=nj(geodist(mynet)$gdist)

return(phylotree)
}

### Plot transmission tree
plotTranstree <- function (dat) {
    sink("graph.dot")
    cat("digraph G{\n\nrankdir=LR;\n")
    #Time structure
    cat("node[shape=none]\n")
    for (i in min(dat[,3]):max(dat[,3])) cat("t",i,"[label=\"t=",i,"\",fontname=Helvetica]\n",sep="")
    for (i in min(dat[,3])+1:max(dat[,3])) cat("t",i-1," -> t",i," [color=white]\n",sep="")
    cat("node[shape=circle]\n")
    #When individuals became infected
    for (i in 1:(length(dat)/3)) cat("{rank=same;",i,"[fontname=Helvetica];t",dat[i,3],"}\n",sep="")
    #Who infected whom
    for (i in 1:(length(dat)/3)) if (!is.na(dat[i,2])) cat(dat[i,2],"->",i,"[fontname=Helvetica,label=\"",0,"\"]\n",sep="")
    cat("}")
    sink()
    system("dot graph.dot -Tps -o graph.ps")
    PostScriptTrace('graph.ps','graph.xml')
    graph<-readPicture('graph.xml')
    grid.picture(graph)
} 

### Test functions in this file
testSimu <- function() {
	set.seed(1);
	ret<-simuEpi(f=0,D=50)
	plotEpi(ret$S)
	plot(infectorTableToNetwork(ret$T))
	plot.new()
	plotTranstree(ret$T)
	plot(phylofromtranstree(ret$T)) 
}