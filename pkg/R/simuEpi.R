library(network)

#Simulate an epidemic following a SIRS model
#N=Size of the population
#D=Duration of simulation
#beta=Rate of infection
#nu=Rate of recovery
#f=Rate of loss of immunity
simuEpi <- function (N=1000,D=365,beta=0.5,nu=0.1,f=0.5) {
	S<-matrix(0,D,3)
	T<-matrix(0,N,3)
	S[1,1]=N-1;S[1,2]=1;S[1,3]=0
	T[1,1]=1;T[1,2]=NA;T[1,1]=0
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
	ret$T=T[T[,1]>0,]
	return(ret)
}

#Plot the epidemic
plotEpi <- function(S) {
	plot(c(0,dim(S)[1]),c(0,sum(S[1,])),type='n',xlab='Days',ylab='Individuals')
	lines(S[,1],col='black')
	lines(S[,2],col='red')
	lines(S[,3],col='blue')
	legend('right',lty=c(1,1),col=c('black','red','blue'),c('Susceptible','Infected','Recovered'))
}

#Convert transmission tree to a network
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

ret<-simuEpi(f=0)
plotEpi(ret$S)
plot(infectorTableToNetwork(ret$T))
