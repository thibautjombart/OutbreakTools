#Simulate an epidemic following a SIRS model
#N=Size of the population
#D=Duration of simulation
#beta=Rate of infection
#nu=Rate of recovery
#f=Rate of loss of immunity
simuEpi <- function (N=1000,D=365,beta=0.5,nu=0.1,f=0.5) {
	S<-matrix(0,D,3)
	S[1,1]=N-1;S[1,2]=1;S[1,3]=0
	for (i in 2:D) {
		inf=rbinom(1,S[i-1,1],beta*S[i-1,2]/N)
		rec=rbinom(1,S[i-1,2],nu)
		los=rbinom(1,S[i-1,3],f)
		S[i,1]=S[i-1,1]-inf+los
		S[i,2]=S[i-1,2]+inf-rec
		S[i,3]=S[i-1,3]+rec-los
	}
	return(S)
}

#Plot the epidemic
plotEpi <- function(S) {
	plot(c(0,dim(S)[1]),c(0,sum(S[1,])),type='n',xlab='Days',ylab='Individuals')
	lines(S[,1],col='black')
	lines(S[,2],col='red')
	lines(S[,3],col='blue')
	legend('right',lty=c(1,1),col=c('black','red','blue'),c('Susceptible','Infected','Recovered'))
}

S<-simuEpi()
plotEpi(S)
