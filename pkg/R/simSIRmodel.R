# Epidemic simulator tracking who infects whom
# Code is probably excessive but it works and is reasonably fast
# Input:
# N=population size
# mu=contact rate (Poisson)
# beta=transmission rate
# Output: 
# ID 
# time (time of infection)
# source (source case)
# ICI (infectious period generated from an exponential)
# S (number of susceptibles)
# I (number of infected inds)
# PI (potential infectors)
# gen (generation interval)
#########################################################


# Debugging: N=100; mu=1; beta=4: R0 is beta/mu
SIRsim=function(N,mu,beta){
v=rep(NA,N)
id=rep(NA,N)
P=seq(1:N)
time=rep(NA,N)
gen=rep(NA,N)
ICI=rep(0,N)
temp.t=0
upd.t=0
I.temp=0
I=S=rep(NA,N)
inf.indicator=rep(0,N)
prop.indicator=rep(0,N)
PI=rep(0,N)
# introductory case
case=1
prop.indicator[1]=1
id[1]=1
v[1]=NA
upd.t[1]=0
temp.t[1]=0
time[1]=0
inf.indicator[1]=1
ICI[1]=rexp(1,mu)
last.id=1
Q=case
I[1]=1

repeat{
temp.gen=rexp(1,beta)
if(temp.gen<=ICI[case]){break}
}
newcase=sample(P[-case],1,replace=FALSE)
prop.indicator[newcase]=1
v[newcase]=case
temp.t[newcase]=temp.t[v[newcase]]+temp.gen
time[newcase]=temp.t[newcase]

upd.t[v[newcase]]=temp.t[newcase]
upd.t[newcase]=temp.t[newcase]

ICI[newcase]=rexp(1,mu)
inf.indicator[newcase]=1
latest.set=c(v[newcase],newcase)
id[newcase]=last.id+1
last.id=last.id+1
Q=c(Q,newcase)

I.temp=time+ICI
I.temp=I.temp[I.temp=!is.na(I.temp)]
I[newcase]=id[newcase]-length(I.temp[I.temp<time[newcase]])

repeat{
temp.gen=jump=simultaneous.case=0
for(i in 1:length(latest.set))
{
temp.gen[latest.set[i]]=rexp(1,beta)

if((upd.t[latest.set[i]]+temp.gen[latest.set[i]])>(time[latest.set[i]]+ICI[latest.set[i]])){
Q=Q[Q!=latest.set[i]]
}

if((upd.t[latest.set[i]]+temp.gen[latest.set[i]])<=(time[latest.set[i]]+ICI[latest.set[i]])){
propcase=sample(P[-latest.set[i]],1,replace=FALSE)

if(inf.indicator[propcase]==1){
jump[i]=latest.set[i]
}

if(inf.indicator[propcase]==0&prop.indicator[propcase]==1&(upd.t[latest.set[i]]+temp.gen[latest.set[i]])>(temp.t[propcase])){
simultaneous.case[i]=latest.set[i]
PI[propcase]=PI[propcase]+1
}

if(inf.indicator[propcase]==0&prop.indicator[propcase]==1&(upd.t[latest.set[i]]+temp.gen[latest.set[i]])<(temp.t[propcase])){
prop.indicator[propcase]=0
simultaneous.case[i]=v[propcase]
PI[propcase]=PI[propcase]+1
}

upd.t[latest.set[i]]=upd.t[latest.set[i]]+temp.gen[latest.set[i]]

if(inf.indicator[propcase]==0&prop.indicator[propcase]==0){
Q=c(Q,propcase)
unique(Q)   
prop.indicator[propcase]=1
ICI[propcase]=rexp(1,mu)
upd.t[propcase]=upd.t[latest.set[i]]
temp.t[propcase]=upd.t[propcase]
v[propcase]=latest.set[i]
}
}
}
latest.set=NA
PC=0
PC=Q[inf.indicator[Q]==0]
if(length(PC)==0){break}
if(length(PC)>0){
newcase=PC[temp.t[PC]==min(temp.t[PC])]
inf.indicator[newcase]=1
time[newcase]=temp.t[newcase]
id[newcase]=last.id+1
last.id=last.id+1
latest.set=c(v[newcase],newcase)
I.temp=time+ICI
I.temp=I.temp[I.temp=!is.na(I.temp)]
I.temp=unique(I.temp)
newcase=unique(newcase)
time[newcase]=unique(time[newcase])
ICI[newcase]=unique(ICI[newcase])
id[newcase]=unique(id[newcase])
I[newcase]=id[newcase]-length(I.temp[I.temp<time[newcase]])
}

latest.set=c(latest.set,jump,simultaneous.case)
latest.set=latest.set[latest.set!=0]
latest.set=latest.set[latest.set=!is.na(latest.set)]
if(length(latest.set)==0){break}
if(length(Q)==0){break}
if(last.id==N){break}
}
finalsize=last.id
v=id[v]
data=data.frame(id[order(id)],time[order(id)],v[order(id)],ICI[order(id)])
for(i in 1:finalsize){
gen[i]=data$time[i]-data$time[data$v[i]]
}
I=I[order(id)]
S[1:finalsize]=(N-1):(N-finalsize)
PI=PI[order(id)]+1
data=data.frame(data,S,I,PI,gen)
names(data)=c("ID","time","source","ICI","S","I","PI","gen")
return(data)
}

