# Simulation of an epidemic tree following Kenah 2008 
# "Competition between infectors"
# Assumptions:
# 		  constant infectious period
# 		  exponential infectious contact interval
#         no intervention
# Output: time= infection time
#         v= source case
#         genvec= generation interval
#         S= number susceptibles
#         I= number infected
#         PI= number of potential infectors

#rm(list=ls())
count=function(C,k){
rC=rank(C)
j=0
for(i in 1:length(C)){
if(rC[i]==k){
j=j+1
break
}
else{j=j+1}
}
return(j)
}

search=function(M,x){
for(i in 1:dim(M)[1]){
for(j in 1:dim(M)[2]){
if(M[i,j]==x){return(c(i,j))}
}
}
}


n=1000
r=1
R0=4
t.intro=0
lambda=R0/(n-1)
v=rep(NA,n)
id=rep(NA,n)
time=rep(0,n)
PI=rep(0,n)
S=I=rep(NA,n)
time[1]=t.intro
id[1]=1
PI[1]=1
PI.temp=0
I.temp=0
I[1]=1
tij=matrix(0,nrow=n,ncol=n)
for(i in 1:1){
for(j in 1:(n-1)){
tij[i,(j+1)]=t.intro+rexp(1,lambda)
if(tij[i,(j+1)]>r){tij[i,(j+1)]=0}
}
}

temp.tij=tij[1,]
time[2]=min(temp.tij[temp.tij!=0])
PI[2]=1
id[2]=search(tij,time[2])[2]
v[2]=1
I.temp=time+r
I.temp=I.temp[1:2]
I[2]=2-length(I.temp[I.temp<time[2]])

finalsize=0
for(k in 2:(n-1)){
tij[k,]=time[k]+rexp(n,lambda)
for(h in 1:n){
if(tij[k,h]>time[k]+r){tij[k,h]=0}
}
tij[,id[1:k]]=0
temp.tij=tij[1:k,]
temp.tij=temp.tij[temp.tij!=0]
if(length(temp.tij)==0){
finalsize=k
break
}
time[k+1]=min(temp.tij)
id[k+1]=search(tij,time[k+1])[2]
v[k+1]=search(tij,time[k+1])[1]
I.temp=time+r
I.temp=I.temp[1:(k+1)]
I[k+1]=k+1-length(I.temp[I.temp<time[k+1]])
PI.temp=tij[,id[k+1]]
PI.temp=PI.temp[PI.temp!=0]
PI[k+1]=length(PI.temp)
}

genvec=0
for(i in 1:n){
genvec[i]=time[i]-time[v[i]]
}
time=time[1:finalsize]
v=v[1:finalsize]
genvec=genvec[1:finalsize]
I=I[1:finalsize]
S=999:(1000-finalsize)
PI=PI[1:finalsize]
data=data.frame(time,v,genvec,S,I,PI)
