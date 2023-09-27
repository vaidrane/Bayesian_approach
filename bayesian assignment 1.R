
#prior distribution p~beta(a,b) 

a=2
b=3
p=rbeta(1,a,b)
p
x=numeric(1000)
set.seed(77)
for(i in 1:1000){
  p=rbeta(1,a,b)
  x[i]=rbinom(1,1,p)
}
#under quadratic loss,mean of posterior is estimate
phat=mean(x)
phat


pm=numeric(1000)
x=numeric(1000)
set.seed(77)
for(i in 1:1000){
  p=rbeta(1,a,b)
  x[i]=rbinom(1,1,p)
  pm[i]=(x[i]+a)/(a+b+1)
}
mean(x)
e=mean(pm)
e
t= (a+sum(x))/(a+b+length(x))  
t

(e-t)/t


set.seed(79)
xp=numeric(1000)
pm=numeric(1000)
n=12

for(i in 1:1000){
  p=rbeta(1,a,b)
  xp[i]=rbinom(1,n,p)
  pm[i]=((xp[i])+a)/(a+b+n)
}


em=mean(pm)
em
(a+mean(xp))/(a+b+n)
tm=(a+sum(xp))/(a+b+n*length(x))
tm
(em-tm)/tm
