sim.dat=read.csv('gag_tip_sa.csv')
cutoff=50


rnd = function(x) trunc(x+0.5) #define round function (r's version of round is not typical US version)

len=rnd(sim.dat$LENGTH1_MM/10)

b.dat=sample(len,size=20)
b=density(b.dat,bw=5)
plot(b,col='red',ylim=c(0,.05),xlim=c(6,185))
for(i in 1:99){
b.dat=sample(len,size=cutoff)
b=density(b.dat)
lines(b$x,b$y,col='red')
}

a=density(len,bw=5)
lines(a,lwd=3)
# place in loop
out.ks=ks.test(a$y,b$y)
#temp code to plot histogram of each bin
x=hist(len,breaks=seq(from=range(len)[1],to=range(len)[2],l=(range(len)[2]-range(len)[1]+1)/3))
b.dat=sample(len,size=cutoff)
b=hist(b.dat,breaks=seq(from=range(len)[1],to=range(len)[2],l=(range(len)[2]-range(len)[1]+1)/3),plot=FALSE)
lines(b$breaks[1:59]-1,b$counts*length(len)/cutoff,lwd=2,col='red')
t.test(x$counts,b$counts*length(len)/cutoff)