#
#  SOLVE TRAJECTORY BY ITERATIVE INTEGRATION
#
#  METHODE ITERATIVE PAR ADDITION: N(t+1) = N(t) + (dN/dt . delta) avec dt=1
#  Advantage: we controle the pertubation dN (that can be multiplied by eps)
#  

N0 = 1
P0 = 1
S0 = 50
r1max=0.2
r2=0.2372
r2max=0.2
#r2=0.2
K1=100
K2=100
m1=0.2
m2=0.2
km1=40.0
km2=40.0
#km2=1.0*10^-6
alpha=0.01
gama=2.5

a=r1/K1
b=r2/K2	

delta=0.1
V=c(N0,P0)

timetot = 1500
x=1:timetot
IterPP <- function(timetot, V, param){
i=2
prey = rep(N0,timetot)
pred = rep(P0,timetot)
sug =  rep(S0,timetot)
while (i <= timetot) {
#	prey[i]=prey[i-1] + (r1*prey[i-1] - a* prey[i-1]*pred[i-1])*delta
#	pred[i]=pred[i-1] + (r2*pred[i-1] + b* prey[i-1]*pred[i-1])*delta
#
	if (sug[i-1]<0){sug[i-1]=0}
	
	r1=gama*r1max * sug[i-1]/(km1 + sug[i-1])
	r2=gama*r2max * sug[i-1]/(km2 + sug[i-1])
	
	a=r1*gama/K1
	b=r2*gama/K2

    dN=(r1-m1)*prey[i-1] - a* prey[i-1]*prey[i-1]
    dP=(r2-m2)*pred[i-1] - b* pred[i-1]*pred[i-1]
	dS=-r1*prey[i-1]-r2*pred[i-1]
	
	prey[i]=prey[i-1] + dN*delta
	pred[i]=pred[i-1] + dP*delta
	sug[i]=sug[i-1] + dS*delta
#
	i=i+1
	}
	return(list(prey=prey,pred=pred,sug=sug))
}

param=c(a,b,r1,r2,delta)
F= IterPP(timetot, V, param)
prey=F$prey
pred=F$pred
sug=F$sug
COLORS     <- 1:2
LINE.TYPES <- c("solid","solid","solid")
matplot(x, cbind(prey,pred,sug), type="l", col=c("blue","red","green"), xlab="Time",ylab="Population of preys and predators")
legend("topleft", c("prey", "pred","sug"), col = c("blue","red","green"), lty = LINE.TYPES)

