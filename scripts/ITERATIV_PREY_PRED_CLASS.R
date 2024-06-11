#
#  SOLVE TRAJECTORY BY ITERATIVE INTEGRATION
#
#  METHODE ITERATIVE PAR ADDITION: N(t+1) = N(t) + (dN/dt . delta) avec dt=1
#  Advantage: we controle the pertubation dN (that can be multiplied by eps)
#  

N0 = 1
P0 = 1
r1=0.1
r2=0.3
K1=100
K2=100
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
while (i <= timetot) {
#	prey[i]=prey[i-1] + (r1*prey[i-1] - a* prey[i-1]*pred[i-1])*delta
#	pred[i]=pred[i-1] + (r2*pred[i-1] + b* prey[i-1]*pred[i-1])*delta
#
	a=r1/K1
	b=r2/K2

	prey[i]=prey[i-1] + (r1*prey[i-1] - a* prey[i-1]*prey[i-1])*delta
	pred[i]=pred[i-1] + (r2*pred[i-1] - b* pred[i-1]*pred[i-1])*delta
#
	i=i+1
	}
	return(list(prey=prey,pred=pred))
}

param=c(a,b,r1,r2,delta)
F= IterPP(timetot, V, param)
prey=F$prey
pred=F$pred
COLORS     <- 1:2
LINE.TYPES <- c("solid","solid")
matplot(x, cbind(prey,pred), type="l", col=c("blue","red"), xlab="Time",ylab="Population of preys and predators")
legend("topleft", c("prey", "pred"), col = c("blue","red"), lty = LINE.TYPES)

