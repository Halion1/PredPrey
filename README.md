# SHIFT OF REGULATION SIGNALS BETWEEN MICROORGANISM COMMUNITY MEMBERS

Author: Jeremy Guerrero 

In this project we model the behavior of three organism. The first two were predators (fox and wolf) and the third one was a prey (rabbit). 
The model made considered a shift in the conduct of both predators. We consider that when both are very small in population then they should help each other (mutualism). But when they grow past a threshold then they should compete for their food source (competition). The model was obtain by using the equation from Yuang shi Wang [1] and the Lokta-Volterra predation formula. 

	%Creating variables 
	rs=[0.1,0.1,0.9]; cs=[80,60,1]; as=[1,0.8]; ds=[0.01, 0.2]; bs=[100,80]; inits=[1,1,100];
	timefin=100; t=[1:timefin];
	k=[1,1];
	Y2Sol=shift (rs, cs, as, bs, ds, timefin, inits,k);
	N1 Y 2 Sol (1,:);
	N2=Y 2 Sol (2,:);
	preyt Y 2 Sol (3,:)
	figure;
	plot (t, N1);
	ylabel (Density of population);
	hold on
	plot(t, N2, r--);
	plot (t, preyt,k);
	xlabel(Time);
	title(Shift model of 2 predators and 1 prey)
	legend (N1, N2, prey)
	hold off


	function trial=shift(r,c,a,b,d,time, init, x) 
		r1=r(1,1);r2=r(1,2);r3=r(1,3); c1=c(1,1);c2=c(1,2);
		c3=c(1,3); a1=a(1,1); a2=a(1,2);
		b1=b(1,1); b2=b(1,2);
		d1=d(1,1); d2=d(1,2); delta=0.1;i=2;
		alpha 1=x(1,1)/x (1,2); alpha2=x(1,2)/x(1,1);
		N0=init (1,1); M 0=init (1,2); P0=init (1,3); N=repmat (N 0,1, time); M=repmat (M0,1,time); prey=repmat (P0,1, time);
		while i<time
		N(i)=N(i−1)+(r1*N(i−1)*(c1+a1*b2−N(i−1)−a1*|(M (i−1)−b2)|)+0.1* prey (i−1)* N ( i−1)); 
		M(i)=M(i−1)+(r2* M (i −1)* (c 2+ a2*b1–M(i−1)—a2*|(N (i−1)−b1)|)+0.1* prey (i − 1) * M (i−1) 
		prey(i)= prey (i−1)+(r 3* prey (i−1)−d 1* N (i −1) * prey (i −1)−d 2× M (i −1) * prey (i −1)) *delta ;
		i=i+1; 
		end
	trial=[N; M; prey];
	end

![alt text]('Final Result'.png)

What we can see is that the population of both predators start growing up rapidly when they are in a small quantity. But, after a period when the threshold b1 or b2 is passed then their population stops their upgrowth, and they begin to decrease because of the competition. Afterwards, their population number crosses again the threshold, and they begin to rise. The reason for this is that they return to the mutualism model.



References
[1]	Y. Wang and H. Wu, “A mutualism-competition model characterizing competitors with mutualism at low density,” Math. Comput. Model., vol. 53, no. 9–10, pp. 1654–1663, 2011, doi: 10.1016/j.mcm.2010.12.033.

