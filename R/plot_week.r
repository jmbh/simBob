plot_week<-function(t,days,S,Cs_up,Cs_low,sleep_bool,H,Ch_up,Ch_low,eat_bool,A,W,F,App,M,hb,loi,EM){

##preplotting things
#sleep
lwd<-2
N<-length(t);
counter_s<-0;
t_sleep<-rep(0,N);

#hunger
counter_h<-0;
t_eat<-rep(0,N);

for (i in 1:N)
{
    if (sleep_bool[i])
	{
        counter_s<-counter_s+1;
        t_sleep[counter_s]<-t[i];
    }
	if (eat_bool[i])
	{
        counter_h<-counter_h+1;
        t_eat[counter_h]<-t[i];
    }
}

t_sleep<-t_sleep[1:counter_s];

sleep_plot<-rep(0,length(t_sleep));


t_eat<-t_eat[1:counter_h];

eat_plot<-rep(0,length(t_eat));


###plotting

dev.new(noRStudioGD = TRUE)
plot(t/24,Cs_low,
			bty='l',
			bty='l',
			col='red',
			type='l',
			lwd=lwd,
			xlab='days',
			ylab='Sleep propensity',
			ylim=c((min(c(Cs_low,Cs_up,S))),
				   (max(c(Cs_low,Cs_up,S)))),
			las=1)

lines(t/24,Cs_up,col='red',lwd=lwd)
lines(t/24,S,col='black',lwd=lwd)
points(t_sleep/24,sleep_plot+min(Cs_low)-0.015,pch=19,lwd=0.5,col='black',cex=0.5)
title('Sleep pattern')

dev.new(noRStudioGD = TRUE)
plot(days,F,bty='l',
				   col='black',
				   type='l',
				   lwd=lwd,
				   xlab='day',
				   ylab='Fatigue' ,
				   las=1)
title('Fatigue')

dev.new(noRStudioGD = TRUE)
plot(t/24,A,
			bty='l',
			col='black',
			type='l',
			lwd=lwd,
			xlab='days',
			ylab='Activity',
			las=1)
points(t_sleep/24,sleep_plot+0.105,pch=19,lwd=0.5,col='red',cex=0.5)
points(t_eat/24,eat_plot+0.095,pch=19,lwd=0.5,col='blue',cex=0.5)
legend('topright', c('Activity', 'Sleeping', 'Eating'),
	   lty=1,
	   xpd=TRUE,
	   col=c('black','red','blue'),
	   bty='l'
	   )
title('Daily behaviour')

dev.new(noRStudioGD = TRUE )
plot(t/24,Ch_low,
			bty='l',
			col='red',
			type='l',
			lwd=lwd,
			xlab='minutes',
			ylab='Hunger feeling',
			ylim=c((min(c(Ch_low,Ch_up,H))),
				   (max(c(Ch_low,Ch_up,H)))),
			las=1)
lines(t/24,Ch_up,col='red',lwd=lwd)
lines(t/24,H,col='black',lwd=lwd)
points(t_eat/24,eat_plot+min(Ch_low)-0.015,pch=19,lwd=0.5,col='black',cex=0.5)
title('Eating pattern')


dev.new(noRStudioGD = TRUE)
plot(t/24,App,bty='l',
				   col='black',
				   type='l',
				   lwd=lwd,
				   xlab='day',
				   las=1,
				   ylab='Appetite')
title('Appetite')

dev.new(noRStudioGD = TRUE)
plot(t/24,M
			,bty='l',
			col='black',
			type='l',
			lwd=lwd,
			xlab='day',
			ylab='Mood',
			las=1
			)
title(main='mood')
lines(t/24,rep(hb,N),lwd=lwd,col='red')
legend('topright', c('Mood', 'Home base'),
	   lty=1,
	   xpd=TRUE,
	   col=c('black','red'),
	   bty='l'
	   )

dev.new(noRStudioGD = TRUE)
plot(t/24,loi,
			bty='l',
			col='black',
			type='l',
			lwd=lwd,
			xlab='day',
			ylab='Loss of interest',
			las=1)
title('Loss of interest')

if (sum(EM)!=0){
  dev.new(noRStudioGD = TRUE)
  #external misery
	plot(t/24,EM,
				col='black',
				bty='l',
				type='l',
				lwd=3,
				las=1,
				xlab='day',
				ylab='External misery ')
	title('External misery')
}



}
