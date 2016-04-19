plot_month<-function(days,t,S,H,A,W,F,loi,M,EM){


#dev.new()
#sleep
plot(days,S,
			bty='l',
			type='l',
			lwd=3,
			las=1,
			xlab='day',
			ylab='sleep hours per day')
title('Hours of sleep per day')

#dev.new()
#hunger
plot(days,H,
			col='black',
			bty='l',
			type='l',
			lwd=3,
			las=1,
			xlab='day',
			ylab='minutes per day')
title('Minutes per day spend eating')


#dev.new()
#activity
plot(days,A,
			col='black',
			bty='l',
			type='l',
			lwd=3,
			las=1,
			xlab='day',
			ylab='activity intensity in minutes per day')
title('Minutes times activity intensity per day')

#dev.new()
#weight
plot(days,W,
			col='black',
			bty='l',
			type='l',
			lwd=3,
			las=1,
			xlab='day',
			ylab='kg')
title('Body weight')

#dev.new()
#fatigue
plot(days,F,
			col='black',
			bty='l',
			type='l',
			lwd=3,
			las=1,
			xlab='day',
			ylab='fatigue')
title('Fatigue')

#dev.new()
#loss of interest
plot(t/24,loi,
			col='black',
			bty='l',
			type='l',
			lwd=3,
			las=1,
			xlab='day',
			ylab='Loss of interest')
title('Loss of interest')

#dev.new()
#mood
plot(days,M,
			col='black',
			bty='l',
			type='l',
			lwd=3,
			las=1,
			xlab='day',
			ylab='mean mood per day')
title('Mean mood per day')

if (sum(EM)!=0){
#	dev.new()
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
