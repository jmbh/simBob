# #' @title Network figures of Bob
# #' @author Lisanne Huurdeman
#' @import qgraph
# #' @description Shows the network that is used to simulate Bob. Returns two figures, first figure is the normal network,
# #'              second figure is the network with all the parameters at the location where they are used.

show_network<-function(){

#library('qgraph')

# Mood 1
# Activity 2
# Eat 3
# Weight 4
# Sleep 5
# Fatigue 6
# Appetite 7
# Loss of Interest 8
# External misery 9
# Social eating 10


label=c('Mood','Activity','Eat','Weight','Sleep','Fatigue','Appetite','Loss of \nInterest','External \nmisery','Social\neating')
label_para=c(expression(atop(bold(Mood)*','~c[M]*',',  #mood
							  mu[hb]*','~beta)),
			 expression(atop(bold(Activity)*','~mu[a]*','~sigma[a]*',',~c[sleep]*','~c[rest]*','~c[work])), #activity
			 expression(atop(bold(Hunger)*','~tau[d]*','~tau[r]*',',tau[rs]*','~C*','~H*','~L*','~A*','~t[0])), #eat
			 expression(atop(bold(Weight)*',',c[BW])),#weight
			 expression(atop(bold(Sleep)*','~tau[d]*','~tau[r]*',',C*','~H*','~L*','~A*','~t[0])),#Sleep
			 expression(atop(bold(Fatigue)*',',c[F]*','~Sl[normal])), #Fatigue
			 expression(atop(bold(Appetite)*',',c[app])), #Appetite
			 expression(atop(bold(Loss*' '~of),bold(interest))), #Loss of \nInterest
			 'External \n misery', #expression(External*' '~misery), #External \nmisery
			 'Social \n eating') #expression(Social*' '~eating)) #Social\neating
N=10*10


shape<-rep('circle',10)
shape[9]<-'rectangle'
shape[10]<-'rectangle'

net<-matrix(c(0,1,0,0,1,0,0,1,0,0,
			  1,0,1,1,1,0,0,0,0,0,
			  0,1,0,1,0,0,1,0,0,0,
			  1,0,0,0,0,0,0,0,0,0,
			  1,1,1,0,0,1,0,0,0,0,
			  1,1,0,0,1,0,0,0,0,0,
			  1,0,1,0,0,0,0,0,0,0,
			  0,1,0,0,0,0,1,0,0,0,
			  1,0,0,0,0,0,0,0,0,0,
			  0,0,0,0,0,0,1,0,0,0
			  ),10,10,byrow=TRUE)




elabel<-seq(1, 24, length = 24)
curve<-rep(NA,24)
curve[3]<- -1.7 #sleep to mood
curve[17]<- 2.3 #mood to sleep
curve[8]<- -0.5 #eat to activity
curve[12]<- -0.5 #activity to eat
curve[9]<- -0.5 #sleep to activity
curve[18]<- -0.5 #activity to sleep
curve[21]<- -0.65 #eat to appetite
curve[14]<- -0.65 #appetite to eat
curve[20]<- -0.75 #sleep to fatigue
curve[19]<- -0.75 #fatigue to sleep
curve[5]<- 2.6 #appetite to mood
curve[1]<- -0.3 #activity to mood
curve[7]<- -0.3 #mood to activity

layout<-matrix(0,7,6)
layout[1,4]<-1
layout[4,3]<-2
layout[5,1]<-3
layout[3,2]<-4
layout[5,5]<-5
layout[2,5]<-6
layout[2,1]<-7
layout[1,2]<-8
layout[1,6]<-9
layout[1,1]<-10

# dev.new(noRStudioGD = TRUE)
q<-qgraph(net,labels=label,
	   shape=shape,
	   label.scale=FALSE,
	   label.cex=1,
	   vsize=10,
	   fade=FALSE,
	   edge.color='black',
	   layout=layout,
	   DoNotPlot=TRUE)

layout2<-q$layout
# q$graphAttributes$Edges$curve
layout2[4,1]<-layout2[4,1]
layout2[4,2]<-layout2[4,2]+0.2
layout2[6,1]<-layout2[6,1]-0.3
layout2[9,1]<-layout2[9,1]-0.25
layout2[8,1]<-layout2[8,1]+0.1
layout2[8,2]<-layout2[8,2]-0.15
layout2[1,1]<-layout2[1,1]-0.1
layout2[10,1]<-layout2[10,1]-0.15



q<-qgraph(net,labels=label,
	   shape=shape,
	   label.scale=FALSE,
	   label.cex=1,
	   vsize=10,
	   fade=FALSE,
	   edge.color='black',
	   layout=layout2,
	   curve=curve)

vsize<-rep(15,10)
vsize[9]<-12
vsize[10]<-12

#edge labels parameters

n_edges=sum(net)
elabel_para<-rep('',n_edges)
elabel_para[1]<- expression(alpha[1])
elabel_para[2]<- expression(alpha[4])
elabel_para[3]<- expression(alpha[5])
elabel_para[4]<- expression(alpha[2])
elabel_para[5]<- expression(alpha[3])
elabel_para[6]<- expression(alpha[6])
elabel_para[7]<- expression(c[muM])
elabel_para[8]<- ''
elabel_para[9]<- ''
elabel_para[10]<- expression(c[muF])
elabel_para[11]<- expression(c[muloi])
elabel_para[12]<- ''
elabel_para[13]<- ''
elabel_para[14]<- expression(atop(c[taurapp_pos]*', ',c[taurapp_neg]))
elabel_para[15]<- expression(c[ee])
elabel_para[16]<- expression(c[ei])
elabel_para[17]<- expression(c[taurM])
elabel_para[18]<- ''
elabel_para[19]<- expression(atop(c[taudF]*', ',c[taurF]))
elabel_para[20]<- expression(c[FS])
elabel_para[21]<-expression(c[appH])
elabel_para[22]<-expression(c[apploi])
elabel_para[23]<-expression(c[appSE])
elabel_para[24]<- expression(c[loiM])

epos<-rep(0.5,n_edges)

epos[1]<-0.54;
epos[7]<-0.54;
epos[11]<-0.6;
epos[23]<-0.3;

# dev.new(noRStudioGD = TRUE)
qgraph(net,labels=label_para,
       #directed=d,
	   shape=shape,
	   label.scale=FALSE,
	   label.cex=rep(0.8,10),
	   vsize=vsize,
	   fade=FALSE,
	   edge.color='black',
   	   edge.labels=elabel_para,
	   edge.label.position=epos,
	   layout=layout2,
	   curve=curve)

}
