weight_check<-function(w)
{
if (w<30)
{
	stop('The body weight is below 30 kg and it is not possible the simulated person is still alive. Game over.')  
	print('stop')
}else if (w<35) {
	warning('The body weight is below 35 kg, this is dangerously low')
	print('warninig')
}

if(w>650)
{
	stop('The body weight is above 650 kg, simulated person is definitely death')
}else if (w>400){
	warning('The body weight is above 400 kg, simulated person probably died already.')
} else if (w>175){
	warning('The body weight is above 175 kg, this is dangerously high') 
}	

}