# simBob
This R package simulates different aspects of the daily life of Bob. Bob can sleep, eat, work, be active, have an appetite,
feel fatigue, feel loss of interest and feel mood. All these different aspects are simulated separately, but are influencing 
each other according to a network model. The network of Bob receives input from an external misery node that projects onto 
the mood and from social eating that projects on the appetite. Bob is also monitored for 6 different symptoms of depression.


The R package can be installed using (if the devtools package is already installed):
library('devtools')
install_github('lisannehuurdeman/simBob')


