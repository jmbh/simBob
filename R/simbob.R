#' @title Daily Life of Bob
#' @author Lisanne Huurdeman
#'
#' @description This function simulates different aspects of the daily life of a human called Bob. Bob sleeps, eats, works,
#' has an appetite, can be fatigue, has a mood and can have the feeling of loss of interest. All these different aspects are simulated
#' separately, but do influence each other according to a network model. The network model can be obtained by the network_figures
#' input. The network of Bob receives input from an external misery node that projects onto the mood and from social eating that
#' projects on the appetite. Bob is also monitored for 6 different symptoms of depression.
#' The different parameters that influence the processes and connections can be modified, the parameter values vary
#' interpersonal. The default values result in a stable average system that correspond to Bob his life.
#' The only input required is t_days, the number of days that should be simulated.
#'
#'
#' @param t_days The number of days that will be simulated. A scalar is required as input.
#'
#' @param t_step The iteration time step in hours of the simulation. A scalar is required as input. The default value is
#' 1/60, which corresponds to 1 minute.
#'
#' @param work The boolean that determines if Bob will have a job or not. The job is 5 days a week from 9.00 to 17.00 hours with
#' lunch from 12.30 to 13.00 hours. A boolean or scalar is required as input. The default value is TRUE.
#'
#' @param body_weight The body weight in kg of Bob at the onset of the simulation. A scalar is required as input. The default
#' value is 70 kg.
#'
#' @param c_BW The constant that determines the influence of the energy difference on the body weight. The energy difference
#' is defined as the difference between food intake times a constant and the activity times a constant. The constant is comparable
#' to the inverse energy density. The energy density depends among others on fat percentage and the type of tissues in the body.
#' A scalar is required as input. The default value is 0.3.
#'
#' @param c_ee_work The constant that scales the energy expenditure (activity) before the body weight can be calculated when Bob
#' has a job. This constant is the inverse average activity of a day, such that a change in activity is calculated as a relative
#' change in the body weight. A scalar is required as input. The default value is 1/205.5.
#'
#' @param c_ee_nowork The constant that scales the energy expenditure (activity) before the body weight can be calculated when
#' Bob is unemployed. This constant is the inverse average activity of a day, such that a change in activity is calculated as
#' a relative change in the body weight. A scalar is required as input. The default value is 1/163.
#'
#' @param c_ei_work The constant that scales the energy intake (food intake) before the body weight can be calculated when Bob has a job.
#' This constant is the inverse average food intake of a day, such that a change in food intake is calculated as a relative
#' change in the body weight. A scalar is required as input. The default value is 1/80.3.
#'
#' @param c_ei_nowork The constant that scales the energy intake (food intake) before the body weight can be calculated when Bob is
#' unemployed. This constant is the inverse average food intake of a day, such that a change in food intake is calculated as a relative
#' change in the body weight. A scalar is required as input. The default value is 1/75.
#'
#' @param tau_d The time constant for the decaying part of the homeostatic sleep process. The homeostatic curve decays
#' during sleep. This parameter determines how steep the decay is. When the parameter is higher the decay will be less steep,
#' such that the sleep duration will be higher. When the parameter is lower the curve will decay more steep, the sleep duration will
#' be shorter.  A scalar or vector of length N (24*t_step*t_days) is required as input.  The value of the time constant can be modified
#' during the simulation.The default value is 4.2.
#'
#' @param tau_r The time constant of the rising part of the homeostatic sleep process. The rising time constant  determines
#' the steepness of the rising curve and thus the timing of sleep onset. Higher values of the rising time constant will
#' approximately cause a later sleep onset, even so  lower values of this time constant will result approximately in an  earlier
#' sleep onset.  A scalar or vector of length N (24*t_step*t_days) is required as input. The value of the time constant can be
#' modified during the simulation. The default value is 18.2.
#'
#' @param tau_dh The decaying time constant of the homeostatic hunger process. The decaying curve represents eating and the value
#' of the time constant determines the steepness of the decaying curve. When the time constant is increased, the curve will be
#' less steep and more time will be spend on eating, even so when the time constant decreases, the curve will be more steep and
#' less time will be spend on eating.  A scalar or vector of length N (24*t_step*t_days) is required as input. The value of the
#' time constant can be modified during the simulation. The default value is 0.25.
#'
#' @param tau_rh The rising time constant of the homeostatic hunger process while  awake. The rising curve represents the build up
#' of a hunger feeling. The rising  time constant determines the steepness of the curve. Therefore, the time between meals, thus
#' also how many meals a day are consumed. A higher value of the time constant results in less meals consumed per  day, because
#' there is more time between the meals.
#' A lower value of the time constant  results in a  faster rise of the homeostatic curve. Thus, the time between meals
#' is less and therefore more meals per day will be consumed.  A scalar or vector of length N (24*t_step*t_days) is required as input.
#' The value of the time constant can be modified during the simulation. The default value is 3.7.
#'
#' @param tau_rhs The rising time constant of the homeostatic hunger process during sleep. This parameter has the same properties as
#' tau_rh. During sleep the build up of the hunger feeling is slower, therefore the default value is higher for this parameter in
#' contrast to tau_rh.
#' A scalar or vector of length N (24*t_step*t_days) is required as input. The value of the time constant can be modified
#' during the simulation. The default value is 8.
#'
#' @param tau_c The period of the circadian rhythm in hours. One oscillation of the circadian process represents one day.
#' A scalar is required as input. The default value is 24.
#'
#' @param A_s The amplitude of the circadian process of sleep that  scales how much influence the circadian
#' rhythm has on sleep. When the amplitude is small the circadian rhythm is almost flat. Therefore, the rhythmic influence will be small.
#' In contrast to a large amplitude that causes the rhythmic process to be influential on the sleep process.  A scalar or vector of
#' length N (24*t_step*t_days) is required as input.
#' The default value is 0.12.
#'
#' @param A_h The amplitude of the circadian process of the food intake process that scales how much influence the circadian oscillation
#' has. A low amplitude results in a more flat circadian rhythm. Thus, the rhythmic influence of the circadian process will be small.
#' A high amplitude results in more influence of the circadian rhythm on the food intake process. A scalar
#' or vector of length N (24*t_step*t_days) is required as input.The default value is 0.14.
#'
#' @param t_0s The starting point of the circadian rhythm for the sleep process. This parameter executes a horizontal translation
#' of the oscillations. This translation determines the phase of the circadian rhythm. A scalar is required as input.The default
#' value is 9.8.
#'
#' @param t_0h The starting point of the circadian rhythm for the food intake process. This parameter executes a horizontal translation
#' of the oscillations. This translation determines the phase of the circadian rhythm. A scalar is required as input. The default value is
#'
#' @param H_s The upper boundary of the sleep process. The upper limit is added to the circadian rhythm resulting in the upper threshold.
#' The threshold influences the sleep onset.
#' A higher boudary  will approximately cause a later sleep onset. Lowering the boundary will cause an earlier  sleep onset. The effect
#' of a lower boundary can be compared with a person who is bored.
#' A scalar or vector of length N (24*t_step*t_days) is required as input.
#' The default value is 0.67.
#'
#' @param L_s  The lower boundary of the sleep process. The lower limit is added to the circadian rhythm resulting in the lower
#' threshold. The threshold influences the wake onset. An increased lower boundary results in an earlier wake onset. In contrast
#' to a decrease of the lower boundary that results in a later wake onset. A sudden increase of this boundary is compared with
#' an alarm going off.
#' A scalar or vector of length N (24*t_step*t_days) is required as input.
#' The default value is 0.17.
#'
#' @param H_h The upper boundary of the food intake process. The upper limit is added to the circadian rhythm resulting in the upper
#' threshold. The threshold influences the moment of food intake.
#' A higher boudary  will approximately cause food intake at a later moment. Lowering the boundary will cause food intake to be earlier.
#' A scalar or vector of length N (24*t_step*t_days) is required as input. The default value is 0.65.
#'
#' @param L_h  The lower boundary of the food intake process. The lower limit is added to the circadian rhythm resulting in the lower
#' threshold. The threshold influences the moment of satisfaction that results in stopping with eating.
#' An increased lower boundary results quicker in a satisfaction. In contrast to a decrease of the lower boundary that results
#' in a longer period of food intake because of a later moment of satisfaction.
#' A scalar or vector of length N (24*t_step*t_days) is required as input. The default value is 0.17.
#'
#' @param mu_a The mean of the distribution where from the activity intensity is drawn. The default value is
#'
#' @param sigma_a The standard deviation of the distribution where from the activity intensity is drawn. The default value is
#'
#' @param a_thres The threshold that the activity intensity has to reach to be executed. The default value is
#'
#' @param A_rest The activity intensity in rest. The default value is
#'
#' @param mu_a_night The mean of the distribution where from the activity intensity is drawn at night. The default value is
#' so it might could be that at night you try to sleep, but cannot sleep and then you are less active than during the day.
#' The default value is
#'
#' @param sigma_a_night The standard deviation of the distribution where from the activity intensity is drawn at night.
#' The default value is
#'
#' @param a_thres_night The activity threshold at night. The default value is
#'
#' @param A_night The activity intensity at night. The default value is
#'
#' @param mu_a_work The mean of the distribution where from the activity intensity is drawn on a working day. Assuming that after
#' a day of work the activity could be different than the activity when Bob has a day off. The default value is
#'
#' @param sigma_a_work The standard deviation of the distribution where from the activity intensity is drawn on a work day.
#' The default value is
#'
#' @param A_work The activity intensity during working hours. The default value is
#'
#' @param standard_sleep The amount of sleep in hours that is standard or necessary. The default value is 7.88 hours.
#'
#' @param c_F The autoregressive constant of fatigue. The default value is
#'
#' @param c_FS The constant that determines the influence of sleep on fatigue. The default value is
#'
#' @param c_taudF the constant that determines the influence of fatigue on tau_d. The default value is
#'
#' @param c_taurF The constant that determines the influence of fatigue on tau_r. The default value is
#'
#' @param c_muF The constant that determines the influence of fatigue on mu_a. When fatigue is above higher than 5 then mu_a is lowered
#' by fatigue times this constant. The default value of this constant is 1/60. The default value is
#'
#' @param c_app The autoregressive coefficient of appetite. The default value is 0.99. The default value is
#'
#' @param c_appH The constant that determines the influence of the homeostatic hunger feeling on the appetite. The default value is
#'
#' @param c_apploi The constant that determines the influence of the loss of interest on the appetite. The default value is
#'
#' @param c_appse The constant that determines the influence of the social eating on the appetite. The default value is
#'
#' @param c_taurapp_neg The constant that determines the influence of a negative appetite on the tau_rh (how fast a hunger
#' feeling grows). The default value is
#'
#' @param c_taurapp_pos The constant that determines the influence of a positive appetite on the tau_rh (how fast a hunger
#' feeling grows). The default value is
#'
#' @param social_eating Increases the appetite because of an external/social reason. For example, breakfast and lunch are regulated
#' by social eating if work and social_eat_work are both true. The default value is
#'
#' @param loss_int Loss of interest determines if you feel like eating and activity. The default value is
#' Loss of interest can only be positive and is also influenced by the mood. A scalar as basic value can be given or vector
#' of the length of the time series (t_step*24*t_days). The default value is
#'
#' @param home_base The constant that determines the default mood value. The home base determines where the attractor point of
#' the mood is. The default value is
#'
#' @param attrac_strength The strength of the attractor pulling on the mood. The default value is
#'
#' @param c_M The autoregressive coefficient of the mood. The default value is
#'
#' @param a1 The constant that determines the influence of activity on the mood. The default value is
#'
#' @param a2 The constant that determines the influence of fatigue on the mood. The default value is
#'
#' @param a3 The constant that determines the influence of appetite on the mood. The default value is
#'
#' @param a4 The constant that determines the influence of weight difference between body_weight and current weight on the mood.
#' The default value is
#'
#' @param a5 The constant that determines the influence of sleep on the mood.The default value is
#'
#' @param a6 The constant that determines the influence of the external misery on the mood. The default value is
#'
#' @param c_muM The constant that determines the influence of the mood on mu_a, the mean of the distribution where from the activity
#' intensity is drawn. The default value is
#'
#' @param c_muloi The constant that determines the influence of the loss of interest on mu_a. The mean of the distribution where from
#' the activity intensity is drawn. The default value is
#'
#' @param c_loiM The constant that determines the influence of the mood on the loss of interest. The default value is
#'
#' @param c_taurM The constant that determines the influence of the mood on tau_r, thus how the mood determines the sleeping
#' pattern. The default value is
#'
#' @param external_misery External misery is input from the external world and can represent positive and negative events.
#' A scalar as default value can be given or a vector of the same length as the time axis (length(t)->t_step*24*t_days).
#' The default value is
#'
#' @param social_eat_work If breakfast and lunch are stimulated by social eating or not when Bob has work. The default value is
#'
#' @param extended_output The output consists all the default values of all the different parameters. The default value is
#'
#' @param minimal_output The minimal output only returns the time series of the variables, but no parameter values.
#' The default value is
#'
#' @param plot_week Plots will be made that are insightful for time series around one week (7 days). The default value is
#'
#' @param plot_months Plots of time series longer than weeks. The plots are made of mean or total daily value of the different
#' variables. The default value is
#'
#' @param network_figures If TRUE this will return to pictures of the network. The first figure shows the simulated
#' network with all the edges and nodes. The second figure shows the network with all the parameters that are used
#' plotted in or on the node or connection. The default value is
#'
#' @return The output is divided in different lists, the content of the list depends on the chosen output.
#' @return \bold{\code{sleep}} \cr
#'        Included in the minimal output
#'        \item{sleep}{The homeostatic sleep curve with length N. The curve is measured at every time step.}
#'        \item{sleep_day}{The hours of sleep per day. This is measured from 12.00 to 12.00. }
#'        \item{C}{The circadian rhythm of sleep. }
#'        \item{H}{The upper boundary of sleep. }
#'        \item{L}{The lower boundary of sleep. }
#'        Included in the default output
#'        \item{A}{Amplitude of the circadian rhythm of sleep. }
#'        \item{t_0}{The time shift of the circadian rhythm of sleep. }
#'        \item{tau_d}{The time constant of the decaying homeostatic curve.  }
#'        \item{tau_r}{The time constant of the rising homeostatic curve.  }
#'        Included in the extended output
#'        \item{c_taudF}{The constant that determines how fatigue is changing the tau_r of the sleep process. }
#'        \item{c_taurF}{The constant that determines how fatigue is changing the tau_d of the sleep process. }
#'        \item{c_taurM}{The constant that determines how the mood is changing the tau_r of the sleep process. }
#'
#'
#'
#' @return \bold{\code{eat}} \cr
#'        Included in the minimal output
#'        \item{hunger}{The homeostatic hunger curve with length N. The curve is measured at every time step.}
#'        \item{eating_day}{The minutes per day spend on eating. This is measured from 12.00 to 12.00. }
#'        \item{C}{The circadian rhythm of eating. }
#'        \item{H}{The upper boundary of eating. }
#'        \item{L}{The lower boundary of eating. }
#'        Included in the default output
#'        \item{A}{Amplitude of the circadian rhythm of eating. }
#'        \item{t_0}{The time shift of the circadian rhythm of eating. }
#'        \item{tau_d}{The time constant of the decaying homeostatic curve.  }
#'        \item{tau_r}{The time constant of the rising homeostatic curve.  }
#'        \item{tau_rs}{The time constant of the rising homeostatic curve while Bob is sleeping.  }
#'        Included in the extended output
#'        \item{c_taurapp_pos}{The constant that determines how positive appetite influences tau_r,
#'        such that the eating pattern will be changed.}
#'        \item{c_taurapp_neg}{The constant that determines how negative appetite influences tau_r,
#'        such that the eating pattern will be changed. }
#'
#' @return \bold{\code{activity}} \cr
#'        Included in the minimal output
#'        \item{activity}{The time series of the activity of length N. The values are the intensity of the activity at the particular time point. }
#'        \item{activity_day}{The intensity*minutes active. This is measured from 12.00 to 12.00. }
#'        Included in the default output
#'        \item{mu_a}{The mean of the distribution where from the activity intensity is drawn. This is vector of length N.}
#'        \item{sigma_a}{The mean of the distribution where from the activity intensity is drawn. This is vector of length N. }
#'        Included in the extended output
#'        \item{c_muF}{The constant that determines how fatigue is changing mu_a. }
#'        \item{c_muM}{The constant that determines how mood is changing mu_a. }
#'        \item{c_muloi}{The constant that determines how the loss of interest is changing mu_a. }
#'
#' @return \bold{\code{body_weight}} \cr
#'        Included in the minimal output
#'        \item{weight}{The body weight measured per day. The measured time of the day is 12.00.}
#'        Included in the default output
#'        \item{c_ee_work}{The constant that determines the contribution of activity to body weight when Bob has work.
#'        This constant should be the inverse of the mean of activity_day for a stable body weight.    }
#'        \item{c_ee_nowork}{The constant that determines the contribution of activity to body weight when Bob is unemployed.
#'        This constant should be the inverse of the mean of activity_day for a stable body weight.    }
#'        \item{c_ei_work}{The constant that determines the contribution of food intake to body weight when Bob has work.
#'        This constant should be the inverse of the mean of eating_day for a stable body weight.    }
#'        \item{c_ei_nowork}{The constant that determines the contribution of food intake to body weight when Bob is unemployed.
#'        This constant should be the inverse of the mean of eating_day for a stable body weight.    }
#'        Included in the extended output
#'        \item{c_BW}{The constant that determines the influence of the difference between energy intake and expenditure.
#'        This constant is the inverse of the energy densit.y }
#'
#' @return \bold{\code{appetite}} \cr
#'        Included in the minimal output
#'        \item{appetite}{The appetite time series of length N. The curve is measured at every time step.}
#'        Included in the extended output
#'        \item{c_app}{The autoregressive coefficient of appetite. }
#'        \item{c_appH}{The constant that determines the influence of the homeostatic hunger feeling on appetite. }
#'        \item{c_apploi}{The constant that determines the influence of loss of interest on appetite. }
#'        \item{c_appse}{The constant that determines the influence of social eating on appetite. }
#'
#' @return \bold{\code{fatigue}} \cr
#'        Included in the minimal output
#'        \item{fatigue}{The fatigue time series of length N.}
#'        Included in the extended output
#'        \item{c_F}{The autoregressive coefficient of fatigue. }
#'        \item{c_FS}{The influence of missing sleep on fatigue.  }
#'        \item{standard_sleep}{The standard amount of sleep necessary. }
#'
#' @return \bold{\code{loss_of_interest}} \cr
#'        Included in the minimal output
#'        \item{loss_int}{The time series of the loss of interest of length N.}
#'        Included in the extended output
#'        \item{c_loiM}{The constant that determines the influence of mood on the loss of interest.}
#'
#' @return \bold{\code{mood}} \cr
#'        Included in the minimal output
#'        \item{mood}{The time series of the mood of length N.}
#'        \item{mood_day}{The daily mean of the mood. This is vector of length t_days.}
#'        Included in the default output
#'        \item{home_base}{The home base of the mood is where the location of the attractor point.}
#'        \item{attrac_strength}{The strength that the mood is pulled to the attractor point.}
#'        Included in the extended output
#'        \item{a1}{The constant that determines the influence of activity on the mood.}
#'        \item{a2}{The constant that determines the influence of fatigue on the mood.}
#'        \item{a3}{The constant that determines the influence of appetite on the mood.}
#'        \item{a4}{The constant that determines the influence of weight change on the mood.}
#'        \item{a5}{The constant that determines the influence of sleep on the mood.}
#'        \item{a6}{The constant that determines the influence of external misery on the mood.}
#'        \item{c_M}{The autoregressive coefficient of the mood. }
#'
#' @return \bold{\code{external_misery}} \cr
#'        Included in the default output
#'        \item{external}{The external misery time series of length N. }
#'
#' @return \bold{\code{depression}} \cr
#'        Included in the minimal output
#'        \item{depression}{A time series that represents the number of symptoms of depression per day.}
#'        Included in the default output
#'        \item{symptoms_day}{Matrix of the measured symptoms by the number of simulated days. A 1 means
#'        that the symptom is present and a 0 means that the symptom is absent that day.}
#'        \item{symptoms_14days}{Matrix of the measured symptoms by the number of simulated days. This
#'        matrix returns a 1 when the symptom is present for 14 days or more. }
#'
#' @return \bold{\code{other}} \cr
#'        Included in the minimal output
#'        \item{t}{The time series of the simulation. This is the time per t_step.}
#'        \item{t_days}{Number of simulated days.}
#'        \item{N}{Number of time points of the time series t. This is the length of the time series. }
#'        Included in the default output
#'        \item{t_step}{The time step that is used to simulate. }
#'        \item{tau_c}{The time constant of the circadian rhythm. This is by default 24, because there are 24 hours in one day. }
#'
#' @export
#' @seealso \code{\link{qgraph}}
#' @examples \dontrun{
#'
#' ##Show the network
#' bob<-simBob(1,network_figures = TRUE)
#'
#' #Simulate one week of Bob his life and return the plots appropiate for
#' #one simulation week and extended output.
#' bob<-simBob(7,plot_week=TRUE,extended_output=TRUE)
#'
#' #Simulate again one week but now Bob is unemployed.
#' bob<-simBob(7,work=FALSE,plot_week=TRUE,extended_output=TRUE)
#'
#' #Simulate a whole year of Bob his life and return the appropriate plots.
#' #To save memory only the minimal output will be returned with these options.
#' bob<-simBob(365,plot_months=TRUE,minimal_output=TRUE)
#'
#' #The mean mood over the whole simulation is:
#' mean_mood<-mean(bob$mood$mood)
#'
#' #An example with external misery as input
#'
#' t_days<-100
#' t_step<-1/60; #hours, so time steps of a minute
#' t_end<-24*t_days; #convert end time in hours
#' t<-seq(from=0,to=(t_end-t_step),by=t_step); #make time vector
#'
#' #Create a Gaussian for the external input
#' mean_mis=24*30; sigma_mis=24*10; #The mean and standard deviation times 24 so it is in days
#' external_misery<- 2*(2*sqrt(2*pi)*sigma_mis*dnorm(t, mean=mean_mis,sd=sigma_mis))
#'
#' #Run the simulation with the external misery input and
#' #return plots for simulations longer than a week
#' bob<-simBob(t_days=t_days,external_misery = external_misery,plot_months = TRUE)
#'
#' # Rename the depression variables
#' depression<-bob$depression$depression
#' depressed<-bob$depression$symptoms_14days
#' depression_symp<-bob$depression$symptoms_day
#'
#' day_vec<-bob$other$t_day
#' lwd<-2
#'
#' plot(day_vec,colSums(depression_symp),bty='l',
#'      col='black',
#'      type='l',
#'      lwd=lwd,
#'      xlab='day',
#'      ylab='number of symptoms' ,
#'      las=1)
#'
#' plot(day_vec,depression,bty='l',
#'      col='black',
#'      type='l',
#'      lwd=lwd,
#'      xlab='day',
#'      ylab='Number of symptoms more than 2 weeks' ,
#'      las=1)
#'
#'}

simBob<-function(t_days,
                  t_step=1/60,
                  work=TRUE,
                  body_weight=70,
                  c_BW=0.3,
                  c_ee_work=1/205.5,
                  c_ee_nowork=1/163,
                  c_ei_work=1/85.75,
                  c_ei_nowork=1/75,
                  tau_d=4.2,
                  tau_r=18.2,
                  tau_dh=0.25,
                  tau_rh=3.7,
                  tau_rhs=8,
                  tau_c=24,
                  A_s=0.12,
                  A_h=0.14,
                  t_0s=9.8,
                  t_0h=7.5,
                  H_s=0.67,
                  L_s=0.17,
                  H_h=0.65,
                  L_h=0.17,
                  mu_a=0,
                  sigma_a=0.3,
                  a_thres=0.1,
                  A_rest=0.1,
                  mu_a_night=0,
                  sigma_a_night=0,
                  a_thres_night=0.1,
                  A_night=0.1,
                  mu_a_work=-0.3,
                  sigma_a_work=0.4,
                  A_work=0.35,
                  standard_sleep=7.88,
                  c_F=0.9,
                  c_FS=1,
                  c_taudF=4.5,
                  c_taurF=0.25,
                  c_muF=1/60,
                  c_app=0.99,
                  c_appH=1/60,
                  c_apploi=1/60,
                  c_appse=1,
                  c_taurapp_neg= -1.73,
                  c_taurapp_pos=0.72,
                  social_eating=0,
                  loss_int=0,
                  home_base=0.1,
                  attrac_strength=0.35,
                  c_M=0.99,
                  a1=2,
                  a2=0.5,
                  a3=0.01,
                  a4=0.01,
                  a5=0.1,
                  a6=1,
                  c_muM=0.06,
                  c_muloi=0.06,
                  c_loiM=2.5,
                  c_taurM=1,
                  external_misery=0,
                  social_eat_work=TRUE,
                  extended_output=FALSE,
                  minimal_output=FALSE,
                  plot_week=FALSE,
                  plot_months=FALSE,
                  network_figures=FALSE
){

  ### Check the input
  parameter_check(t_days,t_step,work,body_weight,c_BW,c_ee_work,c_ee_nowork,c_ei_work,c_ei_nowork,tau_d,
                  tau_r,tau_dh,tau_rh,tau_rhs,tau_c,A_s,A_h,t_0s,t_0h,H_s,L_s,H_h,L_h,mu_a,sigma_a,a_thres,
                  A_rest,mu_a_night,sigma_a_night,a_thres_night,	A_night,mu_a_work,sigma_a_work,A_work,
                  standard_sleep,c_F,c_FS,c_taudF,c_taurF,c_muF,c_app,c_appH,c_apploi,c_appse,c_taurapp_neg,
                  c_taurapp_pos,social_eating,loss_int,home_base,attrac_strength,c_M,a1,a2,a3,a4,a5,a6,c_muM,
                  c_muloi,c_loiM,c_taurM,external_misery,social_eat_work,extended_output,minimal_output,
                  plot_week,plot_months,network_figures)




  ###Show network
  if (network_figures){
    show_network()
  }

  ### Define time scales and vector lengths

  #day<-t_days
  t_end<-t_days*24; #convert end time in hours
  t<-seq(from=0,to=(t_end-t_step),by=t_step); #make time vector
  N<-length(t);#length of the time vector, should also be the length of the others
  N_day<-round(24/t_step); #time points per day
  day_vec<-seq(from=1, to=t_days,by=1); #vector of the days (needed for plotting)

  ###parameters

  #circadian processes (C)
  omega<-2*pi/tau_c;

  #sleep
  if (length(A_s)==1){
    A_s<-rep(A_s,N);
  }
  #hunger
  if (length(A_h)==1){
    A_h<-rep(A_h,N);
  }

  #weight
  weight<-rep(body_weight,(t_days-1)); #the weight vector, so can store the weight over time
  #c_BW<-0.3; #this constant is determines the influence of the food intake minus activity
  #according to the literature dependent on the amount of fat and the lean
  #tissue

  #sleep
  if (length(tau_d)==1){
    tau_d<-rep(tau_d,N); #hours the time constant determining when to wake up again
  }
  if (length(tau_r)==1){
    tau_r<-rep(tau_r,N); #hours the time constant determining when to go to sleep
  }
  if (length(H_s)==1){
    H_s<-rep(H_s,N);
  }
  if (length(L_s)==1){
    L_s<-rep(L_s,N);
  }
  sleep_wake<-rep(0,N); #vector to save if awake or asleep
  sleep_day<-rep(0,t_days-1); #vector to save how much sleep a day

  #hunger
  if (length(tau_dh)==1){
    tau_dh<-rep(tau_dh,N); #hours the time constant for how long to eat (one meal or snack or something)
  }
  if (length(tau_rh)==1){
    tau_rh<-rep(tau_rh,N); #hours the time constant for getting hungry again (awake) when no work, for work adjusted lower in the code
  }
  if (length(tau_rhs)==1){
    tau_rhs<-rep(tau_rhs,N); #hours the time constant for getting hungry again (awake) when no work, for work adjusted lower in the code
  }
  if (length(H_h)==1){
    H_h<-rep(H_h,N);
  }
  if (length(L_h)==1){
    L_h<-rep(L_h,N);
  }
  eating<-rep(0,N); #vector to save when eating
  eat_day<-rep(0,t_days-1); #vector to save how much minutes eating for a each day

  #activity
  if (length(mu_a)==1){
    mu_a<-rep(mu_a,N); #activity during day time
  }
  if (length(sigma_a)==1){
    sigma_a<-rep(sigma_a,N); #activity during day time
  }
  if (length(mu_a_night)==1){
    mu_a_night<-rep(mu_a_night,N); #activity during night time, assume when people
    #can't sleep they're as active as normal
  }
  if (length(sigma_a_night)==1){
    sigma_a_night<-rep(sigma_a_night,N);#activity during night time, assume when people
    #can't sleep they're as active as normal
  }

  A<-rep(A_rest,N); #activity vector, when awake standard at 0.1
  activ_day<-rep(0,t_days-1); #vector to store activity of each day


  #fatigue
  fatigue<-rep(0,t_days-1); # save per day how fatigue


  #appetite
  if (length(social_eating)==1){
    social_eating<-rep(social_eating,N);  #set between 0 and 1
  }
  appetite<-rep(0,N);
  mean_app<-rep(0,t_days-1);

  #loss of interest
  if (length(loss_int)==1){
    loss_int<-rep(loss_int,N); #set between 0  and 10
  }
  mean_loi<-rep(0,t_days-1);

  #mood
  beta1<-t_step*attrac_strength;#*(1/(60*t_step));
  mood<-rep(home_base,N);
  mean_mood<-rep(0,t_days-1);
  theta<-rep(0,N);
  d_theta<-rep(0,N);
  wiener<-rep(0,N); #TO DO Wiener on or off button #wiener<-rnorm(N,0,0.1); # Noisy input of the wiener process

  #external misery
  if (length(external_misery)==1){
    external_misery<-rep(external_misery,N);
  }

  ###initial values and vectors and implement work values

  #sleep
  C_s<-A_s*(0.97*sin(omega*(t-t_0s)) + 0.22*sin(2*omega*(t-t_0s))+0.007*sin(3*omega*(t-t_0s))
            + 0.03*sin(4*omega*(t-t_0s))+0.001*sin(5*omega*(t-t_0s))); #circadian process for sleep

  S<-rep(0,N); #homeostatic sleep process, sleep propensity
  S[1]<-C_s[1]+H_s[1]; #initializing time series, so you start sleeping

  #hunger
  C_h<-A_h*(0.97*sin(omega*(t-t_0h)) + 0.22*sin(2*omega*(t-t_0h))+0.007*sin(3*omega*(t-t_0h))
            + 0.03*sin(4*omega*(t-t_0h))+0.001*sin(5*omega*(t-t_0h))); #circadian process for hunger

  H<-rep(0,N); #initiate hunger vector
  H[1]<-C_s[1]+L_h[1]; #fill in the first element, such that there is no hunger feeling, full belly

  week_day<-numeric(); #make a vector of all the days of the week
  weekend<-numeric(); #make a vector of the days of the weekend
  for (i in 1:t_days){
    if (i%%7!=6 && i%%7!=0)	{
      week_day<-c(week_day,day_vec[i]);
    } else {
      weekend<-c(weekend,day_vec[i]);
    }
  }

  work_time<-rep(0,N);

  #TO DO people should be able to change this as well. Determines the time indices independent on time step
  half_day<-12;
  half_day_index<-which(t==half_day);
  end_night<-8;
  end_night_index<-which(t==end_night);

  if (work){
    start_work<-9;
    start_work_index<-which(t==start_work);
    start_lunch<-12.5;
    start_lunch_index<-which(t==start_lunch);
    end_lunch<-13;
    end_lunch_index<-which(t==end_lunch);
    end_work<-17;
    end_work_index<-which(t==end_work);
    min_work_day<-start_lunch_index-start_work_index+ end_work_index-end_lunch_index;
    min_work_A<-min_work_day*A_work+ (24-standard_sleep-min_work_day*t_step)*(1/t_step)*A_rest;
    for (i in week_day){
      A[(start_work_index+N_day*(i-1)):(start_lunch_index+N_day*(i-1)-1)]<-A_work; #work before lunch from 9 to 12.30
      A[(end_lunch_index+N_day*(i-1)):(end_work_index+N_day*(i-1)-1)]<-A_work; #work after lunch from 13 to 17
      work_time[(start_work_index+N_day*(i-1)):(start_lunch_index+N_day*(i-1)-1)]<-1; #work is turned on at this time points
      work_time[(end_lunch_index+N_day*(i-1)):(end_work_index+N_day*(i-1)-1)]<-1;
      mu_a[(1+N_day*(i-1)):(N_day*(i))]<- mu_a_work;
      sigma_a[(1+N_day*(i-1)):(N_day*(i))]<-sigma_a_work;
      if (social_eat_work){
        social_eating[(start_work_index-round(N_day/48)+N_day*(i-1)):(start_work_index-round(N_day/48)+N_day*(i-1)+round(N_day/(24*10)))]<-1; #social eat, half hour before you go to work, you should eat breakfast
        social_eating[(start_lunch_index+N_day*(i-1)-round(N_day/96)):(start_lunch_index+N_day*(i-1)+round(N_day/(24*20))-round(N_day/96))]<-1;
      }
    }
  }


  min_rest_A<-(24-standard_sleep)*(1/t_step)*A_rest;

  #Depression
  num_symp<-6;
  depression_symp<-matrix(rep(0,t_days*num_symp),nrow=num_symp);
  depressed<-matrix(rep(0,t_days*num_symp),nrow=num_symp);
  depression<-rep(0,t_days);

  ### support variable for the loop so the events
  sleep<-1;#so when the starting point is not above the upper bound, the simulation starts awake
  eat<-0;#so when the starting point is not above the upper bound the simulation starts not eating
  j<-1; #counting the days, start at 1
  k<-1; #counting the days, start at 1
  fatigue_lowering<-0; #counting vector

  ###loop over time
  for (i in 1:(N-1)){
    d<-exp(-t_step/tau_d[i]); #during sleep decay factor of S
    r<-exp(-t_step/tau_r[i]); # wake rising factor of S
    dh<-exp(-t_step/tau_dh[i]); #during eating decay factor of H, the honger feeling
    rh<-exp(t_step/tau_rh[i]); # No food, rising factor of H, get more hungry (awake)
    rh_s<-exp(t_step/tau_rhs[i]); # No food, rising factor of H, get more hungry (sleep)

    done_eating<-FALSE; #turn done eating every loop off

    if (S[i]>H_s[i]+C_s[i] && !work_time[i]){ #sleeping only when not working
      S[(i+1)]<-d*S[i]; #sleeping
      sleep<-1; #sleeping
      sleep_wake[i]<-TRUE; #sleeping
      #no eating during sleeping
      H[(i+1)]<-rh_s*H[i];#not eating while sleeping
      eat<-0; #not eating
      eating[i]<-FALSE; #not eating
      A[i]<-0; #no activity
      social_eating[i]<-0;#when you are sleeping there are no social cue that you have to eat
    }else if (S[i]<L_s[i]+C_s[i]){ #awake
      S[(i+1)]<-1-r*(1-S[i]); #awake
      sleep<-0; #awake
      sleep_wake[i]<-FALSE; #not sleeping
      #while you're awake it is possible to eat
      if (H[i]>H_h[i]+C_h[i] && !work_time[i]){ #eating only when not working
        H[(i+1)]<-dh*H[i]; #eating
        eat<-1;#eating
        eating[i]<-TRUE; #eating
      }else if (H[i]<L_h[i]+C_h[i]){ #not eating, possibly working
        H[(i+1)]<-rh*H[i];#not eating while awake
        eat<-0; #not eating
        eating[i]<-FALSE; #not eating
        done_eating<-TRUE; #This is when you reached the threshold and stop eating
        if (i%%round(N_day/96)==0 && !work_time[i]){ #if not eating nor sleeping nor working an activity can be done each 15 min for 15 min
          if  (t[i]<t[(1+N_day*(j-1))] || t[i]> t[(end_night_index-1+N_day*(j-1))]){#when it's not night
            activity<-rnorm(1,mu_a[i],sigma_a[i]); #draw random number for intensity activity
            if (activity>a_thres){ #random number has to exceed threshold
              A[(i+1)]<-activity; #activity intensity accepted
            }
          }else{ #when it is night different parameter can be used
            activity<-rnorm(1,mu_a_night[i],sigma_a_night[i]);#draw random number for intensity activity
            if (activity>a_thres_night){ #random number has to exceed threshold
              A[(i+1)]<-activity; #activity intensity accepted
            }else{
              A[(i+1)]<-A_night; #random number not accepted, use standard night activity
            }
          }

        }else if  (!work_time[i]){ #if not working continue spontaneous activity
          A[(i+1)]<-A[i]; #if not moment of picking a new activity, continue what you were doing
        }
      }else{ #no threshold is reached so continue what you did
        if (eat &&  !work_time[i]){ #if eating and not working continue eating
          H[(i+1)]<-dh*H[i];#eating
          eating[i]<-TRUE;#eating
        } else{
          H[(i+1)]<-rh*H[i]; #not eating
          eating[i]<-FALSE; #not eating
          if ( i%%round(N_day/96)==0 && !work_time[i]){ #if not eating nor sleeping nor working an activity can be done each 15 min for 15 min
            if  (t[i]<t[(1+N_day*(j-1))] || t[i]> t[(end_night_index-1+N_day*(j-1))]){ #when it's not night
              activity<-rnorm(1,mu_a[i],sigma_a[i]); #draw random number for intensity activity
              if (activity>a_thres){ #random number has to exceed threshold
                A[(i+1)]<-activity; #activity intensity accepted
              }
            } else { #when it is night different parameter can be used
              activity<-rnorm(1,mu_a_night[i],sigma_a_night[i]); #draw random number for intensity activity
              if (activity>a_thres_night){ #random number has to exceed threshold
                A[(i+1)]<-activity; #activity intensity accepted
              } else{
                A[(i+1)]<-A_night; #random number not accepted, use standard night activit
              }
            }
          }else if  (!work_time[i]){ #if not working continue spontaneous activity
            A[(i+1)]<-A[i]; #if not moment of picking a new activity, continue what you were doing
          }
        }
      }

    }else{ #no threshold is reached so continue what you did
      if (sleep && !work_time[i]){ #sleeping and not working
        S[(i+1)]<-d*S[i]; #sleeping
        sleep_wake[i]<-TRUE; #sleeping
        #no eating during sleeping
        H[(i+1)]<-rh_s*H[i];#not eating
        eat<-0; #not eating
        eating[i]<-FALSE; #not eating
        A[i]<-0; #no activity
        social_eating[i]<-0;#when you are sleeping there are no social cue that you have to eat
      }else{
        S[(i+1)]<-1-r*(1-S[i]); #awake
        sleep_wake[i]<-FALSE; #not sleeping
        #while awake it's possible to eat
        if (H[i]>H_h[i]+C_h[i] && !work_time[i]){ #eating if not working
          H[(i+1)]<-dh*H[i]; #eating
          eat<-1; #eating
          eating[i]<-TRUE; #eating
        }else if (H[i]<L_h[i]+C_h[i]){ #not eating
          H[(i+1)]<-rh*H[i];#not eating
          eat<-0; #not eating
          eating[i]<-FALSE; #not eating
          done_eating<-TRUE; #This is when you reached the threshold and stop eating
          if (i%%round(N_day/96)==0 &&  !work_time[i]){ #if not eating nor sleeping nor working an activity can be done each 15 min for 15 min
            if  (t[i]<t[(1+N_day*(j-1))] || t[i]> t[(end_night_index-1+N_day*(j-1))] ){ #when it's not night
              activity<-rnorm(1,mu_a[i],sigma_a[i]); #draw random number for intensity activity
              if (activity>a_thres) { #random number has to exceed threshold
                A[(i+1)]<-activity; #activity intensity accepted
              }
            }else {#when it is night different parameter can be used
              activity<-rnorm(1,mu_a_night[i],sigma_a_night[i]); #draw random number for intensity activity
              if (activity>a_thres_night){ #random number has to exceed threshold
                A[(i+1)]<-activity; #activity intensity accepted
              }else{
                A[(i+1)]<-A_night; #random number not accepted, use standard night activity
              }
            }
          }else if(!work_time[i]) {#if not working, continue spontaneous activity
            A[(i+1)]<-A[i];
          }
        }else{ #no threshold is reached so continue what you did
          if (eat  && !work_time[i]) {#not working
            H[(i+1)]<-dh*H[i];#eating
            eating[i]<-TRUE;
          }else{
            H[(i+1)]<-rh*H[i]; #not eating
            eating[i]<-FALSE;
            if ( i%%round(N_day/96)==0  && !work_time[i]){#if not eating nor sleeping nor working an activity can be done each 15 min for 15 min
              if  (t[i]<t[(1+N_day*(j-1))] || t[i]> t[(end_night_index-1+N_day*(j-1))]) {#when it's not night
                activity<-rnorm(1,mu_a[i],sigma_a[i]); #draw random number for intensity activity
                if (activity>a_thres) {#random number has to exceed threshold
                  A[(i+1)]<-activity; #activity intensity accepted
                }
              }else {#when it is night different parameter can be used
                activity<-rnorm(1,mu_a_night[i],sigma_a_night[i]); #draw random number for intensity activity
                if (activity>a_thres_night) {#random number has to exceed threshold
                  A[(i+1)]<-activity; #activity intensity accepted
                }else{
                  A[(i+1)]<-A_night; #random number not accepted, use standard night activity
                }
              }
            }else if  (!work_time[i]) {#if not working continue spontaneous activity
              A[(i+1)]<-A[i]; #if not moment of picking a new activity, continue what you were doing
            }
          }
        }
      }
    }
    H[(i+1)]=min(20,H[(i+1)]);
    S[(i+1)]=min(20,S[(i+1)]);

    appetite[(i+1)]<-c_app*appetite[i]+c_appH*H[i]+social_eating[i]-c_apploi*loss_int[i]; #feeling of appetite, is hunger feeling and other facotrs (scoial stuff, loss of interest)

    if (done_eating){# when just eaten, the appetite is 0
      appetite[(i+1)]<-0;
    }
    if (appetite[(i+1)]>2) {#when the appetite is above 2 (the normal daily hunger feeling) then the hunger feeling will rise faster
      tau_rh[(i+1)]<-max(tau_rh[1]-c_taurapp_pos*appetite[(i+1)],0.05);#change parameter of how fast you get hungry (hours)
      #0.72 is the slope of
      #the curve when I want
      #to be able to lower
      #3.6 in 5 min
    }
    if (appetite[(i+1)]< (-0.1)) {#when appetite is negative, the hunger feeling will rise slower
      tau_rh[(i+1)]<-tau_rh[1]+c_taurapp_neg*appetite[(i+1)];#tau_rh[i+1]-0.05*appetite[i+1];
      tau_rhs[(i+1)]<-max(tau_rhs[1],tau_rh[1]+c_taurapp_neg*appetite[(i+1)]);#tau_rh[i+1]-0.05*appetite[i+1];
      #2/3 is to scale
      #the appetite, 2.6 is the desired slope
    }

    if (j>1){
      theta[i]<-a1*A[i]-a2*fatigue[(j-1)]+-a3*appetite[i]+-a4*(weight[(j-1)]-body_weight)+a5*S[i]-a6*external_misery[i];
    } else{
      theta[i]<-a1*A[i]+-a3*appetite[i]+a5*S[i]-a6*external_misery[i];
    }
    d_theta[i]=-beta1*(home_base-theta[i]);
    mood[(i+1)]=c_M*mood[i]+d_theta[i]+wiener[i]; # mood(i)+t_step*attrac_strength*theta(i);%(-fatigue(j)+A(i)+-appetite(i)+-(weight(j)-body_weight)+external_misery(i));

    if (i==(half_day_index+N_day*k)){ #save the data per day from noon to noon
      sleep_day[k]<-sum(sleep_wake[(half_day_index+(N_day*(k-1))):(half_day_index+N_day*(k)-1)])*t_step; #sum minutes of sleep of the whole day
      #calculate fatigue
      if (k==1){#first day, can't take the previous value into account
        fatigue[k]<-c_FS*(standard_sleep-sleep_day[k]); #hours
      }else{
        fatigue[k]<-c_F*fatigue[(k-1)]+c_FS*(standard_sleep-sleep_day[k]); #hours, 90# of the fatigue feeling of before is taken with you everyday
      }
      if ((k>9 && mean(sleep_day[(k-2):k])>=9) && fatigue[k]>0){#if slept well for 3 nights, than fatigue is set to 0, you're not fatigue anymore (fatigue(k-2)>=20 || sum(fatigue(k-8:k-2)>10)==7)
        fatigue[k]<-0;
      }
      if (fatigue[k]>=20 && fatigue_lowering<1 && k<(t_days-3)){
        tau_d[i:(i+N_day*3)]<-tau_d[i:(i+N_day*3)]+c_taudF;#normally 4.2
        fatigue_lowering<-3;
      }else{
        fatigue_lowering<-fatigue_lowering-1;
      }
      #       fatigue_lowering
      #intervention, when missed too much sleep, at some point you have
      #to sleep longer and will wake up later
      if (k>7 && sum(fatigue[(k-6):k]>8)==7 && fatigue_lowering<1 && k<(t_days-1)){
        #H_s[i:(i+1440-1)]<-H_s[i:(i+1440-1)]-0.3;
        tau_d[i:(i+N_day)]<-tau_d[i:(i+N_day)]+c_taudF; #normally 4.2, wake up later
      }else if (fatigue[k]>10 && fatigue_lowering<1 && k<t_days-1){
        tau_d[i:(i+N_day)]<-tau_d[i:(i+N_day)]+c_taudF; #normally 4.2, wake up later
      }else if (k>1 && fatigue[k]-fatigue[(k-1)]>7 && fatigue_lowering<1 && k<(t_days-1)){
        tau_d[i:(i+N_day)]<-tau_d[i:(i+N_day)]+c_taudF; #normally 4.2, wake up later
      }
      #small amout of sleep lost, so you go to bed a little earlier.
      if (fatigue[k]>2 && k<(t_days-1)) {#lost more than 2 hours of sleep
        tau_r[i:(i+N_day)]<-tau_r[i:(i+N_day)]-c_taurF*fatigue[k]; #normally 18.2, go to bed earlier
      }
      #being fatigue is decreasing the activity
      if (fatigue[k]>5 && k<(t_days-1)) {#lost more than 5 hours of sleep
        mu_a[i:(i+N_day)]<-mu_a[i:(i+N_day)]-c_muF*fatigue[k]; #normally 0, decrease the activity because when fatigue everything goes a little slower.
      }

    #}

    #if (i==(N_day*j)) {#save the data per day from midnight to midnight
      eat_day[j]<-sum(eating[(1+(N_day*(j-1))):(N_day*j)])*(60*t_step); #sum minutes of eating of the whole day
      activ_day[j]<-sum(A[(1+(N_day*(j-1))):(N_day*j)])*(60*t_step); # sum minutes times intensity of the activity for the whole day
      mean_mood[j]<-mean(mood[(1+(N_day*(j-1))):(N_day*j)]);
      mean_loi[j]<-mean(loss_int[(1+(N_day*(j-1))):(N_day*j)]);
      mean_app[j]<-mean(appetite[(1+(N_day*(j-1))):(N_day*j)]);
      if (work) {#different average values with or without work for both eating and activity
        if (j!=1){
          weight[j]<-weight[(j-1)]+c_BW*((eat_day[j]*c_ei_work)-(activ_day[j]*c_ee_work)); #weight calculated per day
        }
      }else{
        if (j!=1){
          weight[j]<-weight[(j-1)]+c_BW*((eat_day[j]*c_ei_nowork)-(activ_day[j]*c_ee_nowork)); #weight calculated per day
        }
      }
      weight_check(weight[j])

      #Depression
      if (mean_mood[j]<(home_base-0.7)){
        depression_symp[1,j]=1;
      }
      if (mean_loi[j]>1){
        depression_symp[2,j]=1;
      }
      if (weight[j]< c_BW*0.95 | weight[j]> c_BW*1.05 | mean_app[j]>2 | mean_app[j]< -2){
        depression_symp[3,j]=1;
      }
      if (sleep_day[j]*t_step<0.9375*standard_sleep | sleep_day[j]*t_step>1.625*standard_sleep){
        depression_symp[4,j]=1;
      }
      if (work & j%in%week_day){
        if (activ_day[j]<1.05*min_work_A){
          depression_symp[5,j]=1;
        }
      }else{
        if (activ_day[j]<1.1*min_rest_A){
          depression_symp[5,j]=1;
        }
      }
      if (fatigue[k]>1.5){
        depression_symp[6,j]=1;
      }

      if (j>13){
        if (sum(rowSums(depression_symp[,((j-13):j)])>13) &  sum(depression_symp[1,((j-13):j)])>13) {
          depressed[,j]=rowSums(depression_symp[,((j-13):j)]);
          depression[j]=sum(rowSums(depression_symp[,((j-13):j)])>13)
        }
      }
      k<-k+1;
      j<-j+1;
    }

    tau_r[(i+1)]=max(tau_r[(i+1)],tau_r[(i+1)]-c_taurM*(mood[(i+1)]-home_base)); #hours the time constant determining when to go to sleep
    loss_int[(i+1)]=max(c(0,loss_int[i+1],-c_loiM*(mood[(i+1)]-home_base)));
    mu_a[(i+1)]=mu_a[(i+1)]-c_muloi*loss_int[(i+1)]; #mu_a decreases such that when max loss_int is 10 then mu_a is -0.6 (because then you're already doing nothing anymore)
    mu_a[(i+1)]=mu_a[(i+1)]+c_muM*(mood[(i+1)]-home_base); #mu_a is also directly dependent on mood, mood is 5 then mu_a changes by 0.
  }

  ###plots

  if(plot_week){
    plot_week(t,day_vec[1:(t_days-1)],S,(C_s+H_s),(C_s+L_s),sleep_wake,H,(C_h+H_h),(C_h+L_h),
              eating,A,weight,fatigue,appetite,mood,home_base,loss_int,external_misery)

  }

  if(plot_months){
    plot_month(day_vec[1:(t_days-1)],t,sleep_day,eat_day,activ_day,weight,fatigue,loss_int,mean_mood,external_misery)
  }



  bob<-list(sleep=list(),
            eat=list(),
            activity=list(),
            body_weight=list(),
            appetite=list(),
            fatigue=list(),
            loss_of_interest=list(),
            mood=list(),
            external_misery=list(),
            depression=list(),
            other=list()
  )

  ### fill in the lists
  #sleep
  bob$sleep$sleep<-S
  bob$sleep$sleep_day<-sleep_day
  bob$sleep$C<-C_s
  bob$sleep$H<-H_s
  bob$sleep$L<-L_s
  if(!minimal_output){
    bob$sleep$A<-A_s
    bob$sleep$t_0<-t_0s
    bob$sleep$tau_d<-tau_d
    bob$sleep$tau_r<-tau_r
    if (extended_output){
      bob$sleep$c_taudF<-c_taudF
      bob$sleep$c_taurF<-c_taurF
      bob$sleep$c_taurM<-c_taurM
    }
  }

  #eat
  bob$eat$hunger<-H
  bob$eat$eating_day<-eat_day
  bob$eat$C<-C_h
  bob$eat$H<-H_h
  bob$eat$L<-L_h
  if(!minimal_output){
    bob$eat$A<-A_h
    bob$eat$t_0<-t_0h
    bob$eat$tau_d<-tau_dh
    bob$eat$tau_r<-tau_rh
    bob$eat$tau_rs<-tau_rhs
    if (extended_output){
      bob$eat$c_taurapp_pos<-c_taurapp_pos
      bob$eat$c_taurapp_neg<-c_taurapp_neg
    }
  }

  #activity
  bob$activity$activity<-A
  bob$activity$activity_day<-activ_day
  if(!minimal_output){
    bob$activity$mu_a<-mu_a
    bob$activity$sigma_a<-sigma_a
    if (extended_output){
      bob$activity$c_muF<-c_muF
      bob$activity$c_muM<-c_muM
      bob$activity$c_muloi<-c_muloi
    }
  }

  #body weight
  bob$body_weight$weight<-weight
  if(!minimal_output){
    if (extended_output){
      bob$body_weight$c_BW<-c_BW
    }
    bob$body_weight$c_ee_work<-c_ee_work
    bob$body_weight$c_ee_nowork<-c_ee_nowork
    bob$body_weight$c_ei_work<-c_ei_work
    bob$body_weight$c_ei_nowork<-c_ei_nowork
  }

  #appetite
  bob$appetite$appetite<-appetite
  if(!minimal_output & extended_output){
    bob$appetite$c_app<-c_app
    bob$appetite$c_appH<-c_appH
    bob$appetite$c_apploi<-c_apploi
    bob$appetite$c_appse<-c_appse
  }
  #fatigue
  bob$fatigue$fatigue<-fatigue
  if(!minimal_output & extended_output){
    bob$fatigue$c_F<-c_F
    bob$fatigue$c_FS<-c_FS
    bob$fatigue$standard_sleep<-standard_sleep
  }

  #loss of interest
  bob$loss_of_interest$loss_int<-loss_int
  if(!minimal_output & extended_output){
    bob$loss_of_interest$c_loiM<-c_loiM
  }

  #mood
  bob$mood$mood<-mood
  bob$mood$mean_mood_day<-mean_mood
  if(!minimal_output){
    bob$mood$home_base<-home_base
    bob$mood$attrac_strength<-attrac_strength
    if (extended_output){
      bob$mood$a1<-a1
      bob$mood$a2<-a2
      bob$mood$a3<-a3
      bob$mood$a4<-a4
      bob$mood$a5<-a5
      bob$mood$a6<-a6
      bob$mood$c_M<-c_M
    }
  }
  #external misery
  if(!minimal_output){
    bob$external_misery$external<-external_misery
  }

  #Depression
  bob$depression$depression<-depression
  if(!minimal_output){
    bob$depression$symptoms_day<-depression_symp
    bob$depression$symptoms_14days<-depressed
  }

  #other
  bob$other$t<-t
  bob$other$t_day<-day_vec
  bob$other$N<-N
  if(!minimal_output){
    bob$other$t_step<-t_step
    bob$other$tau<-tau_c

  }
  return(bob)

}
