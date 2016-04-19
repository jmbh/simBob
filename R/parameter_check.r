parameter_check<-function(t_days,t_step,work,body_weight,weight_cons,c_ee_work,c_ee_nowork,c_ei_work,c_ei_nowork,
						  tau_d,tau_r,tau_dh,tau_rh,tau_rhs,tau_c,A_s,A_h,t_0s,t_0h,H_s,L_s,H_h,L_h,mu_a,
						  sigma_a,a_thres,A_rest,mu_a_night,sigma_a_night,a_thres_night,A_night,mu_a_work,sigma_a_work,
						  A_work,standard_sleep,c_F,c_FS,c_taudF,c_taurF,c_muF,c_app,c_appH,c_apploi,c_appse,c_tapp,
						  cc_tapp,social_eating,loss_int,home_base,attrac_strength,c_M,a1,a2,a3,a4,a5,a6,c_muM,
						  c_muloi,c_loiM,c_taurM,external_misery,social_eat_work,extended_output,
						  minimal_output,plot_week,plot_months,network_figures){

if (!is.numeric(t_days)){
  stop('t_days has to be numeric')
}
if (length(t_days)>1) {
  stop('t_days has to be a scalar')
}

if (!is.numeric(t_step)){
	stop('t_step has to be numeric')
}
if (length(t_step)>1) {
	stop('t_step has to be a scalar')
}

N=24*t_days/t_step;

if (!is.numeric(work) && !is.logical(work)){
	stop('work has to be a boolean or numeric')
}
if (length(work)>1) {
	stop('work has to be a scalar or a boolean')
}

if (!is.numeric(body_weight)){
	stop('body_weight has to be numeric')
}
if (length(body_weight)>1 | body_weight<0) {
	stop('body_weight has to be a positive scalar')
}

if (!is.numeric(weight_cons)){
	stop('weight_cons has to be numeric')
}
if (length(weight_cons)>1) {
	stop('weight_cons has to be a scalar')
}


if (!is.numeric(c_ee_work)){
	stop('c_ee_work has to be numeric')
}
if (length(c_ee_work)>1) {
	stop('c_ee_work has to be a scalar')
}

if (!is.numeric(c_ee_nowork)){
	stop('c_ee_nowork has to be numeric')
}
if (length(c_ee_nowork)>1) {
	stop('c_ee_nowork has to be a scalar')
}

if (!is.numeric(c_ei_work)){
	stop('c_ei_work has to be numeric')
}
if (length(c_ei_work)>1) {
	stop('c_ei_work has to be a scalar')
}

if (!is.numeric(c_ei_nowork)){
	stop('c_ei_nowork has to be numeric')
}
if (length(c_ei_nowork)>1) {
	stop('c_ei_nowork has to be a scalar')
}


if (!is.numeric(tau_d)){
	stop('tau_d has to be numeric')
}
if (!(length(tau_d)==1 | length(tau_d)==N)) {
	stop('tau_d has to be a scalar or a vector of the length ',N)
}

if (!is.numeric(tau_r)){
	stop('tau_r has to be numeric')
}
if (!(length(tau_r)==1 | length(tau_r)==N)){
	stop('tau_r has to be a scalar or a vector of the length ',N)
}

if (!is.numeric(tau_dh)){
	stop('tau_dh has to be numeric')
}
if (!(length(tau_dh)==1 | length(tau_dh)==N)) {
	stop('tau_dh has to be a scalar or a vector of the length ',N)
}

if (!is.numeric(tau_rh)){
	stop('tau_rh has to be numeric')
}
if (!(length(tau_rh)==1 | length(tau_rh)==N)) {
	stop('tau_rh has to be a scalar or a vector of the length ',N)
}


if (!is.numeric(tau_rhs)){
	stop('tau_rhs has to be numeric')
}
if (!(length(tau_rhs)==1 | length(tau_rhs)==N)) {
	stop('tau_rhs has to be a scalar or a vector of the length ',N)
}


if (!is.numeric(tau_c)){
	stop('tau has to be numeric')
}
if (length(tau_c)>1) {
	stop('tau has to be a scalar')
}

if (!is.numeric(A_s)){
	stop('A_s has to be numeric')
}
if (!(length(A_s)==1 | length(A_s)==N)) {
	stop('A_s has to be a scalar or a vector of the length ',N)
}
if (!is.numeric(A_h)){
	stop('A_h has to be numeric')
}
if (!(length(A_h)==1 | length(A_h)==N)){
	stop('A_h has to be a scalar or a vector of the length ',N)
}

if (!is.numeric(t_0s)){
	stop('t_0s has to be numeric')
}
if (length(t_0s)>1) {
	stop('t_0s has to be a scalar')
}

if (!is.numeric(t_0h)){
	stop('t_0h has to be numeric')
}
if (length(t_0h)>1) {
	stop('t_0h has to be a scalar')
}

if (!is.numeric(H_s)){
	stop('H_s has to be numeric')
}
if (!(length(H_s)==1 | length(H_s)==N)) {
	stop('H_s has to be a scalar or a vector of the length ',N)
}
if (!is.numeric(L_s)){
	stop('L_s has to be numeric')
}
if (!(length(L_s)==1 | length(L_s)==N)) {
	stop('L_s has to be a scalar or a vector of the length ',N)
}

if (!is.numeric(H_h)){
	stop('H_h has to be numeric')
}
if (!(length(H_h)==1 | length(H_h)==N)) {
	stop('H_h has to be a scalar or a vector of the length ',N)
}
if (!is.numeric(L_h)){
	stop('L_h has to be numeric')
}
if (!(length(L_h)==1 | length(L_h)==N)) {
	stop('L_h has to be a scalar or a vector of the length ',N)
}


if (!is.numeric(mu_a)){
	stop('mu_a has to be numeric')
}
if (!(length(mu_a)==1 | length(mu_a)==N)) {
	stop('mu_a has to be a scalar or a vector of the length ',N)
}
if (!is.numeric(sigma_a)){
	stop('sigma_a has to be numeric')
}
if (!(length(sigma_a)==1 | length(sigma_a)==N)) {
	stop('sigma_a has to be a scalar or a vector of the length ',N)
}
if (!is.numeric(a_thres)){
	stop('a_thres has to be numeric')
}
if (length(a_thres)>1) {
	stop('a_thres has to be a scalar')
}
if (!is.numeric(A_rest)){
	stop('A_rest has to be numeric')
}
if (length(A_rest)>1 | A_rest<0) {
	stop('A_rest has to be a positive scalar')
}
if (!is.numeric(mu_a_night)){
	stop('mu_a_night has to be numeric')
}
if (!(length(mu_a_night)==1 | length(mu_a_night)==N)) {
	stop('mu_a_night has to be a scalar or a vector of the length ',N)
}
if (!is.numeric(sigma_a_night)){
	stop('sigma_a_night has to be numeric')
}
if (!(length(sigma_a_night)==1 | length(sigma_a_night)==N)) {
	stop('sigma_a_night has to be a scalar or a vector of the length ',N)
}
if (!is.numeric(a_thres_night)){
	stop('a_thres_night has to be numeric')
}
if (length(a_thres_night)>1) {
	stop('a_thres_night has to be a scalar')
}
if (!is.numeric(A_night)){
	stop('A_night has to be numeric')
}
if (length(A_night)>1 | A_night<0) {
	stop('A_night has to be a positive scalar')
}
if (!is.numeric(mu_a_work)){
	stop('mu_a_work has to be numeric')
}
if (!(length(mu_a_work)==1 | length(mu_a_work)==N)) {
	stop('mu_a_work has to be a scalar or a vector of the length ',N)
}
if (!is.numeric(sigma_a_work)){
	stop('sigma_a_work has to be numeric')
}
if (!(length(sigma_a_work)==1 | length(sigma_a_work)==N)) {
	stop('sigma_a_work has to be a scalar or a vector of the length ',N)
}
if (!is.numeric(A_work)){
	stop('A_work has to be numeric')
}
if (length(A_work)>1 | A_work<0) {
	stop('A_work has to be a positive scalar')
}

if (!is.numeric(standard_sleep)){
	stop('standard_sleep has to be numeric')
}
if (length(standard_sleep)>1 | standard_sleep<0) {
	stop('standard_sleep has to be a positive scalar')
}

if (!is.numeric(c_F)){
	stop('c_F has to be numeric')
}
if (length(c_F)>1) {
	stop('c_F has to be a scalar')
}

if (!is.numeric(c_FS)){
  stop('c_FS has to be numeric')
}
if (length(c_FS)>1) {
  stop('c_FS has to be a scalar')
}

if (!is.numeric(c_taudF)){
	stop('c_taudF has to be numeric')
}
if (length(c_taudF)>1) {
	stop('c_taudF has to be a scalar')
}

if (!is.numeric(c_taurF)){
	stop('c_taurF has to be numeric')
}
if (length(c_taurF)>1) {
	stop('c_taurF has to be a scalar')
}

if (!is.numeric(c_muF)){
	stop('c_muF has to be numeric')
}
if (length(c_muF)>1) {
	stop('c_muF has to be a scalar')
}

if (!is.numeric(c_app)){
	stop('c_app has to be numeric')
}
if (length(c_app)>1) {
	stop('c_app has to be a scalar')
}
if (!is.numeric(c_appH)){
	stop('c_appH has to be numeric')
}
if (length(c_appH)>1) {
	stop('c_appH has to be a scalar')
}

if (!is.numeric(c_apploi)){
	stop('c_apploi has to be numeric')
}
if (length(c_apploi)>1) {
	stop('c_apploi has to be a scalar')
}

if (!is.numeric(c_appse)){
	stop('c_appse has to be numeric')
}
if (length(c_appse)>1) {
	stop('c_appse has to be a scalar')
}

if (!is.numeric(c_tapp)){
	stop('c_tapp has to be numeric')
}
if (length(c_tapp)>1) {
	stop('c_tapp has to be a scalar')
}


if (!is.numeric(cc_tapp)){
	stop('cc_tapp has to be numeric')
}
if (length(cc_tapp)>1) {
	stop('cc_tapp has to be a scalar')
}


if (!is.numeric(social_eating)){
	stop('social_eating has to be numeric')
}
if (!(length(social_eating)==1 | length(social_eating)==N)) {
	stop('social_eating has to be a scalar or a vector of the length',N)
}
if (!is.numeric(loss_int)){
	stop('loss_int has to be numeric')
}
if (!(length(loss_int)==1 | length(loss_int)==N)) {
	stop('loss_int has to be a scalar or a vector of the length ',N)
}

if (!is.numeric(home_base)){
	stop('home_base has to be numeric')
}
if (length(home_base)>1) {
	stop('home_base has to be a scalar')
}
if (!is.numeric(attrac_strength)){
	stop('attrac_strength has to be numeric')
}
if (length(attrac_strength)>1) {
	stop('attrac_strength has to be a scalar')
}
if (!is.numeric(c_M)){
	stop('c_M has to be numeric')
}
if (length(c_M)>1) {
	stop('c_M has to be a scalar')
}
if (!is.numeric(a1)){
	stop('a1 has to be numeric')
}
if (length(a1)>1) {
	stop('a1 has to be a scalar')
}
if (!is.numeric(a2)){
	stop('a2 has to be numeric')
}
if (length(a2)>1) {
	stop('a2 has to be a scalar')
}
if (!is.numeric(a3)){
	stop('a3 has to be numeric')
}
if (length(a3)>1) {
	stop('a3 has to be a scalar')
}
if (!is.numeric(a4)){
	stop('a4 has to be numeric')
}
if (length(a4)>1) {
	stop('a4 has to be a scalar')
}
if (!is.numeric(a5)){
	stop('a5 has to be numeric')
}
if (length(a5)>1) {
	stop('a5 has to be a scalar')
}
if (!is.numeric(a6)){
	stop('a6 has to be numeric')
}
if (length(a6)>1) {
	stop('a6 has to be a scalar')
}
if (!is.numeric(c_muM)){
	stop('c_muM has to be numeric')
}
if (length(c_muM)>1) {
	stop('c_muM has to be a scalar')
}
if (!is.numeric(c_muloi)){
	stop('c_muloi has to be numeric')
}
if (length(c_muloi)>1) {
	stop('c_muloi has to be a scalar')
}
if (!is.numeric(c_loiM)){
	stop('c_loiM has to be numeric')
}
if (length(c_loiM)>1) {
	stop('c_loiM has to be a scalar')
}
if (!is.numeric(c_taurM)){
	stop('c_taurM has to be numeric')
}
if (length(c_taurM)>1) {
	stop('c_taurM has to be a scalar')
}

if (!is.numeric(external_misery)){
	stop('external_misery has to be numeric')
}
if (!(length(external_misery)==1 | length(external_misery)==N)) {
	stop('external_misery has to be a scalar or a vector of the length ',N)
}
if (!is.numeric(social_eat_work) && !is.logical(social_eat_work)){
	stop('social_eat_work has to be a boolean or numeric')
}
if (length(social_eat_work)>1) {
	stop('social_eat_work has to be a scalar or a boolean')
}

if (!is.numeric(extended_output) && !is.logical(extended_output)){
	stop('extended_output has to be a boolean or numeric')
}
if (length(extended_output)>1) {
	stop('extended_output has to be a scalar or a boolean')
}
if (!is.numeric(minimal_output) && !is.logical(minimal_output)){
	stop('minimal_output has to be a boolean or numeric')
}
if (length(minimal_output)>1) {
	stop('minimal_output has to be a scalar or a boolean')
}
if (!is.numeric(plot_week) && !is.logical(plot_week)){
	stop('plot_week has to be a boolean or numeric')
}
if (length(plot_week)>1) {
	stop('plot_week has to be a scalar or a boolean')
}
if (!is.numeric(plot_months) && !is.logical(plot_months)){
	stop('plot_months has to be a boolean or numeric')
}
if (length(plot_months)>1) {
	stop('plot_months has to be a scalar or a boolean')
}

  if (!is.numeric(network_figures) && !is.logical(network_figures)){
    stop('network_figures has to be a boolean or numeric')
  }
  if (length(network_figures)>1) {
    stop('network_figures has to be a scalar or a boolean')
  }







}
