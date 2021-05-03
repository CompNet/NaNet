#############################################################################################
# Functions used to test data distribution.
# 
# 05/2021 Vincent Labatut
#############################################################################################
library("poweRlaw")




#############################################################################################
# Test the type of empirical distribution of the specified continuous data.
#
# data: data to test.
#############################################################################################
test.cont.distr <- function(data)
{	tlog(0,"Test data distribution")
	
	tlog(2,"Handling power law")
	# create continuous power law distribution
	power.law <- conpl$new(data)
	# estimate parameters
	est <- estimate_xmin(power.law)
	power.law$setXmin(est)
#	print(est)
	# plot model
	plot(power.law, col="RED")
	lines(power.law, col="BLUE", lwd=2)
	# bootstrap test
	pl.bs <- bootstrap_p(power.law, no_of_sims=100, threads=8)
	tlog(4,"p-value for power law: ",pl.bs$p)
		
	tlog(2,"Handling log-normal law")
	# create continuous log-normal law
	log.normal <- conlnorm$new(data)
	# estimate parameters
	est <- estimate_xmin(log.normal)
	log.normal$setXmin(est)
#	print(est)
	# plot model
	lines(log.normal, col="GREEN", lwd=2)
	# bootstrap test
	ln.bs <- bootstrap_p(log.normal, no_of_sims=100, threads=8)
	tlog(4,"p-value for log-normal law: ",ln.bs$p)
	
	tlog(2,"Handling Weibull law")
	# create weibull law
	weib.law <- conweibull$new(data)
	# estimate parameters
	est <- estimate_xmin(weib.law)
	weib.law$setXmin(est)
#	print(est)
	# plot model
	lines(weib.law, col="PURPLE", lwd=2)
	# bootstrap test
	el.bs <- bootstrap_p(weib.law, no_of_sims=100, threads=8)
	tlog(4,"p-value for Weibull law: ",el.bs$p)
	
	tlog(2,"Handling exponential law")
	# create exponential law
	exp.law <- conexp$new(data)
	# estimate parameters
	est <- estimate_xmin(exp.law)
	exp.law$setXmin(est)
#	print(est)
	# plot model
	lines(exp.law, col="ORANGE", lwd=2)
	# bootstrap test
	el.bs <- bootstrap_p(exp.law, no_of_sims=100, threads=8)
	tlog(4,"p-value for exponential law: ",el.bs$p)
	
	# add legend
	legend(
		x="bottomleft",
		legend=c("Power law", "Log-normal law", "Weibull law", "Exponential law"),
		fill=c("BLUE", "GREEN", "PURPLE", "ORANGE")
	)
	
	# compare power and log-normal laws
	tlog(2,"Comparing power and log-normal laws")
	log.normal$setXmin(power.law$getXmin())
	est <- estimate_pars(log.normal)
	log.normal$setPars(est)
	comp.ln <- compare_distributions(power.law, log.normal)
#	print(comp.ln)
	tlog(4,"Test statistic: ",comp.ln$test_statistic)
	tlog(6,"p-values: ",comp.ln$p_one_sided,", ",comp.ln$p_two_sided)
	comp.ln <- compare_distributions(log.normal, power.law)
#	print(comp.ln)
	tlog(4,"Test statistic (reverse order): ",comp.ln$test_statistic)
	tlog(6,"p-values: ",comp.ln$p_one_sided,", ",comp.ln$p_two_sided)
	
	# compare power and Weibull laws
	tlog(2,"Comparing power and Weibull laws")
	weib.law$setXmin(power.law$getXmin())
	est <- estimate_pars(weib.law)
	weib.law$setPars(est)
	comp.wl <- compare_distributions(power.law, weib.law)
#	print(comp.wl)
	tlog(4,"Test statistic: ",comp.wl$test_statistic)
	tlog(6,"p-values: ",comp.wl$p_one_sided,", ",comp.wl$p_two_sided)
	comp.wl <- compare_distributions(weib.law, power.law)
#	print(comp.wl)
	tlog(4,"Test statistic (reverse order): ",comp.wl$test_statistic)
	tlog(6,"p-values: ",comp.wl$p_one_sided,", ",comp.wl$p_two_sided)
	
	# compare power and exponential laws
	tlog(2,"Comparing power and exponential laws")
	exp.law$setXmin(power.law$getXmin())
	est <- estimate_pars(exp.law)
	exp.law$setPars(est)
	comp.el <- compare_distributions(power.law, exp.law)
#	print(comp.el)
	tlog(4,"Test statistic: ",comp.el$test_statistic)
	tlog(6,"p-values: ",comp.el$p_one_sided,", ",comp.el$p_two_sided)
	comp.el <- compare_distributions(exp.law, power.law)
#	print(comp.el)
	tlog(4,"Test statistic (reverse order): ",comp.el$test_statistic)
	tlog(6,"p-values: ",comp.el$p_one_sided,", ",comp.el$p_two_sided)
	
	tlog(0,"-------------------------------")
	tlog(0,"Interpretation of the distribution test: statistically significant if large enough")
	tlog(0,"-------------------------------")
	tlog(0,"Interpretation of the comparison test:")
	tlog(2,"- The test statistic indicates whether the power-law (positive) or the other distribution (negative) is preferred")
	tlog(2,"- The p-value indicates whether this sign is significant (small p)")
}




#############################################################################################
# Test the type of empirical distribution of the specified discrete data.
#
# data: data to test.
#############################################################################################
test.disc.distr <- function(data)
{	tlog(0,"Test data distribution")
	
	tlog(2,"Handling power law")
	# create discrete power law distribution
	power.law <- displ$new(data)
	# estimate parameters
	est <- estimate_xmin(power.law)
	power.law$setXmin(est)
#	print(est)
	# plot model
	plot(power.law, col="RED")
	lines(power.law, col="BLUE", lwd=2)
	# bootstrap test
	pl.bs <- bootstrap_p(power.law, no_of_sims=100, threads=8)
	tlog(4,"p-value for power law: ",pl.bs$p)
	
	tlog(2,"Handling log-normal law")
	# create discrete log-normal law
	log.normal <- dislnorm$new(data)
	# estimate parameters
	est <- estimate_xmin(log.normal)
	log.normal$setXmin(est)
#	print(est)
	# plot model
	lines(log.normal, col="GREEN", lwd=2)
	# bootstrap test
	ln.bs <- bootstrap_p(log.normal, no_of_sims=100, threads=8)
	tlog(4,"p-value for log-normal law: ",ln.bs$p)
	
	tlog(2,"Handling Poisson law")
	# create poisson law
	pois.law <- dispois$new(data)
	# estimate parameters
	est <- estimate_xmin(pois.law)
	pois.law$setXmin(est)
#	print(est)
	# plot model
	lines(pois.law, col="PURPLE", lwd=2)
	# bootstrap test
	el.bs <- bootstrap_p(pois.law, no_of_sims=100, threads=8)
	tlog(4,"p-value for exponential law: ",el.bs$p)
	
	tlog(2,"Handling discrete exponential law")
	# create discrete exponential law
	exp.law <- disexp$new(data)
	# estimate parameters
	est <- estimate_xmin(exp.law)
	exp.law$setXmin(est)
#	print(est)
	# plot model
	lines(exp.law, col="ORANGE", lwd=2)
	# bootstrap test
	el.bs <- bootstrap_p(exp.law, no_of_sims=100, threads=8)
	tlog(4,"p-value for exponential law: ",el.bs$p)
	
	# add legend
	legend(
		x="bottomleft",
		legend=c("Power law", "Log-normal law", "Poisson law", "Exponential law"),
		fill=c("BLUE", "GREEN", "PURPLE", "ORANGE")
	)
	
	# compare power and log-normal laws
	tlog(2,"Comparing power and log-normal laws")
	log.normal$setXmin(power.law$getXmin())
	est <- estimate_pars(log.normal)
	log.normal$setPars(est)
	comp.ln <- compare_distributions(power.law, log.normal)
#	print(comp.ln)
	tlog(4,"Test statistic: ",comp.ln$test_statistic)
	tlog(6,"p-values: ",comp.ln$p_one_sided,", ",comp.ln$p_two_sided)
	comp.ln <- compare_distributions(log.normal, power.law)
#	print(comp.ln)
	tlog(4,"Test statistic (reverse order): ",comp.ln$test_statistic)
	tlog(6,"p-values: ",comp.ln$p_one_sided,", ",comp.ln$p_two_sided)
	
	# compare power and poisson laws
	tlog(2,"Comparing power and poisson laws")
	pois.law$setXmin(power.law$getXmin())
	est <- estimate_pars(pois.law)
	pois.law$setPars(est)
	comp.pl <- compare_distributions(power.law, pois.law)
#	print(comp.pl)
	tlog(4,"Test statistic: ",comp.pl$test_statistic)
	tlog(6,"p-values: ",comp.pl$p_one_sided,", ",comp.pl$p_two_sided)
	comp.pl <- compare_distributions(pois.law, power.law)
#	print(comp.pl)
	tlog(4,"Test statistic (reverse order): ",comp.pl$test_statistic)
	tlog(6,"p-values: ",comp.pl$p_one_sided,", ",comp.pl$p_two_sided)
	
	# compare power and exponential laws
	tlog(2,"Comparing power and exponential laws")
	exp.law$setXmin(power.law$getXmin())
	est <- estimate_pars(exp.law)
	exp.law$setPars(est)
	comp.el <- compare_distributions(power.law, exp.law)
#	print(comp.el)
	tlog(4,"Test statistic: ",comp.el$test_statistic)
	tlog(6,"p-values: ",comp.el$p_one_sided,", ",comp.el$p_two_sided)
	comp.el <- compare_distributions(exp.law, power.law)
#	print(comp.el)
	tlog(4,"Test statistic (reverse order): ",comp.el$test_statistic)
	tlog(6,"p-values: ",comp.el$p_one_sided,", ",comp.el$p_two_sided)
	
	tlog(0,"-------------------------------")
	tlog(0,"Interpretation of the distribution test: statistically significant if large enough")
	tlog(0,"-------------------------------")
	tlog(0,"Interpretation of the comparison test:")
	tlog(2,"- The test statistic indicates whether the power-law (positive) or the other distribution (negative) is preferred")
	tlog(2,"- The p-value indicates whether this sign is significant (small p)")
	tlog(2,"- The one-sided value is order dependent, the two-sided one is not")
}
