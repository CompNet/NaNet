#############################################################################################
# Functions used to test data distribution.
# 
# 05/2021 Vincent Labatut
#############################################################################################
library("poweRlaw")




#############################################################################################
# column names
C_DISTR <- c()
C_PL_EXP <- "PowerLaw_exp"; C_DISTR <- c(C_DISTR, C_PL_EXP)
C_PL_PVAL <- "PowerLaw_pval"; C_DISTR <- c(C_DISTR, C_PL_PVAL)
C_POIS_PVAL <- "Poisson_pval"; C_DISTR <- c(C_DISTR, C_POIS_PVAL)
C_POIS_CLR <- "Poisson_cmp_LR"; C_DISTR <- c(C_DISTR, C_POIS_CLR)
C_POIS_CPVAL <- "Poisson_cmp_pval"; C_DISTR <- c(C_DISTR, C_POIS_CPVAL)
C_LNORM_PVAL <- "LogNormal_pval"; C_DISTR <- c(C_DISTR, C_LNORM_PVAL)
C_LNORM_CLR <- "LogNormal_cmp_LR"; C_DISTR <- c(C_DISTR, C_LNORM_CLR)
C_LNORM_CPVAL <- "LogNormal_cmp_pval"; C_DISTR <- c(C_DISTR, C_LNORM_CPVAL)
C_EXPO_PVAL <- "Exponential_pval"; C_DISTR <- c(C_DISTR, C_EXPO_PVAL)
C_EXPO_CLR <- "Exponential_cmp_LR"; C_DISTR <- c(C_DISTR, C_EXPO_CLR)
C_EXPO_CPVAL <- "Exponential_cmp_pval"; C_DISTR <- c(C_DISTR, C_EXPO_CPVAL)
C_WEIB_PVAL <- "Weibull_pval"; C_DISTR <- c(C_DISTR, C_WEIB_PVAL)
C_WEIB_CLR <- "Weibull_cmp_LR"; C_DISTR <- c(C_DISTR, C_WEIB_CLR)
C_WEIB_CPVAL <- "Weibull_cmp_pval"; C_DISTR <- c(C_DISTR, C_WEIB_CPVAL)
C_DECISION <- "Decision"; C_DISTR <- c(C_DISTR, C_DECISION)



#############################################################################################
# Tests the type of empirical distribution of the specified continuous data.
#
# data: data to test.
# return_stats: whether or not to return all the computed stats.
# sims: number of bootstrap simulations.
#
# returns: either the p-value obtained for the PL fit, or all the results obtained when comparing
#          the PL to other laws.
#############################################################################################
test.cont.distr <- function(data, return_stats=FALSE, sims=1000)
{	tlog(0,"Test data distribution")
	cols <- c(
		C_PL_EXP, C_PL_PVAL, 
		C_LNORM_PVAL, C_LNORM_CLR, C_LNORM_CPVAL, 
		C_EXPO_PVAL, C_EXPO_CLR, C_EXPO_CPVAL,
		C_WEIB_PVAL, C_WEIB_CLR, C_WEIB_CPVAL, 
	)
	tab <- rep(NA,length(cols))
	names(tab) <- cols
	
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
	pl.bs <- bootstrap_p(power.law, no_of_sims=sims, threads=8)
	# display results
	tlog(4,"Parameters: x_min=",power.law$xmin," exp=",power.law$pars)
	tlog(4,"p-value for power law: ",pl.bs$p)
	tab[C_PL_EXP] <- power.law$pars
	tab[C_PL_PVAL] <- pl.bs$p
	
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
	ln.bs <- bootstrap_p(log.normal, no_of_sims=sims, threads=8)
	# display results
	tlog(4,"p-value for log-normal law: ",ln.bs$p)
	tab[C_LNORM_PVAL] <- ln.bs$p
	
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
	el.bs <- bootstrap_p(exp.law, no_of_sims=sims, threads=8)
	# display results
	tlog(4,"p-value for exponential law: ",el.bs$p)
	tab[C_EXPO_PVAL] <- el.bs$p
	
	tlog(2,"Handling Weibull law (sometimes called stretched exponential)") # 
	# create weibull law
	weib.law <- conweibull$new(data)
	# estimate parameters
	est <- estimate_xmin(weib.law)
	weib.law$setXmin(est)
#	print(est)
	# plot model
	lines(weib.law, col="PURPLE", lwd=2)
	# bootstrap test
	weib.bs <- bootstrap_p(weib.law, no_of_sims=sims, threads=8)
	# display results
	tlog(4,"p-value for Weibull law: ",weib.bs$p)
	tab[C_WEIB_PVAL] <- weib.bs$p
	
	# add legend
	legend(
		x="bottomleft",
		legend=c("Power law", "Log-normal law", "Exponential law", "Weibull law"),
		fill=c("BLUE", "GREEN", "ORANGE", "PURPLE")
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
	tab[C_LNORM_CLR] <- comp.ln$test_statistic
	tab[C_LNORM_CPVAL] <- comp.ln$p_two_sided
	comp.ln <- compare_distributions(log.normal, power.law)
#	print(comp.ln)
	tlog(4,"Test statistic (reverse order): ",comp.ln$test_statistic)
	tlog(6,"p-values: ",comp.ln$p_one_sided,", ",comp.ln$p_two_sided)
	
	# compare power and exponential laws
	tlog(2,"Comparing power and exponential laws")
	exp.law$setXmin(power.law$getXmin())
	est <- estimate_pars(exp.law)
	exp.law$setPars(est)
	comp.el <- compare_distributions(power.law, exp.law)
#	print(comp.el)
	tlog(4,"Test statistic: ",comp.el$test_statistic)
	tlog(6,"p-values: ",comp.el$p_one_sided,", ",comp.el$p_two_sided)
	tab[C_EXPO_CLR] <- comp.el$test_statistic
	tab[C_EXPO_CPVAL] <- comp.el$p_two_sided
	comp.el <- compare_distributions(exp.law, power.law)
#	print(comp.el)
	tlog(4,"Test statistic (reverse order): ",comp.el$test_statistic)
	tlog(6,"p-values: ",comp.el$p_one_sided,", ",comp.el$p_two_sided)
	
	# compare power and Weibull laws
	tlog(2,"Comparing power and Weibull laws")
	weib.law$setXmin(power.law$getXmin())
	est <- estimate_pars(weib.law)
	weib.law$setPars(est)
	comp.wl <- compare_distributions(power.law, weib.law)
#	print(comp.wl)
	tlog(4,"Test statistic: ",comp.wl$test_statistic)
	tlog(6,"p-values: ",comp.wl$p_one_sided,", ",comp.wl$p_two_sided)
	tab[C_WEIB_CLR] <- comp.wl$test_statistic
	tab[C_WEIB_CPVAL] <- comp.wl$p_two_sided
	comp.wl <- compare_distributions(weib.law, power.law)
#	print(comp.wl)
	tlog(4,"Test statistic (reverse order): ",comp.wl$test_statistic)
	tlog(6,"p-values: ",comp.wl$p_one_sided,", ",comp.wl$p_two_sided)
	
	tlog(0,"-------------------------------")
	tlog(0,"Interpretation of the distribution test:")
	tlog(0,"  h_0: original data could have been drawn from the fitted distribution")
	tlog(0,"  >> statistically significant if large enough")
	tlog(0,"-------------------------------")
	tlog(0,"Interpretation of the comparison test:")
	tlog(2,"- The test statistic indicates whether the power-law (positive) or the other distribution (negative) is preferred")
	tlog(2,"- The p-value indicates whether this sign is significant (small p)")
	tlog(2,"- The one-sided value is order-dependent, the two-sided one is not (They seem to use the latter)")
	
	# draw conclusion
	tlog(0,"-------------------------------")
	tab[C_DECISION] <- make.decision.distr(tab, threshold=0.01)
	tlog(2,"Conclusion: ", tab[C_DECISION])
	tlog(0,"-------------------------------")
	
	if(return_stats)
		res <- tab
	else
		res <- power.law$pars
	return(res)
}




#############################################################################################
# Tests the type of empirical distribution of the specified discrete data.
#
# data: data to test.
# return_stats: whether or not to return all the computed stats.
# sims: number of bootstrap simulations.
#
# returns: either the p-value obtained for the PL fit, or all the results obtained when comparing
#          the PL to other laws.
#############################################################################################
test.disc.distr <- function(data, return_stats=FALSE, sims=100)
{	tlog(0,"Test data distribution")
	cols <- c(
		C_PL_EXP, C_PL_PVAL, 
		C_LNORM_PVAL, C_LNORM_CLR, C_LNORM_CPVAL, 
		C_EXPO_PVAL, C_EXPO_CLR, C_EXPO_CPVAL, 
		C_POIS_PVAL, C_POIS_CLR, C_POIS_CPVAL 
	)
	tab <- rep(NA,length(cols))
	names(tab) <- cols
	
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
	pl.bs <- bootstrap_p(power.law, no_of_sims=sims, threads=8)
	# display results
	tlog(4,"Parameters: x_min=",power.law$xmin," exp=",power.law$pars)
	tlog(4,"p-value for power law: ",pl.bs$p)
	tab[C_PL_EXP] <- power.law$pars
	tab[C_PL_PVAL] <- pl.bs$p
	
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
	ln.bs <- bootstrap_p(log.normal, no_of_sims=sims, threads=8)
	# display results
	tlog(4,"p-value for log-normal law: ",ln.bs$p)
	tab[C_LNORM_PVAL] <- ln.bs$p
	
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
	el.bs <- bootstrap_p(exp.law, no_of_sims=sims, threads=8)
	# display results
	tlog(4,"p-value for exponential law: ",el.bs$p)
	tab[C_EXPO_PVAL] <- el.bs$p
	
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
	pois.bs <- bootstrap_p(pois.law, no_of_sims=sims, threads=8)
	# display results
	tlog(4,"p-value for exponential law: ",pois.bs$p)
	tab[C_POIS_PVAL] <- pois.bs$p
	
	# add legend
	legend(
		x="bottomleft",
		legend=c("Power law", "Log-normal law", "Exponential law", "Poisson law"),
		fill=c("BLUE", "GREEN", "ORANGE", "PURPLE")
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
	tab[C_LNORM_CLR] <- comp.ln$test_statistic
	tab[C_LNORM_CPVAL] <- comp.ln$p_two_sided
	comp.ln <- compare_distributions(log.normal, power.law)
#	print(comp.ln)
	tlog(4,"Test statistic (reverse order): ",comp.ln$test_statistic)
	tlog(6,"p-values: ",comp.ln$p_one_sided,", ",comp.ln$p_two_sided)
	
	# compare power and exponential laws
	tlog(2,"Comparing power and exponential laws")
	exp.law$setXmin(power.law$getXmin())
	est <- estimate_pars(exp.law)
	exp.law$setPars(est)
	comp.el <- compare_distributions(power.law, exp.law)
#	print(comp.el)
	tlog(4,"Test statistic: ",comp.el$test_statistic)
	tlog(6,"p-values: ",comp.el$p_one_sided,", ",comp.el$p_two_sided)
	tab[C_EXPO_CLR] <- comp.el$test_statistic
	tab[C_EXPO_CPVAL] <- comp.el$p_two_sided
	comp.el <- compare_distributions(exp.law, power.law)
#	print(comp.el)
	tlog(4,"Test statistic (reverse order): ",comp.el$test_statistic)
	tlog(6,"p-values: ",comp.el$p_one_sided,", ",comp.el$p_two_sided)
	
	# compare power and poisson laws
	tlog(2,"Comparing power and poisson laws")
	pois.law$setXmin(power.law$getXmin())
	est <- estimate_pars(pois.law)
	pois.law$setPars(est)
	comp.pl <- compare_distributions(power.law, pois.law)
#	print(comp.pl)
	tlog(4,"Test statistic: ",comp.pl$test_statistic)
	tlog(6,"p-values: ",comp.pl$p_one_sided,", ",comp.pl$p_two_sided)
	tab[C_POIS_CLR] <- comp.pl$test_statistic
	tab[C_POIS_CPVAL] <- comp.pl$p_two_sided
	comp.pl <- compare_distributions(pois.law, power.law)
#	print(comp.pl)
	tlog(4,"Test statistic (reverse order): ",comp.pl$test_statistic)
	tlog(6,"p-values: ",comp.pl$p_one_sided,", ",comp.pl$p_two_sided)
	
	tlog(0,"-------------------------------")
	tlog(0,"Interpretation of the distribution test:")
	tlog(0,"  h_0: original data could have been drawn from the fitted distribution")
	tlog(0,"  >> statistically significant if large enough")
	tlog(0,"-------------------------------")
	tlog(0,"Interpretation of the comparison test:")
	tlog(2,"- The test statistic indicates whether the power-law (positive) or the other distribution (negative) is preferred")
	tlog(2,"- The p-value indicates whether this sign is significant (small p)")
	tlog(2,"- The one-sided value is order-dependent, the two-sided one is not (They seem to use the latter)")
	
	# draw conclusion
	tlog(0,"-------------------------------")
	tab[C_DECISION] <- make.decision.distr(tab, threshold=0.05)
	tlog(2,"Conclusion: ", tab[C_DECISION])
	tlog(0,"-------------------------------")
	
	if(return_stats)
		res <- tab
	else
		res <- power.law$pars
	return(res)
}




#############################################################################################
# Takes the statistics computed with test.disc.distr or test.cont.distr, and take a decision
# regarding the nature of the studied distribution.
#
# tab: stats computed with the other methods.
#
# returns: string representing the final decision.
#############################################################################################
make.decision.distr <- function(tab, threshold=0.01)
{	# determine which distribution fits better than the power law
	indist <- c()
	better <- c()
	if(C_POIS_PVAL %in% names(tab))
	{	if(tab[C_POIS_CPVAL]<threshold)
		{	if(tab[C_POIS_CLR]<0)
				better <- c(better, "Poisson")
		}
		else
			indist <- c(indist, "Poisson")
	}
	if(C_LNORM_PVAL %in% names(tab))
	{	if(tab[C_LNORM_CPVAL]<threshold)
		{	if(tab[C_LNORM_CLR]<0)
				better <- c(better, "LogNormal")
		}
		else
			indist <- c(indist, "LogNormal")
	}
	if(C_EXPO_PVAL %in% names(tab))
	{	if(tab[C_EXPO_CPVAL]<threshold)
		{	if(tab[C_EXPO_CLR]<0)
				better <- c(better, "Exponential")
		}
		else
			indist <- c(indist, "Exponential")
	}
	if(C_WEIB_PVAL %in% names(tab))
	{	if(tab[C_WEIB_CPVAL]<threshold)
		{	if(tab[C_WEIB_CLR]<0)
				better <- c(better, "Weibull")
		}
		else
			indist <- c(indist, "Weibull")
	}
	
	# build result string
	if(length(better)>0)
		res <- paste(better, collapse=", ")
	else
	{	if(tab[C_PL_PVAL] > threshold)
			indist <- c("PowerLaw", indist)
		res <- paste(indist, collapse=", ")
	}
	return(res)
}
