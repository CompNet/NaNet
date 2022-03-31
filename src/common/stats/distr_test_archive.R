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
C_TRUNC_CLR <- "TruncPL_cmp_LR"; C_DISTR <- c(C_DISTR, C_TRUNC_CLR)
C_TRUNC_CPVAL <- "TruncPL_cmp_pval"; C_DISTR <- c(C_DISTR, C_TRUNC_CPVAL)
C_YUSIM_CLR <- "YuleSimon_cmp_LR"; C_DISTR <- c(C_DISTR, C_YUSIM_CLR)
C_YUSIM_CPVAL <- "YuleSimon_cmp_pval"; C_DISTR <- c(C_DISTR, C_YUSIM_CPVAL)
C_DECISION <- "Decision"; C_DISTR <- c(C_DISTR, C_DECISION)



#############################################################################################
# Tests the type of empirical distribution of the specified continuous data.
#
# data: data to test.
# return_stats: whether or not to return all the computed stats.
# sims: number of bootstrap simulations.
# plot.file: base name of the plot file, or NA if not plot desired.
#
# returns: either the p-value obtained for the PL fit, or all the results obtained when comparing
#          the PL to other laws.
#############################################################################################
test.cont.distr <- function(data, return_stats=FALSE, sims=1000, plot.file=NA)
{	tlog(0,"Test data distribution")
	cols <- c(
		C_PL_EXP, C_PL_PVAL, 
		C_LNORM_PVAL, C_LNORM_CLR, C_LNORM_CPVAL, 
		C_EXPO_PVAL, C_EXPO_CLR, C_EXPO_CPVAL,
		C_WEIB_PVAL, C_WEIB_CLR, C_WEIB_CPVAL,
		C_TRUNC_CLR, C_TRUNC_CPVAL 
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
	# possibly plot model
	if(!is.na(plot.file))
	{	# pdf
		plot.file <- paste0(plot.file,".pdf")
		pdf(plot.file, width=15, height=15)
		plot(power.law, 
			col="BLACK",
			xlab=TeX("Degree $k$"), ylab="Probability Density"
		)
		lines(power.law, col="BLUE", lwd=2)
#		# ccdf
#		y <- 1 - c(0, dist_data_cdf(power.law))
#		x <- seq(from=min(data), to=max(data), by=(max(data)-min(data))/(length(y)-1))
#		plot(x[-length(x)], y[-length(y)], 
#			col="BLACK",
#			xlab=TeX("Degree $k$"), ylab="Complementary Cumulative Density",
#			log="xy"
#		)
#		x <- seq(from=power.law$xmin, to=max(data), by=(max(data)-power.law$xmin)/100)
#		y <- 1 - c(0, dist_cdf(power.law, x[-length(x)]))
#		lines(x[-length(x)], y[-length(y)], col="BLUE", lwd=2)
	}
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
	# possibly plot model
	if(!is.na(plot.file))
	{	# pdf
		lines(log.normal, col="GREEN", lwd=2)
#		# ccdf
#		x <- seq(from=log.normal$xmin, to=max(data), by=(max(data)-log.normal$xmin)/100)
#		y <- 1 - c(0, dist_cdf(log.normal, x[-length(x)]))
#		lines(x[-length(x)], y[-length(y)], col="GREEN", lwd=2)
	}
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
	# possibly plot model
	if(!is.na(plot.file))
	{	# pdf
		lines(exp.law, col="ORANGE", lwd=2)
#		# ccdf
#		x <- seq(from=exp.law$xmin, to=max(data), by=(max(data)-exp.law$xmin)/100)
#		y <- 1 - c(0, dist_cdf(exp.law, x[-length(x)]))
#		lines(x[-length(x)], y[-length(y)], col="ORANGE", lwd=2)
	}
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
	# possibly plot model
	if(!is.na(plot.file))
	{	# pdf
		lines(weib.law, col="PURPLE", lwd=2)
#		# ccdf
#		x <- seq(from=weib.law$xmin, to=max(data), by=(max(data)-weib.law$xmin)/100)
#		y <- 1 - c(0, dist_cdf(weib.law, x[-length(x)]))
#		lines(x[-length(x)], y[-length(y)], col="PURPLE", lwd=2)
	}
	# bootstrap test
	weib.bs <- bootstrap_p(weib.law, no_of_sims=sims, threads=8)
	# display results
	tlog(4,"p-value for Weibull law: ",weib.bs$p)
	tab[C_WEIB_PVAL] <- weib.bs$p
	
	################
	
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
	
	# compare power law and truncated power law
	tmp <- test_pl_expcutoff(data)
	tab[C_TRUNC_CLR] <- tmp$stat
	tab[C_TRUNC_CPVAL] <- tmp$pvalue
	
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
	
	# add legend to plot
	legend(
		x="bottomleft",
		legend=c("Power law", "Truncated Power Law", "Log-normal law", "Exponential law", "Weibull law"),
		fill=c("BLUE", "RED", "GREEN", "ORANGE", "PURPLE")
	)
	# and close plot file
	if(!is.na(plot.file))
		dev.off()
	
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
# plot.file: base name of the plot file, or NA if not plot desired.
#
# returns: either the p-value obtained for the PL fit, or all the results obtained when comparing
#          the PL to other laws.
#############################################################################################
test.disc.distr <- function(data, return_stats=FALSE, sims=100, plot.file=NA)
{	tlog(0,"Test data distribution")
	cols <- c(
		C_PL_EXP, C_PL_PVAL, 
		C_LNORM_PVAL, C_LNORM_CLR, C_LNORM_CPVAL, 
		C_EXPO_PVAL, C_EXPO_CLR, C_EXPO_CPVAL, 
		C_POIS_PVAL, C_POIS_CLR, C_POIS_CPVAL,
		C_TRUNC_CLR, C_TRUNC_CPVAL
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
	# possibly plot model
	if(!is.na(plot.file))
	{	# pdf
		plot.file <- paste0(plot.file,".pdf")
		pdf(plot.file, width=15, height=15)
		plot(power.law, 
			col="BLACK",
			xlab=TeX("Degree $k$"), ylab="Probability Density"
		)
		lines(power.law, col="BLUE", lwd=2)
#		# ccdf
#		y <- 1 - c(0, dist_data_cdf(power.law))
#		x <- seq(from=min(data), to=max(data), by=(max(data)-min(data))/(length(y)-1))
#		plot(x[-length(x)], y[-length(y)], 
#			col="BLACK",
#			xlab=TeX("Degree $k$"), ylab="Complementary Cumulative Density",
#			log="xy"
#		)
#		x <- seq(from=power.law$xmin, to=max(data), by=(max(data)-power.law$xmin)/100)
#		y <- 1 - c(0, dist_cdf(power.law, x[-length(x)]))
#		lines(x[-length(x)], y[-length(y)], col="BLUE", lwd=2)
	}
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
	# possibly plot model
	if(!is.na(plot.file))
	{	# pdf
		lines(log.normal, col="GREEN", lwd=2)
#		# ccdf
#		x <- seq(from=log.normal$xmin, to=max(data), by=(max(data)-log.normal$xmin)/100)
#		y <- 1 - c(0, dist_cdf(log.normal, x[-length(x)]))
#		lines(x[-length(x)], y[-length(y)], col="GREEN", lwd=2)
	}
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
	# possibly plot model
	if(!is.na(plot.file))
	{	# pdf
		lines(exp.law, col="ORANGE", lwd=2)
#		# ccdf
#		x <- seq(from=exp.law$xmin, to=max(data), by=(max(data)-exp.law$xmin)/100)
#		y <- 1 - c(0, dist_cdf(exp.law, x[-length(x)]))
#		lines(x[-length(x)], y[-length(y)], col="ORANGE", lwd=2)
	}
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
	# possibly plot model
	if(!is.na(plot.file))
	{	# pdf
		lines(pois.law, col="PURPLE", lwd=2)
#		# ccdf
#		x <- seq(from=pois.law$xmin, to=max(data), by=(max(data)-pois.law$xmin)/100)
#		y <- 1 - c(0, dist_cdf(pois.law, x[-length(x)]))
#		lines(x[-length(x)], y[-length(y)], col="PURPLE", lwd=2)
	}
	# bootstrap test
	pois.bs <- bootstrap_p(pois.law, no_of_sims=sims, threads=8)
	# display results
	tlog(4,"p-value for exponential law: ",pois.bs$p)
	tab[C_POIS_PVAL] <- pois.bs$p
	
	################
	
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
	
	# compare power law and truncated power law
	tmp <- test_pl_expcutoff(data)
	tab[C_TRUNC_CLR] <- tmp$stat
	tab[C_TRUNC_CPVAL] <- tmp$pvalue
	
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
	
	# add legend to plot
	legend(
		x="bottomleft",
		legend=c("Power law", "Truncated Power Law", "Log-normal law", "Exponential law", "Poisson law"),
		fill=c("BLUE", "RED", "GREEN", "ORANGE", "PURPLE")
	)
	# and close plot file
	if(!is.na(plot.file))
		dev.off()
	
	if(return_stats)
		res <- tab
	else
		res <- power.law$pars
	return(res)
}




#############################################################################################
# Uses the Python library powerlaw to hand the power law with exponential cutoff, which is
# not supported by the R library poweRlaw.
#
# data: data to test.
#
# returns: ????????????????????????
#############################################################################################
test_pl_expcutoff <- function(data)
{	tlog(2,"Handling power law within the Python library")
	
	# import necessary tools
	library("reticulate")
	pl <- import("powerlaw")
	
	# fit power law (and other distributions)
	fit = pl$Fit(deg, discrete=TRUE)
	# display results
	tlog(4,"Parameters: x_min=",fit$truncated_power_law$xmin," exp=",fit$truncated_power_law$alpha)
	
	# add to plot
	x <- seq(from=fit$truncated_power_law$xmin, to=max(data), by=(max(data)-fit$truncated_power_law$xmin)/100)
#	y <- x^(-fit$truncated_power_law$alpha) * exp(-fit$truncated_power_law$Lambda*x)
	y <- fit$truncated_power_law$pdf(x)
	lines(x, y, col="RED", lwd=2)
	
	# compare to other distributions
	tmp <- fit$distribution_compare("power_law", "lognormal")
	tlog(4,"Compare with log-normal distribution: test stat=",tmp[[1]]," p=",tmp[[2]])
	tmp <- fit$distribution_compare("power_law", "exponential")
	tlog(4,"Compare with exponential distribution: test stat=",tmp[[1]]," p=",tmp[[2]])
	tmp <- fit$distribution_compare("power_law", "stretched_exponential")
	tlog(4,"Compare with stretched exponential distribution: test stat=",tmp[[1]]," p=",tmp[[2]])
	tmp <- fit$distribution_compare("power_law", "truncated_power_law")
	tlog(4,"Compare with truncated power law: test stat=",tmp[[1]]," p=",tmp[[2]])
	
	res <- list(stat=res[[1]], pvalue=tmp[[2]])
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
