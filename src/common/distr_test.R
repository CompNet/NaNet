#############################################################################################
# Functions used to test data distribution.
# 
# 05/2021 Vincent Labatut
#############################################################################################
# Installation of pli: The C code must be compiled. 
# On a Windows system:
#  1) download and install MinGW and MSYS using the installer https://sourceforge.net/projects/mingw/files/Installer/
#  2) add to path (C:\MinGW\bin, C:\MinGW\msys\1.0\bin)
#  3) open terminal, go to C code folder
#  4) compile simple C file: gcc discpowerexp.c -o discpowerexp.exe
#  5) move resulting file to res/pli folder
#  6) edit path variable in R file discpowerexp.R
#  7) download and install GSL http://gnuwin32.sourceforge.net/packages/gsl.htm
#  8) add to path (D:\MinGW\GSL\bin, D:\MinGW\GSL\include, D:\MinGW\GSL\lib)
#  9) edit both makefiles to reflect above paths
#  9) open terminal, go to C code folder
# 10) compile GSL-dependent code: mingw32-make -f Makefile
# 11) move both compiled files like before
# 12) edit path variables in both R files (discpowerexp.R & zeta.R)
#
# Sys.getenv("PATH")
########################################################
library("poweRlaw")

source("src/pli/discexp.R")
source("src/pli/disclnorm.R")
source("src/pli/discpowerexp.R")
source("src/pli/discweib.R")
source("src/pli/exp.R")								# continuous distribution >> not needed
source("src/pli/lnorm.R")							# continuous distribution >> not needed
source("src/pli/pareto.R")							# continuous distribution >> not needed
source("src/pli/poisson.R")
source("src/pli/powerexp.R")						# continuous distribution >> not needed
source("src/pli/powerexp-exponential-integral.R")	# continuous distribution >> not needed
source("src/pli/power-law-test.R")
source("src/pli/weibull.R")							# continuous distribution >> not needed
source("src/pli/yule.R")
source("src/pli/zeta.R")




#############################################################################################
# column names
C_DISTR <- c()
C_PL_EXP <- "PowerLaw_exp"; C_DISTR <- c(C_DISTR, C_PL_EXP)
C_PL_XMIN <- "PowerLaw_xmin"; C_DISTR <- c(C_DISTR, C_PL_XMIN)
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
C_TRUNC_EXP <- "TruncPL_exp"; C_DISTR <- c(C_DISTR, C_TRUNC_EXP)
C_TRUNC_CLR <- "TruncPL_cmp_LR"; C_DISTR <- c(C_DISTR, C_TRUNC_CLR)
C_TRUNC_CPVAL <- "TruncPL_cmp_pval"; C_DISTR <- c(C_DISTR, C_TRUNC_CPVAL)
C_YUSIM_CLR <- "YuleSimon_cmp_LR"; C_DISTR <- c(C_DISTR, C_YUSIM_CLR)
C_YUSIM_CPVAL <- "YuleSimon_cmp_pval"; C_DISTR <- c(C_DISTR, C_YUSIM_CPVAL)
C_DECISION <- "Decision"; C_DISTR <- c(C_DISTR, C_DECISION)



#############################################################################################
# Tests the type of empirical distribution of the specified continuous data.
#
# data: data to test.
# xlab: label of the x-axis of the generated plot (name of the plotted quantity).
# return_stats: whether or not to return all the computed stats.
# sims: number of bootstrap simulations.
# plot.file: base name of the plot file, or NA if not plot desired.
#
# returns: either the p-value obtained for the PL fit, or all the results obtained when comparing
#          the PL to other laws.
#############################################################################################
test.cont.distr <- function(data, xlab=NA, return_stats=FALSE, sims=1000, plot.file=NA)
{	# init
	tab <- data.frame(matrix(NA, nrow=1, ncol=length(C_DISTR), dimnames=list(c(), C_DISTR)), 
			stringsAsFactors=FALSE)
	msgs <- c()
	msg <- "Test data distribution";tlog(0,msg);msgs <- c(msgs, msg)
	if(any(data==0))	# just to avoid zeroes or negative values, which prevents fitting certain distributions
	{	msg <- "Translating all values, in order to avoid zero/negative values";tlog(2,msg);msgs <- c(msgs, paste0("..",msg))
		data <- data - min(data) + 1
	}
	
	################## continuous power law
	msg <- "Handling continuous power law";tlog(2,msg);msgs <- c(msgs, paste0("..",msg))
	power.law <- conpl$new(data)		# create the model
	est <- estimate_xmin(power.law)		# estimate parameters
	tmp <- tryCatch(expr={power.law$setXmin(est);1}, 
			error=function(e) NA)
	if(is.na(tmp) || is.na(power.law$xmin))
	{	msg <- "ERROR could not fit the continuous power law at all";tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
		if(return_stats)
			res <- tab
		else
			res <- power.law$pars
		return(res)
	}
	# possibly plot model
	if(!is.na(plot.file))
	{	# pdf
		pdf(paste0(plot.file,".pdf"), width=15, height=15)
		plot(power.law, 
			col="BLACK",
			xlab=xlab, ylab="Probability Density"
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
	pl.bs <- bootstrap_p(power.law, no_of_sims=sims, threads=8)	# bootstrap test
	msg <- paste0("Parameters: x_min=",power.law$xmin," exp=",power.law$pars);tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
	msg <- paste0("p-value for power law: ",pl.bs$p);tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
	tab[1,C_PL_EXP] <- power.law$pars
	tab[1,C_PL_XMIN] <- power.law$xmin
	tab[1,C_PL_PVAL] <- pl.bs$p
	# alternative, with library pli
	power.law2 <- tryCatch(expr=pareto.fit(data=data, threshold=power.law$xmin, method="ml"), 
			error=function(e) NA)
	if(all(is.na(power.law2)))
	{	msg <- "ERROR while applying pareto.fit (for alt. test statistic)";tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
	}
	else
	{	msg <- paste0("Alt. exp=",power.law2$exponent);tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
	}
	
	################## continuous log-normal law
	msg <- "Handling continuous log-normal law";tlog(2,msg);msgs <- c(msgs, paste0("..",msg))
	log.normal <- conlnorm$new(data)		# create the model
	log.normal$setXmin(power.law$getXmin())	# set min x based on power law
	est <- estimate_pars(log.normal)		# estimate parameters
	log.normal$setPars(est)
	# possibly plot model
	if(!is.na(plot.file))
	{	# pdf
		lines(log.normal, col="GREEN", lwd=2)
#		# ccdf
#		x <- seq(from=log.normal$xmin, to=max(data), by=(max(data)-log.normal$xmin)/100)
#		y <- 1 - c(0, dist_cdf(log.normal, x[-length(x)]))
#		lines(x[-length(x)], y[-length(y)], col="GREEN", lwd=2)
	}
	comp.ln <- compare_distributions(power.law, log.normal)
	msg <- paste0("Test statistic: ",comp.ln$test_statistic, " p-value: ", comp.ln$p_two_sided);tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
	tab[1,C_LNORM_CLR] <- comp.ln$test_statistic
	tab[1,C_LNORM_CPVAL] <- comp.ln$p_two_sided
	# alternative, with library pli
	log.normal2 <- tryCatch(expr=lnorm.fit(x=data, threshold=power.law$xmin), 
			error=function(e) NA)
	if(all(is.na(log.normal2)))
	{	msg <- "ERROR while applying lnorm.fit (for alt. test statistic)";tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
	}
	else
	{	comp.ln2 <- vuong(pareto.lnorm.llr(x=data, pareto.d=power.law2, lnorm.d=log.normal2))
		msg <- paste0("Alt. Test statistic: ",comp.ln2$loglike.ratio, " p-value: ", comp.ln2$p.two.sided);tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
	}

	
	################## continuous exponential law
	msg <- "Handling continuous exponential law";tlog(2,msg);msgs <- c(msgs, paste0("..",msg))
	exp.law <- conexp$new(data)				# create the model
	exp.law$setXmin(power.law$getXmin())	# set min x based on power law
	est <- estimate_pars(exp.law)			# estimate parameters
	exp.law$setPars(est)
	# possibly plot model
	if(!is.na(plot.file))
	{	# pdf
		lines(exp.law, col="ORANGE", lwd=2)
#		# ccdf
#		x <- seq(from=exp.law$xmin, to=max(data), by=(max(data)-exp.law$xmin)/100)
#		y <- 1 - c(0, dist_cdf(exp.law, x[-length(x)]))
#		lines(x[-length(x)], y[-length(y)], col="ORANGE", lwd=2)
	}
	comp.el <- compare_distributions(power.law, exp.law)
	msg <- paste0("Test statistic: ",comp.el$test_statistic, " p-value: ", comp.el$p_two_sided);tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
	tab[1,C_EXPO_CLR] <- comp.el$test_statistic
	tab[1,C_EXPO_CPVAL] <- comp.el$p_two_sided
	# alternative, with library pli
	exp.law2 <- tryCatch(expr=exp.fit(x=data, threshold=power.law$xmin), 
		error=function(e) NA)
	if(all(is.na(exp.law2)))
	{	msg <- "ERROR while applying exp.fit (for alt. test statistic)";tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
	}
	else
	{	comp.el2 <- vuong(pareto.exp.llr(x=data, pareto.d=power.law2, exp.d=exp.law2))
		msg <- paste0("Alt. Test statistic: ",comp.el2$loglike.ratio, " p-value: ", comp.el2$p.two.sided);tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
	}
	
	################## continuous weibull distribution
	msg <- "Handling continuous Weibull law (sometimes called stretched exponential)";tlog(2,msg);msgs <- c(msgs, paste0("..",msg))
	weib.law <- conweibull$new(data)				# create the model
	weib.law$setXmin(power.law$getXmin())			# set min x based on power law
	est <- tryCatch(expr=estimate_pars(weib.law),	# estimate parameters 
			error=function(e) NA)
	if(all(is.na(est)))
	{	msg <- "ERROR while applying estimate_pars";tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
	}
	else
	{	weib.law$setPars(est)
		# possibly plot model
		if(!is.na(plot.file))
		{	# pdf
			lines(weib.law, col="PURPLE", lwd=2)
#			# ccdf
#			x <- seq(from=weib.law$xmin, to=max(data), by=(max(data)-weib.law$xmin)/100)
#			y <- 1 - c(0, dist_cdf(weib.law, x[-length(x)]))
#			lines(x[-length(x)], y[-length(y)], col="PURPLE", lwd=2)
		}
		weib.el <- tryCatch(expr=compare_distributions(power.law, weib.law),
				error=function(e) NA)
		if(all(is.na(weib.el)))
		{	msg <- "ERROR while applying comparing to the power law";tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
		}
		else
		{	msg <- paste0("Test statistic: ",weib.el$test_statistic, " p-value: ", weib.el$p_two_sided);tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
			tab[1,C_WEIB_CLR] <- weib.el$test_statistic
			tab[1,C_WEIB_CPVAL] <- weib.el$p_two_sided
		}
	}
	# alternative, with library pli
	weib.law2 <- tryCatch(expr=weibull.fit(x=data, threshold=power.law$xmin), 
			error=function(e) NA)
	if(all(is.na(weib.law2)))
	{	msg <- "ERROR while applying weibull.fit";tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
	}
	else
	{	weib.el2 <- vuong(pareto.weibull.llr(x=data, pareto.d=power.law2, weibull.d=weib.law2))
		msg <- paste0("Alt. Test statistic: ",weib.el2$loglike.ratio, " p-value: ", weib.el2$p.two.sided);tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
		if(is.na(tab[1,C_WEIB_CLR]))
			tab[1,C_WEIB_CLR] <- weib.el2$loglike.ratio
		if(is.na(tab[1,C_WEIB_CPVAL]))
			tab[1,C_WEIB_CPVAL] <- weib.el2$p.two.sided
	}
	
	################## continuous truncated power law
	msg <- "Handling continuous truncated power law";tlog(2,msg);msgs <- c(msgs, paste0("..",msg))
	# using Python
#	tmp <- test_pl_expcutoff(data, discrete=FALSE)
#	tab[1,C_TRUNC_CLR] <- tmp$stat
#	tab[1,C_TRUNC_CPVAL] <- tmp$pvalue
	if(!is.na(plot.file))					# possibly plot model
	{	# TODO
	}
	# only possibility is library pli
	trunc.law2 <- tryCatch(expr=powerexp.fit(data=data,threshold=power.law$xmin), 
			error=function(e) NA)
	if(all(is.na(trunc.law2)))
	{	msg <- "ERROR while applying powerexp";tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
	}
	else
	{	msg <- paste0("Parameters: x_min=",trunc.law2$xmin," exp=",trunc.law2$exponent);tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
		tab[1,C_TRUNC_EXP] <- trunc.law2$exponent
		comp.tl2 <- power.powerexp.lrt(power.d=power.law2, powerexp.d=trunc.law2)
		msg <- paste0("Alt. Test statistic: ",comp.tl2$log.like.ratio, " p-value: ", comp.tl2$p_value);tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
		tab[1,C_TRUNC_CLR] <- comp.tl2$log.like.ratio
		tab[1,C_TRUNC_CPVAL] <- comp.tl2$p_value
	}
	
	################
	
	msg <- "-------------------------------";tlog(0,msg);msgs <- c(msgs, msg)
	msg <- "Interpretation of the distribution test:";tlog(0,msg);msgs <- c(msgs, msg)
	msg <- "  h_0: original data could have been drawn from the fitted distribution";tlog(0,msg);msgs <- c(msgs, msg)
	msg <- "  >> statistically significant if large enough";tlog(0,msg);msgs <- c(msgs, msg)
	msg <- "-------------------------------";tlog(0,msg);msgs <- c(msgs, msg)
	msg <- "Interpretation of the comparison test:";tlog(0,msg);msgs <- c(msgs, msg)
	msg <- "- The test statistic indicates whether the power-law (positive) or the other distribution (negative) is preferred";tlog(0,msg);msgs <- c(msgs, msg)
	msg <- "- The p-value indicates whether this sign is significant (small p)";tlog(0,msg);msgs <- c(msgs, msg)
	msg <- "- The one-sided value is order-dependent, the two-sided one is not (They seem to use the latter)";tlog(0,msg);msgs <- c(msgs, msg)
	
	# draw conclusion
	msg <- "-------------------------------";tlog(0,msg);msgs <- c(msgs, msg)
	tab[1,C_DECISION] <- make.decision.distr(tab, threshold=0.01)
	msg <- paste0("Conclusion: ", tab[1,C_DECISION]);tlog(0,msg);msgs <- c(msgs, msg)
	msg <- "-------------------------------";tlog(0,msg);msgs <- c(msgs, msg)
	
	if(!is.na(plot.file))
	{	# add legend to plot
		legend(
			x="bottomleft",
			legend=c("Power law", "Truncated Power Law", "Log-normal law", "Exponential law", "Weibull law"),
			fill=c("BLUE", "RED", "GREEN", "ORANGE", "PURPLE")
		)
		# and close plot file
		dev.off()
	}
	
	# record log as a separate file
	conx <- file(paste0(plot.file,"_log.txt"))
		writeLines(msgs,conx)
	close(conx)
	
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
# xlab: label of the x-axis of the generated plot (name of the plotted quantity).
# return_stats: whether or not to return all the computed stats.
# sims: number of bootstrap simulations.
# plot.file: base name of the plot file, or NA if not plot desired.
#
# returns: either the p-value obtained for the PL fit, or all the results obtained when comparing
#          the PL to other laws.
#############################################################################################
test.disc.distr <- function(data, xlab=NA, return_stats=FALSE, sims=100, plot.file=NA)
{	# init
	tab <- data.frame(matrix(NA, nrow=1, ncol=length(C_DISTR), dimnames=list(c(), C_DISTR)), 
			stringsAsFactors=FALSE)
	msgs <- c()
	msg <- "Test data distribution";tlog(0,msg);msgs <- c(msgs, msg)
	if(any(data==0))	# just to avoid zeroes or negative values, which prevents fitting certain distributions
	{	msg <- "Translating all values, in order to avoid zero/negative values";tlog(2,msg);msgs <- c(msgs, paste0("..",msg))
		data <- data - min(data) + 1
	}
	
	################## discrete power law
	msg <- "Handling power law";tlog(2,msg);msgs <- c(msgs, paste0("..",msg))
	power.law <- displ$new(data)		# create the model
	est <- estimate_xmin(power.law)		# estimate parameters
	tmp <- tryCatch(expr={power.law$setXmin(est);1}, 
			error=function(e) NA)
	if(is.na(tmp))
	{	msg <- "ERROR could not fit the discrete power law at all";tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
		if(return_stats)
			res <- tab
		else
			res <- power.law$pars
		return(res)
	}
	if(!is.na(plot.file))				# possibly plot model
	{	# pdf
		pdf(paste0(plot.file,".pdf"), width=15, height=15)
		plot(power.law, 
			col="BLACK",
			xlab=xlab, ylab="Probability Density"
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
	pl.bs <- bootstrap_p(power.law, no_of_sims=sims, threads=8)	# bootstrap test
	msg <- paste0("Parameters: x_min=",power.law$xmin," exp=",power.law$pars);tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
	msg <- paste0("p-value for power law: ",pl.bs$p);tlog(4,msg);tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
	tab[1,C_PL_EXP] <- power.law$pars
	tab[1,C_PL_XMIN] <- power.law$xmin
	tab[1,C_PL_PVAL] <- pl.bs$p
	# alternative, with library pli
	power.law2 <- tryCatch(expr=zeta.fit(x=data, threshold=power.law$xmin, method="ml.direct"), 
			error=function(e) NA)
	if(all(is.na(power.law2)))
	{	msg <- "ERROR could not fit the discrete power law (for alt. test statistic)";tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
	}
	else
	{	msg <- paste0("Alt. exp=",power.law2$exponent);tlog(4,msg);tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
	}
	
	################## discrete log-normal law
	msg <- "Handling log-normal law";tlog(2,msg);msgs <- c(msgs, paste0("..",msg))
	log.normal <- dislnorm$new(data)		# create the model
	log.normal$setXmin(power.law$getXmin())	# set min x based on power law
	est <- estimate_pars(log.normal)		# estimate parameters
	log.normal$setPars(est)
	if(!is.na(plot.file))					# possibly plot model
	{	# pdf
		lines(log.normal, col="GREEN", lwd=2)
#		# ccdf
#		x <- seq(from=log.normal$xmin, to=max(data), by=(max(data)-log.normal$xmin)/100)
#		y <- 1 - c(0, dist_cdf(log.normal, x[-length(x)]))
#		lines(x[-length(x)], y[-length(y)], col="GREEN", lwd=2)
	}
	comp.ln <- compare_distributions(power.law, log.normal)
	msg <- paste0("Test statistic: ",comp.ln$test_statistic, " p-value: ", comp.ln$p_two_sided);tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
	tab[1,C_LNORM_CLR] <- comp.ln$test_statistic
	tab[1,C_LNORM_CPVAL] <- comp.ln$p_two_sided
	# alternative, with library pli
	log.normal2 <- tryCatch(expr=fit.lnorm.disc(x=data, threshold=power.law$xmin), 
			error=function(e) NA)
	if(all(is.na(log.normal2)) || all(is.na(power.law2)))
	{	msg <- "ERROR could not fit the log-normal law (for alt. test statistic)";tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
	}
	else
	{	comp.ln2 <- vuong(zeta.lnorm.llr(x=data, zeta.d=power.law2, lnorm.d=log.normal2))
		msg <- paste0("Alt. Test statistic: ",comp.ln2$loglike.ratio, " p-value: ", comp.ln2$p.two.sided);tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
	}
	
	################## discrete exponential law
	msg <- "Handling discrete exponential law";tlog(2,msg);msgs <- c(msgs, paste0("..",msg))
	exp.law <- disexp$new(data)				# create the model
	exp.law$setXmin(power.law$getXmin())	# set min x based on power law
	est <- estimate_pars(exp.law)			# estimate parameters
	exp.law$setPars(est)
	if(!is.na(plot.file))					# possibly plot model
	{	# pdf
		lines(exp.law, col="ORANGE", lwd=2)
#		# ccdf
#		x <- seq(from=exp.law$xmin, to=max(data), by=(max(data)-exp.law$xmin)/100)
#		y <- 1 - c(0, dist_cdf(exp.law, x[-length(x)]))
#		lines(x[-length(x)], y[-length(y)], col="ORANGE", lwd=2)
	}
	comp.el <- compare_distributions(power.law, exp.law)
	msg <- paste0("Test statistic: ",comp.el$test_statistic, " p-value: ", comp.el$p_two_sided);tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
	tab[1,C_EXPO_CLR] <- comp.el$test_statistic
	tab[1,C_EXPO_CPVAL] <- comp.el$p_two_sided
	# alternative, with library pli
	exp.law2 <- tryCatch(expr=discexp.fit(x=data, threshold=power.law$xmin), 
		error=function(e) NA)
	if(all(is.na(exp.law2)) || all(is.na(power.law2)))
	{	msg <- "ERROR could not fit the discrete exponential law (for alt. test statistic)";tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
	}
	else
	{	comp.el2 <- vuong(zeta.exp.llr(x=data, zeta.d=power.law2, exp.d=exp.law2))
		msg <- paste0("Alt. Test statistic: ",comp.el2$loglike.ratio, " p-value: ", comp.el2$p.two.sided);tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
	}
	
	################## poisson law
	msg <- "Handling Poisson law";tlog(2,msg);msgs <- c(msgs, paste0("..",msg))
	pois.law <- dispois$new(data)			# create model
	pois.law$setXmin(power.law$getXmin())	# set min x based on power law
	est <- estimate_pars(pois.law)			# estimate parameters
	pois.law$setPars(est)
	if(!is.na(plot.file))					# possibly plot model
	{	# pdf
		lines(pois.law, col="PURPLE", lwd=2)
#		# ccdf
#		x <- seq(from=pois.law$xmin, to=max(data), by=(max(data)-pois.law$xmin)/100)
#		y <- 1 - c(0, dist_cdf(pois.law, x[-length(x)]))
#		lines(x[-length(x)], y[-length(y)], col="PURPLE", lwd=2)
	}
	comp.pl <- compare_distributions(power.law, pois.law)
	msg <- paste0("Test statistic: ",comp.pl$test_statistic, " p-value: ", comp.pl$p_two_sided);tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
	tab[1,C_POIS_CLR] <- comp.pl$test_statistic
	tab[1,C_POIS_CPVAL] <- comp.pl$p_two_sided
	# alternative, with library pli
	pois.law2 <- tryCatch(expr=pois.tail.fit(x=data, threshold=power.law$xmin), 
			error=function(e) NA)
	if(all(is.na(pois.law2)) || all(is.na(power.law2)))
	{	msg <- "ERROR could not fit the Poisson law (for alt. test statistic)";tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
	}
	else
	{	comp.pl2 <- vuong(zeta.poisson.llr(x=data, zeta.d=power.law2, pois.d=pois.law2))
		msg <- paste0("Alt. Test statistic: ",comp.pl2$loglike.ratio, " p-value: ", comp.pl2$p.two.sided);tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
	}
	
	################## discrete weibull distribution
	msg <- "Handling discrete Weibull law";tlog(2,msg);msgs <- c(msgs, paste0("..",msg))
	if(!is.na(plot.file))					# possibly plot model
	{	# TODO
	}
	# only possibility is library pli
	weib.law2 <- tryCatch(expr=discweib.fit(x=data, threshold=power.law$xmin),
					error=function(e) NA)
	if(all(is.na(weib.law2)) || all(is.na(power.law2)))
	{	msg <- paste0("ERROR: could not fit the Weibull law");tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
	}
	else
	{	comp.wl2 <- vuong(zeta.weib.llr(x=data, zeta.d=power.law2, weib.d=weib.law2))
		msg <- paste0("Alt. Test statistic: ",comp.wl2$loglike.ratio, " p-value: ", comp.wl2$p.two.sided);tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
		tab[1,C_WEIB_CLR] <- comp.wl2$loglike.ratio
		tab[1,C_WEIB_CPVAL] <- comp.wl2$p.two.sided
	}
	
	################## discrete truncated power law
	msg <- "Handling discrete truncated power law";tlog(2,msg);msgs <- c(msgs, paste0("..",msg))
	# using Python
#	tmp <- test_pl_expcutoff(data, discrete=TRUE)
#	tab[1,C_TRUNC_CLR] <- tmp$stat
#	tab[1,C_TRUNC_CPVAL] <- tmp$pvalue
	if(!is.na(plot.file))					# possibly plot model
	{	# TODO
	}
	# only possibility is library pli
	trunc.law2 <- tryCatch(expr=discpowerexp.fit(x=data,threshold=power.law$xmin),
			error=function(e) NA)
	if(all(is.na(trunc.law2)) || all(is.na(power.law2)))
	{	msg <- paste0("ERROR: could not fit the discrete truncated power law");tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
	}
	else
	{	msg <- paste0("Parameters: x_min=",trunc.law2$xmin," exp=",trunc.law2$exponent);tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
		tab[1,C_TRUNC_EXP] <- trunc.law2$exponent
		comp.tl2 <- power.powerexp.lrt(power.d=power.law2, powerexp.d=trunc.law2)
		msg <- paste0("Alt. Test statistic: ",comp.tl2$log.like.ratio, " p-value: ", comp.tl2$p_value);tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
		tab[1,C_TRUNC_CLR] <- comp.tl2$log.like.ratio
		tab[1,C_TRUNC_CPVAL] <- comp.tl2$p_value
	}
	
	################## yule-simon distribution
	msg <- "Handling Yule-Simon distribution";tlog(2,msg);msgs <- c(msgs, paste0("..",msg))
	if(!is.na(plot.file))					# possibly plot model
	{	# TODO
	}
	# only possibility is library pli
	yusim.law2 <- tryCatch(expr=yule.fit(x=data, threshold=power.law$xmin),
			error=function(e) NA)
	if(all(is.na(yusim.law2)) || all(is.na(power.law2)))
	{	msg <- paste0("ERROR: could not fit the Yule-Simon distribution");tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
	}
	else
	{	comp.ys2 <- vuong(zeta.yule.llr(x=data, zeta.d=power.law2, yule.d=yusim.law2))
		msg <- paste0("Alt. Test statistic: ",comp.ys2$loglike.ratio, " p-value: ", comp.ys2$p.two.sided);tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
		tab[1,C_YUSIM_CLR] <- comp.ys2$loglike.ratio
		tab[1,C_YUSIM_CPVAL] <- comp.ys2$p.two.sided
	}
	
	################
	
	msg <- "-------------------------------";tlog(0,msg);msgs <- c(msgs, msg)
	msg <- "Interpretation of the distribution test:";tlog(0,msg);msgs <- c(msgs, msg)
	msg <- "  h_0: original data could have been drawn from the fitted distribution";tlog(0,msg);msgs <- c(msgs, msg)
	msg <- "  >> statistically significant if large enough";tlog(0,msg);msgs <- c(msgs, msg)
	msg <- "-------------------------------";tlog(0,msg);msgs <- c(msgs, msg)
	msg <- "Interpretation of the comparison test:";tlog(0,msg);msgs <- c(msgs, msg)
	msg <- "- The test statistic indicates whether the power-law (positive) or the other distribution (negative) is preferred";tlog(0,msg);msgs <- c(msgs, msg)
	msg <- "- The p-value indicates whether this sign is significant (small p)";tlog(0,msg);msgs <- c(msgs, msg)
	msg <- "- The one-sided value is order-dependent, the two-sided one is not (They seem to use the latter)";tlog(0,msg);msgs <- c(msgs, msg)
	
	# draw conclusion
	msg <- "-------------------------------";tlog(0,msg);msgs <- c(msgs, msg)
	tab[1,C_DECISION] <- make.decision.distr(tab, threshold=0.05)
	msg <- paste0("Conclusion: ", tab[1,C_DECISION]);tlog(0,msg);msgs <- c(msgs, msg)
	msg <- "-------------------------------";tlog(0,msg);msgs <- c(msgs, msg)
	
	if(!is.na(plot.file))
	{	# add legend to plot
		legend(
			x="bottomleft",
			legend=c("Power law", "Truncated Power Law", "Log-normal law", "Exponential law", "Poisson law"),
			fill=c("BLUE", "RED", "GREEN", "ORANGE", "PURPLE")
		)
		# and close plot file
		dev.off()
	}
	
	# record log as a separate file
	conx <- file(paste0(plot.file,"_log.txt"))
		writeLines(msgs,conx)
	close(conx)
	
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
# discrete: whether the data is discrete (TRUE) or continuous (FALSE).
#
# returns: stat and p-value.
#############################################################################################
test_pl_expcutoff <- function(data, discrete=TRUE)
{	msg <- "Handling power law within the Python library";tlog(2,msg);msgs <- c(msgs, paste0("..",msg))
	
	# import necessary tools
	library("reticulate")
	pl <- import("powerlaw")
	
	# fit power law (and other distributions)
	fit = pl$Fit(data, discrete=discrete)
	# display results
	msg <- paste0("Parameters: x_min=",fit$truncated_power_law$xmin," exp=",fit$truncated_power_law$alpha);tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
	
	# add to plot
	x <- seq(from=fit$truncated_power_law$xmin, to=max(data), by=(max(data)-fit$truncated_power_law$xmin)/100)
#	y <- x^(-fit$truncated_power_law$alpha) * exp(-fit$truncated_power_law$Lambda*x)
	y <- fit$truncated_power_law$pdf(x)
	lines(x, y, col="RED", lwd=2)
	
	# compare to other distributions
	tmp <- fit$distribution_compare("power_law", "lognormal")
	msg <- paste0("Compare with log-normal distribution: test stat=",tmp[[1]]," p=",tmp[[2]]);tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
	tmp <- fit$distribution_compare("power_law", "exponential")
	msg <- paste0("Compare with exponential distribution: test stat=",tmp[[1]]," p=",tmp[[2]]);tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
	tmp <- fit$distribution_compare("power_law", "stretched_exponential")
	msg <- paste0("Compare with stretched exponential distribution: test stat=",tmp[[1]]," p=",tmp[[2]]);tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
	tmp <- fit$distribution_compare("power_law", "truncated_power_law")
	msg <- paste0("Compare with truncated power law: test stat=",tmp[[1]]," p=",tmp[[2]]);tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
	
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
#make.decision.distr <- function(tab, threshold=0.01)
#{	# determine which distribution fits better than the power law
#	indist <- c()
#	better <- c()
#	if(C_POIS_PVAL %in% names(tab))
#	{	if(tab[1,C_POIS_CPVAL]<threshold)
#		{	if(tab[1,C_POIS_CLR]<0)
#				better <- c(better, "Poisson")
#		}
#		else
#			indist <- c(indist, "Poisson")
#	}
#	if(C_LNORM_PVAL %in% names(tab))
#	{	if(tab[1,C_LNORM_CPVAL]<threshold)
#		{	if(tab[1,C_LNORM_CLR]<0)
#				better <- c(better, "LogNormal")
#		}
#		else
#			indist <- c(indist, "LogNormal")
#	}
#	if(C_EXPO_PVAL %in% names(tab))
#	{	if(tab[1,C_EXPO_CPVAL]<threshold)
#		{	if(tab[1,C_EXPO_CLR]<0)
#				better <- c(better, "Exponential")
#		}
#		else
#			indist <- c(indist, "Exponential")
#	}
#	if(C_WEIB_PVAL %in% names(tab))
#	{	if(tab[1,C_WEIB_CPVAL]<threshold)
#		{	if(tab[1,C_WEIB_CLR]<0)
#				better <- c(better, "Weibull")
#		}
#		else
#			indist <- c(indist, "Weibull")
#	}
#	
#	# build result string
#	if(length(better)>0)
#		res <- paste(better, collapse=", ")
#	else
#	{	if(tab[1,C_PL_PVAL] > threshold)
#			indist <- c("PowerLaw", indist)
#		res <- paste(indist, collapse=", ")
#	}
#	return(res)
#}
make.decision.distr <- function(tab, threshold=0.01)
{	# power laws
	power <- tab[1,C_PL_PVAL] > threshold
	truncated <- !is.na(tab[1,C_TRUNC_CPVAL]) && tab[1,C_TRUNC_CPVAL]<threshold && tab[1,C_TRUNC_CLR]<0
	# other functions
	poisson <- !is.na(tab[1,C_POIS_CPVAL]) && tab[1,C_POIS_CPVAL]<threshold && !is.na(tab[1,C_POIS_CLR]) && tab[1,C_POIS_CLR]<0 && tab[1,C_POIS_CLR]<tab[1,C_TRUNC_CLR] 
	lognormal <- !is.na(tab[1,C_LNORM_CPVAL]) && tab[1,C_LNORM_CPVAL]<threshold && !is.na(tab[1,C_LNORM_CLR]) && tab[1,C_LNORM_CLR]<0 && tab[1,C_LNORM_CLR]<tab[1,C_TRUNC_CLR] 
	exponential <- !is.na(tab[1,C_EXPO_CPVAL]) && tab[1,C_EXPO_CPVAL]<threshold && !is.na(tab[1,C_EXPO_CLR]) && tab[1,C_EXPO_CLR]<0 && tab[1,C_EXPO_CLR]<tab[1,C_TRUNC_CLR] 
	weibull <- !is.na(tab[1,C_WEIB_CPVAL]) && tab[1,C_WEIB_CPVAL]<threshold && !is.na(tab[1,C_WEIB_CLR]) && tab[1,C_WEIB_CLR]<0 && tab[1,C_WEIB_CLR]<tab[1,C_TRUNC_CLR] 
	yule.simon <- !is.na(tab[1,C_YUSIM_CPVAL]) && tab[1,C_YUSIM_CPVAL]<threshold && !is.na(tab[1,C_YUSIM_CLR]) && tab[1,C_YUSIM_CLR]<0 && tab[1,C_YUSIM_CLR]<tab[1,C_TRUNC_CLR]
	flags <- c(poisson, lognormal, exponential, weibull, yule.simon)
	
	if(power)
	{	if(any(!is.na(flags) & flags))
			res <- "Moderate"
		else if(truncated)
			res <- "Truncated"
		else
			res <- "Good"
	}
	else
	{	if(any(!is.na(flags) & flags))
			res <- "None"
		else if(truncated)
			res <- "Truncated"
		else
			res <- "No fit"
	}
	
	return(res)
}
