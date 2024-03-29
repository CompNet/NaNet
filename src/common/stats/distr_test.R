#############################################################################################
# Functions used to test data distribution.
# 
# 05/2021 Vincent Labatut
#
# source("src/common/stats/distr_test.R")
#############################################################################################
# Installation of pli: The C code must be compiled. 
# On a Windows system:
#  1) download and install MinGW and MSYS using the installer https://sourceforge.net/projects/mingw/files/Installer/
#  2) add to path (C:\MinGW\bin, C:\MinGW\msys\1.0\bin)
#  3) open terminal, go to C code folder
#  4) compile simple C file: gcc discpowerexp.c -o discpowerexp.exe
#  5) move resulting executable file to res/common/stats/pli folder
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

source("src/common/stats/pli/discexp.R")
source("src/common/stats/pli/disclnorm.R")
source("src/common/stats/pli/discpowerexp.R")
source("src/common/stats/pli/discweib.R")
source("src/common/stats/pli/exp.R")
source("src/common/stats/pli/lnorm.R")
source("src/common/stats/pli/pareto.R")
source("src/common/stats/pli/poisson.R")
source("src/common/stats/pli/powerexp.R")
source("src/common/stats/pli/powerexp-exponential-integral.R")
source("src/common/stats/pli/power-law-test.R")
source("src/common/stats/pli/weibull.R")
source("src/common/stats/pli/yule.R")
source("src/common/stats/pli/zeta.R")




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

# law names
LAW_NAME_PL <- "Power Law"   
LAW_NAME_TRUNC <- "Truncated Power Law" 
LAW_NAME_LNORM <- "Log-Normal Law" 
LAW_NAME_EXPO <- "Exponential Law" 
LAW_NAME_WEIB <- "Weibull Law" 
LAW_NAME_POIS <- "Poisson Law" 
LAW_NAME_YUSIM <- "Yule-Simon Law"

# colors
LAW_COLORS <- c()
LAW_COLORS[LAW_NAME_PL] <- "BLUE"
LAW_COLORS[LAW_NAME_TRUNC] <- "RED"
LAW_COLORS[LAW_NAME_LNORM] <- "GREEN"
LAW_COLORS[LAW_NAME_EXPO] <- "ORANGE"
LAW_COLORS[LAW_NAME_WEIB] <- "PURPLE"
LAW_COLORS[LAW_NAME_POIS] <- "MAGENTA"
LAW_COLORS[LAW_NAME_YUSIM] <- "CHOCOLATE"




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
	foos <- list()
	msgs <- c()
	msg <- "Test data distribution";tlog(0,msg);msgs <- c(msgs, msg)
	if(any(data==0))	# just to avoid zeroes or negative values, which prevent fitting certain distributions
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
	foos[[LAW_NAME_PL]] <- power.law
	# possibly plot model
	if(!is.na(plot.file))
	{	pdf(paste0(plot.file,".pdf"), width=15, height=15)
		plot(power.law, 
			col="BLACK",
			xlab=xlab, ylab="Probability Density"
		)
		add.line.plot(data=data, model=power.law, col=LAW_COLORS[LAW_NAME_PL], lwd=2)
	}
	msg <- paste0("Parameters: x_min=",power.law$xmin," exp=",power.law$pars);tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
	tab[1,C_PL_EXP] <- power.law$pars
	tab[1,C_PL_XMIN] <- power.law$xmin
	# bootstrap test
	pl.bs <- tryCatch(expr={bootstrap_p(power.law, no_of_sims=sims, threads=8)}, 
			error=function(e) NA)
	if(is.na(pl.bs))
	{	msg <- "ERROR could not apply the bootstrap test to the estimated power law";tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
	}
	else
	{	msg <- paste0("p-value for power law: ",pl.bs$p);tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
		tab[1,C_PL_PVAL] <- pl.bs$p
	}
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
	foos[[LAW_NAME_LNORM]] <- log.normal
	# possibly plot model
	if(!is.na(plot.file))
		add.line.plot(data=data, model=log.normal, col=LAW_COLORS[LAW_NAME_LNORM], lwd=2)
	# comparison
	comp.ln <- tryCatch(expr={compare_distributions(power.law, log.normal)}, 
			error=function(e) NA)
	if(is.na(comp.ln))
	{	msg <- paste0("ERROR: could not perform the comparison test");tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
	}
	else
	{	msg <- paste0("Test statistic: ",comp.ln$test_statistic, " p-value: ", comp.ln$p_two_sided);tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
		tab[1,C_LNORM_CLR] <- comp.ln$test_statistic
		tab[1,C_LNORM_CPVAL] <- comp.ln$p_two_sided
	}
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
	foos[[LAW_NAME_EXPO]] <- exp.law
	# possibly plot model
	if(!is.na(plot.file))
		add.line.plot(data=data, model=exp.law, col=LAW_COLORS[LAW_NAME_EXPO], lwd=2)
	# comparison
	comp.el <- tryCatch(expr={compare_distributions(power.law, exp.law)}, 
			error=function(e) NA)
	if(is.na(comp.el))
	{	msg <- paste0("ERROR: could not perform the comparison test");tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
	}
	else
	{	msg <- paste0("Test statistic: ",comp.el$test_statistic, " p-value: ", comp.el$p_two_sided);tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
		tab[1,C_EXPO_CLR] <- comp.el$test_statistic
		tab[1,C_EXPO_CPVAL] <- comp.el$p_two_sided
	}
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
		foos[[LAW_NAME_WEIB]] <- weib.law
		# possibly plot model
		if(!is.na(plot.file))
			add.line.plot(data=data, model=weib.law, col=LAW_COLORS[LAW_NAME_WEIB], lwd=2)
		# comparison
		weib.el <- tryCatch(expr={compare_distributions(power.law, weib.law)},
			error=function(e) NA)
		if(is.na(weib.el))
		{	msg <- paste0("ERROR: could not perform the comparison test");tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
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
	{	if(is.null(foos[["Weibull Law"]])) 
		{	weib.law2$xmin <- power.law$xmin
			foos[[LAW_NAME_WEIB]] <- weib.law2
			# possibly plot model
			if(!is.na(plot.file))
				add.line.plot(data=data, model=weib.law2, col=LAW_COLORS[LAW_NAME_WEIB], lwd=2)
		}
		weib.el2 <- vuong(pareto.weibull.llr(x=data, pareto.d=power.law2, weibull.d=weib.law2))
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
	{	foos[[LAW_NAME_TRUNC]] <- trunc.law2
		# possibly plot model
		if(!is.na(plot.file))
			add.line.plot(data=data, model=trunc.law2, col=LAW_COLORS[LAW_NAME_TRUNC], lwd=2)
		msg <- paste0("Parameters: x_min=",trunc.law2$threshold," exp=",trunc.law2$exponent," rate=",trunc.law2$rate);tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
		tab[1,C_TRUNC_EXP] <- trunc.law2$exponent
		# comparison
		comp.tl2 <- tryCatch(expr={power.powerexp.lrt(power.d=power.law2, powerexp.d=trunc.law2)},
				error=function(e) NA)
		if(is.na(comp.tl2))
		{	msg <- paste0("ERROR: could not perform the comparison test");tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
		}
		else
		{	msg <- paste0("Alt. Test statistic: ",comp.tl2$log.like.ratio, " p-value: ", comp.tl2$p_value);tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
			tab[1,C_TRUNC_CLR] <- comp.tl2$log.like.ratio
			tab[1,C_TRUNC_CPVAL] <- comp.tl2$p_value
		}
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
	tmp <- make.decision.distr(tab, threshold=0.01)
	tab[1,C_DECISION] <- tmp$dec
	msg <- paste0("Conclusion: ", tab[1,C_DECISION]);tlog(0,msg);msgs <- c(msgs, msg)
	msg <- "-------------------------------";tlog(0,msg);msgs <- c(msgs, msg)
	
	if(!is.na(plot.file))
	{	# add legend to plot
		lnames <- c(LAW_NAME_PL, LAW_NAME_TRUNC, LAW_NAME_LNORM, LAW_NAME_EXPO, LAW_NAME_WEIB)
		legend(
			x="bottomleft",
			legend=lnames,
			fill=LAW_COLORS[lnames]
		)
		# and close plot file
		dev.off()
	}
	
	# record log as a separate file
	if(!is.na(plot.file))
	{	log.file <- paste0(plot.file,"_log.txt")
		tlog(2,"Recording log in file '",log.file,"'")
		conx <- file(log.file, encoding="UTF-8")
			writeLines(msgs,conx)
		close(conx)
	}
	
	# get distrib functions
	if(all(is.na(tmp$laws)))
		laws <- NA
	else
		laws <- foos[tmp$laws]
	
	if(return_stats)
		res <- list(stats=tab, laws=laws)
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
	foos <- list()
	msgs <- c()
	msg <- "Test data distribution";tlog(0,msg);msgs <- c(msgs, msg)
	if(any(data==0))	# just to avoid zeroes or negative values, which prevent fitting certain distributions
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
	foos[[LAW_NAME_PL]] <- power.law
	if(!is.na(plot.file))				# possibly plot model
	{	pdf(paste0(plot.file,".pdf"), width=15, height=15)
		plot(power.law, 
			col="BLACK",
			xlab=xlab, ylab="Probability Density"
		)
		add.line.plot(data=data, model=power.law, col=LAW_COLORS[LAW_NAME_PL], lwd=2)
	}
	msg <- paste0("Parameters: x_min=",power.law$xmin," exp=",power.law$pars);tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
	tab[1,C_PL_EXP] <- power.law$pars
	tab[1,C_PL_XMIN] <- power.law$xmin
	# bootstrap test
	pl.bs <- tryCatch(expr={bootstrap_p(power.law, no_of_sims=sims, threads=8)}, 
			error=function(e) NA)
	if(is.na(pl.bs))
	{	msg <- "ERROR could not apply the bootstrap test to the estimated power law";tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
	}
	else
	{	msg <- paste0("p-value for power law: ",pl.bs$p);tlog(4,msg);tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
		tab[1,C_PL_PVAL] <- pl.bs$p
	}
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
	foos[[LAW_NAME_LNORM]] <- log.normal
	if(!is.na(plot.file))					# possibly plot model
		add.line.plot(data=data, model=log.normal, col=LAW_COLORS[LAW_NAME_LNORM], lwd=2)
	# comparison
	comp.ln <- tryCatch(expr={compare_distributions(power.law, log.normal)}, 
		error=function(e) NA)
	if(is.na(comp.ln))
	{	msg <- paste0("ERROR: could not perform the comparison test");tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
	}
	else
	{	msg <- paste0("Test statistic: ",comp.ln$test_statistic, " p-value: ", comp.ln$p_two_sided);tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
		tab[1,C_LNORM_CLR] <- comp.ln$test_statistic
		tab[1,C_LNORM_CPVAL] <- comp.ln$p_two_sided
	}
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
	foos[[LAW_NAME_EXPO]] <- exp.law
	if(!is.na(plot.file))					# possibly plot model
		add.line.plot(data=data, model=exp.law, col=LAW_COLORS[LAW_NAME_EXPO], lwd=2)
	# comparison
	comp.el <- tryCatch(expr={compare_distributions(power.law, exp.law)}, 
			error=function(e) NA)
	if(is.na(comp.el))
	{	msg <- paste0("ERROR: could not perform the comparison test");tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
	}
	else
	{	msg <- paste0("Test statistic: ",comp.el$test_statistic, " p-value: ", comp.el$p_two_sided);tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
		tab[1,C_EXPO_CLR] <- comp.el$test_statistic
		tab[1,C_EXPO_CPVAL] <- comp.el$p_two_sided
	}
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
	foos[[LAW_NAME_POIS]] <- pois.law
	if(!is.na(plot.file))					# possibly plot model
		add.line.plot(data=data, model=pois.law, col=LAW_COLORS[LAW_NAME_POIS], lwd=2)
	# comparison
	comp.pl <- tryCatch(expr={compare_distributions(power.law, pois.law)}, 
			error=function(e) NA)
	if(is.na(comp.pl))
	{	msg <- paste0("ERROR: could not perform the comparison test");tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
	}
	else
	{	msg <- paste0("Test statistic: ",comp.pl$test_statistic, " p-value: ", comp.pl$p_two_sided);tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
		tab[1,C_POIS_CLR] <- comp.pl$test_statistic
		tab[1,C_POIS_CPVAL] <- comp.pl$p_two_sided
	}
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
	{	foos[[LAW_NAME_WEIB]] <- weib.law2
		# possibly plot model
		if(!is.na(plot.file))
			add.line.plot(data=data, model=weib.law2, col=LAW_COLORS[LAW_NAME_WEIB], lwd=2)
		# comparison
		comp.wl2 <- tryCatch(expr={vuong(zeta.weib.llr(x=data, zeta.d=power.law2, weib.d=weib.law2))}, 
				error=function(e) NA)
		if(is.na(comp.wl2))
		{	msg <- paste0("ERROR: could not perform the comparison test");tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
		}
		else
		{	msg <- paste0("Alt. Test statistic: ",comp.wl2$loglike.ratio, " p-value: ", comp.wl2$p.two.sided);tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
			tab[1,C_WEIB_CLR] <- comp.wl2$loglike.ratio
			tab[1,C_WEIB_CPVAL] <- comp.wl2$p.two.sided
		}
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
	{	foos[[LAW_NAME_TRUNC]] <- trunc.law2
		# possibly plot model
		if(!is.na(plot.file))
			add.line.plot(data=data, model=trunc.law2, col=LAW_COLORS[LAW_NAME_TRUNC], lwd=2)
		msg <- paste0("Parameters: x_min=",trunc.law2$threshold," exp=",trunc.law2$exponent," rate=",trunc.law2$rate);tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
		tab[1,C_TRUNC_EXP] <- trunc.law2$exponent
		# comparison
		comp.tl2 <- tryCatch(expr={power.powerexp.lrt(power.d=power.law2, powerexp.d=trunc.law2)}, 
				error=function(e) NA)
		if(is.na(comp.tl2))
		{	msg <- paste0("ERROR: could not perform the comparison test");tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
		}
		else
		{	msg <- paste0("Alt. Test statistic: ",comp.tl2$log.like.ratio, " p-value: ", comp.tl2$p_value);tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
			tab[1,C_TRUNC_CLR] <- comp.tl2$log.like.ratio
			tab[1,C_TRUNC_CPVAL] <- comp.tl2$p_value
		}
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
	{	foos[[LAW_NAME_YUSIM]] <- yusim.law2
		# possibly plot model
		if(!is.na(plot.file))
			add.line.plot(data=data, model=yusim.law2, col=LAW_COLORS[LAW_NAME_YUSIM], lwd=2)
		# comparison
		comp.ys2 <- tryCatch(expr={vuong(zeta.yule.llr(x=data, zeta.d=power.law2, yule.d=yusim.law2))}, 
				error=function(e) NA)
		if(is.na(comp.ys2))
		{	msg <- paste0("ERROR: could not perform the comparison test");tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
		}
		else
		{	msg <- paste0("Alt. Test statistic: ",comp.ys2$loglike.ratio, " p-value: ", comp.ys2$p.two.sided);tlog(4,msg);msgs <- c(msgs, paste0("....",msg))
			tab[1,C_YUSIM_CLR] <- comp.ys2$loglike.ratio
			tab[1,C_YUSIM_CPVAL] <- comp.ys2$p.two.sided
		}
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
	tmp <- make.decision.distr(tab, threshold=0.05)
	tab[1,C_DECISION] <- tmp$dec
	msg <- paste0("Conclusion: ", tab[1,C_DECISION]);tlog(0,msg);msgs <- c(msgs, msg)
	msg <- "-------------------------------";tlog(0,msg);msgs <- c(msgs, msg)
	
	if(!is.na(plot.file))
	{	# add legend to plot
		lnames <- c(LAW_NAME_PL, LAW_NAME_TRUNC, LAW_NAME_LNORM, LAW_NAME_EXPO, LAW_NAME_WEIB, LAW_NAME_POIS, LAW_NAME_YUSIM)
		legend(
			x="bottomleft",
			legend=lnames,
			fill=LAW_COLORS[lnames]
		)
		# and close plot file
		dev.off()
	}
	
	# possibly record log as a separate file
	if(!is.na(plot.file))
	{	log.file <- paste0(plot.file,"_log.txt")
		tlog(2,"Recording log in file '",log.file,"'")
		conx <- file(log.file, encoding="UTF-8")
			writeLines(msgs,conx)
		close(conx)
	}
	
	# get distrib functions
	if(all(is.na(tmp$laws)))
		laws <- NA
	else
		laws <- foos[tmp$laws]
	
	if(return_stats)
		res <- list(stats=tab, laws=laws)
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
# returns: string representing the final decision, and the selected distribution (if any).
#############################################################################################
make.decision.distr <- function(tab, threshold=0.01)
{	# power laws
	power <- tab[1,C_PL_PVAL] > threshold
	truncated <- !is.na(tab[1,C_TRUNC_CPVAL]) && tab[1,C_TRUNC_CPVAL]<threshold && tab[1,C_TRUNC_CLR]<0
	# other functions
	flags <- c()
	flags[LAW_NAME_POIS]  <- !is.na(tab[1,C_POIS_CPVAL]) && tab[1,C_POIS_CPVAL]<threshold && !is.na(tab[1,C_POIS_CLR]) && tab[1,C_POIS_CLR]<0 && tab[1,C_POIS_CLR]<tab[1,C_TRUNC_CLR]
	flags[LAW_NAME_LNORM] <- !is.na(tab[1,C_LNORM_CPVAL]) && tab[1,C_LNORM_CPVAL]<threshold && !is.na(tab[1,C_LNORM_CLR]) && tab[1,C_LNORM_CLR]<0 && tab[1,C_LNORM_CLR]<tab[1,C_TRUNC_CLR] 
	flags[LAW_NAME_EXPO]  <- !is.na(tab[1,C_EXPO_CPVAL]) && tab[1,C_EXPO_CPVAL]<threshold && !is.na(tab[1,C_EXPO_CLR]) && tab[1,C_EXPO_CLR]<0 && tab[1,C_EXPO_CLR]<tab[1,C_TRUNC_CLR]
	flags[LAW_NAME_WEIB]  <- !is.na(tab[1,C_WEIB_CPVAL]) && tab[1,C_WEIB_CPVAL]<threshold && !is.na(tab[1,C_WEIB_CLR]) && tab[1,C_WEIB_CLR]<0 && tab[1,C_WEIB_CLR]<tab[1,C_TRUNC_CLR]
	flags[LAW_NAME_YUSIM] <- !is.na(tab[1,C_YUSIM_CPVAL]) && tab[1,C_YUSIM_CPVAL]<threshold && !is.na(tab[1,C_YUSIM_CLR]) && tab[1,C_YUSIM_CLR]<0 && tab[1,C_YUSIM_CLR]<tab[1,C_TRUNC_CLR]
	
	if(!is.na(power) && power)
	{	if(any(!is.na(flags) & flags))
		{	dec <- "Moderate"
			if(!is.na(truncated) && truncated)
				laws <- LAW_NAME_TRUNC
			else
				laws <- LAW_NAME_PL
			laws <- c(laws, names(flags)[!is.na(flags) & flags])
		}
		else if(!is.na(truncated) && truncated)
		{	dec <- "Truncated"
			laws <- LAW_NAME_TRUNC
		}
		else
		{	dec <- "Good"
			laws <- LAW_NAME_PL
		}
	}
	else
	{	if(any(!is.na(flags) & flags))
		{	dec <- "None"
			if(!is.na(truncated) && truncated)
				laws <- LAW_NAME_TRUNC
			else
				laws <- c()
			laws <- c(laws, names(flags)[!is.na(flags) & flags])
		}
		else if(!is.na(truncated) && truncated)
		{	dec <- "Truncated"
			laws <- LAW_NAME_TRUNC
		}
		else
		{	dec <- "No fit"
			laws <- NA
		}
	}
	
	res <- list(dec=dec, laws=laws)
	return(res)
}




#############################################################################################
# Adds a line to an existing plot, depending on the nature of the fit model.
#
# data: processed data.
# model: model fit to the data.
# ...: additional parameters fetched to the "lines" function.
#############################################################################################
add.line.plot <- function(data, model, ...)
{	attr <- attr(class(model),"package")
	if(!is.null(attr) && attr=="poweRlaw")
		lines(model, ...)
	else
	{	# init x
		cont <- c("weibull", "powerexp")
		disc <- c("discweib", "discpowerexp", "Yule")
		if(model$type %in% cont)
			x <- seq(model$xmin, max(data), (max(data)-model$xmin)/100)
		else if(model$type %in% disc)
			x <- seq(model$threshold, max(data))
		
		# init y (continuous)
		if(model$type=="weibull")
			y <- 1 - pweibull.tail(x=x, shape=model$shape, scale=model$scale, threshold=model$xmin)
		else if(model$type=="powerexp")
			y <- 1 - ppowerexp(x=x, exponent=model$exponent, rate=model$rate, threshold=model$xmin)
		
		# init y (discrete)
		else if(model$type=="discweib")
			y <- 1 - pdiscweib(x=x, shape=model$shape, scale=model$scale, threshold=model$threshold)
		else if(model$type=="discpowerexp")
			y <- 1 - cumsum(ddiscpowerexp(x=x, exponent=model$exponent, rate=model$rate, threshold=model$threshold))
		else if(model$type=="Yule")
			y <- 1 - pyule(x=x, alpha=model$exponent, xmin=model$threshold)
		
		# TODO Poisson and Yule-Simon seem to be plot higher than expected. pb in the cumulative function computation?
		
		lines(x, y, ...)
	}
}




#############################################################################################
# tests
#test.cont.distr(data=runif(100), xlab="Test", return_stats=TRUE, sims=1000, plot.file="Test")
#test.disc.distr(data=sample(1:10,100,replace=TRUE), xlab="Test", return_stats=TRUE, sims=1000, plot.file="Test")
