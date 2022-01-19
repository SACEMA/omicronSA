
#' WARNING sourcing this file will purge environment before creating
#' and saving these functions
rm(list = ls())

.args <- if (interactive()) file.path(
	"analysis", "input", "tmb.rda"
) else commandArgs(trailingOnly = TRUE)

## split a vector by (repeated) names and assign
## values to elements of an existing list
## (i.e., update a starting parameter list with the
## results of running optim()
##' @param orig original list of parameters
##' @param pars named vector of parameters (names not necessarily unique)
splitfun <- function(orig, pars) {
	for (n in unique(names(pars))) {
		orig[[n]] <- unname(pars[names(pars) == n])
	}
	return(orig)
}

##' safely get levels *or* unique values of a vector that
##' may or may not be a factor
get_names <- function(x) {
	if (!is.null(levels(x))) return(levels(x))
	return(unique(x))
}


##' disambiguate locations
##' @param x named object (matrix or vector)
##' @param names character vector to append to target names
##' @param fix_vars variables to disambiguate
fix_prov_names <- function(
	x,
	names = get_names(x$prov),
    fix_vars = "loc"
) {
    for (f in fix_vars) {
        target <- paste0("^", f)
        repl <- sprintf("%s.%s", f, names)
        #' TODO assumes repl length is matched
        if (!is.null(dim(x))) {
            colnames(x)[grepl(target, colnames(x))] <- repl
        } else {
            names(x)[grepl(target, names(x))] <- repl
        }
    }
    return(x)
}

anonymize_names <- function(x) {
    return(setNames(x, gsub("\\..*$","",names(x))))
}

#' TODO unused?          
##' turn on tracing for a TMB object
##' @param obj a TMB object (result of \code{MakeADFun})
##' @param trace should tracing be enabled?
# set_trace <- function(obj, trace = TRUE) {
# 		environment(obj$fn)$tracepar <- trace
# 		return(invisible(NULL))
# }

#' TODO unused?
## FIXME: separate coef.logistfit that sanitizes/disambiguates province-specific values?
# coef.TMB <- function(x, random = FALSE) {
# 		ee <- environment(x$fn)
# 		r <- ee$last.par.best
# 		rand <- ee$random
# 		if (!random && length(rand)>0) {
# 				r <- r[-rand]
# 		}
# 		return(r)
# }

#' TODO unused?
# vcov.TMB <- function(x, random = FALSE, use_numDeriv = FALSE) {
#     cc <- coef(x, random = random)
#     if (use_numDeriv) {
#         if (!require("numDeriv")) stop('need numDeriv package for TMB vcov')
#         H <- numDeriv::jacobian(func = x$gr,
#                                 x = cc)
#         ## fixme: robustify?
#         V <- solve(H)
#         nn <- names(cc)
#         dimnames(V) <- list(nn,nn)
#         return(V)
#     }
#     sdr <- get_sdr(x)
#     if (!random) return(sdr$cov.fixed)
#     return(solve(sdr$jointPrecision))
# }

#' TODO unused?
# logLik.TMB <- function(x) {
# 		## FIXME: include df? (length(coef(x)))?
# 		## is x$fn() safe (uses last.par) or do we need last.par.best ?
#     val <- -1*x$fn()
#     attr(val, "df") <- length(coef(x))
# 		return(val)
# }

#' TODO unused?
# print.TMB <- function(x) {
# 		cat("TMB model\n\nParameters:\n",x$par,"\n")
# 		return(invisible(x))
# }

## compute mean and SD of Gaussian prior from lower/upper bounds of
## confidence interval
prior_params <- function(lwr, upr, conf = 0.95) {
		m <- (lwr + upr)/2
		s <- (upr-m)/qnorm((1+conf)/2)
		c(mean = m, sd = s)
}

#' @param data data frame containing (at least) columns "prov", "time",
#' "omicron", "tot", "prop", and "reinf" (may be NA if reinf param is mapped to 0)
#' @param two_stage (logical) fit binomial model first?
#' @param start named list of starting values
#' @param upper named list of upper bounds
#' @param lower named list of lower bounds
#' @param priors named list of vectors of mean and sd for independent Gaussian priors on parameters
#' @param map list of parameters to be fixed to starting values (in the form of a factor with NA values for any elements in the vector to be fixed: see \code{map} argument of \code\link{MakeADFun}})
#' @param debug_level numeric specifying level of debugging
#' @param tmb_file name of tmb file to use
#' @param include_sdr compute sdreport and attach it to fitted object?
tmb_fit <- function(
	data,
	two_stage = TRUE, reinf_effect = NULL,
    betabinom_param = c("log_theta", "log_sigma"),
	start = list(
		log_deltar = log(0.1),
		lodrop = -4, logain = -7,
		beta_reinf = 0
	),
	upper = list(log_theta = 20),
	lower = list(logsd_logdeltar = -5),
	priors = list(
		logsd_logdeltar =	prior_params(log(0.01), log(0.3)),
        logsd_reinf = prior_params(-3, 3)
	),
    map = list(),
	debug_level = 0,
	tmb_file = NULL,
    include_sdr = TRUE,
    perfect_tests = FALSE,
	browsing = FALSE
) {
	if (browsing) browser()
	if(!is.null(tmb_file)) {
			TMB::compile(paste0(tmb_file, ".cpp"))
			dyn.load(dynlib(tmb_file))
	}

	data_vars <- c("prov", "time", "omicron", "tot", "reinf")

    betabinom_param <- match.arg(betabinom_param)
	if (!("beta_reinf" %in% names(start))) {
		warning("please add beta_reinf to your starting parameter list (set to zero for back-compatibility)")
		start$beta_reinf <- 0
	}

	tmb_pars_binom <- c(start, list(log_theta = NA_real_, log_sigma = NA_real_))
	## make sure 'prov' is a factor (TMB doesn't auto-convert)
	data$prov <- factor(data$prov)
	has_reinf <- "reinf" %in% names(data)
	if (is.null(reinf_effect)) {
			reinf_effect <- has_reinf
	}
	if (reinf_effect && !has_reinf) {
			stop("reinf effect specified, but reinf missing in data")
	}
	if (!reinf_effect && has_reinf) {
			warning("reinf in data, but no reinf effect specified")
	}
	np <- length(levels(data$prov))
	
	## TMB wants a reinf variable, even if it's ignored (i.e. non-reinf case)
	if (is.null(data[["reinf"]])) {
			data[["reinf"]] <- 1
	}

    if (!reinf_effect) {
		## fix reinf to starting value (== 0 by default)
		map <- c(map, list(
			beta_reinf = factor(NA),
        	b_reinf = factor(rep(NA, np)),
            logsd_reinf = factor(NA)
        ))
	}

	tmb_data <- c(
		data[, data_vars, with = F], list(
		nprov = np, debug = debug_level,
        perfect_tests = perfect_tests
	))
	
	if (!is.null(priors)) {
			for (nm in names(priors)) {
					tmb_data[[paste0("prior_",nm)]] <- priors[[nm]]
			}
	}
	loc_start <- mean(data$time)
	nRE <- 1 ## FIXME: need to reuse/adapt/adjust if we have correlated REs
	tmb_pars_binom <- c(
		tmb_pars_binom, list(
		loc = rep(loc_start, np),
		b_logdeltar = rep(0, np),
        b_reinf = rep(0, np),
		logsd_logdeltar = -1,
        logsd_reinf = -1
	))

	binom_args <- list(
		data = tmb_data,
		parameters = tmb_pars_binom,
		random = c("b_logdeltar", "b_reinf"),
		## inner.method = "BFGS",
		inner.control = list(
			maxit = 1000,
			fail.action = rep("warning", 3)
		),
		map = c(map, list(log_theta = factor(NA), log_sigma = factor(NA))),
		silent = TRUE
	)

	if (two_stage) {
		tmb_binom <- do.call(MakeADFun, binom_args)
		r0 <- tmb_binom$fn()
		stopifnot(is.finite(r0))
		## Fit!
		## Important to use something derivative-based (optim()'s default is
		##	Nelder-Mead, which wastes the effort spent in doing autodiff ...
		## TMB folks seem to like nlminb() but not clear why
		t1 <- system.time(
				tmb_binom_opt <- with(tmb_binom, optim(par = par, fn = fn, gr = gr, method = "BFGS",
																							 control = list(trace = 10)))
		)
		## 0.6 seconds
		class(tmb_binom) <- c("TMB")
		## FIXME: check for inner-optimization failure here, return with meaningful error
	}
		## update binomial args for beta-binomial case
		betabinom_args <- binom_args
		## don't 'map' log_theta (dispersion) any more; set starting value to 0
    if (two_stage) betabinom_args$parameters <- splitfun(binom_args$parameters, tmb_binom_opt$par)
    if (betabinom_param == "log_theta") {
        betabinom_args$map$log_theta <- NULL
        betabinom_args$parameters$log_theta <- 0
    } else {
        betabinom_args$map$log_sigma <- NULL
        betabinom_args$parameters$log_sigma <- 0
    }
	tmb_betabinom <- do.call(MakeADFun, betabinom_args)
	uvec <- Inf ## default: optim will replicate as necessary
	#' TODO DRY by function extraction
	if (!is.null(upper)) {
			uvec <- tmb_betabinom$par
			uvec[] <- Inf ## set all upper bounds to Inf (default/no bound)
			for (nm in names(upper)) {
					uvec[[nm]] <- upper[[nm]]
			}
	}
	lvec <- -Inf
	if (!is.null(lower)) {
			lvec <- tmb_betabinom$par
			lvec[] <- -Inf
			for (nm in names(lower)) {
					lvec[[nm]] <- lower[[nm]]
			}
	}
		
	tmb_betabinom_opt <- with(
		tmb_betabinom, optim(
			par = par, fn = fn, gr = gr, method = "L-BFGS-B",
			control = list(), ## trace = 1),
			upper = uvec, lower = lvec
		)
	)
	
	return(mklogistfit(tmb_betabinom, tmb_file, get_names(data$prov), include_sdr = include_sdr))
}

## extract original data frame from TMB object
#' TODO remove tidyverse elements
get_data <- function(x) {
		dd <- x$env$data
		L <- lengths(dd)
		dd <- (dd[L == max(L)]
				|> as.data.frame()	 ## not tibble (collapsing list)
        |> dplyr::mutate(dplyr::across(prov, factor, labels = get_prov_names(x)))
				|> tibble::as_tibble()
		)
		return(dd)
}


##' add class and file attributes
##' @param x fitted TMB object
##' @param tmb_file name of tmb file (without extension) used in fitting
##' @param prov_names (character vector) province names
##' @param include_sdr include sdreport as an attribute (could save time downstream) ?
## FIXME: what should the class be called?
## FIXME: do we need to carry province names downstream if we've already fixed the parameter vector?
mklogistfit <- function(x, tmb_file, prov_names, include_sdr = FALSE) {
	attr(x, "tmb_file") <- tmb_file
	attr(x, "prov_names") <- prov_names
	## fix up province name vector
	x$env$last.par.best <- fix_prov_names(
		x$env$last.par.best,
		prov_names
	)
    if (include_sdr) {
        attr(x, "sdr") <- sdreport(x, getJointPrecision = TRUE)
    }
	class(x) <- c("logistfit", "TMB")
	return(x)
}

get_tmb_file <- function(x) { attr(x, "tmb_file") }

get_prov_names <- function(x) { attr(x, "prov_names") }

##' this is specific to logistic fits, not generic TMB machinery
##' deep-copy TMB object, then modify data within it appropriately,
##' call TMB::sdreport() on the modified object
##' by default, expands time x province list (and reinf 0/1?) and generates
##' predicted values (and Wald CIs on the log scale)
##' @param fit a fitted model
##' @param newdata data frame for prediction (should include province, time, reinf(?)); if NULL, automatic expansion is done;
##' if NA, no replacement is done
##' @param include_reinf expand prediction frame over reinf status?
##' @param new parameters to substitute (only used for simulate at the moment)
##' @param simulate (logical)
##' @param perfect_tests predict/simulate with perfect specificity/sensitivity of SGTF for omicron detection?
##' @param confint (logical): return full data frame with province/time/probability/CIs (TRUE), or just a vector of predicted probabilities (FALSE)? (The latter is much faster, and appropriate for ensembles)
predict.logistfit <- function(
	fit, newdata = NULL,
	include_reinf = uses_reinf(fit),
	newparams = NULL, simulate = FALSE,
	perfect_tests = FALSE,
	confint = TRUE
) {

    ## FIXME: currently assumes models were fitted _without_ perfect testing,
    ## i.e. that perfect testing is being assumed for prediction/ensemble purposes only

    n_orig <- max(lengths(fit$env$data))

    if (!is.null(newdata) && !is.data.frame(newdata) && !is.na(newdata)) {
        stop("newdata should be NULL, NA, or a data frame")
    }
    old_data <- !is.null(newdata) && is.na(newdata)
    ## if using new data *or* perfect testing *or* new params (including RE) + predict, need to re-run MakeADFun
    mknew <- (!old_data || perfect_tests || (!simulate && !is.null(newparams)))
    remake_adfun <- confint && mknew
    map <- fit$env$map
    if (old_data && !perfect_tests) {
        newdata <- fit$env$data
    } else {
        ## need to reconstruct data
        if (is.null(newdata)) {
            ## FIXME: pass include_reinf? check uses_reinf() internally?
            newdata <- mk_completedata(fit, expand = remake_adfun)
        }
        if (perfect_tests) {
            newdata$perfect_tests <- 1
            map <- c(map, list(lodrop = factor(NA), logain = factor(NA)))
        }
        if (is.null(newparams)) newparams <- fit$env$last.par.best
        random <- fit$env$random
        ## FIXME: allow switch to substitute fixed-only
        ## in that case (and *if* confint == TRUE) newparams could be put passed through to sdreport 'par.fixed' arg
        ##  rather than hacking environment
    }
    if (!simulate) {
        newparams_vec <- newparams
        newparams <- anonymize_names(newparams)
        newparams <- split(newparams, names(newparams))
        if (!confint) {
            pred_env <- c(newdata, newparams)
            b_deltar <- with(pred_env, exp(log_deltar + exp(logsd_logdeltar)*b_logdeltar))
            if (!uses_reinf(fit)) {
                pn <- get_prov_names(fit)
                b_reinfvec <- setNames(rep(0, length(pn)), pn)
            } else {
                ## reinfvec to disambiguate from beta_reinf in params list
                b_reinfvec <- with(pred_env, beta_reinf + exp(logsd_reinf)*b_reinf)
            }
            ## do this in R (sigh)
            if (perfect_tests) {
                ss2 <- with(
                	pred_env,
                    plogis(b_deltar[prov]*(time-loc[prov]) + b_reinfvec[prov]*reinf)
                )
            } else {
                ss2 <- with(
                	pred_env,
                    baselogis(time, loc[prov], b_deltar[prov],
                                      lodrop, logain, b_reinfvec[prov]*reinf))
            }
        } else {
            ## restore parameters that got left out because of mapping
            ## use original names in case we have added to map in the meantime
            for (nm in names(fit$env$map)) {
                newparams[[nm]] <- attr(fit$env$param.map[[match(nm, names(fit$env$map))]],
                              "shape")
            }
            newfit <- MakeADFun(data = newdata,
                                parameters = newparams, 
                                random.start = newparams_vec[random],
                                map = map)
            newfit$fn()
            rr <- sdreport_split(newfit)
            L <- lengths(newdata)
            newdata <- newdata[L == max(L)]  ## return long/time-varying parms only (not flags etc.)
            #' TODO remove tidyverse dependency
            ss2 <- (newdata
                |> tibble::as_tibble()
                |> dplyr::mutate(
                              prov = factor(prov,
                                            labels = get_prov_names(fit)),
                              pred = plogis(rr$value$loprob),
                              pred_lwr = plogis(rr$value$loprob - 1.96*rr$sd$loprob),
                              pred_upr = plogis(rr$value$loprob + 1.96*rr$sd$loprob))
            )
            ## expanded data: need to drop old values
            if (nrow(ss2) > n_orig) {
                ss2 <- ss2[-(1:n_orig),]
            }
                
        }
    } else {
        if (is.null(newparams)) {
            ss2 <- newfit$simulate()$omicron
        } else {
            ss2 <- newfit$simulate(newparams)$omicron
        }
    }
    return(ss2)
}

## generate new data (all crosses of time/province, possibly reinf status),
## filling in holes (FIXME: call this complete_newdata() ?)
mk_completedata <- function(fit, expand = FALSE) {
    dd0 <- get_data(fit)
    n_orig <- max(lengths(dd0))
    args <- with(dd0, list(prov = unique(prov), time = unique(time)))
    if (uses_reinf(fit)) {
        args <- c(args, list(reinf = 0:1))
    }
    newdata <- do.call(expand.grid, args)
    n <- nrow(newdata)
    dd <- fit$env$data ## all data
    for (nm in names(dd)) {
        if (nm %in% names(newdata)) {
            dd[[nm]] <- newdata[[nm]]
            if (expand) {
                dd[[nm]] <- c(dd0[[nm]], dd[[nm]])
            }
        } else {
            L <- length(dd[[nm]])
            if (L == n_orig) {
                if (nm == "reinf") {
                    ## model uses reinf, but include_reinf not specified
                    if (uses_reinf(fit)) warning("setting reinf to 0 (STUB)")
                    dd[[nm]] <- rep(0, nrow(newdata))
                    if (expand) {
                        dd[[nm]] <- c(dd0$reinf, dd[[nm]])
                    }
                } else {
                    ## other variables aren't used (we think)
                    dd[[nm]] <- rep(NA_real_, nrow(newdata))
                    if (expand) {
                        dd[[nm]] <- c(dd0[[nm]], dd[[nm]])
                    }
                } ## not reinf
            } ## variable to replace
        }
    } ## loop over names(dd)
    ## set to re-sanitize
    attr(dd, "check.passed") <- FALSE
    return(dd)
}
    
## FIXME: this is the clever 'change head and re-evaluate' trick.
##  better to refactor 'base-pred' into one component that modifies the object data/parms
## (if necessary) and another that predicts or simulates ??
simulate.logistfit <- function(fit, newdata = NULL, include_reinf = uses_reinf(fit),
                               newparams = NULL) {
    mc <- match.call()
    mc[[1]] <- quote(predict)
    mc$simulate <- TRUE
    eval.parent(mc)
}

## compute sdreport and split by name
sdreport_split <- function(fit, newparams = NULL) {
    rr <- sdreport(fit, par.fixed = newparams)
    nm <- names(rr$value)
    rr$value <- split(rr$value, nm)
    rr$sd <- split(rr$sd, nm)
    return(rr)
}

uses_reinf <- function(fit) {
    !("beta_reinf" %in% names(fit$env$map))
}


## factor in order **except** for first level
## (current use doesn't really require collapsing via vapply() ...)
mk_order <- function(x, v, anchor = "overall", FUN = mean) {
    v0 <- v[x != anchor]
    x0 <- x[x != anchor]
    u0 <- unique(x0)
    ord2 <- order(vapply(split(v0, x0), FUN, numeric(1), na.rm = TRUE))
    x <- factor(x, levels = c(anchor, u0[ord2]))
}

##' return information from fit on population-level estimate and province-level values
##' @param fit fitted model
##' @param vnm name of top-level parameter (overall value)
##' @param vec_nm name of vector as returned by REPORT()/SDREPORT()
## FIXME: 
get_prov_params <- function(fit, vnm = "log_deltar",
                       vec_nm = paste0(vnm, "_vec"))
{
    rr <- sdreport_split(fit)
    v <- c(coef(fit)[[vnm]],
           rr$value[[vec_nm]]
           )
    s <- c(
        ## FIXME: re-extracting vcov is a little slow. Store it with the object?
        sqrt(diag(vcov(fit)))[[vnm]],
        rr$sd[[vec_nm]]
    )
    res <- (tibble::tibble(
        prov = c("overall", get_prov_names(fit)),
        value = exp(v),
        lwr = exp(v - 1.96*s),
        upr = exp(v + 1.96*s))
        |> dplyr::mutate(across(prov, mk_order, value))
    )
    return(res)
}

## get sdreport if already stored, otherwise compute it
## FIXME: tradeoff for computing joint precision by default?
get_sdr <- function(fit) {
    if (!is.null(sdr <- attr(fit, "sdr"))) return(sdr)
    return(sdreport(fit, getJointPrecision = TRUE))
}

## Logistic function with imperfect testing
baselogis <- function(tvec, loc, delta_r, lodrop, logain, intercept = 0) {
    drop <- plogis(lodrop)
    gain <- plogis(logain)

    ptrue <- plogis((tvec-loc)*delta_r + intercept)
    return(ptrue*(1-gain) + (1-ptrue)*drop)
}

save(list = ls(), file = tail(.args, 1))
