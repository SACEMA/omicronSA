
suppressPackageStartupMessages({
    require(bbmle)
    require(data.table)
})

.debug <- c("2021-12-06", "2021-11-27")[1]
.args <- if (interactive()) c(
    file.path("analysis", "output", "sgtf", .debug, "mergedfit.rds"),
    file.path("analysis", "output", "sgtf", .debug, "sims.rds")
) else commandArgs(trailingOnly = TRUE)

fits <- readRDS(.args[1])

set.seed(1202)

samples <- 100
lozero <- -Inf ## Set misreads to zero to estimate true values
timevec <- 0:90

baselogis <- function(
    tvec, # observation times
    loc, delta_r, lodrop,
    # fitting: offset, relative growth rate, drop
    logain # fixed: gain
){
    drop <- plogis(lodrop)
    gain <- plogis(logain)
    ptrue <- plogis((tvec-loc)*delta_r)
    
    return(ptrue*(1-gain) + (1-ptrue)*drop)
}

simlist <- rbindlist(lapply(fits, function(fit) {
  	mod <- fit$m
  	pps <- try(pop_pred_samp(mod, return_all=TRUE, n=samples))
	## print(pps)
	if (any(class(pps)=="try-error")) return(NULL)

  	return(as.data.table(pps)[, sample := 1:.N ][,.(
  	  time = timevec, prop = baselogis(timevec, loc, delta_r, lozero, lozero)  
  	), by=sample])
}), idcol = "prov")

#' code smell: magic constant
dateZero <- as.Date("2021-09-24")
simlist[, date := dateZero + time ]
simlist$time <- NULL

saveRDS(simlist, tail(.args, 1))
