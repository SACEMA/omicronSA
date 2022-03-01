
#' not including `tools` here, as it should be standard-install package
#' and is only used for one function
suppressPackageStartupMessages({
	stopifnot(all(sapply(c("data.table", "TMB"), require, character.only = TRUE)))
})

.debug <- c("2021-11-27", "2021-12-06")[2]
.args <- if (interactive()) c(
	file.path("analysis", "input", "tmb.rda"),
	file.path("analysis", "output", .debug, "fit.rds"),
	file.path("analysis", "output", .debug, "ensemble.rds")
) else commandArgs(trailingOnly = TRUE)

load(.args[1])
fit <- readRDS(.args[2])

nsim <- 1000
set.seed(42)
## need covariance matrix/random values for both fixed & random effects
pop_vals <- MASS::mvrnorm(
	nsim,
	mu = coef(fit, random = TRUE),
	Sigma = vcov(fit, random =TRUE)
)

deltar_mat <- exp(
	pop_vals[, colnames(pop_vals) %like% "b_logdeltar", drop = F] *
		exp(pop_vals[, "logsd_logdeltar"]) +
		pop_vals[, "log_deltar"]
)

colnames(deltar_mat) <- gsub("b_logdeltar", "deltar", colnames(deltar_mat))

loc_vals <- pop_vals[, colnames(pop_vals) %like% "loc", drop = F]

shape_regex <- "^log_(theta|sigma)$"
if (sum(grepl(shape_regex, colnames(pop_vals))) != 1) {
	stop(sprintf("columns '%s' missing or non-unique", shape_regex))
}

beta_shape <- exp(pop_vals[, grepl(shape_regex, colnames(pop_vals)), drop = F])

if("beta_reinf" %in% colnames(pop_vals)){
	reinf_mat <- 
		pop_vals[, colnames(pop_vals) %like% "b_reinf", drop = F] *
		exp(pop_vals[,"logsd_reinf"]) +
		pop_vals[,"beta_reinf"]
	colnames(reinf_mat) <- gsub("b_reinf", "reinf", colnames(reinf_mat))
} else {
	reinf_mat <- c()
}

wide.dt <- as.data.table(cbind(
	deltar_mat, loc_vals, reinf_mat
))[, sample := 1:.N ]

long.dt <- melt(
	wide.dt, id.vars = "sample"
)[, c("variable", "cprov") := tstrsplit(variable, split = ".", fixed = TRUE) ]
long.dt[, prov := factor(cprov, levels = get_prov_names(fit), ordered = TRUE)]

byprov.dt <- dcast(long.dt, prov + sample ~ variable, value.var = "value")
byprov.dt[, beta_shape := beta_shape, by = prov ]

saveRDS(byprov.dt, tail(.args, 1))
