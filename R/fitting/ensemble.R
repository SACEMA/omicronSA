
.pkgs <- c("data.table", "TMB", "tools")
stopifnot(all(sapply(.pkgs, require, character.only = TRUE, quietly = TRUE)))

.debug <- c("2021-11-27", "2021-12-06")[2]
.args <- if (interactive()) c(
	file.path("analysis", "input", "tmb.rda"),
	file.path("analysis", "output", .debug, "fit.rds"),
	file.path("analysis", "output", .debug, "ensemble.rds")
) else commandArgs(trailingOnly = TRUE)

load(.args[1])
fit <- readRDS(.args[2])

nsim <- 100000
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
dimnames(beta_shape)[2] <- "beta_shape"

if("beta_reinf" %in% colnames(pop_vals)){
	reinf_mat <- 
		pop_vals[, colnames(pop_vals) %like% "b_reinf", drop = F] *
		exp(pop_vals[,"logsd_reinf"]) +
		pop_vals[,"beta_reinf"]
	colnames(reinf_mat) <- gsub("b_reinf", "reinf", colnames(reinf_mat))
} else {
	reinf_mat <- c()
}

err_mat <- pop_vals[, c("logain", "lodrop"), drop = F]

wide.dt <- as.data.table(cbind(
	deltar_mat, loc_vals, reinf_mat, err_mat, beta_shape
))[, sample := 1:.N ]

long.dt <- melt(
	wide.dt, id.vars = c("sample", colnames(err_mat), colnames(beta_shape))
)[, c("variable", "cprov") := tstrsplit(variable, split = ".", fixed = TRUE) ]
long.dt[, prov := factor(cprov, levels = get_prov_names(fit), ordered = TRUE)]

#' FIXME construct formula? prov, sample, colnames(...)
byprov.dt <- dcast(long.dt, prov + sample + logain + lodrop + beta_shape ~ variable, value.var = "value")

#' TODO possible to extract this from fit object?
med.dt <- dcast(
	long.dt[,.(value = median(value), logain = median(logain), lodrop = median(lodrop), beta_shape = median(beta_shape)), by=.(prov, variable)],
	prov + logain + lodrop + beta_shape ~ variable, value.var = "value"
)[, sample := NA_integer_ ]

saveRDS(setkeyv(rbind(byprov.dt[sample <= 1000], med.dt), key(byprov.dt)), tail(.args, 1))
