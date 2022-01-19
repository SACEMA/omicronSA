
#' not including `tools` here, as it should be standard-install package
#' and is only used for one function
suppressPackageStartupMessages({
	stopifnot(all(sapply(c("data.table", "TMB"), require, character.only = TRUE)))
})

.debug <- c("2021-11-27", "2021-12-06")[2]
.args <- if (interactive()) c(
	file.path("analysis", "output", "sgtf.rds"),
	file.path("analysis", "input", "tmb.rda"),
	file.path("C", "logistic.so"),
	file.path("analysis", "output", .debug, "fit.rds")
) else commandArgs(trailingOnly = TRUE)

dt <- readRDS(.args[1])
end.date <- as.Date(basename(dirname(tail(.args, 1))))
load(.args[2])
symblib <- dynlib(tools::file_path_sans_ext(.args[3]))
dyn.load(symblib)

## fit (using all defaults)
fit <- tmb_fit(
	data = dt[
		date <= end.date, .(
			prov, time, reinf = reinfection, omicron = SGTF, tot = total
		)
	],
	two_stage = TRUE,
	upper = list(log_theta = 20),
	lower = NULL,
	map = list(),  ## no fixed params
	betabinom_param = "log_theta",
	debug_level = 0,
	browsing = FALSE
)

nsim <- 500
set.seed(8675309)
## need covariance matrix/random values for both fixed & random effects
pop_vals <- MASS::mvrnorm(
	nsim,
	mu = coef(fit, random = TRUE),
	Sigma = vcov(fit, random =TRUE)
)

deltar_mat <- exp(
	pop_vals[, colnames(pop_vals) == "b_logdeltar", drop = F] *
	exp(pop_vals[, "logsd_logdeltar"]) +
	pop_vals[, "log_deltar"]
)

colnames(deltar_mat) <- sprintf("deltar.%s", get_prov_names(fit))

loc_vals <- pop_vals[, colnames(pop_vals) %like% "loc", drop = F]

shape_regex <- "^log_(theta|sigma)$"
if (sum(grepl(shape_regex,
			  colnames(pop_vals))) != 1) {
	stop(sprintf("columns '%s' missing or non-unique",
				 shape_regex))
}

beta_shape <- exp(pop_vals[, grepl(shape_regex, colnames(pop_vals)), drop = F])

if("beta_reinf" %in% colnames(pop_vals)){
	reinf_mat <- 
		pop_vals[, colnames(pop_vals) == "b_reinf", drop = F] *
		exp(pop_vals[,"logsd_reinf"]) +
		pop_vals[,"beta_reinf"]
	colnames(reinf_mat) <- sprintf("reinf.%s", get_prov_names(fit))
} else {
	reinf_mat <- c()
}

prov_vals <- dcast(melt(as.data.table(cbind(
	deltar_mat, loc_vals, reinf_mat
))[, sample := 1:.N ], id.vars = "sample")[,
	c("variable", "prov") := tstrsplit(variable, split = ".", fixed = TRUE)
], prov + sample ~ variable, value.var = "value")
prov_vals[, beta_shape := beta_shape, by = prov ]

saveRDS(tt, tail(.args, 1))
