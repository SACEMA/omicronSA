
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

fixed_pars <- c("lodrop","logain","log_theta")

cc <- coef(fit,random=TRUE)

print(names(cc))

quit()

new_cc <- cc[-fixed_pars_position]

new_vcov <- vcov(fit, random = TRUE)[-fixed_pars_position,-fixed_pars_position]

new_pop_vals <- as.data.frame(MASS::mvrnorm(nsim
		, mu = new_cc
		, Sigma = new_vcov
		)
)

print(cc[fixed_pars_position])

fixed_pars_df <- t(as.data.frame(cc[fixed_pars_position]))[rep(1,nsim),]
	
rownames(fixed_pars_df) <- NULL
colnames(fixed_pars_df) <- names(cc[fixed_pars_position])
new_pop_vals <- (bind_cols(new_pop_vals,fixed_pars_df)
)

## Need to work on the names
print(new_pop_vals)

print(dim(new_pop_vals))

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
if (sum(grepl(shape_regex, colnames(pop_vals))) != 1) {
	stop(sprintf("columns '%s' missing or non-unique", shape_regex))
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
