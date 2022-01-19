library(TMB) ## still need it to operate on TMB objects
library(tidyr)
library(dplyr)

.args <- if (interactive()) c(
	file.path("C", "logistic.so"),
	file.path("analysis", "input", "tmb.rda"),
	file.path("analysis", "output", "thing.rds"),
	file.path("analysis", "output", "thing2.rds")
) else commandArgs(trailingOnly = TRUE)

library(shellpipes)
rpcall("btfake.sg.tmb_ensemble.Rout tmb_ensemble.R btfake.sg.ltfit.tmb_fit.rds tmb_funs.rda logistic.so")



fit <- rdsRead()
loadEnvironments()
soLoad()

nsim <- 500
set.seed(101)
## need covariance matrix/random values for both fixed & random effects
pop_vals <- as.data.frame(MASS::mvrnorm(nsim
	, mu = coef(fit, random = TRUE)
	, Sigma = vcov(fit, random =TRUE)
))

## reconstruct deltar for each province
## FIXME: tidy this (rowwise?)
deltar_mat <- t(apply(pop_vals, MARGIN = 1
	, function(x) {
		return(exp(
			exp(x[["logsd_logdeltar"]])*x[names(x) == "b_logdeltar"] 
			+ x[["log_deltar"]]
		))
	}
))
colnames(deltar_mat) <- get_prov_names(fit)

deltar_vals <- (deltar_mat
	|> as.data.frame()
	|> mutate(sample_no = 1:n())
	|> pivot_longer(-sample_no,
					names_to = "prov",
					values_to = "deltar")
)

loc_vals <- (pop_vals
	|> select(starts_with("loc"))
	|> rename_with(stringr::str_remove, pattern = "loc\\.")
	|> mutate(sample_no = 1:n())
	|> pivot_longer(-sample_no,
					names_to = "prov",
					values_to = "loc")
)

shape_regex <- "^log_(theta|sigma)$"
if (sum(grepl(shape_regex,
              colnames(pop_vals))) != 1) {
    stop(sprintf("columns '%s' missing or non-unique",
                 shape_regex))
}
beta_shape <- (pop_vals
    |> as.data.frame()
    |> select(ll = matches(shape_regex))
    ## select & rename
    |> transmute(beta_shape = exp(ll))
    |> mutate(sample_no = 1:n())
)

all_vals <- (deltar_vals
	|> full_join(loc_vals, by = c("prov", "sample_no"))
	|> full_join(beta_shape, by = "sample_no")
)

if("beta_reinf" %in% names(pop_vals)){
	reinf_mat <- t(apply(pop_vals, MARGIN = 1
		, function(x) {
			return(
				exp(x[["logsd_reinf"]])*x[names(x) == "b_reinf"] 
				+ x[["beta_reinf"]]
			)
		}
	))
	colnames(reinf_mat) <- get_prov_names(fit)

	reinf_vals <- (reinf_mat
		|> as.data.frame()
		|> mutate(sample_no = 1:n())
		|> pivot_longer(-sample_no
			, names_to = "prov"
			, values_to = "reinf"
		)
	)
	all_vals <- (all_vals
		|> full_join(reinf_vals, by = c("prov", "sample_no"))
		|> mutate(reloc = loc - reinf/deltar)
	)
}

summary(all_vals)

rdsSave(all_vals)
