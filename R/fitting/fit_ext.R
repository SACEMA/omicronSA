
.pkgs <- c("dplyr", "data.table", "TMB")
stopifnot(all(sapply(.pkgs, require, character.only = TRUE)))

.debug <- c("2021-11-27", "2021-12-06")[2]
.args <- if (interactive()) c(
	file.path("analysis", "input", "sgtf.rds"),
	file.path("analysis", "input", "tmb_ext.rda"),
	file.path("C", "logistic.so"),
	file.path("analysis", "output", .debug, "fit_ext.rds")
) else commandArgs(trailingOnly = TRUE)

dt <- readRDS(.args[1])
end.date <- as.Date(basename(dirname(tail(.args, 1))))
load(.args[2])
symblib <- dynlib(tools::file_path_sans_ext(.args[3]))
dyn.load(symblib)

## fit (using all defaults)
fit <- tmb_fit(data = as_tibble(dt),
	two_stage = TRUE,
	upper = list(log_theta = 20),
	lower = NULL,
	map = list(),  ## no fixed params
	betabinom_param = betabinom_param,
	debug_level = 0
)

dyn.unload(symblib)

saveRDS(fit, tail(.args, 1))
