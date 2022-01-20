
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

dyn.unload(symblib)

saveRDS(fit, tail(.args, 1))
