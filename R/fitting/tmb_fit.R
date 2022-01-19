
suppressPackageStartupMessages({
	stopifnot(all(sapply(c("data.table","TMB"), require, character.only = TRUE)))
})

.args <- if (interactive()) c(
	file.path("analysis", "output", "sgtf.rds"),
	file.path("analysis", "input", "tmb.rda"),
	file.path("C", "logistic"),
	file.path("analysis", "output", "thing.rds")
) else commandArgs(trailingOnly = TRUE)

dt <- readRDS(.args[1])
load(.args[2])
symblib <- dynlib(.args[3])
dyn.load(symblib)

## fit (using all defaults)
tt <- tmb_fit(
	data = dt[, .(prov, time, reinf = reinfection, omicron = SGTF, tot = total)],
	two_stage = TRUE,
	upper = list(log_theta = 20),
	lower = NULL,
	map = list(),  ## no fixed params
	betabinom_param = "log_theta",
	debug_level = 0,
	browsing = FALSE
)

saveRDS(tt, tail(.args, 1))
