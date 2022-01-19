
suppressPackageStartupMessages({
	require(data.table)
})

.debug <- sprintf("sgtf%s.rds", c("","_hold","_trim","_90","_60","_30")[1])
.args <- if (interactive()) file.path(
	"analysis", c(
		file.path("input", c(.debug, "simDates.rda")),
		file.path("output", .debug)
)) else commandArgs(trailingOnly = TRUE)

dt <- readRDS(.args[1])
load(.args[2])

saveRDS(dt[, .(
	prov = factor(prov), date,
	time = as.integer(date-zeroDate),
	#' n.b. warning might apply if higher-resolution date info provided
	#' assumes day-scale resolution
	reinfection = (infection == "reinfection"),
	nonSGTF, SGTF, total, propSGTF = SGTF/total
)], tail(.args, 1))
