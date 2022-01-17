
.args <- if (interactive()) {
	file.path("analysis", "input", "simDates.rda")
} else commandArgs(trailingOnly = TRUE)

simStart <- as.Date("2021-10-01")
simEnd   <- as.Date("2021-12-06")
zeroDate <- as.Date("2021-09-01")

save(simStart, simEnd, zeroDate, file = tail(.args, 1))