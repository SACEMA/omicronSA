
suppressPackageStartupMessages({
	require(data.table)
})

.debug <- "50" # how many samples to make
.args <- if (interactive()) c(
	file.path("analysis", "input", "sgtf.rds"),
	file.path("analysis", "output"),
	.debug[1],
	file.path("analysis", "input", "rtslurmref.txt")
) else commandArgs(trailingOnly = TRUE)

provref <- readRDS(.args[1])[, unique(prov)]
dateref <- basename(grep("2021", list.dirs(.args[2], recursive = FALSE), value = TRUE))
samples <- formatC(1:as.integer(.args[3]), width = nchar(.args[3]), flag = "0") 

pathfmt <- file.path(.args[2], "%s", "%s_%s_rt.rds")

ref.dt <- as.data.table(expand.grid(
	prov = provref, date = dateref, sample = samples
))

ref.dt[, path := sprintf(pathfmt, date, prov, sample) ]

fwrite(ref.dt[,.(path)], tail(.args, 1), col.names = FALSE)