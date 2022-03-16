
.pkgs <- c("data.table")
stopifnot(all(sapply(.pkgs, require, character.only = TRUE, quietly = TRUE)))

.debug <- "50" # how many samples to make
.args <- if (interactive()) c(
	file.path("analysis", "input", "sgtf.rds"),
	file.path("analysis", "output",
		rep(c("2021-11-27", "2021-12-06"), each = 4),
		rep(c("delta", "omicron", "omicronredlat", "omicronredinf"), times = 2)
	),
	.debug[1],
	file.path("analysis", "input", "rtslurmref.txt")
) else commandArgs(trailingOnly = TRUE)

provref <- readRDS(.args[1])[prov != "EC", unique(prov)]
scenref <- head(tail(.args, -1), -2) #' slice off first and last two args 
samples <- formatC(1:as.integer(tail(.args, 2)[1]), width = nchar(tail(.args, 2)[1]), flag = "0") 

pathfmt <- file.path("%s", "%s_%s_rt.rds")

ref.dt <- setkey(as.data.table(expand.grid(
	scenario = scenref, prov = provref, sample = samples
)), scenario, prov, sample)

ref.dt[, path := sprintf(pathfmt, scenario, prov, sample) ]

fwrite(ref.dt[,.(path)], tail(.args, 1), col.names = FALSE)