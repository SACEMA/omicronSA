
suppressPackageStartupMessages({
    require(data.table)
})

.args <- if (interactive()) c(
	file.path("analysis", "input", "tmb.rda"),
    file.path("refdata", "sgtf.csv"),
    file.path("analysis", "input", "sgtf.rds")
) else commandArgs(trailingOnly = TRUE)

load(.args[1])
dt <- fread(.args[2])

ref <- setkey(dt[, .(
	prov = factor(prov, levels = names(regionkey), ordered = TRUE), date,
	time = as.integer(date-zeroDate),
	#' n.b. warning might apply if higher-resolution date info provided
	#' assumes day-scale resolution
	reinf = (infection == "reinfection"),
	delta = nonSGTF, omicron = SGTF, tot = nonSGTF + SGTF, propSGTF = SGTF/(nonSGTF + SGTF)
)], prov, date, reinf)

saveRDS(ref, tail(.args, 1))
