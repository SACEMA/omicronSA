
suppressPackageStartupMessages({
    require(data.table)
})

.args <- if (interactive()) c(
    file.path("refdata", "sgtf.csv"),
    file.path("analysis", "input", "sgtf.rds")
) else commandArgs(trailingOnly = TRUE)

ref <- fread(.args[1])[, .(nonSGTF = sum(nonSGTF), SGTF = sum(SGTF)), by=.(prov, date)]
ref[, total := nonSGTF + SGTF ]

saveRDS(ref, tail(.args, 1))
