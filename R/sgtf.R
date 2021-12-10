
suppressPackageStartupMessages({
    require(data.table)
})

.args <- if (interactive()) c(
    file.path("refdata", "sgtf_ll.rds"),
    file.path("analysis", "input", "sgtf.rds")
) else commandArgs(trailingOnly = TRUE)

ref <- readRDS(.args[1])[province != ""][!is.na(sgtf)]

res <- dcast(ref, province + specreceiveddate ~ sgtf, fun.aggregate = length)
setnames(res, c("0", "1"), c("nonSGTF","SGTF"))
res[, total := nonSGTF + SGTF ]

saveRDS(res, tail(.args, 1))
