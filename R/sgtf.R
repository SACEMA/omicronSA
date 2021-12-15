
suppressPackageStartupMessages({
    require(data.table)
})

.args <- if (interactive()) c(
    file.path("refdata", "sgtf_ll.rds"),
    file.path("analysis", "input", "sgtf.rds")
) else commandArgs(trailingOnly = TRUE)

ref <- readRDS(.args[1])

ref[, outcome := c("nonSGTF","SGTF")[as.integer(sgtf)+1] ]
ref[, date := specreceiveddate ]
ref[, reinf := inf > 1 ]

res <- dcast(ref, prov + province + date + reinf + publicprivateflag ~ outcome, fun.aggregate = length)
res[, total := nonSGTF + SGTF ]

saveRDS(res, tail(.args, 1))
