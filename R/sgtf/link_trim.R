
suppressPackageStartupMessages({
    require(data.table)
})

.args <- if (interactive()) c(
    file.path("refdata", "sgtf_ll_90.rds"),
    file.path("refdata", "sgtf_ll_trim.rds")
) else commandArgs(trailingOnly = TRUE)

res.dt <- readRDS(.args[1])[sgtfdate < (date + 30)]

saveRDS(res.dt, tail(.args, 1))
