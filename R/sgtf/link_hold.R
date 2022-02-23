
suppressPackageStartupMessages({
    require(data.table)
})

.args <- if (interactive()) c(
    file.path("refdata", "sgtf_ll_90.rds"),
    file.path("refdata", "sgtf_ll_hold.rds")
) else commandArgs(trailingOnly = TRUE)

res.dt <- readRDS(.args[1])[,
	date := fifelse(sgtfdate < (date + 30), date, sgtfdate)
]

saveRDS(res.dt, tail(.args, 1))
