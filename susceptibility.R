
suppressPackageStartupMessages({
    require(data.table)
})

.args <- if (interactive()) c(
    file.path("refdata",c("escapable.rds", "non-reinfectable.rds")),
    file.path("analysis", "input", "susceptibility.rds")
) else commandArgs(trailingOnly = TRUE)

proj.dt <- readRDS(.args[1])[
    readRDS(.args[2]),
    on=.(province, time, age),
    .(province, date=time, age, escapable, non_reinfectable)
]

#' covidm NGM calculation uses 16 instead of 7 age categories,
#' so have to smear protection estimates
AR.dt <- proj.dt[, .(
    date = rep(date[1], 16),
    agegrp = 1:16,
    escapable = escapable[c(rep(1,3), rep(2,4), rep(3,5), 4:7)],
    non_reinfectable = non_reinfectable[c(rep(1,3), rep(2,4), rep(3,5), 4:7)]
),
    by=.(province, date)
]

saveRDS(AR.dt, tail(.args, 1))
