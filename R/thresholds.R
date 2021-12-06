
suppressPackageStartupMessages({
    require(data.table)
})

.args <- if (interactive()) c(
    file.path("analysis", "input", "timing.rds"),
    file.path(
        "analysis",
        "output",
        c("omicron_ratios.rds", "ngm_ratios.rds")
    ),
    file.path("analysis", "output", "thresholds.rds")
) else commandArgs(trailingOnly = TRUE)

timing <- readRDS(.args[1])

omiratios.dt <- readRDS(.args[2])[
    timing, on = .(region = abbr)][
    between(date, start, start + 6)][,
    .(ratio = exp(mean(log(ratio))),
      ratiolow = exp(mean(log(ratiolow)))
    ),
    by = .(region, freq_sample, rt_sample)
]
ngmref.dt <- readRDS(.args[3])
omisub <- omiratios.dt[,
    .SD[sample(.N, ngmref.dt[, max(epi_sample)])],
    by = .(region)
]
omisub[, epi_sample := 1:.N, by = region]

comps <- rbind(
    ngmref.dt[
        ngmref.dt[
          between(immune_escape, 0.145, 0.155),
            .SD,
            .SDcols = -c("immune_escape")],
            on = .(epi_sample, sero, province)
    ][,
    ngmratio := multiplier / i.multiplier][,
    delesc := "0"],
    ngmref.dt[
        ngmref.dt[
            between(immune_escape, 0.245, 0.255),
            .SD,
            .SDcols = -c("immune_escape")
        ],
        on = .(epi_sample, sero, province)
    ][,
    ngmratio := multiplier / i.multiplier][,
    delesc := "ref"]
)

scan.dt <- comps[omisub, on = .(province = region, epi_sample), nomatch = 0]

scan.dt[,
    transmissibility := ratio / ngmratio][,
    transmissibilitylo := ratiolow / ngmratio
]

res <- melt(
    scan.dt,
    id.vars = c("epi_sample", "sero", "province", "immune_escape", "delesc"),
    measure.vars = c("transmissibility", "transmissibilitylo")
)

saveRDS(res, tail(.args, 1))
