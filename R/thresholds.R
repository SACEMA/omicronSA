
suppressPackageStartupMessages({
    require(data.table)
})

.debug <- c("2021-11-27", "2021-12-06")[2]
.args <- if (interactive()) c(
    file.path("analysis", "input", "timing.rds"),
    file.path("analysis", "output", .debug, "omicron_ratios.rds"),
    file.path("analysis", "output", "ngm_ratios.rds")
    ,
    file.path("analysis", "output", .debug, "thresholds.rds")
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

delta_imm_escape <- c(lo=0.05, md=0.10, hi=0.15)

comps <- rbindlist(lapply(names(delta_imm_escape), function(ndimm) {
  dimm <- delta_imm_escape[ndimm]
  ngmref.dt[
    ngmref.dt[
      between(immune_escape, dimm-1e-3, dimm+1e-3),
      .SD,
      .SDcols = -c("immune_escape")],
    on = .(epi_sample, sero, province)
  ][,
    ngmratio := multiplier / i.multiplierNo
  ][,
    delesc := ndimm
  ]
}))

scan.dt <- comps[omisub, on = .(province = region, epi_sample), nomatch = 0]

scan.dt[,
  c("transmissibility", "transmissibilitylo") := .(ratio / ngmratio, ratiolow / ngmratio)
]

res <- melt(
    scan.dt,
    id.vars = c("epi_sample", "sero", "province", "immune_escape", "delesc"),
    measure.vars = c("transmissibility", "transmissibilitylo")
)

saveRDS(res, tail(.args, 1))
