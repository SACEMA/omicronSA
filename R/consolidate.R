
suppressPackageStartupMessages({
    require(data.table)
})

.args <- if (interactive()) file.path(
    "analysis",
    c("output", "output"),
    c("omicron_ratios", "omicron_ratios.rds")
) else commandArgs(trailingOnly = TRUE)

fls <- grep("2021-11-27", list.files(
  .args[1], "estimate_samples.rds", full.names = TRUE, recursive = TRUE
), value = TRUE)

consolidated <- rbindlist(lapply(fls, function(fl) {
  tmp <- tail(strsplit(dirname(fl), "/")[[1]], 3)
  regionsamp <- strsplit(tmp[2], "_")[[1]]
  scenario <- tmp[1]
  readRDS(fl)[variable == "R", .(
    scenario = scenario,
    region = regionsamp[1],
    freq_sample = as.integer(regionsamp[2]),
    rt_sample = sample,
    date, value
  )]
}))

res.dt <- dcast(
  consolidated,
  region + freq_sample + rt_sample + date ~ scenario
)[,
  c("ratio", "ratiolow") := .(omicron / delta, omicronlow / delta)
]

saveRDS(res.dt, tail(.args, 1))
