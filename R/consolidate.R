
suppressPackageStartupMessages({
    require(data.table)
})

.debug <- c("2021-12-06", "2021-11-27")[1]
.args <- if (interactive()) c(
  file.path("analysis","output","omicron_ratios"),
  .debug,
  file.path("analysis","output",.debug,"omicron_ratios.rds")
) else commandArgs(trailingOnly = TRUE)

tarend <- .args[2]
fls <- grep(tarend, list.files(
  .args[1], "estimate_samples.rds", full.names = TRUE, recursive = TRUE
), value = TRUE)

subsample <- 100; start.date <- "2021-10-15"
consolidated <- rbindlist(lapply(fls, function(fl) {
  tmp <- tail(strsplit(dirname(fl), "/")[[1]], 3)
  regionsamp <- strsplit(tmp[2], "_")[[1]]
  scenario <- tmp[1]
  readRDS(fl)[
    sample <= subsample
  ][
    date >= start.date
  ][variable == "R", .(
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
  c("ratio", "ratiolow", "ratiolower") := .(omicron / delta, omicronlow / delta, omicronlower / delta)
]

saveRDS(res.dt, tail(.args, 1))
