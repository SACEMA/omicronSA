
suppressPackageStartupMessages({
    require(data.table)
    require(EpiNow2)
})

.args <- if (interactive()) {
  c(
    file.path("analysis", "output", "incidence.rds"),
    file.path("analysis", "output", "rt")
  )
} else {
  commandArgs(trailingOnly = TRUE)
}

inc.dt <- readRDS(.args[1])[date >= "2021-09-01"]

#' from covidm parameterization
mean_generation_interval <- 6.375559

gen_time <- function(meangi) {
  generation_time <- as.list(
    EpiNow2::generation_times[
      disease == "SARS-CoV-2", .(
        mean, mean_sd, sd, sd_sd, max = 30
      )]
  )

  tarmcv <- generation_time$mean_sd / generation_time$mean
  tarscv <- generation_time$sd_sd / generation_time$sd
  tarcv <- generation_time$sd / generation_time$mean

  generation_time$mean <- meangi
  generation_time$mean_sd <- generation_time$mean * tarmcv
  generation_time$sd <- generation_time$mean * tarcv
  generation_time$sd_sd <- generation_time$sd * tarscv
  generation_time
}

generation_time <- gen_time(mean_generation_interval)

#' also bootstrapped from covidm assumptions
incubation_period <- list(
    mean = 0.7012403,
    mean_sd = 0.05633224,
    sd = 0.6135941,
    sd_sd = 0.04874248,
    max = 14
)

crs <- 4
smps <- 1e3

Rtcalc <- function(
    case.dt,
    gp = gp_opts(),
    rt = rt_opts(),
    gi = generation_time, ip = incubation_period, verbose = FALSE,
    log = "rt",
    ...
) regional_epinow(
    reported_cases = case.dt,
    generation_time = gi,
    delays = delay_opts(ip),
    rt = rt,
    stan = stan_opts(
        samples = smps,
        warmup = 200,
        cores = crs,
        control = list(adapt_delta = 0.99, max_treedepth = 20)
    ),
    gp = gp,
    verbose = verbose,
    horizon = 0,
    output = "samples",
    logs = file.path("logs", log),
    ...
)

gt <- generation_time
inc <- incubation_period

tarcols <- setdiff(names(inc.dt), c("date", "province"))

lapply(tarcols, function(variable, tlim) {
  Rtcalc(
    inc.dt[date <= tlim, .(region=province, date, confirm = get(variable))],
    target_folder = file.path(tail(.args,1), variable),
    verbose = TRUE,
    gi = gt, ip = inc
  )
}, tlim = as.Date("2021-11-27"))

lapply(tarcols, function(variable, tlim) {
  Rtcalc(
    inc.dt[date <= tlim, .(region=province, date, confirm = get(variable))],
    target_folder = file.path(tail(.args,1), variable),
    verbose = TRUE,
    gi = gt, ip = inc
  )
}, tlim = inc.dt[, max(date)-3])
