
suppressPackageStartupMessages({
    require(data.table)
    require(EpiNow2)
})

.debug <- c("omicron", "delta", "omicronlow", "omicronlower")[4]
.args <- if (interactive()) {
  c(
    file.path("analysis", "output", "2021-12-06", "incidence_ensemble.rds"),
    file.path("analysis", "input", sprintf("%s.json", .debug[1]))
    file.path("analysis", "output", "omicron_ratios", .debug[1])
  )
} else {
  commandArgs(trailingOnly = TRUE)
}

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

shorteng <- function(shrink) list(
  mean = 0.7012403 + log(shrink),
  mean_sd = 0.05633224,
  sd = 0.6135941,
  sd_sd = 0.04874248,
  max = 14
)

shortinc <- shorteng(0.5)
shortgi <- gen_time(
  mean_generation_interval -
  exp(incubation_period$mean + incubation_period$sd^2 / 2) +
  exp(shortinc$mean + shortinc$sd^2 / 2)
)

shortestinc <- shorteng(0.5)
shortestgi <- gen_time(
  (mean_generation_interval -
  exp(incubation_period$mean + incubation_period$sd^2 / 2))/2 +
  exp(shortestinc$mean + shortestinc$sd^2 / 2)
)

est.ext <- 30

crs <- 4
smps <- 1e3

inc.dt <- as.data.table(readRDS(.args[1]))[prov == "GP"][between(sample,44,50)]
# inc.dt <- as.data.table(readRDS(.args[1]))[prov != "GP" & between(sample, 1, 38)]

src.dt <- inc.dt[,
  .(
    region = sprintf("%s_%i", prov, sample), date, var, ref = tot - var
  )
]

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

variant_type <- match.arg(basename(tail(.args, 1)), c("omicron", "omicronlow", "delta", "omicronlower"))
variable <- fcase(
  variant_type == "omicron", "var",
  variant_type == "omicronlow", "var",
  variant_type == "omicronlower", "var",
  variant_type == "delta", "ref"
)

gt <- if (
  variant_type == "omicronlow"
) { 
  shortgi 
} else if (
  variant_type == "omicronlower"
) { 
  shortestgi
} else generation_time

inc <- if (
  variant_type == "omicronlow"
) { 
  shortinc 
} else if (
  variant_type == "omicronlower"
) { 
  shortestinc
} else incubation_period

Rtcalc(
    src.dt[, .(region, date, confirm = get(variable))],
    target_folder = tail(.args, 1),
    log = variant_type,
 verbose = TRUE,
    gi = gt, ip = inc
)
