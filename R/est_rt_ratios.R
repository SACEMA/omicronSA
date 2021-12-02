
suppressPackageStartupMessages({
    require(data.table)
    require(EpiNow2)
})

.args <- if (interactive()) file.path(
    "analysis",
    c("input", "input", "input", "output"),
    c("timing.rds", "incidence.rds", "frequencies.rds", "omicron_ratios")
) else commandArgs(trailingOnly = TRUE)

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

est.ext <- 30

crs <- 4
smps <- 1e3

time.dt <- readRDS(.args[1])

freq <- as.data.table(readRDS(.args[3]))
freq <- freq[sample < 5]

inc.dt <- readRDS(.args[2])[
  freq, on = .(date, province), allow.cartesian = TRUE, nomatch = 0][,
  var := rbinom(.N, tot, est_prop)
]

time.dt[wave == "omicron" & !is.na(start),
    # inc.dt[, .(edate = max(date)), by=province], on=.(province),
    end := start + 6
]

src.dt <- inc.dt[
  time.dt,
  on = .(province),
  nomatch = 0,
  .(
    region = sprintf("%s_%i", abbr, sample), date, var, ref = tot - var,
    breakpoint = between(date, start, end)
  )
]

#' @examples
#' p <- ggplot(src.dt[region == "GAUTENG"]) + aes(date) +
#'     geom_line(aes(y=tot, color = "total")) +
#'     geom_line(aes(y=tot-inf1, color="reinf")) +
#'     theme_minimal(base_size = 16) +
#'     scale_x_date(NULL, date_breaks = "week", date_labels = "%b %d") +
#'     scale_y_continuous("Incidence", trans = "log2") + 
#'     theme(legend.position = c(0, 1), legend.justification = c(0, 1)) +
#'     scale_color_discrete(NULL)
#'
#' ggsave("spim-gauteng.png", p, width = 14, height = 7, dpi = 600)
#'
#' p2 <- ggplot(src.dt[region == "GAUTENG"]) + aes(date) +
#'     geom_line(aes(y=tot, color = "total")) +
#'     geom_line(aes(y=vartot, color="variant")) +
#'     theme_minimal(base_size = 16) +
#'     scale_x_date(NULL, date_breaks = "week", date_labels = "%b %d") +
#'     scale_y_continuous("Incidence", trans = "log2") +
#'     theme(legend.position = c(0, 1), legend.justification = c(0, 1)) +
#'     scale_color_discrete(NULL)
#'
#' ggsave("spim-gauteng-var.png", p2, width = 14, height = 7, dpi = 600)

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

#' (region == "GAUTENG" & wave %in% c("delta","omicron"))

#' TODO split these into separate make steps?
Rtcalc(
    src.dt[, .(region, date, confirm = ref)],
    target_folder = file.path(tail(.args, 1), "delta"),
    log = "delta"
)

Rtcalc(
    src.dt[, .(region, date, confirm = var)],
    target_folder = file.path(tail(.args, 1), "omicron"),
    log = "omicron"
)

Rtcalc(
    src.dt[, .(region, date, confirm = var)],
    gi = shortgi, ip = shortinc,
    target_folder = file.path(tail(.args, 1), "omicronlow"),
    log = "omicronlow"
)
