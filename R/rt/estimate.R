
suppressPackageStartupMessages({
    require(data.table)
    require(jsonlite)
    require(EpiNow2)
})

.dtdebug <- c("2021-11-27", "2021-12-06")[2]
.debug <- c("omicron", "delta", "omicronredlat", "omicronredinf")[2]
.args <- if (interactive()) c(
	file.path("analysis", "output", .dtdebug[1], "incidence_ensemble.rds"),
	file.path("analysis", "input", sprintf("%s.json", .debug[1])),
	"GP", "01",
	file.path("analysis", "output", "omicron_ratios", .debug[1], sprintf("%s_%s_rt.rds", "GP", "01"))
) else {
  commandArgs(trailingOnly = TRUE)
}

pars <- read_json(.args[2])
parsvar <- match.arg(gsub("^.*(omicron|delta).*$"
	,"\\1", basename(tools::file_path_sans_ext(.args[2]))
), c("omicron", "delta"))

variable <- c(omicron = "var", delta = "ref")[parsvar]

tarprov <- .args[3]
tarsamp <- as.integer(.args[4])

#' on this date, significant Omicron announcement
#' followed by more extensive testing, likely to artificially increase case
#' growth
#' see: XXXXX

est.ext <- 30

crs <- 4
smps <- 1e3

src.dt <- as.data.table(
	readRDS(.args[1])
)[prov == tarprov][sample == tarsamp][, .(date, confirm = get(variable)) ]
# inc.dt <- as.data.table(readRDS(.args[1]))[prov != "GP" & between(sample, 1, 38)]

gt <- pars$gi
inc <- pars$inc

Rtcalc <- function(
    case.dt,
    gp = gp_opts(),
    rt = rt_opts(),
    gi, ip, verbose = interactive(),
    log = "rt",
    ...
) estimate_infections(
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
    ...
)

res <- Rtcalc(
    src.dt,
	verbose = interactive(),
    gi = gt, ip = inc
)

ret <- setkey(res$samples[variable == "R", .(sample, date, Rt=value) ], sample, date)

saveRDS(ret, tail(.args, 1))