
.pkgs <- c("data.table", "jsonlite", "EpiNow2")
stopifnot(all(sapply(.pkgs, require, character.only = TRUE, quietly = TRUE)))

.dtdebug <- c("2021-11-27", "2021-12-06")[2]
.debug <- c("omicron", "delta", "omicronredlat", "omicronredinf")[1]
.args <- if (interactive()) c(
	file.path("analysis", "output", .dtdebug[1], "incidence_ensemble.rds"),
	file.path("analysis", "input", sprintf("%s.json", .debug[1])),
	"GP", "01",
	file.path("analysis", "output", .dtdebug[1], .debug[1], sprintf("%s_%s_rt.rds", "GP", "01"))
) else {
  commandArgs(trailingOnly = TRUE)
}

scenario <- basename(tools::file_path_sans_ext(.args[2]))
pars <- read_json(.args[2])
parsvar <- match.arg(
	gsub("^.*(omicron|delta).*$" ,"\\1", scenario),
	c("omicron", "delta")
)

variable <- c(omicron = "var", delta = "ref")[parsvar]

tarprov <- .args[3]
tarsamp <- as.integer(.args[4])

crs <- 4
smps <- 2e3

#' want a total of 

src.dt <- as.data.table(
	readRDS(.args[1])
)[
	(prov == tarprov) & (sample == tarsamp) # filter to the relevant sample
][,
	.(date, confirm = get(variable)) # transform to EpiNow2 inputs
][
	which.max(confirm > 0):.N # remove leading zeros
]

gt <- pars$gi
inc <- pars$inc

Rtcalc <- function(
    case.dt,
    gp = gp_opts(),
    rt = rt_opts(),
    gi, ip, verbose = interactive(),
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
        control = list(adapt_delta = 0.99, max_treedepth = 20),
        max_execution_time = 60*20,
        seed = tarsamp,
        return_fit = FALSE
    ),
    gp = gp,
    verbose = verbose,
    horizon = 0,
    ...
)

#' convert time out warnings to errors at this stage
#' otherwise, record as stan fitting warnings
res <- Rtcalc(
	src.dt, gi = gt, ip = inc
)

#' only keep a subset of samples; do the calculation with sufficient
#' samples to evaluate mixing etc, but synthesizing many parameter
#' combinations
ret <-setkey(res$samples[
	(sample <= 1000) & (variable == "R"), .(
		sample, date, Rt = value
	)
], sample, date)

saveRDS(ret, tail(.args, 1))

gc()