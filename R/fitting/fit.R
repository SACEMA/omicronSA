
#' not including `tools` here, as it should be standard-install package
#' and is only used for one function
.pkgs <- c("data.table", "TMB")
stopifnot(all(sapply(.pkgs, require, character.only = TRUE, quietly = TRUE)))

.debug <- c("2021-11-27", "2021-12-06")[2]
.args <- if (interactive()) c(
	file.path("analysis", "input", "sgtf.rds"),
	file.path("analysis", "input", "tmb.rda"),
	file.path("C", "logistic.so"),
	file.path("analysis", "output", .debug, "fit.rds")
) else commandArgs(trailingOnly = TRUE)

dt <- readRDS(.args[1])
end.date <- as.Date(basename(dirname(tail(.args, 1))))
load(.args[2])
symblib <- dynlib(tools::file_path_sans_ext(.args[3]))
dyn.load(symblib)

#' based on subsequent steps, the EC province does not have
#' sufficient data to use for estimation
#' hence, we exclude it at this stage, and rebase line the factors
dt <- dt[prov != "EC"][,
	prov := factor(prov, levels = setdiff(levels(prov), "EC"), ordered = TRUE)
]

## fit (using all defaults)
fit <- tmb_fit(
	data = dt[
		date <= end.date, .(
			prov, time, reinf, omicron, tot
		)
	]
)

dyn.unload(symblib)

saveRDS(fit, tail(.args, 1))
