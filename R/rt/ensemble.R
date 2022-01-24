
suppressPackageStartupMessages({
    require(data.table)
})

.debug <- c("2021-11-27", "2021-12-06")[2]
.args <- if (interactive()) c(
	file.path("analysis", "input", "incidence.rds"),
	file.path("analysis", "input", "simDates.rda"),
	file.path("analysis", "input", "tmb.rda"),
	file.path("analysis", "output", .debug, "ensemble.rds"),
	file.path("analysis", "output", .debug, "incidence_ensemble.rds")
) else commandArgs(trailingOnly = TRUE)

end.date <- as.Date(basename(dirname(tail(.args, 1))))

#' load the incidence we're going to use for Rt calculation
#' limited to from OCT 1 to the data truncation date
dt <- readRDS(.args[1])[between(date, as.Date("2021-10-01"), end.date) & province != "ALL"]

#' load the TMB model convenience functions + simulation references
load(.args[2])
load(.args[3])

ens.dt <- readRDS(.args[4])[, province := regionkey[as.character(prov)] ]

dt[, time := as.numeric(date - zeroDate) ]

res.dt <- ens.dt[dt, on=.(province), allow.cartesian = TRUE][,
	c("propreinf", "propprimary") := .(
		baselogis(time, loc, deltar, lodrop = -Inf, logain = -Inf, intercept = reinf),
		baselogis(time, loc, deltar, lodrop = -Inf, logain = -Inf)
	)
][,
	c("a.reinf", "b.reinf", "a.pri", "b.pri") := .(
		beta_shape*propreinf,   beta_shape*(1-propreinf),
		beta_shape*propprimary, beta_shape*(1-propprimary)
	)  
]

inc.dt <- res.dt[, {
	set.seed(8675309)
	#' use the same seed for each sample => only variation due param. est.
	dp <- rbinom(.N, size = inf1, prob = rbeta(.N, a.pri, b.pri))
	dr <- rbinom(.N, size = tot-inf1, prob = rbeta(.N, a.reinf, b.reinf))
	.(prov, date, dp, dr, tot, inf1)
}, by=sample]

inc.dt[, var := .(dp+dr) ][, ref := tot-var ]

saveRDS(inc.dt, tail(.args, 1))

#' @examples
#' inc.dt <- readRDS(tail(.args, 1))
#' require(ggplot2)
#' p1 <- ggplot(inc.dt[prov == "GP"]) + aes(date, group = sample) + facet_wrap(~prov) +
#'   geom_line(aes(y=var, color="var"), alpha = 0.05) +
#'   geom_line(aes(y=ref, color ="ref"), alpha = 0.05) +
#'   scale_x_date(NULL) + scale_y_continuous(NULL, breaks = 2^c(seq(0,9,by=3), 10), trans = "log2") +
#'   theme_minimal()
#' ggsave("incidence_check.png", p1, width = 10, height = 7, units = "in", dpi = 600, bg = "white")
#' p2 <- ggplot(inc.dt) + aes(date, group = sample) + facet_wrap(~prov) +
#'   geom_line(aes(y=dr, color = "var", linetype="reinf"), alpha = 0.05) +
#'   geom_line(aes(y=dp, color = "var", linetype="pri"), alpha = 0.05) +
#'   geom_line(aes(y=tot-inf1-dr, color="ref", linetype="reinf"), alpha = 0.05) +
#'   geom_line(aes(y=inf1-dp, color ="ref", linetype="pri"), alpha = 0.05) +
#'   scale_x_date(NULL) + scale_y_continuous(NULL, trans = "log2") +
#'   theme_minimal()
#' ggsave("incidence_check2.png", p2, width = 10, height = 7, units = "in", dpi = 600, bg = "white")
