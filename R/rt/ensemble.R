
.pkgs <- c("data.table")
stopifnot(all(sapply(.pkgs, require, character.only = TRUE, quietly = TRUE)))

.debug <- c("2021-11-27", "2021-12-06")[1]
.args <- if (interactive()) c(
	file.path("analysis", "input", "incidence.rds"),
	file.path("analysis", "input", "tmb.rda"),
	file.path("analysis", "output", .debug, "ensemble.rds"),
	.debug,
	file.path("analysis", "output", .debug, "incidence_ensemble.rds")
) else commandArgs(trailingOnly = TRUE)

end.date <- as.Date(.args[4])
#' load the TMB model convenience functions + simulation references
load(.args[2])

#' load the incidence we're going to use for Rt calculation
#' limited to from OCT 1 to the data truncation date
dt <- readRDS(.args[1])[between(date, rtStart, end.date) & province != "ALL"]

ens.dt <- readRDS(.args[3])[, province := regionkey[as.character(prov)] ]

dt[, time := as.numeric(date - zeroDate) ]

res.dt <- ens.dt[dt, on=.(province), allow.cartesian = TRUE, nomatch = 0][,
	c("propreinf", "propprime") := .(
		baselogis(time, loc, deltar, lodrop = -Inf, logain = -Inf, intercept = reinf),
		baselogis(time, loc, deltar, lodrop = -Inf, logain = -Inf)
	)
][,
	c("a.reinf", "b.reinf", "a.prime", "b.prime") := .(
		beta_shape*propreinf, beta_shape*(1-propreinf),
		beta_shape*propprime, beta_shape*(1-propprime)
	)  
]

qdraw <- function(n, seedoffset, seedbase = 8675309L) {
	set.seed(seedbase + seedoffset)
	runif(n)
}

#' n.b. this draw scheme ensures that given the same start date each draw has
#' the same series quantile sequence for proportion draws (though possibly longer)
#' and then for value draws (again, possibly longer) the series of random draws
#' is also the same for each underlying sample, so only variability due to
#' parameter uncertainty
#' TODO: 
res.dt[order(sample, time),
	c("qbetareinf", "qbinoreinf", "qbetaprime", "qbinoprime") := {
		smplen <- length(unique(sample))
		serieslen <- .N/smplen
		qbetareinf <- qdraw(serieslen, 000L + as.integer(prov))
		qbinoreinf <- qdraw(serieslen, 100L+ as.integer(prov))
		qbetaprime <- qdraw(serieslen, 200L+ as.integer(prov))
		qbinoprime <- qdraw(serieslen, 300L+ as.integer(prov))
		.(
			rep(qbetareinf, smplen),
			rep(qbinoreinf, smplen),
			rep(qbetaprime, smplen),
			rep(qbinoprime, smplen)
		)
	},
	by = prov
]

#' default to binomial proportion
#' if the fit found support for beta-binomial distribution
#' (i.e. small bb parameter => non-sharp binomial), use beta-binomial
res.dt[, c(
	"reinfprob", "primeprob"
) := .(
	propreinf,
	propprime
) ]
#' N.B. does not appear to be support for beta-binomial for 12-06 end date
#' some instances in earlier truncation date
warnfmt <- "%s qbeta failure on prov %s sample %i time %i: q: %f a: %f b: %f"
res.dt[
	beta_shape <= 30,
	c("reinfprob", "primeprob") := .(
		tryCatch(qbeta(qbetareinf, a.reinf, b.reinf), warning = function(w) {
			write(sprintf(warnfmt, "reinf", as.character(prov), sample, time, qbetareinf, a.reinf, b.reinf), file = stderr())
			return(reinfprob)
		}),
		tryCatch(qbeta(qbetaprime, a.prime, b.prime), warning = function(w) {
			write(sprintf(warnfmt, "prime", as.character(prov), sample, time, qbetaprime, a.prime, b.prime), file = stderr())
			return(primeprob)
		})
	),
	by = .(prov, sample, time)
]

#' deals with some double precision representation errors with 1+epsilon values
#' FIXME: not particularly robust, but checked for these inputs.
res.dt[!between(reinfprob, 0, 1), reinfprob := round(reinfprob) ]
res.dt[!between(primeprob, 0, 1), primeprob := round(primeprob) ]

inc.dt <- res.dt[, {
	dr <- qbinom(qbinoreinf, size = tot-inf1, prob = reinfprob)
	dp <- qbinom(qbinoprime, size = inf1,     prob = primeprob)
	.(prov, sample, date, dp, dr, tot, inf1)
} ]

inc.dt[, var := .(dp+dr) ][, ref := tot-var ]

#' TODO: unnecessary w/ removal of EC?
keep.dt <- inc.dt[,
	.(keep = (sum(ref > 0) > 14) & (sum(var > 0) > 14)), by=.(prov, sample)
][, .(keep = all(keep)), by=sample ][keep == TRUE, .(sample, newsample = 1:.N)][newsample <= 1000]

save.dt <- setkey(
	inc.dt[keep.dt, on=.(sample)][, .(prov, sample = newsample, date, dp, dr, tot, inf1, var, ref)],
	prov, sample, date
)

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
#' 
#' ggplot(melt(
#'   ens.dt[keep.dt, on=.(sample)],
#'   id.vars = c("prov","sample","keep"),
#'   measure.vars = c("deltar","loc","reinf","beta_shape")
#' )) + aes(value, fill=keep) + facet_grid(prov ~ variable, scale = "free") +
#' geom_histogram() + scale_y_log10()
#' 
#' 
#' 
#' 
