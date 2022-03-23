
suppressPackageStartupMessages({
	require(ggplot2)
	require(scales)
})

.args <- if (interactive()) {
	file.path("analysis", "input", "plotref.rda")
} else commandArgs(trailingOnly = TRUE)

#' `gg.scale.wrapper` enables easy creation of re-usable ggplot scales
#' (a Facade Factory, if you're a fan of Go4 Design Patterns)
#' 
#' @param scale_fun the ggplot scale function that will ultimately be called
#' @param ... whatever default arguments for that scale function; the
#'   ones you define will override the defaults
gg.scale.wrapper <- function(
	scale_fun,
	...
) {
	stopifnot(!missing(scale_fun))
	defs <- list(...)
	if (!length(defs)) warning(
		"provided no default arguments; consider using scale_fun directly."
	)
	
	return(function(...) {
		#' this different ... is how you get a function back that let's you
		#' override defaults, set other arguments to scale_... functions
		.ellipsis <- list(...)
		.args <- defs
		.args[names(.ellipsis)] <- .ellipsis
		do.call(scale_fun, .args)
	})
}

scale_color_SGTF <- gg.scale.wrapper(
	scale_color_discrete,
	name = NULL
)

scale_fill_SGTF <- gg.scale.wrapper(
	scale_fill_discrete,
	name = NULL
)

#' date scales for pretty much everything
scale_x_recieptdate <- gg.scale.wrapper(
	scale_x_date,
	name = "Sample receipt date",
	date_breaks = "months",
	date_minor_breaks = "weeks",
	date_labels = "%b"
)

scale_x_impute <- gg.scale.wrapper(
	scale_x_date,
	name = "Imputed Test+ receipt date",
	date_breaks = "months",
	date_minor_breaks = "weeks",
	date_labels = "%b"
)

#' logit scales for the fitting comparisons
scale_y_logitprop <- gg.scale.wrapper(
	scale_y_continuous,
	name = "Proportion (logit scale)",
	trans = "logit",
	breaks = c(0.01, 0.1, 0.25, 0.5, 0.75, 0.9, 0.99)
)

#' transformation for sample size scale
invsqrt_trans <- function(lbase=10, lstep=1) scales::trans_new(
	"invsqrt",
	transform = function(x) sqrt(1/x),
	inverse = function(x) 1/x^2,
	breaks = function(tlims) {
		bs <- 1/tlims^2
		l <- floor(log(bs[2], lbase)); r <- ceiling(log(bs[1], lbase))
		return(sqrt(1/lbase^seq(l, r, by = -lstep*2)))
	},
	format = scales::label_number_si()
)

#' complementary transformation for sample alpha scale
#' TODO provide maximum argument to ensure consistency across plots?
sqrtb_trans <- function(lbase=10, lstep=1) scales::trans_new(
	"sqrtb",
	transform = function(x) sqrt(x),
	inverse = function(x) x^2,
	breaks = function(tlims) {
		bs <- tlims^2
		l <- floor(log(bs[1], lbase)); r <- ceiling(log(bs[2], lbase))
		return(sqrt(lbase^seq(l, r, by = lstep*2)))
	},
	format = scales::label_number_si()
)

scale_size_samples <- gg.scale.wrapper(
	scale_size_area,
	name = "Samples", trans = invsqrt_trans(),
	max_size = 15
)

scale_alpha_samples <- gg.scale.wrapper(
	scale_alpha_continuous,
	name = "Samples", trans = sqrtb_trans(),
	range = c(0.1, 1)
)

scale_y_doubling <- gg.scale.wrapper(
	scale_y_continuous,
	trans = "log2",
	breaks = function(ls) 2^seq(1,log(ls[2],2),by=3),
	minor_breaks = function(ls) 2^seq(0,log(ls[2],2),1)
)

scale_color_incidence <- gg.scale.wrapper(
	scale_color_manual,
	name = NULL,
	labels = c(del="Est. background", var="Est. BA.1", obs="All Test+", rep="Reported"),
	values = c(del="dodgerblue", var="firebrick", obs="black", rep="grey")
)

scale_alpha_ensemble <- gg.scale.wrapper(
	scale_alpha_manual,
	name = NULL,
	values = c(ensemble = 0.1, central = 1), guide = "none"
)

gg.geom.wrapper <- function(
	geom_fun,
	...
) {
	stopifnot(!missing(geom_fun))
	defs <- list(...)
	if (!length(defs)) warning(
		"provided no default arguments; consider using scale_fun directly."
	)
	
	return(function(...) {
		#' this different ... is how you get a function back that let's you
		#' override defaults, set other arguments to scale_... functions
		.ellipsis <- list(...)
		.args <- defs
		.args[names(.ellipsis)] <- .ellipsis
		do.call(geom_fun, .args)
	})
}

geom_sgtf_point <- gg.geom.wrapper(
	geom_point,
	mapping = aes(y=SGTF/total, size = total, alpha = total, color = "observation")
)

#' for use when quantile-ing for plots
qspans <- c(0.95, 0.5)
qlims <- sort(c({
	tmp <- 0 + (1 - qspans) / 2
	names(tmp) <- sprintf("lo%i", qspans * 100)
	tmp
}, {
	tmp <- 1 - (1 - qspans) / 2
	names(tmp) <- sprintf("hi%i", qspans * 100)
	tmp
}, c(md = .5)))

#' TODO DRY
saveslide <- function(
	...,
	dpi = 600, bg = "white"
) ggsave(..., width = 14, height = 7, units = "in", dpi = 600, bg = bg)

savepage <- function(
	...,
	dpi = 600, bg = "white"
) ggsave(..., width = 7.5, height = 10, units = "in", dpi = 600, bg = bg)

savehalfpage <- function(
	...,
	dpi = 600, bg = "white"
) ggsave(..., width = 7.5, height = 5, units = "in", dpi = 600, bg = bg)

savesq <- function(
	..., dim = 6, dpi = 600, bg = "white"
) ggsave(..., width = 6, height = 6, units = "in", dpi = 600, bg = bg)

save(file = tail(.args, 1), list = ls())