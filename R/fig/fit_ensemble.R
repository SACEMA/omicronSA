
.pkgs <- c("data.table", "ggplot2", "patchwork")
stopifnot(all(sapply(.pkgs, require, character.only = TRUE, quietly = TRUE)))

.debug <- c("2021-12-06", "2021-11-27")[1]
.args <- if (interactive()) c(
    file.path("analysis", "output", .debug, "ensemble.rds"),
    file.path("analysis", "input", "incidence.rds"),
    file.path("analysis", "input", "sgtf.rds"),
    file.path("analysis", "input", "tmb.rda"),
    file.path("analysis", "input", "plotref.rda"),
    file.path("analysis", "output", "fig", .debug, "fit_ensemble.png")
) else commandArgs(trailingOnly = TRUE)

enddate <- basename(dirname(tail(.args, 1)))

load(.args[4])
load(.args[5])

fit.dt <- readRDS(.args[1])

inc.dt <- readRDS(.args[2])[between(date, rtStart, enddate)][province != "ALL"]
inc.dt[, prov := names(regionkey)[which(regionkey == province)], by = province ]
inc.dt[, propr := (tot-inf1)/tot ]
inc.dt[prov != "EC", propd := tot/sum(tot), by = date ]

sgtf.dt <- readRDS(.args[3])[between(date, rtStart, enddate)]

all.sgtf <- rbind(
	sgtf.dt[,.(prov = "ALL", SGTF=sum(omicron), total=sum(tot)), keyby=.(date, infection = c("primary", "reinf")[reinf+1])],
	sgtf.dt[,.(infection = "all", prov = "ALL", SGTF=sum(omicron), total=sum(tot)), keyby=.(date)],
	sgtf.dt[,.(SGTF=sum(omicron), total=sum(tot)), keyby=.(date, prov, infection = c("primary", "reinf")[reinf+1])],
	sgtf.dt[,.(infection = "all", SGTF=sum(omicron), total=sum(tot)), keyby=.(date, prov)]
)

dates <- sgtf.dt[, range(date)]
tms <- as.integer(dates - zeroDate)
tm.seq <- seq(tms[1], tms[2])

#' TODO DRY general & aggregate cases
obs.prop <- melt(fit.dt[is.na(sample) | (sample <= 100), .(
	date = tm.seq + zeroDate,
	prime.act = baselogis(tm.seq, loc, deltar, lodrop = -Inf, logain = -Inf),
	prime.obs = baselogis(tm.seq, loc, deltar, lodrop = lodrop, logain = logain),
	reinf.act = baselogis(tm.seq, loc, deltar, lodrop = -Inf, logain = -Inf, intercept = reinf),
	reinf.obs = baselogis(tm.seq, loc, deltar, lodrop = lodrop, logain = logain, intercept = reinf)
	# tot.sgtf <- `???` # weighted average of SGTF proportion reinf
	# tot.inc <- `???` # weighted average of all inc proportion reinf
), by=.(prov, sample)][
	inc.dt, on=.(prov, date), prop.day := propd
],
  id.vars = c("prov", "sample", "date", "prop.day"), variable.name = "infection"
)[, c("infection", "view") := tstrsplit(infection, split = ".", fixed = TRUE) ]

mix.dt <- melt(fit.dt[is.na(sample) | (sample <= 100), .(
	date = tm.seq + zeroDate,
	prime.act = baselogis(tm.seq, loc, deltar, lodrop = -Inf, logain = -Inf),
	prime.obs = baselogis(tm.seq, loc, deltar, lodrop = lodrop, logain = logain),
	reinf.act = baselogis(tm.seq, loc, deltar, lodrop = -Inf, logain = -Inf, intercept = reinf),
	reinf.obs = baselogis(tm.seq, loc, deltar, lodrop = lodrop, logain = logain, intercept = reinf)
), by=.(sample, prov)][
	inc.dt, on=.(prov, date), c("prop.reinf", "prop.day") := .(propr, propd)
][,.(
  	sample, prov, date, prop.day,
  	combo.act = prop.reinf*reinf.act + (1-prop.reinf)*prime.act,
	combo.obs = prop.reinf*reinf.obs + (1-prop.reinf)*prime.obs
)],
	id.vars = c("sample", "prov", "date", "prop.day"), variable.name = "infection"
)[, c("infection", "view") := tstrsplit(infection, split = ".", fixed = TRUE) ]

prov.dt <- rbind(obs.prop, mix.dt)

prov.dt$infection <- factor(prov.dt$infection, levels = c("reinf", "prime", "combo"), ordered = TRUE)

agg.dt <- rbind(
	obs.prop[, .(prov = "ALL", value = sum(prop.day*value)), by=.(sample, date, view, infection = factor(infection, levels = c("reinf", "prime", "combo"), ordered = TRUE))],
	mix.dt[, .(prov = "ALL", value = sum(prop.day*value)), by=.(sample, date, view, infection = factor(infection, levels = c("reinf", "prime", "combo"), ordered = TRUE))]
)

shr <- list(
	aes(date),
	facet_wrap(~prov, nrow = 3, ncol = 3),
	geom_sgtf_point(
		mapping = aes(y=SGTF/total, size = total, alpha = total, fill = infection, stroke = 0),
		shape = 21
	),
	theme_minimal(base_size = 9),
	scale_fill_SGTF(
		name = "Infections",
		labels = c(all = "All", primary = "Primary", reinf = "Reinfections"),
		breaks = c("all", "primary", "reinf"),
		values = c(all="black", reinf = "blue", primary = "green"),
		guide = guide_legend(override.aes = list(stroke = 0, size = 5, alpha = 0.5))
	),
	scale_alpha_samples(guide = guide_legend(override.aes = list(color = NA, fill = "black"))),
	scale_x_impute(),
	scale_y_logitprop(),
	scale_linetype_manual(name = NULL, labels = c(act="Latent", obs="Observed"), values = c(act="solid", obs="dotted")),
	coord_cartesian(ylim = c(0.01, 0.99))
)

geom_fit_lines <- function(dt) list(
	geom_line(
		aes(y = value, linetype = view, color = infection, group=interaction(view, infection, sample)),
		dt[!is.na(sample)],
		alpha = 0.01, size = 0.5
	),
	geom_line(
		aes(y = value, linetype = view, color = infection),
		dt[is.na(sample)],
		alpha = 0.5, size = 0.5
	)
)

plot.all <- ggplot(all.sgtf[prov == "ALL"]) + shr +
	geom_fit_lines(agg.dt) +
	scale_size_samples() +
	scale_color_manual(guide = "none", values = c(combo = "black", reinf = "blue", prime = "green")) +
	theme(
		legend.position = c(0, .95), legend.justification = c(0, 1),
		legend.box = "horizontal"
	)

plot.provs <- ggplot(all.sgtf[prov != "ALL"]) + shr +
	geom_fit_lines(prov.dt) +
	scale_color_manual(guide = "none", values = c(combo = "black", reinf = "blue", prime = "green")) +
	scale_size_samples(max_size = 15/3) +
	theme(
		legend.position = "none",
		axis.title.y = element_blank(),
		panel.grid.minor = element_blank()
	)

p.res <- plot.all + plot.provs

saveslide(tail(.args, 1), p.res)
