
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
inc.dt <- readRDS(.args[2])[between(date, rtStart, enddate)]
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

obs.prop <- melt(fit.dt[sample <= 100, .(
	date = tm.seq + zeroDate,
	prime.act = baselogis(tm.seq, loc, deltar, lodrop = -Inf, logain = -Inf),
	prime.obs = baselogis(tm.seq, loc, deltar, lodrop = lodrop, logain = logain),
	reinf.act = baselogis(tm.seq, loc, deltar, lodrop = -Inf, logain = -Inf, intercept = reinf),
	reinf.obs = baselogis(tm.seq, loc, deltar, lodrop = lodrop, logain = logain, intercept = reinf)
	# tot.sgtf <- `???` # weighted average of SGTF proportion reinf
	# tot.inc <- `???` # weighted average of all inc proportion reinf
), by=.(prov, sample)],
  id.vars = c("prov", "sample", "date"), variable.name = "infection"
)

shr <- list(
	aes(date),
	facet_wrap(~prov, nrow = 3, ncol = 3),
	geom_sgtf_point(
		mapping = aes(y=SGTF/total, size = total, alpha = total, fill = infection, stroke = 0),
		shape = 21
	),
	theme_minimal(),
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
	coord_cartesian(ylim = c(0.01, 0.99))
)

plot.all <- ggplot(all.sgtf[prov == "ALL"]) + shr +
	scale_size_samples() +
	theme(
		legend.position = c(0, .95), legend.justification = c(0, 1),
		legend.box = "horizontal"
	)

plot.provs <- ggplot(all.sgtf[prov != "ALL"]) + shr +
	scale_size_samples(max_size = 15/3) +
	theme(
		legend.position = "none",
		axis.title.y = element_blank(),
		panel.grid.minor = element_blank()
	)

p.res <- plot.all + plot.provs

ggsave(tail(.args, 1), p.res, width = 8, height = 4, units = "in", dpi = 600)

plot.all <- ggplot(all.sgtf) + aes(date) + 
	geom_sgtf_point(mapping = aes(y=SGTF/total, size = total, alpha = total, color = infection)) +
	theme_minimal() +
	theme(
		legend.position = c(0, 1), legend.justification = c(0, 1),
		legend.box = "horizontal"
	) +
	scale_color_SGTF(
		name = "Infections",
		labels = c(all = "All", primary = "Primary", reinf = "Reinfections"),
		breaks = c("all", "primary", "reinf")
	) +
	scale_alpha_samples() +
	scale_size_samples() +
	scale_x_impute() +
	scale_y_logitprop()

geom_truncated_series <- function(..., refdata) list(
    geom_line(..., data = function(dt) refdata[between(date,min(dt$date),max(dt$date))]),
    geom_point(..., data = function(dt) refdata[date >= max(dt$date)])
)

p <- ggplot(e.dt) +
    aes(date) + facet_wrap(~province) +
    geom_line(
        aes(y=tot-var, color="del", alpha = "ensemble", group=sample)) +
    geom_line(
        aes(y=var, color="var", alpha = "ensemble", group=sample)) +
    geom_truncated_series(aes(y=inf1, color="rep"), refdata = raw.dt) +
    geom_truncated_series(aes(y=tot, color="obs"), refdata = raw.dt) +
    scale_x_recieptdate() +
    scale_y_doubling(name="Detected Infections") +
    scale_color_incidence() +
    scale_alpha_ensemble() +
    coord_cartesian(ylim = c(2, NA), expand = FALSE) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom")

savehalfpage(tail(.args, 1), p)
