
suppressPackageStartupMessages({
    require(data.table)
    require(ggplot2)
    require(scales)
})

.debug <- c("2021-11-27", "2021-12-06")[2]
.args <- if (interactive()) c(
    file.path("analysis", "output", .debug, "thresholds.rds"),
    file.path("analysis", "output", "fig", "thresholds.png")
) else commandArgs(trailingOnly = TRUE)

thresholds <- readRDS(.args[1])

qspans <- c(0.95, 0.5)
qlims <- sort(c( {
    tmp <- 0 + (1-qspans)/2
    names(tmp) <- sprintf("lo%i", qspans*100)
    tmp
}, {
    tmp <- 1 - (1-qspans)/2
    names(tmp) <- sprintf("hi%i", qspans*100)
    tmp
}, c(md=.5)))

q.dt <- thresholds[, {
    qs <- quantile(value, probs = qlims, na.rm = TRUE)
    names(qs) <- names(qlims)
    as.list(qs)
}, by=.(Province = province, sero = factor(sero, c("down","ref", "up"), ordered = TRUE), immune_escape, delesc = factor(delesc, c("lo","md","hi"), ordered = TRUE), variable)]

q.dt[, delesclim := c(lo=0.05, md=0.10, hi=0.15)[delesc] ]

q.censor <- q.dt[immune_escape > delesclim-1e-3]

other <- rainbow(4, start = 0.25)
names(other) <- q.dt[Province != "GP", unique(Province)]

plotter <- function(dt) ggplot(dt) + aes(
    immune_escape, color = Province, fill = Province
) + facet_grid(
    delesc ~ sero,
    labeller = labeller(
        delesc = c(lo="Delta Esc. 5%", md="10%", hi="15%"),
        sero = c(down="-Half Sero+", ref="Est. Sero+", up="+Half Sero+")
    )
) + coord_cartesian(expand = FALSE, ylim=c(1/2,8)) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    geom_ribbon(aes(ymin=lo95, ymax=hi95, linetype = "95%"), alpha = 0.1, size = 0.1) +
    geom_ribbon(aes(ymin=lo50, ymax=hi50, linetype = "50%"), alpha = 0.1, size = 0.1) +
    geom_line(aes(y=md, linetype = "median")) +
    geom_rect(
        aes(xmin=0, xmax=delesclim, ymin=0.01, ymax=Inf),
        data = function (dt) dt[,.(delesclim=unique(delesclim))],
        fill="grey", alpha=0.5, inherit.aes = FALSE
    ) +
    scale_y_continuous(
        expression("Omicron Relative Transmissibilty "*(R[C]^Omicron/R[C]^Delta)),
        trans = "log2",
        breaks = 2^(-1:3),
        labels = c("1/2","1","2","4","8")
    ) +
    scale_x_continuous(
        "Immune Escape %", breaks = seq(0,1,by=.1), labels = label_percent(1, suffix = "")
    ) +
    scale_linetype_manual(NULL, values = c(median="solid", `50%`="dashed", `95%`="dotted")) +
    scale_color_manual(
        "Province", aesthetics = c("color","fill"),
        values = c(GP="firebrick", other)
    ) +
    theme_minimal(base_size = 18) + theme(
        panel.spacing = unit(1.5, "line")
    )

zoom <- scale_color_manual(
    "Province", values = c(c(GP="firebrick"), rep("grey", 8)),
    labels = c("GAUTENG", "OTHER"),
    aesthetics = c("color","fill")
)

pref <- plotter(
    q.censor[variable == "transmissibility" & delesc == "md" & sero == "ref"]
) + theme(
    strip.text = element_blank()
)
prefonlygp <- plotter(
    q.censor[(immune_escape >= (delesclim-1e-3)) & Province == "GP" & variable == "transmissibility" & delesc == "md" & sero == "ref"]
) + theme(
    strip.text = element_blank()
) + zoom

p <- plotter(q.censor[variable == "transmissibility"])
plo <- plotter(q.censor[variable == "transmissibilitylo"])

pzoom <- p + zoom
plozoom <- plo + zoom

saver <- function(plt, pat) ggsave(
    file.path(dirname(tail(.args, 1)), sprintf(pat, basename(tail(.args, 1)))),
    plt, width = 14, height = 7, dpi = 600, bg = "white"
)

saver(pref, "ref_%s")
saver(prefonlygp, "ref_onlygp_%s")
saver(pref + zoom, "ref_gp_%s")
saver(p, "%s")
saver(plo, "lo_%s")
saver(pzoom, "gp_%s")
saver(plozoom, "lo_gp_%s")


plotter2 <- function(dt) ggplot(dt) + aes(
    immune_escape, color = variable, fill = variable
) + facet_grid(
    delesc ~ sero,
    labeller = labeller(
        delesc = c(lo="Delta Esc. 5%", md="10%", hi="15%"),
        sero = c(down="-Half Sero+", ref="Est. Sero+", up="+Half Sero+")
    )
) + coord_cartesian(expand = FALSE, ylim=c(1/2,4)) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    geom_ribbon(aes(ymin=lo95, ymax=hi95, linetype = "95%"), alpha = 0.1, size = 0.1) +
    geom_ribbon(aes(ymin=lo50, ymax=hi50, linetype = "50%"), alpha = 0.1, size = 0.1) +
    geom_line(aes(y=md, linetype = "median")) +
    geom_rect(
        aes(xmin=0, xmax=delesclim, ymin=0.01, ymax=Inf),
        data = function (dt) dt[,.(delesclim=unique(delesclim)), by=delesc],
        fill="grey", alpha=0.5, inherit.aes = FALSE
    ) +
    scale_y_continuous(
        expression(atop("BA.1 Relative Transmissibilty,",R[C]^"BA.1"/R[C]^"background")),
        trans = "log2",
        breaks = 2^(-1:2),
        labels = c("1/2","1","2","4")
    ) +
    scale_x_continuous(
        "Immune Escape %", breaks = seq(0,1,by=.25), labels = label_percent(1, suffix = "")
    ) +
    scale_linetype_manual(NULL, values = c(median="solid", `50%`="dashed", `95%`="dotted")) +
    scale_color_discrete(
        NULL, aesthetics = c("color","fill"),
        labels = c(transmissibility="Same Gen. Interval", transmissibilitylo="BA.1 faster")
    ) +
    theme_minimal(base_size = 18) + theme(
        panel.spacing = unit(1.5, "line")
    )

reinfs.dt <- readRDS("analysis/input/emp_haz_sens_an_90_cabp.RDS")
reinfs.dt[, OF := mean_rh_X4*(1-mean_rh_W3)/((1-mean_rh_X4)*mean_rh_W3) ]
OFrange <- reinfs.dt[OF > 1, range(OF)]

immescdel <- c(0.05, 0.10, 0.15)
oddsimmedlo <- immescdel/(1-immescdel)*OFrange[1]
oddsimmedhi <- immescdel/(1-immescdel)*OFrange[2]

reinfs.range <- data.table(
    delesc = factor(
        c("lo","md","hi"), levels = c("lo","md","hi"), ordered = TRUE
    ), immune_escape_lo = oddsimmedlo, immune_escape_hi = oddsimmedhi, study = "reinfection"
)

fold.redrange <- data.table(
    immune_escape_lo = .7, immune_escape_hi = .833, study="neutralization"
)

p <- plotter2(q.censor[Province == "GP"]) +
    geom_rect(aes(
        xmin = immune_escape_lo, xmax = immune_escape_hi, ymin = 1e5, ymax = 1e-5
    ), data = fold.redrange, fill = "green", alpha = 0.3, inherit.aes = FALSE) +
    geom_rect(aes(
        xmin = immune_escape_lo, xmax = immune_escape_hi, ymin = 1e5, ymax = 1e-5
    ), data = reinfs.range, fill = "purple", alpha = 0.3, inherit.aes = FALSE) +
    theme(legend.position = "bottom")

pref <- plotter2(q.censor[Province == "GP" & sero == "ref" & delesc == "md"]) +
    geom_rect(aes(
        xmin = immune_escape_lo, xmax = immune_escape_hi, ymin = 1e5, ymax = 1e-5
    ), data = fold.redrange, fill = "green", alpha = 0.3, inherit.aes = FALSE) +
    geom_rect(aes(
        xmin = immune_escape_lo, xmax = immune_escape_hi, ymin = 1e5, ymax = 1e-5
    ), data = function(dt) reinfs.range[delesc %in% unique(dt$delesc)], fill = "purple", alpha = 0.3, inherit.aes = FALSE) +
    theme(legend.position = "bottom")

saver(p, "mix_gp_all_%s")
saver(pref, "mix_gp_ref_%s")
