
suppressPackageStartupMessages({
    require(data.table)
    require(ggplot2)
    require(scales)
})

.args <- if (interactive()) c(
    file.path("analysis", "output", "thresholds.rds"),
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
}, by=.(Province = province, sero = factor(sero, c("down","ref", "up"), ordered = TRUE), immune_escape, delesc = factor(delesc, c("lo","ref","hi"), ordered = TRUE), variable)]

plotter <- function(dt) ggplot(dt) + aes(
    immune_escape, color = Province, fill = Province
) + facet_grid(
    delesc ~ sero,
    labeller = labeller(
        delesc = c(lo="Delta Esc. 5%", ref="15%", hi="25%"),
        sero = c(down="-Half Sero+", ref="Est. Sero+", up="+Half Sero+")
    )
) + coord_cartesian(expand = FALSE, ylim=c(-1,8)) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    geom_ribbon(aes(ymin=lo95, ymax=hi95, linetype = "95%"), alpha = 0.1, size = 0.1) +
    geom_ribbon(aes(ymin=lo50, ymax=hi50, linetype = "50%"), alpha = 0.1, size = 0.1) +
    geom_line(aes(y=md, linetype = "median")) +
    scale_y_continuous(
        expression(R[0]^Omicron/R[0]^Delta),
        trans = "log2",
        breaks = 2^(-1:3),
        labels = c("1/2","1","2","4","8")
    ) +
    scale_x_continuous(
        "Immune Escape %", breaks = seq(0,1,by=.1), labels = label_percent(1)
    ) +
    scale_linetype_manual(NULL, values = c(median="solid", `50%`="dashed", `95%`="dotted")) +
    scale_color_discrete("Province", aesthetics = c("color","fill")) +
    theme_minimal(base_size = 16) + theme(
        panel.spacing = unit(1.5, "line")
    )

p <- plotter(q.dt[variable == "transmissibility"])
plo <- plotter(q.dt[variable == "transmissibilitylo"])

pzoom <- p + scale_color_manual(
    "Province", values = c(c(GP="firebrick"), rep("grey", 8)),
    labels = c("GAUTENG", "OTHER"),
    aesthetics = c("color","fill")
)

plozoom <- plo + scale_color_manual(
    "Province", values = c(c(GP="firebrick"), rep("grey", 8)),
    aesthetics = c("color","fill")
)

saver <- function(plt, pat) ggsave(
    file.path(dirname(tail(.args, 1)), sprintf(pat, basename(tail(.args, 1)))),
    plt, width = 14, height = 7, dpi = 600, bg = "white"
)

saver(p, "%s")
saver(plo, "lo_%s")
saver(pzoom, "gp_%s")
saver(plozoom, "lo_gp_%s")
