
suppressPackageStartupMessages({
    require(data.table)
    require(ggplot2)
    require(scales)
})

.debug <- c("2021-11-27", "2021-12-06")[2]
.args <- if (interactive()) c(
    file.path("analysis", "output", .debug, "thresholds.rds"),
    file.path("analysis", "output", "fig", .debug, "thresholds.png")
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


plotter2 <- function(dt, bglayer) ggplot(dt) + aes(
    immune_escape, color = variable, fill = variable
) + facet_grid(
    delesc ~ sero,
    labeller = labeller(
        delesc = c(lo="Background\nEvasion 5%", md="Background\nEvasion 10%", hi="Background\nEvasion 15%"),
        sero = c(down="Higher\nSusceptible %", ref="Reference\nSusceptible %", up="Lower\nSusceptible %")
    )
) + coord_cartesian(expand = FALSE, ylim=c(1/2,4)) +
    bglayer +
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

foldredrange <- c(3, 8)
khoury_get_1a = function(delta_evade_prop, fold_reduction) {
    khoury_nat_med_1a = fread("refdata/Khoury_et_al_Nat_Med_fig_1a.csv")
    
    neut_titre = with(khoury_nat_med_1a,
        approx(Efficacy, NeutTitreRelConvPlasma, (1-delta_evade_prop)*100)$y
    )
    neut_titre_new = neut_titre / fold_reduction
    
    1-with(khoury_nat_med_1a,
           approx(NeutTitreRelConvPlasma, Efficacy, neut_titre_new)$y
    )/100
}

imm.ref <- q.censor[, .(esc=unique(delesclim)), by=delesc]
imm.ref[, c("lo", "hi") := as.list(khoury_get_1a(esc, foldredrange)), by=delesc ]

khourycol <- "yellow"

geom_intensify <- function (alpha = 0.2, size = 0.2) list(
    geom_ribbon(
        aes(ymin=lo95, ymax=hi95, linetype = "95%"),
        alpha = alpha, size = size, data = function(dt) {
            rbind(
                dt[imm.ref, on=.(delesc), nomatch = 0][between(immune_escape, lo, hi)],
                dt[imm.ref, on=.(delesc), nomatch = 0][, {
                    .(
                        immune_escape = unique(lo),
                        lo95 = approx(immune_escape, lo95, unique(lo))$y,
                        lo50 = approx(immune_escape, lo50, unique(lo))$y,
                        hi50 = approx(immune_escape, hi50, unique(lo))$y,
                        hi95 = approx(immune_escape, hi95, unique(lo))$y
                    )
                }, by=.(Province, sero, delesc, variable)],
                dt[imm.ref, on=.(delesc), nomatch = 0][, {
                    .(
                        immune_escape = unique(hi),
                        lo95 = approx(immune_escape, lo95, unique(hi))$y,
                        lo50 = approx(immune_escape, lo50, unique(hi))$y,
                        hi50 = approx(immune_escape, hi50, unique(hi))$y,
                        hi95 = approx(immune_escape, hi95, unique(hi))$y
                    )
                }, by=.(Province, sero, delesc, variable)],
                fill = TRUE
            )[order(immune_escape)]
        }
    ),
    geom_ribbon(
        aes(ymin=lo50, ymax=hi50, linetype = "50%"),
        alpha = alpha, size = size, data = function(dt) {
            rbind(
                dt[imm.ref, on=.(delesc), nomatch = 0][between(immune_escape, lo, hi)],
                dt[imm.ref, on=.(delesc), nomatch = 0][, {
                    .(
                        immune_escape = unique(lo),
                        lo95 = approx(immune_escape, lo95, unique(lo))$y,
                        lo50 = approx(immune_escape, lo50, unique(lo))$y,
                        hi50 = approx(immune_escape, hi50, unique(lo))$y,
                        hi95 = approx(immune_escape, hi95, unique(lo))$y
                    )
                }, by=.(Province, sero, delesc, variable)],
                dt[imm.ref, on=.(delesc), nomatch = 0][, {
                    .(
                        immune_escape = unique(hi),
                        lo95 = approx(immune_escape, lo95, unique(hi))$y,
                        lo50 = approx(immune_escape, lo50, unique(hi))$y,
                        hi50 = approx(immune_escape, hi50, unique(hi))$y,
                        hi95 = approx(immune_escape, hi95, unique(hi))$y
                    )
                }, by=.(Province, sero, delesc, variable)],
                fill = TRUE
            )[order(immune_escape)]
        }
    )
)

p <- plotter2(
    q.censor[Province == "GP"],
    geom_rect(aes(
        xmin = lo, xmax = hi, ymin = 1e5, ymax = 1e-5
    ), data = imm.ref, fill = khourycol, alpha = 0.3, inherit.aes = FALSE)
) + geom_intensify() +
    theme(legend.position = "bottom")

pref <- plotter2(
    q.censor[Province == "GP" & sero == "ref" & delesc == "md"],
    geom_rect(aes(
        xmin = lo, xmax = hi, ymin = 1e5, ymax = 1e-5
    ), data = function(dt) imm.ref[delesc %in% unique(dt$delesc)], fill = khourycol, alpha = 0.3, inherit.aes = FALSE)
) + geom_intensify() +
    theme(legend.position = "bottom")

saver(p, "mix_gp_all_%s")
saver(pref, "mix_gp_ref_%s")
