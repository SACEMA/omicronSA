
suppressPackageStartupMessages({
    require(data.table)
    require(ggplot2)
    require(patchwork)
    require(bbmle)
})

.debug <- c("2021-12-06", "2021-11-27")[2]
.args <- if (interactive()) c(
    file.path("analysis", "input", "sgtf.rds"),
    file.path("analysis", "output", "sgtf", .debug, "mergedfit.rds"),
    file.path("analysis", "output", "sgtf", .debug, "sims.rds"),
    file.path("analysis", "output", "fig", .debug, "frequencies.png")
) else commandArgs(trailingOnly = TRUE)

regionkey = c(
    EC="EASTERN CAPE",
    FS="FREE STATE",
    GP="GAUTENG",
    KZN="KWAZULU-NATAL",
    LP="LIMPOPO",
    MP="MPUMALANGA",
    NC="NORTHERN CAPE",
    NW="NORTH WEST",
    WC="WESTERN CAPE",
    ALL="ALL"
)

enddate <- as.Date(basename(dirname(tail(.args, 1))))

sgtf.dt <- readRDS(.args[1])[date <= enddate]
min.date <- sgtf.dt[, min(date)]

fit.ref <- sgtf.dt[,.(
    SGTF = sum(SGTF), nonSGTF = sum(nonSGTF), total = sum(total)
), by=.(prov, date) ]

baselogis <- function(
    tvec, # observation times
    loc, delta_r, lodrop,
    # fitting: offset, relative growth rate, drop
    logain # fixed: gain
){
    drop <- plogis(lodrop)
    gain <- plogis(logain)
    ptrue <- plogis((tvec-loc)*delta_r)
    
    return(ptrue*(1-gain) + (1-ptrue)*drop)
}

times <- 0:90
fittings <- readRDS(.args[2])
delr.dt <- rbindlist(lapply(fittings, function(fit) {
    co <- as.list(coef(fit$m))
    ci <- fit$ci
    data.table(
        delr = sprintf("%.2g (%.2g, %.2g)", co$delta_r, ci["delta_r",1], ci["delta_r",2])
    )
}), idcol = "prov")
estimates <- rbindlist(lapply(fittings, function (fit) {
    co <- as.list(coef(fit$m))
    data.table(
        date = min.date + times,
        est_SGTF = baselogis(times, co$loc, co$delta_r, co$lodrop, co$logain),
        est_BA1 = baselogis(times, co$loc, co$delta_r, -Inf, -Inf)
    )
}), idcol = "prov")

ens.dt <- readRDS(.args[3])

plotter <- function(dt, fit.dt, ens.dt, start.date = "2021-10-15") ggplot(dt[date >= start.date]) + aes(date) +
    facet_wrap(~prov) +
    geom_line(aes(y=prop, group=sample, color = "ensemble"), data = ens.dt[date >= start.date], alpha = 0.05) +
    geom_line(aes(y=est_SGTF, color = "est. SGTF"), data = fit.dt[date >= start.date]) +
    geom_line(aes(y=est_BA1, color = "est. BA.1"), data = fit.dt[date >= start.date]) +
    geom_point(aes(y=SGTF/total, size = sqrt(1/total), alpha = sqrt(total), color = "observation")) +
    geom_text(aes(x=as.Date("2021-10-15"), y=0.9, label = sprintf("delta-r\n%s",delr)), data = delr.dt, size = 2, hjust = 0) +
    theme_minimal() +
    coord_cartesian(ylim=c(0.01, 0.99)) +
    scale_y_continuous("Proportion (logit scale)", trans="logit", breaks = c(0.01, 0.1, 0.25, 0.5, 0.75, 0.9, 0.99)) +
    scale_size_area(
        "Samples", breaks = sqrt(1/c(1,10,100,1000)), max_size = 10, labels = function(b) 1/b^2,
        guide = guide_legend(override.aes = list(alpha = c(0.1,0.3,0.7)))
    ) +
    scale_x_date("Specimen receipt date", date_breaks = "months", date_minor_breaks = "weeks", date_labels = "%b") +
    scale_color_manual(NULL, values = c(observation="black", ensemble = "firebrick", `est. SGTF`="dodgerblue", `est. BA.1`="red")) +
    scale_alpha_continuous(NULL, range = c(0.1, 1), guide = "none")

p <- plotter(
    fit.ref[prov %in% unique(estimates$prov)],
    estimates[date <= max(fit.ref$date)+7],
    ens.dt[date <= max(fit.ref$date)+7]
) + theme(legend.position = "bottom")

ggsave(tail(.args, 1), p, width = 8, height = 5, units = "in", dpi = 600, bg = "white")
