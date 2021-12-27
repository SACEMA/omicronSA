
suppressPackageStartupMessages({
    require(data.table)
    require(ggplot2)
    require(patchwork)
    require(bbmle)
})

.args <- if (interactive()) c(
    file.path("analysis", "input", "sgtf.rds"),
    file.path("analysis", "output", "sgtf"),
    file.path("analysis", "input", "plotref.rda"),
    file.path("analysis", "output", "fig", "stgf_model_comparison.png")
) else commandArgs(trailingOnly = TRUE)

sgtf.dt <- readRDS(.args[1])
min.date <- sgtf.dt[, min(date)]

fit.ref <- sgtf.dt[,.(
    SGTF = sum(SGTF), nonSGTF = sum(nonSGTF), total = sum(total), time = as.numeric(date-min.date)
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
fittings <- list.files(.args[2], "rdata$", full.names = TRUE, recursive = TRUE)

allfits <- rbindlist(lapply(fittings, function(fl) {
    end.date <- as.Date(basename(dirname(fl)))
    mod <- gsub("^.+_(.+)\\.rdata", "\\1", basename(fl))
    fit <- rbindlist(lapply(readRDS(fl), function(provfit) {
        as.list(coef(provfit$m))
    }), idcol = "prov")[, model := mod ][, end.date := end.date ]
    fit
}), fill = TRUE)

plot.dt <- fit.ref[
    allfits, on=.(prov), allow.cartesian = TRUE, nomatch = 0
]

plot.dt[,
    c("SGTF_est", "BA.1_est") := .(
        baselogis(time, loc, delta_r, lodrop, logain),
        baselogis(time, loc, delta_r, -Inf, -Inf)
    )
]

#' load conserved plotting elements
load(tail(.args, 2)[1])

p <- ggplot(plot.dt) + aes(date, linetype=model) +
    facet_grid(end.date ~ prov) +
    geom_line(aes(y=SGTF_est, color = "SGTF est")) +
    geom_line(aes(y=BA.1_est, color = "BA.1 est")) +
    geom_sgtf_point(data = function(dt) dt[date <= end.date]) +
    theme_minimal(base_size = 14) +
    coord_cartesian(ylim=c(0.01, 0.99)) +
    scale_y_logitprop() +
    scale_size_samples() +
    scale_alpha_samples() +
    scale_x_recieptdate() +
    scale_linetype("Fit Model", guide = "none") +
    scale_color_discrete(NULL) +
    theme(legend.position = "bottom")

saveslide(tail(.args, 1), p)
