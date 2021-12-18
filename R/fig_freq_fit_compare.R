
suppressPackageStartupMessages({
    require(data.table)
    require(ggplot2)
    require(patchwork)
    require(bbmle)
})

.args <- if (interactive()) c(
    file.path("analysis", "input", "sgtf.rds"),
    file.path("analysis", "output", "sgtf"),
    file.path("analysis", "output", "fig", "cross_frequencies.png")
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

sgtf.dt <- readRDS(.args[1])[date <= "2021-12-06"]
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

p <- ggplot(plot.dt) + aes(date, linetype=model) +
    facet_grid(end.date ~ prov) +
    geom_line(aes(y=SGTF_est, color = "SGTF est")) +
    geom_line(aes(y=BA.1_est, color = "BA.1 est")) +
    geom_point(aes(y=SGTF/total, size = sqrt(1/total), alpha = sqrt(total), color = "observation"), data = function(dt) dt[date <= end.date]) +
    theme_minimal(base_size = 14) +
    coord_cartesian(ylim=c(0.01, 0.99)) +
    scale_y_continuous("Proportion (logit scale)", trans="logit", breaks = c(0.01, 0.1, 0.25, 0.5, 0.75, 0.9, 0.99)) +
    scale_size_area(
        "Samples", breaks = sqrt(1/c(1,10,100,1000)), max_size = 10, labels = function(b) 1/b^2,
        guide = guide_legend(override.aes = list(alpha = c(0.1,0.3,0.7,1)))
    ) +
    scale_x_date("Sample Receipt Date", date_breaks = "months", date_minor_breaks = "weeks", date_labels = "%b") +
    scale_alpha_continuous(NULL, range = c(0.1, 1), guide = "none") +
    scale_linetype("Fit Model") +
    scale_color_discrete(NULL) +
    theme(legend.position = "bottom")

ggsave(tail(.args, 1), p, width = 14, height = 7, units = "in", dpi = 600, bg = "white")
