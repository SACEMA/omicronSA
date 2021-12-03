suppressPackageStartupMessages({
    require(data.table)
    require(ggplot2)
})

.args <- if (interactive()) c(
    file.path(
        "analysis", rep("input", 2), c("incidence.rds", "frequencies.rds")
    ),
    file.path(
        "analysis", "output", "fig", "incidence.png"
    )
) else commandArgs(trailingOnly = TRUE)

inc.dt <- readRDS(.args[1])
freq.dt <- readRDS(.args[2])

p <- ggplot(inc.dt[province == "GAUTENG" & date > "2021-10-03"]) + aes(date) + 
    geom_point(aes(y=tot, color = "total"), alpha = 0.5) +
    geom_line(aes(y=tot, color = "total"), function(dt) dt[, .(date, tot=exp(frollmean(log(tot), 7, align="center")))]) +
    geom_point(aes(y=tot-inf1, color="reinf"), alpha = 0.5) + 
    geom_line(aes(y=mod, color = "reinf"), function(dt) dt[, .(date, mod=exp(frollmean(log(tot-inf1), 7, align="center")))]) +
    theme_minimal(base_size = 20) +
    scale_x_date(NULL, date_breaks = "week", date_labels = "%b %d", minor_breaks = NULL) +
    scale_y_continuous(
        "Incidence", trans = "log2",
        breaks = 2^(seq(3,12,by=3)),
        minor_breaks = 2^(2:12)
    ) + 
    theme(
        legend.position = c(0, 1), legend.justification = c(0, 1),
        plot.margin = margin(t=1, r=1, unit="line")
    ) +
    scale_color_discrete(NULL, labels = c(reinf="Reinfections", total="Total"))

ggsave("epi-gauteng.png", p, width = 14, height = 7, dpi = 600)

pall <- ggplot(inc.dt[date > "2021-10-03"]) + aes(date) + 
    facet_wrap(~province) +
    geom_point(aes(y=tot, color = "total"), alpha = 0.5) +
    geom_line(aes(y=tot, color = "total"), function(dt) dt[, .(date, tot=exp(frollmean(log(tot), 7, align="center"))), by=province]) +
    geom_point(aes(y=tot-inf1, color="reinf"), alpha = 0.5) + 
    geom_line(aes(y=mod, color = "reinf"), function(dt) dt[, .(date, mod=exp(frollmean(log(tot-inf1), 7, align="center"))), by=province]) +
    theme_minimal(base_size = 16) +
    scale_x_date(NULL, date_breaks = "week", date_labels = "%b %d", minor_breaks = NULL) +
    scale_y_continuous(
        "Incidence", trans = "log2",
        breaks = 2^(seq(3,12,by=3)),
        minor_breaks = 2^(2:12)
    ) + 
    theme(
        legend.position = c(0, 1), legend.justification = c(0, 1),
        plot.margin = margin(t=1, r=1, unit="line")
    ) +
    scale_color_discrete(NULL, labels = c(reinf="Reinfections", total="Total"))

ggsave("epi-all.png", pall, width = 14, height = 7, dpi = 600)

exp.dt <- inc.dt[province == "GAUTENG" & date > "2021-10-03"][
    as.data.table(freq.dt), on=.(province, date),
    allow.cartesian = TRUE, nomatch = 0
]

p2 <- ggplot(exp.dt) + aes(date) + 
    geom_line(aes(y=tot, color = "total"), function(dt) dt[sample == 1]) +
    geom_line(aes(y=vartot, color="variant", group=sample), function(dt) dt[,.(sample, date, vartot=round(est_prop*tot))], alpha = 0.1) + 
    theme_minimal(base_size = 20) +
    scale_x_date(NULL, date_breaks = "week", date_labels = "%b %d") + scale_y_continuous("Incidence", trans = "log2") + 
    theme(legend.position = c(0, 1), legend.justification = c(0, 1)) +
    scale_color_discrete(NULL)

ggsave("epi-gauteng-var.png", p2, width = 14, height = 7, dpi = 600)

thing <- readRDS("analysis/output/omicron_ratios.rds")[region == "GP"]
thing
