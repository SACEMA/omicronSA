
suppressPackageStartupMessages({
    require(data.table)
    require(ggplot2)
    require(patchwork)
})

.args <- if (interactive()) c(
    file.path("analysis", "input", "incidence.rds"),
    file.path("analysis", "output", "fig", "raw_incidence.png")
) else commandArgs(trailingOnly = TRUE)

inc.dt <- readRDS(.args[1])[between(date, "2021-09-01", "2021-12-06")]

#' TODO
#'  - eliminate code smell from using wide instead of long data
#'  - insert geom_blanks to assure matching y-scale across panels
#'  - address 0s in geo mean approach;
#'  should be able to use (nafill(forward) + nafill(reverse)) mixture (by step)
#'  - eliminate y-axis label from GP panel?
plotter <- function(plot.dt) ggplot(plot.dt) + aes(date) +
    facet_wrap(~province) +
    geom_point(aes(y=tot, color="All Infections", alpha = "observed")) +
    geom_line(
        aes(y=tot, color="All Infections", alpha = "mean7"),
        data = function(dt) dt[,.(
            date, tot = exp(frollmean(log(tot), 7, align = "center"))
        ), by = province ]
    ) +
    geom_point(aes(y=tot-inf1, color="Reinfections", alpha = "observed")) +
    geom_line(
        aes(y=reinf, color="Reinfections", alpha = "mean7"),
        data = function(dt) dt[,.(
            date, reinf = exp(frollmean(log(tot-inf1), 7, align = "center"))
        ), by = province ]
    ) +
    scale_y_continuous(
        "Incidence", trans = "log2",
        breaks = function(ls) 2^seq(1,ls[2],by=3),
        minor_breaks = function(ls) 2^(1:ls[2])
    ) +
    scale_x_date(NULL, date_labels = "%b", date_breaks = "months", date_minor_breaks = "weeks") +
    theme_minimal(base_size = 14) +
    scale_alpha_manual(NULL, guide = "none", values = c(observed=0.3, mean7=1)) +
    scale_color_discrete(NULL)

p <- plotter(inc.dt[province != "GAUTENG"])
pg <- plotter(inc.dt[province == "GAUTENG"])

resp <- (p | pg) + plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave(tail(.args, 1), resp, width = 14, height = 7, dpi = 600, units = "in", bg = "white")
