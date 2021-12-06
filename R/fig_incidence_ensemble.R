
suppressPackageStartupMessages({
    require(data.table)
    require(ggplot2)
})

.args <- if (interactive()) {
    file.path("analysis", "output", c("incidence_ensemble.rds", "incidence_ensemble.png"))
} else {
    commandArgs(trailingOnly = TRUE)
}

e.dt <- readRDS(.args[1])[between(date,"2021-10-03","2021-11-27")][sample <= 100]

plotter <- function(sub.dt) ggplot(sub.dt) + aes(date, group = sample) + facet_wrap(~province) +
    geom_line(aes(y=tot-var, color="del"), alpha = 0.05) +
    geom_line(aes(y=tot, color="obs"), data = function(dt) dt[sample == 1]) +
#    geom_line(aes(y=tot-var, color="del")) +
    theme_minimal(base_size = 14) +
    coord_cartesian(ylim = c(1, NA), xlim=as.Date(c("2021-10-03", "2021-11-27"))) +
    scale_x_date(
        NULL, date_breaks = "weeks", date_minor_breaks = "days", labels = function(b) {
            c("",format(b[2],"%b %d"),format(b[3:5],"%d"),format(b[6],"%b %d"), format(b[7:(length(b)-1)],"%d"), "")
        }
    ) +
    scale_y_continuous("Incidence", trans="log2", breaks = 2^seq(1,10,by=3), minor_breaks = 2^seq(0,11,1)) +
    scale_color_manual(
        NULL, labels = c(del="Non-Omicron Ensemble", obs="Reported"), values = c(del="firebrick", obs="black")
    ) +
    theme(
        legend.position = c(0, 1), legend.justification = c(0, 1)
    )

resp <- plotter(e.dt[prov == "GP"])

ggsave(tail(.args, 1), resp, width = 8, height = 5, units = "in", dpi = 600)
