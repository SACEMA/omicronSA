
suppressPackageStartupMessages({
    require(data.table)
    require(ggplot2)
    require(patchwork)
})

.args <- if (interactive()) file.path("analysis", c(
    file.path(
        "input", c("sssims.rds", "simbig.quasi.rds", "simbig.bin.rds")
    ),
    file.path(
        "output", "fig", "frequencies.png"
    )
)) else {
    commandArgs(trailingOnly = TRUE)
}

freq.models <- rbind(
    as.data.table(readRDS(.args[1]))[, model := "ss" ],
    as.data.table(readRDS(.args[2]))[, model := "quasi" ],
    as.data.table(readRDS(.args[3]))[, model := "bin" ]    
)

ppanels <- ggplot(
    freq.models[prov != "GP"][between(date,"2021-10-03","2021-11-27")][sample <= 100]
) + aes(date, est_prop, color = model, group = interaction(sample, model)) +
    facet_wrap(~prov) +
    geom_line(alpha = 0.05) +
    coord_cartesian(ylim = c(0.005, 0.995)) +
    scale_y_continuous(NULL, breaks = c(0.01, 0.1, 0.25, 0.5, 0.75, 0.9, 0.99), trans = "logit") +
    scale_x_date(
        NULL, date_breaks = "weeks", date_minor_breaks = "days", labels = function(b) {
            c("",format(b[2],"%b %d"),format(b[3:5],"%d"),format(b[6],"%b %d"), format(b[7:(length(b)-1)],"%d"), "")
        }
    ) +
    scale_color_discrete("Fitting Model", guide = guide_legend(override.aes = list(alpha = 1))) +
    theme_minimal(base_size = 14) + theme(axis.text.x = element_blank())

gppanel <- ggplot(
    freq.models[prov == "GP"][between(date,"2021-10-03","2021-11-27")][sample <= 100]
) + aes(date, est_prop, color = model, group = interaction(sample, model)) +
    facet_wrap(~prov) +
    geom_line(alpha = 0.05) +
    coord_cartesian(ylim = c(0.005, 0.995)) +
    scale_y_continuous("Fraction SGTF", breaks = c(0.01, 0.1, 0.25, 0.5, 0.75, 0.9, 0.99), trans = "logit") +
    scale_x_date(
        NULL, date_breaks = "weeks", date_minor_breaks = "days", labels = function(b) {
            c("",format(b[2],"%b %d"),format(b[3:5],"%d"),format(b[6],"%b %d"), format(b[7:(length(b)-1)],"%d"), "")
        }
    ) +
    scale_color_discrete("Fitting Model", guide = guide_legend(override.aes = list(alpha = 1))) +
    theme_minimal(base_size = 14)

resp <- (ppanels | gppanel) + plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave(tail(.args, 1), resp, width = 8, height = 5, units = "in", dpi = 600)
