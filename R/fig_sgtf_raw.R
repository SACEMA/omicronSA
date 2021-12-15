
require(ggplot2)
require(data.table)

.args <- if (interactive()) c(
    file.path("analysis", "input", "sgtf.rds"),
    file.path("analysis", "output", "fig", "sgtf_trends.png")
) else commandArgs(trailingOnly = TRUE)

tarfile <- tail(.args, 1)
sgtf.dt <- readRDS(.args[1])[between(date,"2021-11-01","2021-12-06")]
fit.ref <- sgtf.dt[,.(
    SGTF = sum(SGTF), nonSGTF = sum(nonSGTF), total = sum(total)
), by=.(prov, date, publicprivateflag) ]

#' desire a conserved intensity visualization
#' i.e. something like area * alpha = constant
#' area should go up when samples go down
#' if area ~ 1/total, then alpha ~ total to maintain C

plotter <- function(dt, ref) ggplot(dt) + aes(date, color=publicprivateflag) +
    facet_wrap(~prov) +
    geom_point(aes(y=SGTF/total, size = sqrt(1/total), alpha = sqrt(total))) +
#    geom_point(aes(y=SGTF/total, color = "observed SGTF"), size = 0.25, alpha = 1) +
    geom_blank(
        aes(date, prop, size = sqrt(1/total), alpha = sqrt(total), color = NULL),
        ref[,.(date = date[.N], prop = max(SGTF/total), total=max(total))]
    ) +
    # geom_line(aes(y=predict, color="predicted SGTF")) +
    # geom_line(aes(y=predictBA1, color="predicted BA.1")) +
    theme_minimal() +
    coord_cartesian(ylim=c(0.01, 0.99)) +
    scale_y_continuous(trans="logit", breaks = c(0.01, 0.1, 0.25, 0.5, 0.75, 0.9, 0.99)) +
    scale_size_area(
        "Samples", breaks = sqrt(1/c(1,10,100,1000)), max_size = 10, labels = function(b) 1/b^2,
        guide = guide_legend(override.aes = list(alpha = c(0.1,0.3,0.7,1)))
    ) +
    scale_x_date(NULL, date_breaks = "months", date_minor_breaks = "weeks", date_labels = "%b") +
    scale_color_discrete(NULL) +
    scale_alpha_continuous(NULL, range = c(0.1, 1), guide = "none")

p <- (plotter(fit.ref[prov != "GP"], fit.ref) | (plotter(fit.ref[prov == "GP"], fit.ref)+theme(axis.title.y = element_blank()))) +
    plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave(tail(.args, 1), p, width = 14, height = 7, units = "in", dpi = 600, bg = "white")
