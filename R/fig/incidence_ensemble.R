
suppressPackageStartupMessages({
    require(data.table)
    require(ggplot2)
})

.debug <- c("2021-12-06", "2021-11-27")[2]
.args <- if (interactive()) c(
    file.path("analysis", "input", "incidence.rds"),
    file.path("analysis", "output", .debug, "incidence_ensemble.rds"),
    file.path("analysis", "input", "plotref.rda"),
    file.path("analysis", "output", "fig", .debug, "incidence_ensemble.png")
) else commandArgs(trailingOnly = TRUE)

enddate <- basename(dirname(tail(.args, 1)))

e.dt <- readRDS(.args[2])[between(date,"2021-10-01", enddate)][sample <= 100]
raw.dt <- readRDS(.args[1])[province %in% unique(e.dt$province)]

load(tail(.args, 2)[1])

geom_truncated_series <- function(..., refdata) list(
    geom_line(..., data = function(dt) refdata[between(date,min(dt$date),max(dt$date))]),
    geom_point(..., data = function(dt) refdata[date >= max(dt$date)])
)

p <- ggplot(e.dt) +
    aes(date) + facet_wrap(~province) +
    geom_line(
        aes(y=tot-var, color="del", alpha = "ensemble", group=sample)) +
    geom_line(
        aes(y=var, color="var", alpha = "ensemble", group=sample)) +
    geom_truncated_series(aes(y=inf1, color="rep"), refdata = raw.dt) +
    geom_truncated_series(aes(y=tot, color="obs"), refdata = raw.dt) +
    scale_x_recieptdate() +
    scale_y_doubling(name="Detected Infections") +
    scale_color_incidence() +
    scale_alpha_ensemble() +
    coord_cartesian(ylim = c(2, NA), expand = FALSE) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom")

savehalfpage(tail(.args, 1), p)
