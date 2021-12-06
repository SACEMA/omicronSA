
suppressPackageStartupMessages({
    require(data.table)
    require(ggplot2)
})

.args <- if (interactive()) file.path(
    "analysis",
    c("output", file.path("output", "fig")),
    c("omicron_ratios.rds", "omicron_ratios.png")
) else commandArgs(trailingOnly = TRUE)

res.dt <- readRDS(.args[1])

qspans <- c(0.95, 0.5)
qlims <- sort(c({
    tmp <- 0 + (1 - qspans) / 2
    names(tmp) <- sprintf("lo%i", qspans * 100)
    tmp
}, {
    tmp <- 1 - (1 - qspans) / 2
    names(tmp) <- sprintf("hi%i", qspans * 100)
    tmp
}, c(md = .5)))

q.dt <- rbind(
    res.dt[, {
        qs <- quantile(ratio, probs = qlims, na.rm = TRUE)
        names(qs) <- names(qlims)
        c(as.list(qs), list(scenario = "omicron"))
    }, by = .(region, date)],
    res.dt[, {
        qs <- quantile(ratiolow, probs = qlims, na.rm = TRUE)
        names(qs) <- names(qlims)
        c(as.list(qs), list(scenario = "omicronfast"))
    }, by = .(region, date)]
)

p <- function(dt) ggplot(dt) +
    aes(date, color = scenario, fill = scenario) +
    facet_wrap(~region) +
    geom_ribbon(
        aes(ymin = lo95, ymax = hi95, linetype = "95%"),
        alpha = 0.1, size = 0.25
    ) +
    geom_ribbon(
        aes(ymin = lo50, ymax = hi50, linetype = "50%"),
        alpha = 0.1, size = 0.25
    ) +
    geom_line(aes(y = md, linetype = "median")) +
    coord_cartesian(ylim = c(1, 4), xlim=as.Date(c("2021-10-03", "2021-11-27"))) + 
    theme_minimal(base_size = 16) +
    scale_x_date(
        NULL, date_breaks = "weeks", date_minor_breaks = "days", labels = function(b) {
            c("",format(b[2],"%b %d"),format(b[3:5],"%d"),format(b[6],"%b %d"), format(b[7:(length(b)-1)],"%d"), "")
        }
    ) +
    scale_y_continuous(expression(R[eff]^Omicron/R[eff]^Delta)) + # nolint
    scale_linetype_manual(
        NULL, values = c(median = "solid", `50%` = "dashed", `95%`="dotted")
    ) +
    scale_fill_discrete(
        "Generation\nInterval", aesthetics = c("color", "fill")
        , labels = c(omicron = "Same GI", omicronfast = "shorter\nOmicron")
    ) +
    theme(
        legend.position = c(0.5, 0), legend.justification = c(0.5, 0),
        legend.direction = "horizontal"
    )

ggsave(tail(.args, 1), p(q.dt[region == "GP"]), width = 14, height = 7, dpi = 600)
