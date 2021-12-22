
suppressPackageStartupMessages({
    require(data.table)
    require(ggplot2)
})

.args <- if (interactive()) c(
    file.path("analysis", "output", "rt"),
    file.path("analysis", "output", "fig", "prime_vs_reinf.png")
)

fls <- list.files(.args[1], "estimate_samples.rds", recursive = TRUE, full.names = TRUE)

consolidate.dt <- rbindlist(lapply(fls, function(fl) {
    scenstr <- tail(strsplit(dirname(fl), split = .Platform$file.sep)[[1]], 3)
    enddate <- scenstr[3]
    region <- scenstr[2]
    scenario <- scenstr[1]
    res <- readRDS(fl)[variable == "R", .(sample, date, value)]
    res[, c("enddate", "province", "scenario") := .(as.Date(enddate), region, scenario) ]
    res
}))

p <- ggplot(consolidate.dt[date > "2021-11-01"]) + aes(
    date, value, color = scenario, linetype = factor(enddate),
    group = interaction(enddate, sample, scenario)
) + facet_wrap(~province) +
    geom_rect(aes(
        xmin=as.Date("2021-11-14")-0.5,
        xmax=as.Date("2021-11-20")+0.5,
        ymin=0, ymax=Inf
    ), data = data.table(0), color = "grey", alpha = 0.1, inherit.aes = FALSE) +
    geom_line(
        alpha=0.05, data = function(dt) dt[sample <= 100]
    ) + theme_minimal() + 
    scale_x_date("Specimen receipt date", date_labels = "%b %d", date_breaks = "months", date_minor_breaks = "weeks") +
    scale_y_continuous("Rt", breaks = 1:4) + 
    geom_text(
        aes(label=round(value, 2), group=NULL),
        data = function(dt) dt[, .(date = enddate-5, value=median(value)), by=.(enddate, province, scenario)],
        hjust = "left", show.legend = FALSE
    ) +
    scale_color_discrete(
        NULL,
        labels = c(inf1="Primary-Only", tot="w/ Reinfections"),
        guide = guide_legend(override.aes = list(alpha = 1))
    ) +
    scale_linetype_discrete("Date Truncation Date", guide = guide_legend(override.aes = list(alpha = 1))) +
    theme(
        legend.position = c(1,0), legend.justification = c(1,0),
        legend.direction = "horizontal")

ggsave(tail(.args, 1), p, width = 8, height = 6, dpi = 600, bg = "white")
