
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

p <- ggplot(res[date > "2021-11-01"]) + aes(
    date, value, color = scenario, group = interaction(sample, scenario)
) + facet_wrap(~province) +
    geom_line(
        alpha=0.05, data = function(dt) dt[sample <= 100]
    ) + theme_minimal() + scale_x_date(NULL, date_labels = "%b %d", date_breaks = "weeks") +
    scale_y_continuous("Rt", breaks = 1:4) + geom_text(
        aes(label=round(value, 2), group=NULL),
        data = function(dt) dt[date == max(date), .(date, value=median(value)), by=.(province, scenario)],
        hjust = "right"
    )

ggsave(tail(.args, 1), p, width = 8, height = 8, dpi = 600, bg = "white")