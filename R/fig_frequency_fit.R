
suppressPackageStartupMessages({
    require(data.table)
    require(ggplot2)
    require(patchwork)
})

.args <- if (interactive()) c(
    file.path("refdata", "sgtf.rds"),
    file.path("analysis", "output",
        c("sgtf", file.path("fig", "frequencies.png"))
)) else {
    commandArgs(trailingOnly = TRUE)
}

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

sgtf.dt <- readRDS(.args[1])[specreceiveddate <= "2021-12-06"][, est_prop := SGTF/total ]
setnames(sgtf.dt, "specreceiveddate", "date")
fittings <- list.files(.args[2], "\\.rds", recursive = TRUE, full.names = TRUE)

#' TODO remove use.names once ensembling fixed upstream
allfits <- rbindlist(lapply(fittings, function(fl) {
    scnstr <- tail(strsplit(fl, .Platform$file.sep)[[1]], 2)
    enddate <- scnstr[1]
    model <- scnstr[2]
    res <- as.data.table(readRDS(fl))[, c("model", "enddate") := .(model, enddate)]
    res
}), use.names=TRUE)
allfits[, province := regionkey[as.character(prov)] ]

ppanels <- ggplot(
    allfits[province != "GAUTENG"][between(date, "2021-10-01", enddate)][sample <= 100]
) + aes(date, est_prop, color = model, group = interaction(sample, model, province, enddate)) +
    facet_grid(province ~ enddate) +
    geom_line(alpha = 0.05) +
    geom_point(aes(size=total, color="observed", group=NULL), data = sgtf.dt[province != "GAUTENG"], alpha = 0.5) +
    geom_blank(aes(size=total, color="observed", group=NULL), data = sgtf.dt[province == "GAUTENG"]) +
    coord_cartesian(ylim = c(0.005, 0.995)) +
    scale_y_continuous(NULL, breaks = c(0.01, 0.1, 0.25, 0.5, 0.75, 0.9, 0.99), trans = "logit") +
    scale_x_date(
        NULL, date_breaks = "weeks", date_minor_breaks = "days", labels = function(b) {
            c("",format(b[2],"%b %d"),format(b[3:5],"%d"),format(b[6],"%b %d"), format(b[7:(length(b)-1)],"%d"), "")
        }
    ) +
    scale_color_discrete("Fitting Model", guide = guide_legend(override.aes = list(alpha = 1))) +
    scale_size_area() +
    theme_minimal(base_size = 12) + theme(
       # axis.text.x = element_blank()
    )

ggsave("something.png", ppanels, width = 7.5, height = 10, dpi = 600, bg = "white")

gppanel <- ggplot(
    allfits[prov == "GP"][between(date,"2021-10-03", enddate)][sample <= 100]
) + aes(date, est_prop, color = model, group = interaction(sample, model, province, enddate)) +
    facet_grid(province ~ enddate) +
    geom_line(alpha = 0.05) +
    geom_point(aes(size=total, color="observed", group=NULL), data = sgtf.dt[province == "GAUTENG"], alpha = 0.5) +
    geom_blank(aes(size=total, color="observed", group=NULL), data = sgtf.dt[province != "GAUTENG"]) +
    coord_cartesian(ylim = c(0.005, 0.995)) +
    scale_y_continuous("Fraction SGTF", breaks = c(0.01, 0.1, 0.25, 0.5, 0.75, 0.9, 0.99), trans = "logit") +
    scale_x_date(
        NULL, date_breaks = "weeks", date_minor_breaks = "days", labels = function(b) {
            c("",format(b[2],"%b %d"),format(b[3:5],"%d"),format(b[6],"%b %d"), format(b[7:(length(b)-1)],"%d"), "")
        }
    ) +
    scale_color_discrete("Fitting Model", guide = guide_legend(override.aes = list(alpha = 1))) +
    scale_size_area() + theme_minimal(base_size = 12)

ggsave("something2.png", gppanel, width = 7.5, height = 7.5, dpi = 600, bg = "white")


resp <- (ppanels | gppanel) + plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave(tail(.args, 1), resp, width = 8, height = 5, units = "in", dpi = 600, bg = "white")
