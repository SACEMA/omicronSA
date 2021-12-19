
suppressPackageStartupMessages({
    require(data.table)
    require(ggplot2)
    require(gghalves)
})

.debug <- c("2021-12-06", "2021-11-27")[1]
.args <- if (interactive()) file.path(
    "analysis", "output",
    c(
        file.path(.debug,"omicron_ratios.rds"),
        file.path("fig",.debug,"omicron_ratios.png")
    )
) else commandArgs(trailingOnly = TRUE)

enddate <- basename(dirname(tail(.args, 1)))
res.dt <- readRDS(.args[1])

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

res.dt[, province := regionkey[as.character(region)] ]

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
    }, by = .(region, province, date)],
    res.dt[, {
        qs <- quantile(ratiolow, probs = qlims, na.rm = TRUE)
        names(qs) <- names(qlims)
        c(as.list(qs), list(scenario = "omicronfast"))
    }, by = .(region, province, date)]
)

box.dt <- res.dt[between(date, "2021-11-14", "2021-11-20"), .(
    ratio=exp(mean(log(ratio))), ratiolow=exp(mean(log(ratiolow)))
), by=.(region, freq_sample, rt_sample)]

p <- function(dt) ggplot(dt) +
    aes(date, color = scenario, fill = scenario) +
    facet_wrap(~province) +
    geom_ribbon(
        aes(ymin = lo95, ymax = hi95, linetype = "95%"),
        alpha = 0.1, size = 0.25
    ) +
    geom_ribbon(
        aes(ymin = lo50, ymax = hi50, linetype = "50%"),
        alpha = 0.1, size = 0.25
    ) +
    geom_line(aes(y = md, linetype = "median")) +
    coord_cartesian(ylim = c(1, 6), xlim=as.Date(c("2021-11-01", enddate))) + 
    theme_minimal(base_size = 16) +
    scale_x_date(
        "Sample receipt date", date_breaks = "weeks", date_minor_breaks = "days", labels = function(b) {
            c("",format(b[2],"%b %d"),format(b[3:5],"%d"),format(b[6],"%b %d"), format(b[7:(length(b)-1)],"%d"), "")
        }
    ) +
    scale_y_continuous(expression(R[t]^"BA.1"/R[t]^"other")) + # nolint
    scale_linetype_manual(
        NULL, values = c(median = "solid", `50%` = "dashed", `95%`="dotted")
    ) +
    scale_fill_discrete(
        "Generation\nInterval", aesthetics = c("color", "fill")
        , labels = c(omicron = "Same GI", omicronfast = "shorter\nBA.1")
    ) +
    theme(
        legend.position = c(0.5, 0), legend.justification = c(0.5, 0),
        legend.direction = "horizontal"
    ) +
    geom_half_boxplot(aes(
            x=as.Date("2021-11-17"), y=ratio, color="omicron", fill=NULL
        ),
        data = function(dt) box.dt[region %in% unique(dt$region)],
        nudge = 0.1, width = 7, show.legend = FALSE, alpha = 0
    ) +
    geom_half_boxplot(aes(
            x=as.Date("2021-11-17"), y=ratiolow, color="omicronfast", fill=NULL
        ),
        data = function(dt) box.dt[region %in% unique(dt$region)],
        nudge = 0.1, width = 7, show.legend = FALSE, alpha = 0,
        side = "r"
    ) +
    geom_label(
        aes(
            x = as.Date("2021-11-21"),
            y = ratiolow, 
            label = round(ratiolow, 2),
            hjust = 0
        ), color="white",
        data = function(dt) box.dt[
            region %in% unique(dt$region), .(
                ratiolow = median(ratiolow), scenario = "omicronfast"
            ), by=region
        ], show.legend = F
    ) +
    geom_label(
        aes(
            x = as.Date("2021-11-13"), y=ratio,
            label = round(ratio, 2),
            hjust = 1
        ), color="white",
        data = function(dt) box.dt[
            region %in% unique(dt$region), .(
                ratio=median(ratio), scenario = "omicron"
            ), by=region], show.legend = F
    )

ggsave(tail(.args, 1), p(q.dt[region == "GP"]), width = 8, height = 5, dpi = 600, bg="white")

