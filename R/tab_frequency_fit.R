
suppressPackageStartupMessages({
    require(data.table)
    require(ggplot2)
    require(patchwork)
    require(bbmle)
})

.debug <- c("2021-12-06", "2021-11-27")[1]
.args <- if (interactive()) c(
    file.path("analysis", "output", "sgtf", .debug, "mergedfit.rds"),
    file.path("analysis", "output", .debug, "frequencies.csv")
) else commandArgs(trailingOnly = TRUE)

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

enddate <- as.Date(basename(dirname(tail(.args, 1))))
startdate <- as.Date("2021-09-24")
fittings <- readRDS(.args[1])
delr.dt <- rbindlist(lapply(fittings, function(fit) {
    co <- as.list(coef(fit$m))
    ci <- fit$ci
    data.table(
        delr = sprintf("%.2g (%.2g, %.2g)", co$delta_r, ci["delta_r",1], ci["delta_r",2]),
        loc = sprintf("%s (%s, %s)", format(co$loc+startdate, "%b-%d"), format(ci["loc",1]+startdate, "%b-%d"), format(ci["loc",2]+startdate, "%b-%d"))
    )
}), idcol = "prov")

fwrite(delr.dt, tail(.args, 1))
