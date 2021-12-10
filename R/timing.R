
suppressPackageStartupMessages({
    require(data.table)
})

.args <- if (interactive()) file.path(
    "analysis", "input", "timing.rds"
) else commandArgs(trailingOnly = TRUE)

ec.dt <- data.table(
    abbr = "EC",
    province = "EASTERN CAPE",
    wave = c("omicron"),
    start = as.Date("2021-11-14"),
    end   = as.Date(NA)
)

gp.dt <- data.table(
    abbr = "GP",
    province = "GAUTENG",
    wave = c("omicron"),
    start = as.Date(c("2021-11-14")),
    end   = as.Date(c(NA))
)

lp.dt <- data.table(
    abbr = "LP",
    province = "LIMPOPO",
    wave = c("omicron"),
    start = as.Date(c("2021-11-14")),
    end   = as.Date(c(NA))
)

mp.dt <- data.table(
    abbr = "MP",
    province = "MPUMALANGA",
    wave = c("omicron"),
    start = as.Date(c("2021-11-14")),
    end   = as.Date(c(NA))
)

nw.dt <- data.table(
    abbr = "NW",
    province = "NORTH WEST",
    wave = c("omicron"),
    start = as.Date(c("2021-11-14")),
    end   = as.Date(c(NA))
)

wc.dt <- data.table(
    abbr = "WC",
    province = "WESTERN CAPE",
    wave = c("omicron"),
    start = as.Date(c("2021-11-14")),
    end   = as.Date(c(NA))
)

#' not yet apparent in FS, KZN, NC
fs.dt <- data.table(
    abbr = "FS",
    province = "FREE STATE",
    wave = c("omicron"),
    start = as.Date(c(NA)),
    end   = as.Date(c(NA))
)

kz.dt <- data.table(
    abbr = "KZN",
    province = "KWAZULU-NATAL",
    wave = c("omicron"),
    start = as.Date(c(NA)),
    end   = as.Date(c(NA))
)

nc.dt <- data.table(
    abbr = "NC",
    province = "NORTHERN CAPE",
    wave = c("omicron"),
    start = as.Date(c(NA)),
    end   = as.Date(c(NA))
)

dt <- rbind(
    ec.dt, wc.dt, fs.dt, gp.dt, kz.dt, lp.dt, mp.dt, nc.dt, nw.dt
)[!is.na(start)]

saveRDS(dt, .args[1])

#' @examples checking for consistent start day of the week
#' dt[, .(date, wd=wday(start)), by=.(province) ]
