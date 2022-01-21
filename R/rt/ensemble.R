
suppressPackageStartupMessages({
    require(data.table)
})

.debug <- c("2021-11-27", "2021-12-06")[2]
.args <- if (interactive()) {
    c(
        file.path(
            "analysis", "input", "incidence.rds"
        ),
        file.path(
            "analysis", "input", "simDates.rda"
        ),
        file.path(
            "analysis", "output", .debug, "ensemble.rds"
        ),
        file.path(
            "analysis", "output", .debug, "incidence_ensemble.rds"
        )
    )
} else {
    commandArgs(trailingOnly = TRUE)
}

end.date <- as.Date(basename(dirname(tail(.args, 1))))

dt <- readRDS(.args[1])[between(date,"2021-10-01",end.date) & province != "ALL"]
load(.args[2])
ens.dt <- readRDS(.args[3])[, province := regionkey[as.character(prov)] ]
dt[, time := as.numeric(date - zeroDate) ]

res.dt <- ens.dt[
    sample <= 2
][dt, on=.(province), allow.cartesian = TRUE]

baselogis <- function(
    tvec, # observation times
    loc, delta_r, # fit parameters
    lodrop = -Inf, logain = -Inf
){
    drop <- plogis(lodrop)
    gain <- plogis(logain)
    ptrue <- plogis((tvec-loc)*delta_r)
    
    return(ptrue*(1-gain) + (1-ptrue)*drop)
}

set.seed(8675309)
inc.dt <- readRDS(.args[1])[
    fits, on = .(province), allow.cartesian = TRUE, nomatch = 0
][between(date, day0, end.date)][, time := as.numeric(date - day0) ][,
    var := {
        #' TODO split with split estimates
        propreinf <- propprimary <- baselogis(time, loc, deltar)
        dp <- rbetabinom(.N, propprimary, inf1, beta_shape)
        dr <- rbetabinom(.N, propreinf, tot-inf1, beta_shape)
        dp + dr
    }
]

saveRDS(inc.dt[, .(sample = sample_no, prov, date, ref = tot - var, var)], tail(.args, 1))
