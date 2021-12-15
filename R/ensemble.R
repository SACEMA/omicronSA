
suppressPackageStartupMessages({
    require(data.table)
})

.debug <- c("2021-12-06", "2021-11-27")[1]
.args <- if (interactive()) {
    c(
        file.path(
            "analysis", "input", "incidence.rds"
        ),
        file.path(
            "analysis", "output", "sgtf", .debug, "sims.rds"
        ),
        file.path(
            "analysis", "output", "incidence_ensemble.rds"
        )
    )
} else {
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

end.date <- as.Date(basename(dirname(.args[2])))
sims <- readRDS(.args[2])[date <= end.date]
sims[, province := regionkey[as.character(prov)] ]

#' TODO note need to exclude sample 64, 80 for negative slopes
sims[,any(diff(prop)<0),by=.(prov, sample)][V1==TRUE]

#' TODO: naming => preference order
#' something like pref_XX_MODELNAME

set.seed(8675309)
inc.dt <- readRDS(.args[1])[
    sims, on = .(date, province), allow.cartesian = TRUE, nomatch = 0
][,
    var := rbinom(.N, tot, prop)
][, # TODO: consider looking only at primary infections
    inf1var := rbinom(.N, inf1, prop)
]

saveRDS(inc.dt, tail(.args, 1))
