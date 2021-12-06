
suppressPackageStartupMessages({
    require(data.table)
    require(EpiNow2)
})

.args <- if (interactive()) {
    c(
        file.path(
            "analysis", "input",
            c("incidence.rds", "sssims.rds")
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

freq <- as.data.table(readRDS(.args[2]))[, province := regionkey[as.character(prov)] ]

#' TODO some error in upstream results generation
freq <- freq[,.(est_prop = est_prop[1]), keyby=.(prov, province, date, sample)]

set.seed(8675309)
inc.dt <- readRDS(.args[1])[
    freq, on = .(date, province), allow.cartesian = TRUE, nomatch = 0
][,
    var := rbinom(.N, tot, est_prop)
][, # TODO: consider looking only at primary infections
    inf1var := rbinom(.N, inf1, est_prop)
]

saveRDS(inc.dt, tail(.args, 1))
