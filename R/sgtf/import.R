
suppressPackageStartupMessages({
    require(data.table)
    require(haven)
})

.args <- if (interactive()) { c(
    file.path("refdata", "sgtf_list_anon_20211220_updated.dta"),
    file.path("refdata", "sgtf_list_anon.rds")
) } else commandArgs(trailingOnly = TRUE)

warn <- function(ws, to = stderr()) invisible(sapply(ws, function(w) write(w, file = to)))

sgtf <- as.data.table(read_dta(.args[1]))[,
    .(
        caseid_hash,
        province = as.character(province),
        publicprivateflag = as.factor(publicprivateflag),
        ct30 = as.logical(as.integer(ct30)),
        #' note: only ct30 == TRUE results in inputs
        sgtf = as.integer(sgtf),
        specreceiveddate = as.Date(specreceiveddate),
        specreportdate = as.Date(specreportdate),
        speccollectiondate = as.Date(speccollectiondate)
    )
]

sgtf.clean <- setkey(sgtf[!is.na(sgtf)], province, specreceiveddate)

if (sgtf[,.N] != sgtf.clean[,.N]) {
    warn(c(
        "WARN: Removing records with NA sgtf results.",
        sprintf(
        "%i caseid_hashes out of %i records:", sgtf[is.na(sgtf), .N], sgtf[, .N]
        ),
        sgtf[is.na(sgtf)][order(specreceiveddate), paste(caseid_hash, specreceiveddate, sep = " on ", collapse ="\n")]
    ))
}

#' make same date adjustment as in reinfections work:
#' if the specimen receipt date and report date are the same,
#' and are more than a week later than the collection date,
#' set the receive date (which is used as the reference date)
#' to the collection date
#'
#' n.b. assumes there are no receipt dates or report dates that are NA
correctingview <- expression(
    (specreceiveddate == specreportdate) &
    (as.numeric(specreceiveddate - speccollectiondate) > 7)
)
if (sgtf.clean[eval(correctingview), .N]) {
    cview <- sgtf[eval(correctingview)]
    warn(c(
        sprintf(
            "WARN: %i records have specimen receipt dates > specimen collection date + 7.",
            cview[,.N]
        ),
        "The following caseid_hash will have specimen receipt date set to collection date:",
        cview[, paste(caseid_hash, " : ", specreceiveddate, " => ", speccollectiondate, sep = "", collapse ="\n")]
    ))
}

sgtf.clean[eval(correctingview), specreceiveddate := speccollectiondate ]

saveRDS(sgtf.clean, tail(.args, 1))
