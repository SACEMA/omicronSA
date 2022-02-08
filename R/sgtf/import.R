
suppressPackageStartupMessages({
    stopifnot(all(sapply(c("data.table", "haven"), require, character.only = TRUE)))
})

.args <- if (interactive()) file.path("refdata", c(
    "sgtf_list_anon_20220131.dta",
    "sgtf_list_anon.rds"
)) else commandArgs(trailingOnly = TRUE)

warn <- function(ws, to = stderr()) invisible(sapply(ws, function(w) write(w, file = to)))

#' n.b. factor provinces here are not the reference factor levels:
#' just used for later cleaning steps at this stage in code
sgtf.raw <- as.data.table(read_dta(.args[1]))[,.(
    caseid_hash,
    province = factor(
        as.character(province),
        levels = as.character(unique(province))
    ),
    publicprivateflag = as.factor(publicprivateflag),
    ct30 = as.logical(as.integer(ct30)),
    #' note: only ct30 == TRUE results in inputs
    sgtf = as.integer(sgtf),
    specreceiveddate = as.Date(specreceiveddate),
    specreportdate = as.Date(specreportdate),
    speccollectiondate = as.Date(speccollectiondate)
)]

sgtf.clean <- setkey(sgtf.raw[!is.na(sgtf)], province, specreceiveddate)

if (sgtf.raw[,.N] != sgtf.clean[,.N]) {
    #' TODO: more detailed warning?
    #' currently, the sgtf == NA records are the only entries for these
    #' caseids, but if these individuals did have conclusive results for other
    #' tests, then would be fine
    warn(c(
        "WARN: Removing records with NA sgtf results.",
        sprintf(
            "%i caseid_hashes out of %i (%f%%):",
            sgtf.raw[is.na(sgtf), length(unique(caseid_hash))],
            sgtf.raw[, length(unique(caseid_hash))],
            sgtf.raw[, .(ct=all(is.na(sgtf))), by=caseid_hash][,100*sum(ct)/.N]
        ),
        sgtf.raw[is.na(sgtf)][order(specreceiveddate), paste(caseid_hash, specreceiveddate, sep = " on ", collapse ="\n")]
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
    cview <- sgtf.clean[eval(correctingview)]
    warn(c(
        sprintf(
            "WARN: %i records have specimen receipt dates > specimen collection date + 7.",
            cview[,.N]
        ),
        "These caseid_hash will use collection date vice receipt date:",
        cview[, paste(caseid_hash, " : ",  speccollectiondate, " vice ", specreceiveddate, sep = "", collapse ="\n")]
    ))
}

sgtf.clean[,
    date := fifelse(eval(correctingview), speccollectiondate, specreceiveddate)
]

sgtf.clean[order(date), test := 1:.N, by=caseid_hash]

#' all multi-test episodes have test == 2
multi.episode <- sgtf.clean[test == 2, caseid_hash]

#' the entries to be taken directly
sgtf.singular <- sgtf.clean[
	!(caseid_hash %in% multi.episode),
	.(caseid_hash, province, sgtf, date, episode = 1)
]

#' now deal with multi-test episodes, by setting a short threshold
#' for simple consolidation vs potentially splitting test outcomes
#' into multiple episodes

short.threshold <- 28

#' define short, multi-test episodes as all tests short.threshold days
short.episodes <- sgtf.clean[
    caseid_hash %in% multi.episode,
    .(span = as.integer(diff(range(date)))),
    by=caseid_hash
][span < short.threshold, caseid_hash]

#' coalesce as:
#'  - province by first in time (n.b., province.most below gives same result)
#'  - SGTF status = any 0 == 0 (n.b. sgtf.maj below only differs on one record)
#'  - date to earliest test date for individual
consolidate <- function(sd.dt) sd.dt[, .(
	province = province[1],
	# province.most = levels(province)[which.max(tabulate(province))],
	sgtf = as.integer(!any(sgtf == 0)),
	# sgtf.maj = as.integer(sum(sgtf)/.N > 0.5),
	date = min(date)
)]

#' for all short multi-test episodes, just coalesce
sgtf.short <- sgtf.clean[
	caseid_hash %in% short.episodes,
	consolidate(.SD),
	by=caseid_hash
][, episode := 1 ]

#' warn regarding coalescing that does anything other than consensus SGTF
short.mismatch <- sgtf.clean[
    caseid_hash %in% short.episodes,
    .(mismatch = any(diff(sgtf) != 0)),
    by=caseid_hash
]

if (short.mismatch[mismatch == TRUE, .N]) {
    short.shifts <- short.mismatch[mismatch == TRUE, caseid_hash]
    warn(c(
        sprintf(
            "WARN: %i caseid_hash have all tests w/in %i days & show SGTF shifts:",
            length(short.shifts), short.threshold
        ),
        sgtf.clean[order(date)][
            caseid_hash %in% short.shifts,
            .(
                res = sprintf("%s: %s (%i days)", caseid_hash, paste(range(date), collapse = " => "), as.integer(diff(range(date)))),
                shft = unique(grep("repeat", c("TF=>TP", "repeat", "TP=>TF")[diff(sgtf)+2], value = TRUE, invert = TRUE))
            ),
            by=caseid_hash
        ][, paste(res, shft, sep = " : ", collapse = "\n") ]
    ))
}

long.episodes <- setdiff(multi.episode, short.episodes)
sgtf.long <- sgtf.clean[order(date)][
    caseid_hash %in% long.episodes,
    cbind(.SD, delay = c(0, as.integer(diff(date))), change = c(0, diff(sgtf))),
    by = caseid_hash,
    .SDcols = c("province", "sgtf", "date")
]

#' for long episodes, look for the first episode where
#'  - delay from most recent test+ is greater than short.threshold
#'  - the test indication has changed
sgtf.long.processed <- sgtf.long[,{
	ind <- which.max((delay > short.threshold) & change != 0)
	res <- data.table()
	rem <- .SD
	if (ind != 1) {
		while (ind != 1) {
			res <- rbind(res, consolidate(rem[ind - 1]))
			rem <- rem[-(1:(ind-1))]
			ind <- rem[,which.max((delay > short.threshold) & change != 0)]
		}
		rbind(res, consolidate(rem))
	} else consolidate(rem)
}, by = caseid_hash]

change.ids <- sgtf.long.processed[,.N,by=caseid_hash][N!=1, caseid_hash]
non.changes <- setdiff(long.episodes, change.ids)

if (sgtf.long[caseid_hash %in% change.ids, .N]) {
	# TODO stderr output on swaps
	warn(c(
		sprintf(
			"WARN: %i caseid_hash have tests separated by >%i days with SGTF shifts:",
			length(change.ids), short.threshold
		),
		sgtf.long[order(date)][
			caseid_hash %in% change.ids,
			.(
				span = paste(date, collapse = "=>"),
				srs = paste(sgtf, collapse = "")
			),
			by=caseid_hash
		][, paste(sprintf("%s: %s == %s", caseid_hash, span, srs), collapse = "\n") ],
		"Consolidated as:",
		sgtf.long.processed[caseid_hash %in% change.ids,
							.(
								span = paste(date, collapse = "=>"),
								srs = paste(sgtf, collapse = "")
							),
							by=caseid_hash
		][, paste(sprintf("%s: %s == %s", caseid_hash, span, srs), collapse = "\n") ]
	))
}

if (sgtf.long[caseid_hash %in% non.changes, .N]) {
	warn(c(
		sprintf(
			"WARN: %i caseid_hash have tests separated by >%i consolidated into single episodes:",
			length(non.changes), short.threshold
		),
		sgtf.long[order(date)][
			caseid_hash %in% non.changes,
			.(
				span = paste(date, collapse = "=>"),
				srs = paste(sgtf, collapse = "")
			),
			by=caseid_hash
		][, paste(sprintf("%s: %s == %s", caseid_hash, span, srs), collapse = "\n") ],
		"Consolidated as:",
		sgtf.long.processed[caseid_hash %in% non.changes,
							.(
								span = paste(date, collapse = "=>"),
								srs = paste(sgtf, collapse = "")
							),
							by=caseid_hash
		][, paste(sprintf("%s: %s == %s", caseid_hash, span, srs), collapse = "\n") ]
	))
}

sgtf.all <- setkey(rbind(
	sgtf.singular,
	sgtf.short,
	sgtf.long.processed[order(date), episode := 1:.N, by=caseid_hash]
), province, date, caseid_hash)

checkneg <- setnames(sgtf.clean[caseid_hash %in% multi.episode][
	order(date),
	as.list(range(date)),
	by=caseid_hash
], c("V1", "V2"), c("first", "last"))

saveRDS(sgtf.all, tail(.args, 1))
saveRDS(checkneg, gsub("\\.", "_checkneg.",tail(.args, 1)))
