
.pkgs <- c("data.table", "haven")
stopifnot(all(sapply(.pkgs, require, character.only = TRUE, quietly = TRUE)))

.args <- if (interactive()) file.path("refdata", c(
    "sgtf_list_anon_20220131.dta",
    "sgtf_list_anon.rds"
)) else commandArgs(trailingOnly = TRUE)

#' convenience function to send notes to stderr
#' make rules should redirect this to appropriate log
warn <- function(ws, to = stderr()) invisible(sapply(ws, function(w) write(w, file = to)))

#' n.b. factor provinces here are not the reference factor levels:
#' just used for later cleaning steps at this stage in code
#' the `as...` conversions cleanup the SASS conversion to standard
#' data types
sgtf.raw <- as.data.table(read_dta(.args[1]))[,.(
    caseid_hash = as.character(caseid_hash),
    province = factor(
        as.character(province),
        levels = as.character(unique(province))
    ),
    # publicprivateflag = as.factor(publicprivateflag),
    # ct30 = as.logical(as.integer(ct30)),
    #' note: only ct30 == TRUE results in inputs
    #' and publicprivateflag not used in decisions
    sgtf = as.integer(sgtf),
    specreceiveddate = as.Date(specreceiveddate),
    specreportdate = as.Date(specreportdate),
    speccollectiondate = as.Date(speccollectiondate)
)]

unloadNamespace("haven")

#' remove records with `is.na(sgtf)` results - these won't be usable
#' in analysis
sgtf.clean <- setkey(sgtf.raw[!is.na(sgtf)], province, specreceiveddate)

if (sgtf.raw[,.N] != sgtf.clean[,.N]) {
    #' currently, the sgtf == NA records are the only entries for these
    #' caseid_hash, but if these individuals did have conclusive results for other
    #' tests, might want to note?
    warn(c(
        "WARN: Removing records with `is.na(sgtf) == TRUE` results.",
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
            "WARN: %i test+, sgtf records have specimen receipt dates > specimen collection date + 7.",
            cview[,.N]
        ),
        "These caseid_hash will use collection date vice receipt date:",
        cview[, paste(caseid_hash, " : ",  speccollectiondate, " vice ", specreceiveddate, sep = "", collapse ="\n")]
    ))
}

sgtf.clean[,
    date := fifelse(eval(correctingview), speccollectiondate, specreceiveddate)
]

setkey(sgtf.clean, province, caseid_hash, date)
sgtf.clean[, test := 1:.N, by=caseid_hash]

#' based on Hay et al: https://dash.harvard.edu/handle/1/37370587
#' by inspection, all Ct trajectories fall w/in the 15 day window
#' Anything longer this window should be considered with more
#' nuance, but assume anything w/in is the same event
short.threshold <- 15

#' all multi-test episodes have test == 2
multi.episode <- sgtf.clean[test == 2, caseid_hash]

#' the entries to be taken directly
sgtf.singular <- sgtf.clean[
	!(caseid_hash %in% multi.episode),
	.(caseid_hash, province, sgtf, date, episode = 1)
]

#' now deal with multi-test episodes, by using short.threshold
#' for simple consolidation vs potentially splitting test outcomes
#' into multiple episodes

#' identify all the multi-test episodes spans
all.spans <- sgtf.clean[
    caseid_hash %in% multi.episode,
    {
    	spn <- range(date)
    	ret <- as.list(spn)
    	chg <- any(diff(sgtf) != 0)
    	names(ret) <- c("start", "end")
    	consensus <- unique(sgtf)
    	c(ret, list(
    		span = as.integer(diff(spn)),
    		changes = chg,
    		sgtf = ifelse(length(consensus)==1, consensus, NA_integer_),
    		tot = .N
    	))
    },
    by=caseid_hash
]

#' over a short period of time, an observed SG* status change could be due
#' to e.g. random probe failure when near the detection threshold early or
#' late in the infection course.
short.episodes <- all.spans[span <= short.threshold, caseid_hash]

#' coalesce as:
#'  - province by first in time (n.b., province.most below gives same result)
#'  - SGTF status = any 0 == 0 (n.b. sgtf.maj below only differs on one record)
#'  - date to earliest test date for individual
consolidate <- function(sd.dt) sd.dt[, .(
	province = province[which.min(date)],
	# province.most = levels(province)[which.max(tabulate(province))],
	sgtf = as.integer(!any(sgtf == 0)),
	# sgtf.maj = as.integer(sum(sgtf)/.N > 0.5),
	date = min(date)
)]

#' for all short multi-test episodes, just coalesce
sgtf.short <- sgtf.clean[
	caseid_hash %in% short.episodes,
	consolidate(.SD),
	by = caseid_hash
][, episode := 1 ]

#' however, log where consolidation includes changing SG* status
if (all.spans[caseid_hash %in% short.episodes][changes == TRUE, .N]) {
    short.shifts <- all.spans[caseid_hash %in% short.episodes][changes == TRUE, caseid_hash]
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

#' want:
#'  using a rolling reference point, consolidate into short.threshold episodes
#'  i.e. everything within first short.threshold days of first test,
#'  consolidate with first test, then go to next test outside short.threshold of first test,
#'  consolidate with that test, repeat
#'  
#'  this function takes an (ordered) vector of absolute delays from an initial reference time
#'  and converts them into relative delays against time points that are greater than
#'  threshold time away from previous reference
#'  0 values correspond to the new reference time points
rel.delays <- function(
	abs.delays, threshold = short.threshold
) return(abs.delays - Reduce(function(last.ref.delay, next.abs.delay) ifelse(
	next.abs.delay - last.ref.delay <= short.threshold,
	last.ref.delay,
	next.abs.delay
), abs.delays[-1], 0, accumulate = TRUE))

sgtf.long.ref <- sgtf.clean[order(date)][
    caseid_hash %in% long.episodes,
    cbind(.SD, delay = rel.delays(as.integer(date - min(date)))),
    by = caseid_hash,
    .SDcols = c("province", "sgtf", "date", "test")
]

sgtf.long.ref[delay == 0, episode := 1:.N, by=caseid_hash]
sgtf.long.ref[, episode := nafill(episode, "locf"), by=caseid_hash]
sgtf.long.reduced <- sgtf.long.ref[, consolidate(.SD), by=.(caseid_hash, episode)]

long.thresholds <- as.integer(c(30, 60, 90)) #' definitionally distinct results
sgtf.long.reduced[, c(
	"change", "delay", "cdelay"
) := .(
	c(1L, diff(sgtf)),
	c(0L, as.integer(diff(date))),
	as.integer(date - min(date))
), by=caseid_hash]

#' FIXME: currently relies on the fact that there's only one >2 episode
#' caseid_hash, which this logic works for. more generally, need to adjust
#' to behave like the rolling rebaseline used when consolidating records
sgtf.long <- rbindlist(lapply(long.thresholds, function(thr) {
	sgtf.long.reduced[order(date)][
		(change != 0) | (delay > thr) | (cdelay > thr),
		.(province, sgtf, date, episode = 1:.N, threshold = thr),
		by=caseid_hash
	]
}))

sgtf.all <- setkey(rbind(
	sgtf.singular[, threshold := NA_integer_ ],
	sgtf.short[, threshold := NA_integer_ ],
	sgtf.long
), province, date, caseid_hash)

saveRDS(sgtf.all, tail(.args, 1))
