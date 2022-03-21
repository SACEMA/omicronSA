
.pkgs <- c("data.table")
stopifnot(all(sapply(.pkgs, require, character.only = TRUE, quietly = TRUE)))

.dtdebug <- c("2021-11-27", "2021-12-06")[1]
.scndebug <- c("delta", "omicron", "omicronredlat", "omicronredinf")[1]
.args <- if (interactive()) file.path(
	"refdata", c(
		sprintf("rt_%s_%s.log", .dtdebug, .scndebug),
		sprintf("rt_%s_%s.rds", .dtdebug, .scndebug)
	)
) else commandArgs(trailingOnly = TRUE)

#' read in all lines from logging.
#' should be stored as scenario key => optionally any lines about errors => scenario key
srclines <- readLines(.args[1], n = -1L)

keyregex <- "^\\w{2,3}_\\d+$"
#' pick out all the keys
linekeys <- grep(keyregex, srclines, value = TRUE)

#' preallocate error summarizing table
err.dt <- setkey(
	data.table(lk = linekeys)[,
		c("prov", "sample") := tstrsplit(lk, "_")
	],
	prov, sample
)

err.dt[, c("timeout", "divtrans", "bulkesslow", "tailesslow") := .(NA, NA_integer_, NA, NA) ]

parseerror <- function (charvec = character()) {
	dtrans <- as.integer(
		gsub(".* (\\d+) divergent transitions.*", "\\1", grep("\\d+ divergent transitions", charvec, value = TRUE))
	)
	if (!length(dtrans)) dtrans <- 0
	
	list(
		timeout = any(grepl("timed out", charvec)),
		divtrans = dtrans,
		bulkesslow = any(grepl("Bulk Effective Samples Size (ESS)", charvec)),
		tailesslow = any(grepl("Tail Effective Samples Size (ESS)", charvec))
	)
}

#' iterate through srclines to fill in table
#' search for next key, then interpret intervening lines
#' assert: first entry is always a key
currkey <- linekeys[1]
fromi <- toi <- 2

while (toi <= length(srclines)) {
	while ((toi < length(srclines)) & !(srclines[toi] %like% keyregex)) toi <- toi + 1
	(if (fromi == toi) { 
		character(0)
	} else {
		srclines[fromi:(toi-1)]
	}) -> slc
	# record non-error/warning outcome
	err.dt[lk == currkey, c("timeout", "divtrans", "bulkesslow", "tailesslow") := parseerror(slc) ]
	# update key
	currkey <- srclines[toi]
	# advance window
	fromi <- toi <- toi + 1
}

if (srclines[length(srclines)] %like% keyregex) err.dt[lk == currkey, c("timeout", "divtrans", "bulkesslow", "tailesslow") := parseerror() ]

saveRDS(err.dt, tail(.args, 1))