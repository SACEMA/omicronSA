
.pkgs <- c("data.table")
stopifnot(all(sapply(.pkgs, require, character.only = TRUE, quietly = TRUE)))

.args <- if (interactive()) c("refdata", file.path(
	"refdata", "rt_check.rds"
)) else commandArgs(trailingOnly = TRUE)

logfiles <- grep(
	basename(tail(.args, 1)),
	list.files(.args[1], "rt_.*\\.rds", full.names = TRUE),
	value = TRUE,
	invert = TRUE
)

log.dt <- rbindlist(lapply(logfiles, function (fn) {
	tmp <- readRDS(fn)
	fnb <- strsplit(tools::file_path_sans_ext(basename(fn)), "_")[[1]][2:3]
	tmp[, c("date", "scenario") := .(as.Date(fnb[1]), fnb[2]) ]
	tmp
}))[, .(
	timeouts = sum(timeout),
	divtrans = sum(divtrans > 10),
	bulkesslow = sum(bulkesslow),
	tailesslow = sum(tailesslow),
	total = .N
), by = .(date, scenario, prov)]

saveRDS(log.dt, tail(.args, 1))