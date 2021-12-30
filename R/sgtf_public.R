
suppressPackageStartupMessages({
    require(data.table)
})

.debug <- c(30, 60, 90)[1]
.args <- if (interactive()) c(
    file.path("refdata", sprintf(c("sgtf_ll_%i.rds", "sgtf_%i.csv"), .debug[1]))
) else commandArgs(trailingOnly = TRUE)

dt <- readRDS(.args[1])[, infection := c("reinfection", "primary")[(inf == 1)+1] ]

dt[, date := specreceiveddate ]

res <- dcast(dt, prov + specreceiveddate + infection ~ sgtf, fun.aggregate = length)

setnames(res, c("specreceiveddate", "0", "1"), c("date", "nonSGTF", "SGTF"))
setkey(res, prov, date, infection)

fwrite(res, tail(.args, 1))
