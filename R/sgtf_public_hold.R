
suppressPackageStartupMessages({
    require(data.table)
})

.debug <- c(90)[1]
.args <- if (interactive()) c(
    file.path("refdata", sprintf(c("sgtf_ll_%i.rds", "sgtf_%i.csv"), .debug[1]))
) else commandArgs(trailingOnly = TRUE)

dt <- readRDS(.args[1])[, infection := c("reinfection", "primary")[(inf == 1)+1] ]

dt[, date := specreceiveddate ]
dt[as.numeric(sgtfdate-date) >= 30, date := sgtfdate ]

res <- dcast(dt, prov + date + infection ~ sgtf, fun.aggregate = length)

setnames(res, c("0", "1"), c("nonSGTF", "SGTF"))
setkey(res, prov, date, infection)

fwrite(res, tail(.args, 1))
