
suppressPackageStartupMessages({
    require(data.table)
})

.args <- if (interactive()) c(
    file.path("refdata", c("sgtf_ll.rds", "sgtf.csv"))
) else commandArgs(trailingOnly = TRUE)

dt <- readRDS(.args[1])[, infection := c("reinfection", "primary")[(inf == 1)+1] ]

res <- dcast(dt, prov + specreceiveddate + infection ~ sgtf, fun.aggregate = length)

setnames(res, c("specreceiveddate", "0", "1"), c("date", "nonSGTF", "SGTF"))
setkey(res, prov, date, infection)

fwrite(res, tail(.args, 1))
