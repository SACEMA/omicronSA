
suppressPackageStartupMessages({
    require(data.table)
    require(EpiNow2)
    require(ggplot2)
    require(scales)
    require(qs)
    require(devtools)
})



info <- devtools::session_info()
capture.output(info, file = "sessioninfo.txt")