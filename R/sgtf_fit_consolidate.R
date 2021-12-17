
.debug <- c("2021-12-06", "2021-11-27")[1]
.args <- if (interactive()) c(
    file.path("analysis", "output", "sgtf", .debug),
    file.path("analysis", "output", "sgtf", .debug, "mergedfit.rds")
) else commandArgs(trailingOnly = TRUE)

fit.list <- grep(
    basename(tail(.args, 1)),
    grep("sim", list.files(.args[1], "rdata$", full.names = TRUE), invert = TRUE, value = TRUE),
    invert = TRUE, value = TRUE
)

merged <- Reduce(function(bestseen, proposed) {
    newnms <- setdiff(names(proposed), names(bestseen))
    for (nm in newnms) bestseen[[nm]] <- proposed[[nm]]
    bestseen
}, lapply(fit.list[-1], readRDS), readRDS(fit.list[1]))

saveRDS(merged, tail(.args, 1))
