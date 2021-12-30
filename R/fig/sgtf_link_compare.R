
require(ggplot2)
require(data.table)

.args <- if (interactive()) c(
    file.path("refdata"),
    file.path("analysis", "input", "plotref.rda"),
    file.path("analysis", "output", "fig", "sgtf_reinf_comparison.png")
) else commandArgs(trailingOnly = TRUE)

sgtffiles <- list.files(.args[1], "sgtf_.+\\.csv", full.names = TRUE)
names(sgtffiles) <- gsub("^.+_(\\d+)\\.csv$","\\1",sgtffiles)
fit.ref <- rbindlist(lapply(sgtffiles, function(fn) {
    fread(fn)[,.(
        SGTF = sum(SGTF), nonSGTF = sum(nonSGTF), total = sum(SGTF + nonSGTF)
    ), by = .(prov, date) ]
}), idcol = "reinf")[between(date,"2021-10-01","2021-12-06")]

tarfile <- tail(.args, 1)

reinfcast <- dcast(
    fit.ref[, prop := SGTF/total ], prov + date ~ reinf, value.var = c("prop", "total")
)[, allequal := ((prop_30 == prop_60) & (prop_60 == prop_90) & (total_30 == total_60) & (total_60 == total_90)) ]

fit.ref[reinfcast, on=.(prov, date), allequal := allequal ]

load(.args[2])

p <- ggplot(reinfcast) + aes(date) +
    facet_wrap(~prov) +
    geom_sgtf_point(
        mapping = aes(y = prop, size = total, alpha = total, shape = "allequal", color = "allequal"),
        data = function(dt) dt[allequal == TRUE, .(prov, date, prop = prop_30, total = total_30)]
    ) +
    geom_sgtf_point(
        mapping = aes(y = SGTF/total, size = total, alpha = total, shape = reinf, color = reinf),
        data = fit.ref[allequal != TRUE]
    ) +
    theme_minimal() +
    coord_cartesian(ylim=c(0.01, 0.99)) +
    scale_y_logitprop() +
    scale_size_samples() +
    scale_alpha_samples() +
    scale_x_recieptdate() +
    scale_shape_manual(
        "Reinfection\nThreshold",
        values = c(allequal = 20, `30` = 17, `60` = 4, `90` = 18),
        breaks = c("allequal", c(30, 60, 90))
    ) +
    scale_color_discrete("Reinfection\nThreshold", breaks = c("allequal", c(30, 60, 90))) +
    theme(legend.position = "bottom")

saveslide(tail(.args, 1), p)
