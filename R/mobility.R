
suppressPackageStartupMessages({
    require(data.table)
})

.debug <- "analysis"
.args <- if (interactive()) file.path(
    .debug[1],
    c(rep("input", 3), "output"),
    c("mobility.csv", "schools.csv", "schflag.csv", "mobility.rds")
) else commandArgs(trailingOnly = TRUE)

mob.dt <- fread(.args[1])[country_region_code == "ZA"]
mob.dt[, region := fifelse(sub_region_1 == "", "ALL", toupper(sub_region_1))]

#' ignores school flag - all ZAF school interventions are national
school.dt <- melt(
    fread(.args[2], drop = c(1,3))[country_code == "ZAF"],
    id.vars = "country_code"
)
school.dt[, date := as.Date(variable,"%d%b%Y") ]
school.dt[, school := 1-value/3 ]
school.dt[, school := nafill(school, "locf") ]

extract.dt <- mob.dt[,
 c("region", "date", grep("_change_",names(mob.dt), value = TRUE)), with = F
]

tomultiplier <- function(x) (100 + x) / 100

extract.dt[, work := tomultiplier(workplaces_percent_change_from_baseline)]
extract.dt[,
    other1 := tomultiplier(retail_and_recreation_percent_change_from_baseline)
]
extract.dt[,
    other2 := tomultiplier(grocery_and_pharmacy_percent_change_from_baseline)
]
extract.dt[,
    other3 := tomultiplier(transit_stations_percent_change_from_baseline)
]
extract.dt[, other4 := tomultiplier(parks_percent_change_from_baseline)]
extract.dt[, other := exp(fifelse(is.na(other3),
            log(other2), #' bizarre data gap
            (0.3 * log(other1) + 0.3 * log(other2) +
                0.3 * log(other3) + 0.1 * log(other4)
            )
        )
    )
]
extract.dt[, other := (nafill(other, "locf") + nafill(other, "nocb")) / 2]
extract.dt[, work := (nafill(work, "locf") + nafill(work, "nocb")) / 2]

res.dt <- extract.dt[,
    .(region, date, work, other)][
    school.dt, on = .(date), school := school
]

saveRDS(res.dt, tail(.args, 1))
