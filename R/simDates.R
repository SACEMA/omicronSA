
.args <- if (interactive()) {
	file.path("analysis", "input", "simDates.rda")
} else commandArgs(trailingOnly = TRUE)

simStart <- as.Date("2021-10-01")
simEnd   <- as.Date("2021-12-06")
zeroDate <- as.Date("2021-09-01")

#' TODO put in same order as factor
regionkey = c(
	EC="EASTERN CAPE",
	FS="FREE STATE",
	GP="GAUTENG",
	KZN="KWAZULU-NATAL",
	LP="LIMPOPO",
	MP="MPUMALANGA",
	NC="NORTHERN CAPE",
	NW="NORTH WEST",
	WC="WESTERN CAPE",
	ALL="ALL"
)

save(simStart, simEnd, zeroDate, regionkey, file = tail(.args, 1))