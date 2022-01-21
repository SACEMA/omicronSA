
suppressPackageStartupMessages({
	require(jsonlite)
	require(EpiNow2)
})

.debug <- c("omicron", "delta")[2]
.args <- if (interactive()) file.path(
	"analysis", "input", sprintf("%s.json", .debug[1])
) else commandArgs(trailingOnly = TRUE)

#' comes from NCEM model assumptions
#' S or Rvarious to various Es = 1/gamma1
mean_latent_dur <- 1/(182.50/365)

#' typical durations of the various infectious routes
#' Iam1 to R1 = 1/r1
#' Ip1 to IsnotT or IsT = 1/gamma2
#' IsnotT1 to R1 or D = 1/r8
#' IsT1 to H1 or ICU1 = 1/taus

presymp_dur <- 1/(91.25/365)

utreat <- 1/(73/365)
treat <- 1/(91.25/365)
	
mean_inf_dur <- c(
	am = 1/(52.14/365),
	untreated = presymp_dur + utreat,
	treated = presymp_dur + treat
)

#' proportion severe
#' 0.03 (for reference age) - first infections
#' 0.01 (for reference age) - reinfections
#' by age category: 0.5
age_RRs <- c(0.5, 0.5, 1.5, 3, 4, 5, 8)

#' TODO need age specific attack mixture?
weighted.severe <- readRDS(
	"refdata/NCEMpop.rds"
)[Province == "ALL"][, sum(prop*age_RRs*0.03) ]
#' treatment seeking proportions
tsprop <- 0.7

#' proportion in those routes
prop_inf <- c(
	am = 1-weighted.severe,
	untreated = weighted.severe*(1-tsprop),
	treated = weighted.severe*tsprop
)

zeta1 <- 0.88
pa <- 0.88
amfact <- (zeta1*pa + 1*(1-pa)*weighted.severe)/(pa+(1-pa)*weighted.severe)

#' relative infectiousness of those routes
rel_inf <- c(
	am = amfact,
	untreated = (presymp_dur*zeta1 + utreat)/(presymp_dur + utreat),
	treated = (presymp_dur*zeta1 + treat)/(presymp_dur + treat)
)

mean_generation_interval <- mean_latent_dur +
	sum(mean_inf_dur*prop_inf*rel_inf)/(2*sum(prop_inf*rel_inf))

#' assume generation times distributed same as reference GI
gen_time <- function(meangi) {
	generation_time <- as.list(
		EpiNow2::generation_times[
			disease == "SARS-CoV-2", .(
				mean, mean_sd, sd, sd_sd, max = 30
			)]
	)
	
	tarmcv <- generation_time$mean_sd / generation_time$mean
	tarscv <- generation_time$sd_sd / generation_time$sd
	tarcv <- generation_time$sd / generation_time$mean
	
	generation_time$mean <- meangi
	generation_time$mean_sd <- generation_time$mean * tarmcv
	generation_time$sd <- generation_time$mean * tarcv
	generation_time$sd_sd <- generation_time$sd * tarscv
	generation_time
}

generation_time <- gen_time(mean_generation_interval)

detection_delay <- EpiNow2::bootstrapped_dist_fit(
	Filter(function(d) d <= 14, rexp(10000, 1/(mean_latent_dur + presymp_dur))),
	max_value = 14
)

write_json(
	list(gi=generation_time, inc=incubation_period), tail(.args, 1),
	pretty = TRUE, auto_unbox = TRUE
)
