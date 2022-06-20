
.pkgs <- c("data.table", "jsonlite")
stopifnot(all(sapply(.pkgs, require, character.only = TRUE, quietly = TRUE)))

.args <- if (interactive()) c(
  file.path("refdata", "NCEM.json"),
  file.path("refdata", "Khoury_et_al_Nat_Med_fig_1a.csv"),
  file.path("analysis", "input", "susceptibility.rds"), # TODO move to refdata
  file.path("refdata", "contact_matrices.rds"),
  file.path("analysis", "output", "ngm_ratios.rds")
) else commandArgs(trailingOnly = TRUE)

khoury_nat_med_1a = fread(.args[2])

approx_titre <- function(protective_eff) with(
	khoury_nat_med_1a,
	approx(Efficacy, NeutTitreRelConvPlasma, protective_eff*100)$y
)

approx_eff <- function(titre_ratio) with(
	khoury_nat_med_1a,
	approx(NeutTitreRelConvPlasma, Efficacy, titre_ratio)$y/100
)

adj_eff <- function(init_eff, titre_ratio) {
	res <- approx_eff(approx_titre(init_eff)*titre_ratio)
	res[init_eff == 0] <- 0
	res
}

ncemparams <- within(read_json(
	.args[1], simplifyVector = TRUE
), {
	pss <- pmin(pss_scale * pss_ratio, 0.95)
	psr <- pmin(psr_scale * psr_ratio, 0.95)
	
	zeta <- list(
		asymptomatic = (zeta1*pa + 1*(1-pa)*pss)/(pa+(1-pa)*pss),
		presymptomatic = zeta1
	)
	
	names(pts) <- provorder
	names(eff) <- efforder
	
	dur_asympmild <- 1/r1
	dur_presymptomatic <- 1/gamma2
	dur_treated <- 1/taus
	dur_untreated <- 1/r8
	
	# element 1 varies by age, since zeta[[1]] is age specific
	# element 2 varies by province, since pts is province specific
	weight_symp <- list(
		AM = dur_asympmild*zeta[[1]],
		S = dur_presymptomatic*zeta[[2]] + pts*dur_treated + (1-pts)*dur_untreated
	)

	p <- array(pa,
		dim=c(length(pss), length(eff), length(weight_symp)),
		dimnames = list(age = 1:length(pss), fromstate = names(eff), toinf = names(weight_symp))
	)
	
	p[,"S","S"] <- (1-pa)*pss
	p[,"R","S"] <- (1-pa)*psr
	p[,"jj","S"] <- p[,"S","S"]*(1-peffjj)
	p[,"p1","S"] <- p[,"S","S"]*(1-peffp1)
	p[,"p2","S"] <- p[,"S","S"]*(1-peffp2)
	p[,"jjR","S"] <- p[,"R","S"]*(1-peffjj)
	p[,"p1R","S"] <- p[,"R","S"]*(1-peffp1)
	p[,"p2R","S"] <- p[,"R","S"]*(1-peffp2)
	p[,,"AM"] <- 1 - p[,,"S"]
	
})

ncemparams$frac_pop <- {
	dt <- readRDS(.args[3])
	mlt.dt <- melt(
		dt[date == "2021-11-15"][agegrp %in% 1:7][order(agegrp), .SD, .SDcols = -c("date")],
		id.vars = c("province", "agegrp")
	)
	mlt.dt[, compartment := fcase(
		variable == "Rescapable", "R",
		variable == "Vescapable", "jj",
		default = "other"
	)]
	mlt.dt$variable <- NULL
	exp.dt <- dcast(mlt.dt[compartment != "other"][, {
		.(
			compartment = factor(
				c(compartment, "jjR", "p1", "p2", "p1R", "p2R"),
				levels = ncemparams$efforder,
				ordered = TRUE
			),
			value = c(value[1], value[2]/6, rep(value[2]/6, 5)) 
		)
	}, by=.(province, agegrp)][order(compartment)], province + agegrp ~ compartment)
	exp.dt[, S := 1 - rowSums(.SD), .SDcols = -c("province", "agegrp")]
	
	setNames(lapply(
		exp.dt[, unique(province)],
		function(pv) {
			x <- as.matrix(exp.dt[province == pv, -c(1,2), with = FALSE])
			dimnames(x) <- list(age=1:7, compartment=colnames(x))
			x
		}
	), mlt.dt[, unique(province)])
}

ncemparams$contact <- {
	cm <- readRDS(.args[4])
	lapply(cm, function(cms) Reduce(`+`, cms))
}

#' for extracting province specific parameters
get_pars <- function(province) within(ncemparams, {
	# pick out province specific parameters
	weight_symp[[2]] <- rep(weight_symp[[2]][province], length(weight_symp[[1]]))
	frac_pop <- frac_pop[[province]]
	contact <- contact[[province]]
})

#' compute the relative next generation matrix (i.e., w/o transmissibility scaling factor)
#' 
#' @param params a list of the NCEM parameters, from `get_pars` (i.e. for a particular province):
#'  - `weight_symp`, the weighted transmission, `W[frominf][fromage]`
#'  - `contacts`, a matrix of relative contact rates, `C[toage, fromage]`
#'  - asdfafs
#' @param immratio numeric, the fold ratio for immune protection (input into `adj_ratio(...)`)
#' 
#' @details Uses the states-at-infection approach to K_MN next generation matrix.
#' The states-at-infection are
#' 
#'  - \eqn{E_{AM}}, _Exposed_ individuals that will later transition
#' to \eqn{I_{AM}}, _Infectious_ individuals with asymptomatic or mild disease, and
#'  - \eqn{E_{S}}, _Exposed_ individuals that will later transition to \eqn{I_P}, _Infectious_
#'  presymptomatic individuals, that go onto severe disease infectious states, either treatment seeking
#'  or not
#'  
#' ... with additional stratification by age. The \eqn{I_{X}} states result in different
#' 
#' 
ngm <- function(
	params, immratio = 1
) with(params, {
	agecats <- as.integer(dimnames(frac_pop)$age)
	infcats <- names(weight_symp)
	# for convenience while iteratively constructing, make this a tensor
	# but can later reshape it with array(K, dim = rep(length(agecats)*length(infcats), 2))
	K <- array(NA,
		dim = rep(c(length(agecats), length(infcats)), times = 2),
		dimnames = list(
			toage = agecats, # to ages
			toinf = infcats, # to state-at-infection
			fromage = agecats, # from ages
			frominf = infcats # from state-at-infection
		)
	)
	
	eff <- adj_eff(eff, immratio)
	
  for (
  	ageto in as.integer(dimnames(K)$toage)
  ) for (
  	infto in dimnames(K)$toinf
  ) for (
  	agefrom in as.integer(dimnames(K)$fromage)
  ) for (
  	inffrom in dimnames(K)$frominf
  ) K[ageto, infto, agefrom, inffrom] <- 
  		weight_symp[[inffrom]][agefrom]*
  		contact[agefrom, ageto]*
  		sum(frac_pop[ageto,]*(1-eff)*p[ageto,,infto])
	
	K <- array(K, dim = rep(length(agecats)*length(infcats), 2))
	Re(eigen(K)$values[1])
})

ps <- get_pars("WC")
ngm(ps)

immune_escape <- seq(0, 1, by = 0.05)

assumed.AR <- AR.dt[
  timing, on = .(province = abbr, date = start)][,
  .(
    province, date, agegrp, Rescapable, Vescapable, non_reinfectable,
    inc.room = 1 - non_reinfectable - Rescapable - Vescapable
  )
]

sero.scns <- rbind(
  assumed.AR[, sero := "ref" ],
  copy(assumed.AR)[,
    c("Vescapable", "Rescapable") := .(
      Vescapable + inc.room * Vescapable/(Rescapable+Vescapable) * 0.5,
      Rescapable + inc.room * Rescapable/(Rescapable+Vescapable) * 0.5
    )
  ][, sero := "up"],
  copy(assumed.AR)[,
    c("Vescapable", "Rescapable") := .(
      Vescapable * 0.5, Rescapable * 0.5
    )
  ][, sero := "down"]
)

ref.dt <- ensemble.dt[, {
    u <- us(.SD); y <- ys(.SD)
    sero.scns[, {
        cs <- contact_schedule[region == province, c(home, work, other, school)]
        multi <- sapply(immune_escape, function(imm) ngmR(
          mixmatrix = cms[[province]],
          u_multiplier = 1 - (Vescapable + Rescapable) * (1 - imm) - non_reinfectable,
          mweights = cs,
          uval = u,
          yval = y))
        multiNoV <- sapply(immune_escape, function(imm) ngmR(
          mixmatrix = cms[[province]],
          u_multiplier = 1 - Vescapable - Rescapable * (1 - imm) - non_reinfectable,
          mweights = cs,
          uval = u,
          yval = y))
        multiShort <- sapply(immune_escape, function(imm) ngmR(
          mixmatrix = cms[[province]],
          u_multiplier = 1 - (Vescapable + Rescapable) * (1 - imm) - non_reinfectable,
          mweights = cs,
          uval = u,
          yval = y, durmultiplier = 0.5
        ))

        .(immune_escape = immune_escape, multiplier = multi, multiplierNo = multiNoV, multiShort = multiShort)
      }, by = .(sero, province)
    ]
  },
  by = epi_sample
]

saveRDS(ref.dt, tail(.args, 1))
