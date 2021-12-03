
suppressPackageStartupMessages({
  require(data.table)
  require(qs)
})

.args <- if (interactive()) c(
  # 1-2: cm, yu data
  file.path(
    "refdata",
    c("contact_matrices.rds", "covidm_fit_yu.qs")
  ),
  # 3-5: sus, timing, mob
  file.path(
    "analysis",
    "input",
    c("susceptibility.rds", "timing.rds", "mobility.rds")
  ),
  file.path("..", "SA2UK_alt", "covidm"),
  file.path("analysis", "output", "ngm_ratios.rds")
) else commandArgs(trailingOnly = TRUE)

cms <- readRDS(.args[1])

set.seed(8675309)
yusamp <- qread(.args[2])[sample(.N, 1000, replace = FALSE)]

ensemble.dt <- yusamp[, .SD, .SDcols = grep("(u|y)_", names(yusamp))][,
                        epi_sample := 1:.N
]

AR.dt <- readRDS(.args[3])
timing <- readRDS(.args[4])
mob.dt <- readRDS(.args[5])

contact_schedule <- mob.dt[
  timing, on = .(region = province)][
  between(date, start, start + 6)][,
  .(
    home = 1, work = prod(work)^ (1 / .N),
    other = prod(other)^ (1 / .N),
    # school is often 0, so weight slightly differently 
    school = prod(school) ^ (1 / .N)
  ), by = .(region = abbr)
]

cm_path <- tail(.args, 2)[1]
cm_force_rebuild <- F;
cm_build_verbose <-  F;
cm_force_shared <-  T
cm_version <-  2
suppressPackageStartupMessages({
  source(file.path(cm_path, "R", "covidm.R"))
})

calllist <- list(
  "South Africa", "South Africa",
  dE  = cm_delay_gamma(2.5, 2.5, t_max = 15, t_step = 0.25)$p,
  dIp = cm_delay_gamma(1.5, 4.0, t_max = 15, t_step = 0.25)$p,
  dIs = cm_delay_gamma(3.5, 4.0, t_max = 15, t_step = 0.25)$p,
  dIa = cm_delay_gamma(5.0, 4.0, t_max = 15, t_step = 0.25)$p
)

calllist2 <- calllist
calllist2$dE <- cm_delay_gamma(1.25, 1.25, t_max = 15, t_step = 0.25)$p

#' note: population size irrelevant to NGM calculation,
#' and differences in provincial age distro already accounted for
#' in contact matrices and seropos values
params <- cm_base_parameters_SEI3R(
  deterministic = TRUE,
  pop = list(do.call(cm_build_pop_SEI3R, calllist))
)

params2 <- cm_base_parameters_SEI3R(
  deterministic = TRUE,
  pop = list(do.call(cm_build_pop_SEI3R, calllist2))
)

us <- function(sdt.row) {
  rep(
    sdt.row[, as.numeric(.SD), .SDcols = grep("^u_",names(sdt.row))],
    each = 2
  )
}

ys <- function(sdt.row) {
  rep(
    sdt.row[, as.numeric(.SD), .SDcols = grep("^y_",names(sdt.row))],
    each = 2
  )
}

immune_escape <- seq(0, 1, by = 0.05)

assumed.AR <- AR.dt[
  timing, on = .(province = abbr, date = start)][,
  .(
    province, date, agegrp, escapable, non_reinfectable,
    inc.room = 1 - non_reinfectable - escapable
  )
]

sero.scns <- rbind(
  assumed.AR[, sero := "ref" ],
  copy(assumed.AR)[, escapable := escapable + inc.room * 0.5][, sero := "up"],
  copy(assumed.AR)[, escapable := escapable * 0.5][, sero := "down"]
)

ref.dt <- ensemble.dt[, {
    u <- us(.SD); y <- ys(.SD)
    sero.scns[, {
        cs <- contact_schedule[region == province, c(home, work, other, school)]
        testpop <- params;
        testpop$pop[[1]]$matrices <- cms[[province]]
        multi <- sapply(immune_escape, function(imm) cm_eigen_ngm(
          testpop,
          u_multiplier = 1 - escapable * (1 - imm) - non_reinfectable,
          contact_reductions = 1 - cs,
          uval = u,
          yval = y)$R0)
        .(immune_escape = immune_escape, multiplier = multi)
      }, by = .(sero, province)
    ]
  },
  by = epi_sample
]

saveRDS(ref.dt, tail(.args, 1))
