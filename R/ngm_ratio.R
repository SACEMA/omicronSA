
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

delay_gamma <- function(mu, shape, t_max, t_step) {
  scale = mu / shape;
  t_points = seq(0, t_max, by = t_step);
  heights = pgamma(t_points + t_step/2, shape, scale = scale) - 
    pgamma(pmax(0, t_points - t_step/2), shape, scale = scale);
  return (data.table(t = t_points, p = heights / sum(heights)))
}

mean_dur <- function(dX, time_step) {
  ts <- seq(0, by=time_step, length.out = length(dX))
  sum(dX * ts)
}

ngmR <- function(
  dIp = delay_gamma(1.5, 4.0, t_max = 15, t_step = 0.25)$p,
  dIs = delay_gamma(3.5, 4.0, t_max = 15, t_step = 0.25)$p,
  dIa = delay_gamma(5.0, 4.0, t_max = 15, t_step = 0.25)$p,
  mixmatrix, mweights,
  fIs = 1, fIp = 1, fIa = 0.5,
  uval, yval,
  u_multiplier = 1, durmultiplier = 1
) {
  durIp <- mean_dur(dIp, 0.25)*durmultiplier
  durIs <- mean_dur(dIs, 0.25)*durmultiplier
  durIa <- mean_dur(dIa, 0.25)*durmultiplier
  
  mixing <- Reduce(`+`, mapply(function(c, m) c*m, mixmatrix, mweights, SIMPLIFY = FALSE))
  ngm = uval * u_multiplier * t(t(mixing) * (
    yval * (fIp * durIp + fIs * durIs) +(1 - yval) * fIa * durIa))
  Re(eigen(ngm)$values[1])
}

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
