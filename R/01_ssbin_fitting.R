
suppressPackageStartupMessages({
    require(bbmle)
    require(data.table)
})

set.seed(1200)

.debug <- c("2021-11-27", "2021-12-06")[2]
.args <- if (interactive()) c(
    file.path("analysis", "input", "sgtf.rds"),
    file.path("R", "bbmle_utils.R"),
    file.path("analysis", "output", "sgtf", .debug, "ssbin.rds")
) else commandArgs(trailingOnly = TRUE)

tarfile <- tail(.args, 1)
enddate <- as.Date(basename(dirname(tarfile)))

sgtf.dt <- readRDS(.args[1])[date <= enddate]
source(.args[2])

#' Assume gain of S gene binding by BA.1 very unlikely;
#' fitting loss of S gene binding by background
# fixList <- list(logain = -4.5)

#' Iterate until convergence
cList <- list(maxit = 10000)

#' Logistic function with imperfect testing
baselogis <- function(
    tvec, loc, delta_r, lodrop, logain
) {
    drop <- plogis(lodrop)
    gain <- plogis(logain)
    ptrue <- plogis((tvec-loc)*delta_r)
    return(ptrue*(1-gain) + (1-ptrue)*drop)
}

min.date <- sgtf.dt[, min(date)]
fit.ref <- sgtf.dt[,.(
   SGTF = sum(SGTF), nonSGTF = sum(nonSGTF), total = sum(total)
), by=.(prov, time = as.integer(date - min.date))]

sbin <- list(
    loc = as.numeric(as.Date("2021-10-15")-min.date),
    delta_r = 0.3,
    lodrop = -3
    , logain = -4
)

# fixList <- list(
#     logain = -7
# )

res <- lapply(fit.ref[, unique(prov)], function(tarprov) {
    dt <- fit.ref[prov == tarprov]
    
    m0 <- mle2(
        SGTF ~ dbinom(
            prob = baselogis(time, loc, delta_r, lodrop, logain)
            , size = total
        ) 
        , start = sbin, data = dt, method = "Nelder-Mead"
        #, fixed = fixList
        , control = cList
    )
    
    m <- update(m0, start = as.list(coef(m0)), method = "BFGS")
    
    ci <- try(confint(m))
    if (inherits(ci, "matrix") && anyNA(ci["logain",])) {
        sre <- as.list(coef(m))
        sre$logain <- -Inf
        m <- update(m, start = sre, fixed = list(logain=-Inf), method = "BFGS")
        ci <- try(confint(m))
    }
    if (!inherits(ci, "matrix") || anyNA(ci)) {
        return(NULL)
    } else {
        list(m=m, ci=ci)
    }
})

names(res) <- fit.ref[, unique(prov)]

ret <- res[!sapply(res, is.null)]

saveRDS(ret, tail(.args, 1))

#' @examples 
#' require(ggplot2)
#' fit.ref[res[q == "50 %"], on=.(prov), predict := baselogis(time, loc, delta_r, lodrop, logain)]
#' fit.ref[res[q == "50 %"], on=.(prov), predictBA1 := plogis((time-loc)*delta_r) ]
#' ggplot(fit.ref) + aes(time + min.date) + facet_grid(prov ~ .) +
#'     geom_point(aes(y=SGTF/total, size = total, color = "observed SGTF"), alpha = 0.4) +
#'     geom_line(aes(y=predict, color="predicted SGTF")) +
#'     geom_line(aes(y=predictBA1, color="predicted BA.1")) +
#'     theme_minimal() +
#'     coord_cartesian(ylim=c(0.01, 0.99)) +
#'     scale_y_continuous(trans="logit") +
#'     scale_size_area()
