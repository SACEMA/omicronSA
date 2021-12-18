
suppressPackageStartupMessages({
    require(bbmle)
    require(data.table)
})

set.seed(1200)

.debug <- c("2021-11-27", "2021-12-06")[1]
.args <- if (interactive()) c(
    file.path("analysis", "input", "sgtf.rds"),
    file.path("R", "bbmle_utils.R"),
    file.path("analysis", "output", "sgtf", .debug[1], "sbetabin.rds")
) else commandArgs(trailingOnly = TRUE)

end.date <- basename(dirname(tail(.args, 1)))
sgtf.dt <- readRDS(.args[1])[date <= end.date]
source(.args[2])

#' Assume gain of S gene binding by BA.1 very unlikely;
#' fitting loss of S gene binding by background
fixList <- list(logain = -7)

#' Iterate until convergence
cList <- list(maxit = 10000)

#' TODO item to compartmentalize
bbsizemax <- 100

#' Logistic function with imperfect testing
baselogis <- function(
    tvec, # observation times
    loc, delta_r, lodrop,
    # fitting: offset, relative growth rate, drop
    logain # fixed: gain
){
    drop <- plogis(lodrop)
    gain <- plogis(logain)
    ptrue <- plogis((tvec-loc)*delta_r)
    
    return(ptrue*(1-gain) + (1-ptrue)*drop)
}

min.date <- sgtf.dt[, min(date)]
fit.ref <- sgtf.dt[,.(
   SGTF = sum(SGTF), nonSGTF = sum(nonSGTF), total = sum(total)
), by=.(prov, time = as.integer(date - min.date))]

res <- lapply(fit.ref[, unique(prov)], function(tarprov) {
    dt <- fit.ref[prov == tarprov]
    sbin <- list(
        loc = mean(dt$time),
        delta_r = 0.5,
        lodrop = -3, logain = fixList$logain,
        lbbsize = 0
    )
    
    m0 <- mle2(
        SGTF ~ dbetabinom_sigma(
            prob = baselogis(time, loc, delta_r, lodrop, logain)
            , size = total
            , sigma = exp(lbbsize)
        ) 
        , start = sbin, data = dt, method = "Nelder-Mead"
        , fixed = fixList
        , control = cList
    )
    
    if (coef(m0)[["lbbsize"]] > log(bbsizemax)) {
        return(NULL)
    } else {
        m <- update(m0, start = as.list(coef(m0)), fixed = fixList, method = "BFGS")
        ci <- try(confint(m))
        if (!inherits(ci, "matrix") || anyNA(ci)) {
            return(NULL)
        } else {
            return(list(m=m, ci=ci))
        }
    }
})

names(res) <- fit.ref[, unique(prov)]

ret <- res[!sapply(res, is.null)]

saveRDS(ret, tail(.args, 1))

#' @examples 
#' require(ggplot2)
#' fit.ref[res[q == "50 %"], on=.(prov), predict := baselogis(time, loc, delta_r, lodrop, logain)]
#' fit.ref[res[q == "50 %"], on=.(prov), predictBA1 := plogis((time-loc)*delta_r) ]
#' ggplot(fit.ref[!is.na(predict)]) + aes(time) + facet_grid(prov ~ .) +
#'     geom_point(aes(y=SGTF/total, size = total, color = "observed SGTF"), alpha = 0.4) +
#'     geom_line(aes(y=predict, color="predicted SGTF")) +
#'     geom_line(aes(y=predictBA1, color="predicted BA.1")) +
#'     theme_minimal() +
#'     coord_cartesian(ylim=c(0.01, 0.99)) +
#'     scale_y_continuous(trans="logit") +
#'     scale_size_area()
