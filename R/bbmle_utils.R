
require(bbmle)
require(emdbook)

nobs.mle2 <- function(x) unique(lengths(x@data))

df.residual.mle2 <- function(x) nobs(x) - length(coef(x))

overdisp_fun <- function(model) {
    rdf <- df.residual(model)
    rp <- residuals(model, type="pearson")
    Pearson.chisq <- sum(rp^2)
    prat <- Pearson.chisq/rdf
    pval <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
    c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

quasi_table <- function(
    model,
    ctab = coef(summary(model)),
    phi = overdisp_fun(model)["ratio"]
) {
    return(within(as.data.frame(ctab), {
        `Std. Error` <- `Std. Error`*sqrt(phi)
        `z value` <- Estimate/`Std. Error`
        `Pr(>|z|)` <- 2*pnorm(abs(`z value`), lower.tail=FALSE)
    }))
}

## Holling type-2 probability with link functions on parameters
pfun <- function(logit_a, log_h, N) {
    a <- plogis(logit_a)
    h <- exp(log_h)
    return(1/(1 + a*h*N))
}

## modified beta binom parameterization
dbetabinom_sigma <- function(x, prob, size, sigma, log=FALSE) {
    return(emdbook::dbetabinom(x, prob, size, log=log
                               , theta=sigma/(prob*(1-prob)))
    )
}

## I'm curious why sbetabinom is in bbmle and dbetabinom is in emdbook,
##  but it probably doesn't matter ...
sbetabinom_sigma <- function(size, prob, sigma) {
    return(bbmle::sbetabinom(size, prob, theta=sigma/(prob*(1-prob))))
}
