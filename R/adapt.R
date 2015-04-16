#' Fit an adaptive lasso model.
#'
#' This function produces a linear regression model via the adaptive lasso. Given a formula, a data.frame, and a selection.method, \code{adapt} will fit a linear model for the predictor in the formula, using the data, that minimizes the selection.method.
#'
#' @param formula symbolic representation of the model variables
#' @param data data.frame containing observations of the covariates and response from the formula
#' @param weights vector of observation weights or the name of the column that gives the observation weights.
#' @param selection.criterion criterion to minimize in selection step of model fitting
#' @param selectonly use the adaptive lasso only for selection, doing coefficient estimation via OLS?
#' @param na.action how to handle NAs in the data
#' @param subset vector indicating which observations to use in model fitting
#' @param offset vector of offsets or the name of the offset variable in the data. The offset is added to the linear predictor for the corresponding observation.
#'
#' @return An object of class \code{adapt}.
#'
#' @export
adapt <- function(formula, data, family=gaussian, weights, selection.criterion=c('AIC', 'BIC', 'AICc', 'CV', 'CV.overshrink'), selectonly=FALSE, na.action=NULL, subset=NULL, offset=NULL, ...) {
    call = match.call()
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "weights", "na.action", "offset"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- quote(stats::model.frame)
    mf <- eval(mf, parent.frame())
    mt <- attr(mf, "terms")

    #Evaluate the family:
    if (is.character(family))
        family <- get(family, mode = "function", envir = parent.frame())
    if (is.function(family))
        family <- family()
    if (is.null(family$family)) {
        print(family)
        stop("'family' not recognized")
    }

    #Create the object that will hold the output
    result = list()
    result$call = call
    result$formula = as.formula(formula, env=data)
    result$selectonly = selectonly
    result$selection.criterion = selection.criterion = match.arg(selection.criterion)

    #Get the model data:
    Y = model.response(mf)
    X = model.matrix(mf, data=mf, contrasts)[,-attr(mt, "intercept")]
    vars = attr(mt, "term.labels")

    #Get the observation weights
    if (missing(weights)) w = rep(1, nrow(X))
    else w = as.vector(model.weights(mf))

    #Pull out the relevant data
    result$response = attr(mt, 'variables')[attr(mt, 'response')]
    result$predictors = vars
    result$adapt = adaptive.weights(X, Y, family, w, vars)

    adapt = adapt.step(X, Y, family, w, result$adapt, ...)
    result = c(result, adapt)

    result$lambda = result$glmnet$lambda
    result$lambda.index = lambdex = as.integer(which.min(result[[selection.criterion]]))
    vardex = predict(adapt$glmnet, type="nonzero")[lambdex][[1]]

    if (selectonly) {
        variables = paste(vars[vardex], collapse="+")
        f = as.formula(paste(result$response, "~", variables, sep=""))
        m = glm(formula=f, data=data, family=family, weights=w, na.action, subset, offset)
        result$glm = m
        result$coefficients = coef(m)
        result$fitted = m$fitted
        result$residuals = m$residuals
        result$actual = m$fitted + m$residuals
    } else {
        result$coefficients = result$coef
        result$actual = Y
        result$fitted = predict.adapt(result, mf, type='response')
        result$residuals = result$actual - result$fitted
    }

    class(result) = "adapt"
    return(result)
}

