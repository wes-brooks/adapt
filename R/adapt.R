#' Fit an adaptive lasso model.
#'
#' This function produces a linear regression model via the adaptive lasso. Given a formula, a data.frame, and a selection.method, \code{adapt} will fit a linear model for the predictor in the formula, using the data, that minimizes the selection.method.
#'
#' @param formula symbolis representation of the model variables
#' @param data data.frame containing observations of the covariates and response from the formula
#' @param selection.criterion The selection criterion to minimize in model fitting
#' @param selectonly Use the adaptive lasso only for selection, doing coefficient estimation via OLS?
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

    args = list(...)

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
    class(result) = "adapt"
    result[['call']] = call
    result[['formula']] = as.formula(formula, env=data)
    result[['selectonly']] = selectonly
    result[['selection.criterion']] = selection.criterion = match.arg(selection.criterion)

    #Get the model data:
    Y = model.response(mf)
    X = model.matrix(mf, mtcars, contrasts)[,-attr(mt, "intercept")]
    vars = attr(mt, "term.labels")

    #Get the observation weights
    if (missing(weights)) w = rep(1, nrow(X))
    else w = as.vector(model.weights(mf))

    #Pull out the relevant data
    result[['response']] = attr(mt, 'variables')[attr(mt, 'response')]
    result[['predictors']] = vars
    result[['adapt']] = adaptive.weights(X, Y, family, w, vars)

    result[['al']] = adapt.step(X, Y, family, w, result[['adapt']], selection.criterion, ...)
    result[['lambda']] = result[['al']][['model']][['lambda']]

    if (selectonly) {
        variables = paste(result[['al']][['vars']], collapse="+")
        f = as.formula(paste(result[['response']], "~", variables, sep=""))
        m = glm(formula=f, data=data, family=family, weights=w)
        result[['glm']] = m
        result[['coef']] = coef(m)
        result[['fitted']] = m$fitted
        result[['residuals']] = m$residuals
        result[['actual']] = m$fitted + m$residuals
    } else {
        result[['coef']] = result[['al']][['coef']]
        result[['actual']] = Y
        result[['fitted']] = predict.adapt(result, data, type='response', lambda=result[['lambda']][result[['al']][['lambda.index']]])
        result[['residuals']] = result[['actual']] - result[['fitted']]
    }

    return(result)
}

