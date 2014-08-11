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
adapt <- function(formula, data, family=gaussian, weights=NULL, selection.criterion=c('AIC', 'BIC', 'AICc', 'CV', 'CV.overshrink'), selectonly=FALSE, na.action=na.omit) {
    call = match.call()

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
    mf = model.frame(formula, data, na.action=na.action)
    Y = model.response(mf)
    X = get_all_vars(mf)
    vars = attr(terms(mf), "term.labels")
print(terms(mf))
    #Pull out the relevant data
    result[['response']] = attr(mf, 'variables')[attr(mf, 'response')]
    result[['predictors']] = attr(mf, 'term.labels')

    result[['adapt']] = adaptive.weights(y=Y, x=X, family=family, weights=weights, predictor.names=vars)

    result[['al']] = adapt.step(x=X, y=Y, adaptive.object=result[['adapt']], selection.criterion=selection.criterion)
    result[['lambda']] = result[['al']][['model']][['lambda']][result[['al']][['lambda.index']]]

    if (selectonly) {
        variables = paste(result[['al']][['vars']], collapse="+")
        f = as.formula(paste(result[['response']], "~", variables, sep=""))
        m = lm(formula=f, data=data)
        result[['glm']] = m
        result[['coef']] = coef(m)
        result[['fitted']] = m$fitted
        result[['residuals']] = m$residuals
        result[['actual']] = m$fitted + m$residuals
    } else {
        result[['coef']] = result[['al']][['coef']]
        result[['actual']] = Y
        result[['fitted']] = predict.adapt(result, data)
        result[['residuals']] = result[['actual']] - result[['fitted']]
    }

    return(result)
}

