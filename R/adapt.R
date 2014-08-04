#' Fit an adaptive lasso model.
#'
#' This function produces a linear regression model via the adaptive lasso. Given a formula, a data.frame, and a selection.method, \code{adapt} will fit a linear model for the predictor in the formula, using the data, that minimizes the selection.method.
#'
#' @param formula symbolis representation of the model variables
#' @param data data.frame containing observations of the covariates and response from the formula
#' @param selection.method The selection criterion to minimize in model fitting
#' @param selectonly Use the adaptive lasso only for selection, doing coefficient estimation via OLS?
#'
#' @export
adapt <- function(formula, data, criterion=c('AIC', 'BIC', 'AICc', 'CV', 'CV.overshrink'), selectonly=FALSE, na.action=na.omit) {
    #Create the object that will hold the output
    result = list()
    class(result) = "adapt"
    result[['formula']] = as.formula(formula, env=data)
    result[['selectonly']] = selectonly
    result[['criterion']] = criterion = match.arg(criterion)

    #Get the model data:
    mf = model.frame(formula, data, na.action=na.action)
    Y = mf[,1]
    X = mf[,-1]

    #Drop any rows with NA values
    na.rows = (which(is.na(data))-1) %% dim(data)[1] + 1
    if (length(na.rows)>0)
        data = data[-na.rows,]
    result[['data']] = data

    #Pull out the relevant data
    result[['response']] = rownames(attr(terms(formula, data=data), 'factors'))[1]
    result[['predictors']] = attr(terms(formula, data=data), 'term.labels')
    response.col = which(colnames(data)==result[['response']])

    result[['adapt']] = adaptive.weights(y=Y, x=X)

    result[['lars']] = adapt.step(x=X, y=Y, adaptive.object=result[['adapt']], criterion=criterion)
    result[['lambda']] = result[['lars']][['model']][['lambda']][result[['lars']][['lambda.index']]]

    if (selectonly) {
        variables = paste(result[['lars']][['vars']], collapse="+")
        f = as.formula(paste(result[['response']], "~", variables, sep=""))
        m = lm(formula=f, data=data)
        result[['lm']] = m
        result[['coef']] = coef(m)
        result[['fitted']] = m$fitted
        result[['residuals']] = m$residuals
        result[['actual']] = m$fitted + m$residuals
    } else {
        result[['coef']] = result[['lars']][['coef']]
        result[['actual']] = Y
        result[['fitted']] = predict.adapt(result, data)
        result[['residuals']] = result[['actual']] - result[['fitted']]
    }

    return(result)
}

