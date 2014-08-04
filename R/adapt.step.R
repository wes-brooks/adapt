adapt.step <- function(x, y, adaptive.object, criterion) {
    result = list()

    #Pull out the relevant data
    predictor.names = colnames(x)

    y = as.matrix(y)
    x = as.matrix(x)

    m <- ncol(x)
    n <- nrow(x)
    p.max = min(m-2, floor(n/2))

    #Center the covariates and apply the adaptive scaling
    result[['meanx']] = meanx = adaptive.object[['meanx']]
    result[['scale']] = scalex = adaptive.object[['adaweight']]
    x.centered = sweep(x, 2, meanx, '-')
    x.scaled = sweep(x.centered, 2, scalex, '*')

    #Fit the LARS model
    result[['model']] = model = lars(x=x.scaled, y=y, type='lar', max.steps=p.max, normalize=FALSE)

    cv.obj = cv.lars(y=y, x=x.scaled, type='lar', index=1:p.max, K=n, plot.it=FALSE, mode='step', normalize=FALSE)
    cv1sd = cv.obj$cv.error + cv.obj$cv.error

    #Quantities that are needed for computing the selection criteria
    result[['fitted']] = predict.lars(model, newx=x.scaled, type='fit', mode='step')$fit
    result[['residuals']] = apply(result[['fitted']], 2, function(fit) y - fit)
    result[['ssr']] = ssr = apply(result[['residuals']], 2, function(x) sum(x**2))
    result[['s2']] = s2 = ssr[ncol(result[['residuals']])] / (n-1)
    result[['nonzero']] = names(which(model$beta != 0))
    result[['df']] = df = apply(model$beta, 1, function(x) sum(x != 0)) + 1

    #Calculate the selection criteria
    result[['AIC']] = n * log(ssr/n) + 2*df
    result[['AICc']] = n * log(ssr/n) + 2*df + 2*df*(df+1)/(nrow(resid)-df-1)
    result[['BIC']] = n * log(ssr/n) + log(n)*df
    result[['CV']] = cv.obj$cv
    result[['CV.overshrink']] = cv.obj$cv[sapply(1:p.max, function(step) which(cv.obj$cv < cv1sd[step])[1])]

    result[['lambda.index']] = lambdex = which.min(result[[criterion]])

    #Summarize the model fit
    result[['MSEP']] = cv.obj$cv
    result[['RMSEP']] = sqrt(result[['MSEP']])

    #Summarize the model
    result[['predictors']] = predictor.names
    result[['coef']] = model$beta
    result[['Intercept']] = predict(model, newx=matrix(0,1,m), type='fit', s=lambdex, mode='step')$fit

    return(result)
}
