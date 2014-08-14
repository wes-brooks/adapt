adapt.step <- function(x, y, family, weights, adaptive.object, ...) {
    result = list()

    #Pull out the relevant data
    predictor.names = colnames(x)

    y = as.matrix(y)
    x = as.matrix(x)

    m <- ncol(x)
    n <- nrow(x)
    p.max = min(m-2, floor(n/2))

    #Center the covariates and apply the adaptive scaling
    result[['scale']] = scalex = adaptive.object[['adaweight']]
    result[['meanx']] = meanx = colMeans(x)
    adaweights = adaptive.object[['adaweight']]
    x.centered = sweep(x, 2, meanx, '-')
    x.scaled = sweep(x.centered, 2, scalex, '*')

    cv.obj = NULL

    if (family[['family']] == 'binomial') {
        #family == 'binomial' requires p, 1-p to both be specified:
        result[['glmnet']] = model = do.call(glmnet, c(list(x=x.scaled, y=as.matrix(cbind(1-y, y), nrow(x), 2), family='binomial', weights=weights, standardize=FALSE), list(...)))
        cv.obj = do.call(cv.glmnet, c(list(y=as.matrix(cbind(1-y, y), nrow(x), 2), x=x.scaled, nfolds=n, family='binomial', weights=weights, standardize=FALSE, grouped=FALSE), list(...)))
    } else {
        result[['glmnet']] = model = do.call(glmnet, c(list(x=x.scaled, y=y, family=family[['family']], weights=weights, standardize=FALSE), list(...)))
        cv.obj = do.call(cv.glmnet, c(list(y=y, x=x.scaled, nfolds=n, family=family[['family']], weights=weights, standardize=FALSE, grouped=FALSE), list(...)))
    }

    #Get the deviance and degrees of freedom, in order to compute the decision criteria
    result[['dev']] = dev = deviance(model)
    result[['df']] = df = sapply(predict(model, type='nonzero'), length) + 1
    result[['CV']] = cv = cv.obj$cvm
    cv1sd = cv.obj$cvup

    #Calculate the selection criteria
    result[['AIC']] = dev + 2*df
    result[['AICc']] = dev + 2*df + 2*df*(df+1)/(n-df-1)
    result[['BIC']] = dev + log(n)*df
    result[['CV.overshrink']] = cv[sapply(1:p.max, function(step) which(cv < cv1sd[step])[1])]
    result[['PRESS']] = cv

    #Summarize the model
    result[['coef']] = coef(model) %>% sweep(1, c(1, scalex), '*') %>% as.matrix
    result[['coef']][1,] = result[['coef']][1,] - colSums(sweep(result[['coef']][-1,], 1, meanx, '*'))

    return(result)
}
