adaptive.weights <- function(formula, data, verbose=FALSE, ...) {
    #Create the object that will hold the output
    result <- list()

    #This is the amount of error to accept when declaring numbers equal:
    tol = .Machine$double.eps ^ 0.5

    #Pull out the relevant data
    response.name = rownames(attr(terms(formula, data=data), 'factors'))[1]
    response.col = which(colnames(data)==response.name)
    predictor.names = attr(terms(formula, data=data), 'term.labels')

    response = as.matrix(data[,response.col])
    x = as.matrix(data[,-response.col])

    #Set up the lists to hold the adaptive weights:
    result[['meanx']] = colMeans(x)
    x.centered = sweep(x, 2, result[['meanx']])

    #Get the OLS coefficient for each covariate
    coefs = list()
    for (predictor in predictor.names) {
        z = x.centered[,predictor]

        if (abs(max(z)-min(z)) < tol) {
            coefs[[predictor]] = 0
        } else {
            model = lm(response~x.centered[,predictor])
            coefs[[predictor]] = coef(model)[2]
        }
    }

    result[['coefs']] = coefs
    result[['adaweight']] = sapply(predictor.names, function(x) abs(coefs[[x]]))

    return(result)
}
