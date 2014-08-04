adaptive.weights <- function(x, y, verbose=FALSE, ...) {
    #Create the object that will hold the output
    result <- list()

    #This is the amount of error to accept when declaring numbers equal:
    tol = .Machine$double.eps ^ 0.5

    #Set up the lists to hold the adaptive weights:
    result[['meanx']] = colMeans(x)
    x.centered = sweep(x, 2, result[['meanx']])
    predictor.names = colnames(x)

    #Get the OLS coefficient for each covariate
    coefs = list()
    for (predictor in predictor.names) {
        z = x.centered[,predictor]

        if (abs(max(z)-min(z)) < tol) {
            coefs[[predictor]] = 0
        } else {
            model = lm(y~x.centered[,predictor])
            coefs[[predictor]] = coef(model)[2]
        }
    }

    result[['coefs']] = coefs
    result[['adaweight']] = sapply(predictor.names, function(x) abs(coefs[[x]]))

    return(result)
}
