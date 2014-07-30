adaptive.weights <- function(formula, data, verbose=FALSE, ...) {
    #Create the object that will hold the output
    result <- list()

    #Pull out the relevant data
    response.name = rownames(attr(terms(formula, data=data), 'factors'))[1]
    response.col = which(colnames(data)==response.name)
    predictor.names = attr(terms(formula, data=data), 'term.labels')

    response = as.matrix(data[,response.col])
    x = as.matrix(data[,-response.col])

    #Set up the lists to hold the adaptive weights:
    result[['meanx']] = colMeans(x)
    x.centered = sweep(x, 2, result[['meanx']])

    #Make the calls to glm
    coefs = list()
    for (predictor in predictor.names) {
        if (result[['normx']][[predictor]] < Inf) {
            model = lm(response~x.centered[,predictor])
            coefs[[predictor]] = coef(model)[2]
        } else {
            coefs[[predictor]] = 0
        }
    }

    result[['coefs']] = coefs
    result[['adaweight']] = sapply(predictor.names, function(x) abs(coefs[[x]]))

    return(result)
}
