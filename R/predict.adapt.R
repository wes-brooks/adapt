#' Make predictions from an adaptive lasso model.
#'
#' @param obj an adaptive lasso model
#' @param newx the covariate values to use for prediction
#'
#' @export
predict.adapt <-  function(obj, newx, lambda=obj[['al']][['lambda']], type=c("link","response","coefficients","nonzero","class")) {
    pred.data = newx
    predictors = obj[['predictors']]
    pred.data = pred.data[,predictors] %>% as.matrix %>% sweep(2, obj[['al']][['meanx']], '-') %>% sweep(2, obj[['al']][['scale']], '*')
    type = match.arg(type)

    if (obj[['selectonly']]) {
        predictions = predict(obj[['glm']], newx)
    } else {
        predictions = predict(obj[['al']][['model']], newx=pred.data, s=lambda, type=type)
    }

    return(predictions)
}
