#' Make predictions from an adaptive lasso model.
#'
#' @param obj an adaptive lasso model
#' @param newx the covariate values to use for prediction
#'
#' @export
predict.adapt <-  function(obj, newx) {
    pred.data = newx
    predictors = obj[['predictors']]
    colnames(pred.data) = colnames(newx)
    pred.data = pred.data[,predictors]

    if (obj[['selectonly']]) {
        predictions = predict(obj[['glm']], newx)
    } else {
        pred.data = sweep(pred.data, 2, obj[['al']][['meanx']], '-')
        pred.data = sweep(pred.data, 2, obj[['al']][['scale']], '*')
        predictions = predict(obj[['al']][['model']], newx=pred.data, s=obj[['al']][['lambda.index']], mode='step', type='fit')[['fit']]
    }

    return(predictions)
}
