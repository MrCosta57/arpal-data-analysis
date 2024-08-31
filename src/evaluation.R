get_arima_IC <- function(arima_models, suffixes, order_by = "AICc") {
    IC_values <- lapply(arima_models, function(model) {
        IC_values <- c(model$aic, model$aicc, model$bic)
        return(IC_values)
    })
    IC_values <- do.call(rbind, IC_values)
    IC_values <- as.data.frame(IC_values)
    colnames(IC_values) <- c("AIC", "AICc", "BIC")
    # merge name with suffix
    model_names <- sapply(1:length(arima_models), function(i) {
        paste0(
            "ARIMA(",
            length(arima_models[[i]]$model$phi), ",",
            length(arima_models[[i]]$model$Delta), ",",
            length(arima_models[[i]]$model$theta),
            ") ",
            suffixes[[i]]
        )
    })
    rownames(IC_values) <- model_names

    if (!is.null(order_by)) {
        IC_values <- IC_values[order(IC_values[[order_by]]), ]
    }
    return(IC_values)
}
