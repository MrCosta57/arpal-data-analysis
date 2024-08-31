compute_arima_IC <- function(arima_models, suffixes, order_by = "AICc") {
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

compute_forecast_metrics <- function(model, h, ts_train, ts_test) {
    new_data <- c(as.numeric(ts_train), as.numeric(ts_test[1:h]))
    new_ts <- ts(new_data, frequency = frequency(ts_train), start = start(ts_train))
    metrics <- model %>%
        forecast(h = h) %>%
        accuracy(new_ts)

    return(metrics)
}

forecast_residuals_analysis <- function(model, h, model_name) {
    res <- forecast(model, h = h)$residuals
    mres <- mean(res)
    sres <- sqrt(mean(res^2) - mean(res)^2)
    plot(res, main = paste("Residuals of", model_name), ylab = "", xlab = "Day", col = 4)
    abline(h = 0, col = 2, lwd = 2)
    abline(h = mres + 2 * sres, col = 2, lwd = 2, lty = 2)
    abline(h = mres - 2 * sres, col = 2, lwd = 2, lty = 2)

    par(mfrow = c(1, 2))
    a <- density(res)
    hist(res, main = paste("Histogram of residuals:\n", model_name), freq = FALSE, ylim = range(a$y), col = 4)
    lines(a)
    qqPlot(res, main = paste("QQ-plot of residuals:\n", model_name, col = 4))
    par(mfrow = c(1, 1))

    Acf(res, main = paste("ACF of residuals:\n", model_name))
    checkresiduals(res, plot = FALSE)
}

plot_forecast <- function(model, h, ts_train, ts_test, ylab, main) {
    y_hat <- forecast(model, h = h)
    ts_test <- window(ts_test, start = time(ts_test)[1], end = time(ts_test)[h])

    # Compute the range of all lines involved
    combined_range <- range(c(
        ts_train, ts_test,
        y_hat$lower[, "95%"],
        y_hat$upper[, "95%"],
        y_hat$lower[, "80%"],
        y_hat$upper[, "80%"]
    ))

    # Plot the original time series and the y_hat values for the test set with the calculated range
    plot(
        ts_train,
        main = main,
        ylab = ylab,
        xlim = range(do.call(c, lapply(list(ts_train, ts_test), time)), na.rm = TRUE),
        ylim = combined_range
    )
    grid()

    # 95% PI
    polygon(
        c(time(ts_test), rev(time(ts_test))),
        c(y_hat$lower[, "95%"], rev(y_hat$upper[, "95%"])),
        col = "#dbdbdf", border = NA
    )

    # 80% PI
    polygon(
        c(time(ts_test), rev(time(ts_test))),
        c(y_hat$lower[, "80%"], rev(y_hat$upper[, "80%"])),
        col = "#b1b5ce", border = NA
    )

    # True
    lines(ts_test, col = "red", lwd = 2)

    # y_hat
    lines(y_hat$mean, col = "#0d82d1", lwd = 2)

    # Legend
    legend(
        "topleft",
        legend = c("True", "y_hat", "80% CI", "95% CI"),
        col = c("tomato", "#0d82d1", "#b1b5ce", "#dbdbdf"),
        lty = c(1, 1, 1, 1),
        lwd = c(2, 2, 2, 2),
        bg = "white"
    )
}


plot_forecast2 <- function(model, h, train_ts, test_ts) {
    forecast_results <- forecast(model, h = h)
    df <- data.frame(
        time = c(time(train_ts), time(test_ts)), # Combine time indices
        value = c(as.numeric(train_ts), as.numeric(test_ts)), # Combine actual values
        type = c(rep("Train", length(train_ts)), rep("Test", length(test_ts))) # Mark Train/Test
    )

    # Add forecasted values and intervals
    forecast_df <- data.frame(
        time = time(forecast_results$mean),
        value = as.numeric(forecast_results$mean),
        lower = as.numeric(forecast_results$lower[, 2]), # 95% lower bound
        upper = as.numeric(forecast_results$upper[, 2]), # 95% upper bound
        type = "Forecast"
    )

    # Combine both data frames
    combined_df <- rbind(df, forecast_df)

    ggplot(data = combined_df, aes(x = time, y = value, color = type)) +
        geom_line(size = 1) + # Plot the lines
        geom_ribbon(data = forecast_df, aes(ymin = lower, ymax = upper), fill = "blue", alpha = 0.2) + # Add prediction intervals
        labs(
            title = "Forecast vs Actual Data",
            x = "Time",
            y = "Value"
        ) +
        theme_minimal() +
        scale_color_manual(values = c("Train" = "black", "Test" = "red", "Forecast" = "blue"))
}
