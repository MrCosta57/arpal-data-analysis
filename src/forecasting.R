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
            get_model_name(arima_models[[i]]), " ",
            suffixes[[i]]
        )
    })
    rownames(IC_values) <- model_names

    if (!is.null(order_by)) {
        row_order <- order(IC_values[[order_by]])
        IC_values <- IC_values[row_order, ]
    }
    return(list(IC_values, row_order))
}

compute_forecast_metrics <- function(model, h, ts_train, ts_test, xreg_method = "mean") {
    if (xreg_method != "mean") {
        stop("Only mean method is supported for xreg_method")
    }

    new_data <- c(as.numeric(ts_train), as.numeric(ts_test[1:h]))
    new_ts <- ts(new_data, frequency = frequency(ts_train), start = start(ts_train))
    if (!is.null(model$xreg)) {
        metrics <- model %>%
            forecast(h = h, xreg = rep(mean(model$xreg), h)) %>%
            accuracy(new_ts)
    } else {
        metrics <- model %>%
            forecast(h = h) %>%
            accuracy(new_ts)
    }

    metrics <- metrics[, c("RMSE", "MAE", "MAPE", "MASE")]
    return(metrics)
}

get_forecast_metric_table <- function(models, h, ts_train, ts_test, xreg_method = "mean") {
    tmp_metrics_list <- list()
    tmp_model_name_list <- list()
    for (model in models) {
        metrics <- compute_forecast_metrics(model, h, ts_m_train, ts_m_test)[2, ]
        model_name <- get_model_name(model)
        tmp_metrics_list <- append(tmp_metrics_list, list(metrics))
        tmp_model_name_list <- append(tmp_model_name_list, list(model_name))
    }
    tmp_table <- do.call(rbind, tmp_metrics_list)
    rownames(tmp_table) <- tmp_model_name_list
    return(tmp_table)
}

forecast_residuals_analysis <- function(model, h, model_name = NULL, xreg_method = "mean") {
    if (xreg_method != "mean") {
        stop("Only mean method is supported for xreg_method")
    }

    if (!is.null(model$xreg)) {
        y_hat <- forecast(model, h = h, xreg = rep(mean(model$xreg), h))
    } else {
        y_hat <- forecast(model, h = h)
    }

    if (is.null(model_name)) {
        model_name <- get_model_name(model)
    }

    res <- y_hat$residuals
    mres <- mean(res, na.rm = TRUE)
    sres <- sqrt(mean(res^2, na.rm = TRUE) - mean(res, na.rm = TRUE)^2)
    plot(res, main = paste("Residuals of", model_name), ylab = "", xlab = "Day", col = 4)
    abline(h = 0, col = 2, lwd = 2)
    abline(h = mres + 2 * sres, col = 2, lwd = 2, lty = 2)
    abline(h = mres - 2 * sres, col = 2, lwd = 2, lty = 2)

    par(mfrow = c(1, 2))
    a <- density(res, na.rm = TRUE)
    hist(res, main = paste("Histogram of residuals:\n", model_name), freq = FALSE, ylim = range(a$y), col = 4)
    lines(a)
    qqPlot(res, main = paste("QQ-plot of residuals:\n", model_name, col = 4))
    par(mfrow = c(1, 1))

    Acf(res, main = paste("ACF of residuals:\n", model_name))
    checkresiduals(res, plot = FALSE)
}

plot_forecast <- function(model, h, ts_train, ts_test, ylab, main, skip_n_train_obs = 0, xreg_method = "mean") {
    if (xreg_method != "mean") {
        stop("Only mean method is supported for xreg_method")
    }
    ts_train <- window(ts_train, start = time(ts_train)[skip_n_train_obs + 1])
    ts_test <- window(ts_test, start = time(ts_test)[1], end = time(ts_test)[h])

    if (!is.null(model$xreg)) {
        y_hat <- forecast(model, h = h, xreg = rep(mean(model$xreg), h))
    } else {
        y_hat <- forecast(model, h = h)
    }

    if (class(model)[1] == "nnetar") {
        combined_range <- range(c(
            ts_train, ts_test
        ))
    } else {
        # Compute the range of all lines involved
        combined_range <- range(c(
            ts_train, ts_test,
            y_hat$lower[, "95%"],
            y_hat$upper[, "95%"],
            y_hat$lower[, "80%"],
            y_hat$upper[, "80%"]
        ))
    }

    # Plot the original time series and the y_hat values for the test set with the calculated range
    plot(
        ts_train,
        main = main,
        ylab = ylab,
        xlim = range(do.call(c, lapply(list(ts_train, ts_test), time)), na.rm = TRUE),
        ylim = combined_range
    )
    grid()

    if (class(model)[1] != "nnetar") {
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
    }

    # True
    lines(ts_test, col = "red", lwd = 2)

    # y_hat
    lines(y_hat$mean, col = "#0d82d1", lwd = 2)

    # Legend
    if (class(model)[1] == "nnetar") {
        legend(
            "topleft",
            legend = c("True", "y_hat"),
            col = c("red", "#0d82d1"),
            lty = c(1, 1),
            lwd = c(2, 2),
            bg = "white"
        )
    } else {
        legend(
            "topleft",
            legend = c("True", "y_hat", "80% CI", "95% CI"),
            col = c("tomato", "#0d82d1", "#b1b5ce", "#dbdbdf"),
            lty = c(1, 1, 1, 1),
            lwd = c(2, 2, 2, 2),
            bg = "white"
        )
    }
}

plot_forecast_multiple_models <- function(model_list, h, ts_train, ts_test, ylab, main, colors, skip_n_train_obs = 0, xreg_method = "mean") {
    if (xreg_method != "mean") {
        stop("Only mean method is supported for xreg_method")
    }
    ts_train <- window(ts_train, start = time(ts_train)[skip_n_train_obs + 1])
    ts_test <- window(ts_test, start = time(ts_test)[1], end = time(ts_test)[h])

    preds <- list()
    for (model in model_list) {
        if (!is.null(model$xreg)) {
            y_hat <- forecast(model, h = h, xreg = rep(mean(model$xreg), h))
        } else {
            y_hat <- forecast(model, h = h)
        }
        preds <- append(preds, list(y_hat$mean))
    }

    combined_range <- range(c(
        ts_train, ts_test,
        c(preds)
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

    lines(ts_test, col = "red", lwd = 2)

    for (i in 1:length(model_list)) {
        y_hat <- preds[[i]]
        lines(y_hat, col = colors[i], lwd = 2, lty = 5)
    }

    legend(
        "topleft",
        legend = c("True", sapply(model_list, function(x) {
            if (is.null(x$method)) {
                return(as.character(x))
            } else {
                return(as.character(x$method))
            }
        })),
        col = c("red", colors),
        lty = c(1, rep(1, length(model_list))),
        lwd = c(2, rep(2, length(model_list))),
        bg = "white"
    )
}
