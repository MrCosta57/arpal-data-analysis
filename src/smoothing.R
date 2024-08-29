apply_filter <- function(input_ts, weights) {
    # Apply the filter to the time series
    filtered_ts <- filter(input_ts, weights, sides = 2)
    return(filtered_ts)
}

simple_ma <- function(input_ts, p) {
    # Calculate the moving average weights
    ma_weights <- rep(1 / (2 * p + 1), 2 * p + 1)
    # Apply the moving average filter
    ma_ts <- apply_filter(input_ts, ma_weights)
    return(ma_ts)
}

spencer_filter <- function(input_ts) {
    w <- c(74, 67, 46, 21, 3, -5, -6, -3)
    w <- c(rev(w[-1]), w)
    # After these steps w is: (-3 -6 -5  3 21 46 67 74 67 46 21  3 -5 -6 -3)
    weights_spencer <- w / sum(w) # sum(w) is 320
    spencer_ts <- apply_filter(input_ts, weights_spencer)
    return(spencer_ts)
}

ma_for_seasonal <- function(input_ts, freq) {
    weights <- c(0.5, rep(1, freq - 1), 0.5) / freq
    ma_s_ts <- apply_filter(input_ts, weights)
    return(ma_s_ts)
}
