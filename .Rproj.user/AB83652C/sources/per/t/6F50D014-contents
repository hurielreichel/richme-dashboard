
#' @title Get Yahoo Data
#' @description
#' Get a data frame with yahoo data using a date frame.
#'
#' @param code string or list of strings with the stock symbol as in yahoo finance. Example: "AAPL".
#' @param start date to start with in format "YYYY-MM-DD". Default to 4 years in the past.
#' @param end date to end with in format "YYYY-MM-DD". Default to today.
#'
#' @return data frame
#' @export
get_yahoo_data <- function(code, start = lubridate::today() - lubridate::years(4), end = lubridate::today()){

  if(length(code) >= 1){
    df <- tq_get(
      code,
      get = "stock.prices",
      from = start,
      to = end,
      complete_cases = TRUE) |>
      select(symbol, date, adjusted)

  } else{
    cli::cli_alert_danger("Please provide a valid Stock Code. Check Yahoo Finance for them")
  }

  return(df)
}


#' @title Compute Gains
#' @description
#' Computes percentage and absolute value gain.
#'
#' @details
#' Based on a data frame, given by the get_yahoo_data() function, computes its percentual and absolute gains.
#' It can contain multiple stocks.
#'
#' @param df data frame (usually outcome of get_yahoo_data())
#' @param start date to start with in format "YYYY-MM-DD". Default to 4 years in the past.
#' @param end date to end with in format "YYYY-MM-DD". Default to today.
#'
#' @return data frame
#' @export
compute_gains <- function(df, start = min(df$date), end = max(df$date)){

  gains_df <- df |>
    filter(date <= end, date >= start) |>
    group_by(symbol) |>
    summarise(
      percentage_change = ((last(adjusted) / first(adjusted)) - 1) * 100,
      abs_change = last(adjusted) - first(adjusted)
    )

  return(gains_df)

}


#' @title Simulate Portfolio Past
#' @description
#' Simulates the result of a specific investment in given stock(s).
#'
#' @details
#' Makes a simulation of investment based only in the past.
#'
#' @param df data frame (usually outcome of get_yahoo_data())
#' @param investment_df data frame with columns symbol and investment.
#' @param start date to start with in format "YYYY-MM-DD". Default to 4 years in the past.
#' @param end date to end with in format "YYYY-MM-DD". Default to today.
#'
#' @return data frame
#' @export
simulate_portfolio_past <- function(df,
                                     investment_df,
                                     start = min(df$date),
                                     end = max(df$date)){

  portfolio_df <- df |>
    filter(date <= end, date >= start) |>
    compute_gains(start = start, end = end) |>
    left_join(investment_df) |>
    mutate(return = investment + investment * percentage_change / 100)

  return(portfolio_df)

}

#' @title Simulate Portfolio Future
#' @description
#' Simulates a Portfolio of Stocks in the future using STLF method.
#'
#' @details
#' A list is returned with the data frame of predictions for each symbol
#' present, the STLF plots (trend and seasonality), gains, and the ggplot of the
#' multiple stocks in the portfolio.
#'
#' @param df data frame (usually outcome of get_yahoo_data())
#' @param investment_df data frame with columns symbol and investment.
#'
#' @return list
#' @export
simulate_portfolio_future <- function(df, investment_df){

  codes <- investment_df |> pull(symbol) |> unique()

  results <- list()
  for (code in codes){
      res <- forecast_stock(df |> filter(symbol == code), code = code)
      res$gains_forecast <- res$pred_df |>
        filter(date >= lubridate::today() - lubridate::days(2)) |>
        compute_gains() |>
        left_join(investment_df) |>
        mutate(return = investment + investment * percentage_change / 100)

      results[[code]] <- res
  }

  df_c <- tibble()
  for (i in 1:length(results)){
    df_c <- bind_rows(df_c, results[[i]]$pred_df)
  }

  portfolio_plot <- df_c |>
    filter(date > (lubridate::today() - lubridate::days(31))) |>
    ggplot(aes(x = date, y = adjusted, linetype = type, col = symbol)) +
    geom_line() +
    scale_linetype_manual(values=c("dashed", "solid")) +
    ggtitle("Forecast Portfolio Chart")+
    theme_tq()

  results$portfolio_plot <- portfolio_plot
  results$pred_df_c <- df_c
  results$gains <- df_c |>
    filter(date >= lubridate::today() - lubridate::days(2)) |>
    compute_gains() |>
    left_join(investment_df) |>
    mutate(return = investment + investment * percentage_change / 100)

  return(results)
}


#' @title Forecast Stock
#' @description
#' Forecast a single stock in 30 days in the future
#'
#' @details
#' A list is returned with the data frame of predictions for each symbol
#' present, the STLF plots (trend and seasonality), and gains.
#'
#' @param df data frame (usually outcome of get_yahoo_data())
#' @param start date to start with in format "YYYY-MM-DD". Default to 4 years in the past.
#' @param end date to end with in format "YYYY-MM-DD". Default to today.
#' @param code string Optional. In case there is more than one code in the data, define which
#' symbol shall be used with the stock symbol as in yahoo finance. Example: "AAPL".
#'
#' @return list
#' @export
forecast_stock <- function(df,
                           start = min(df$date),
                           end = max(df$date),
                           code = NULL){

  df <- df |>
    filter(date <= end, date >= start)

  n_codes <- df |> pull(symbol) |> unique() |> length()

  if (n_codes > 1 & is.null(code)){
    df <- df |>
      filter(symbol == code)
  } else {
    code = df |> pull(symbol) |> unique()
  }
  # Monthly Seasonality
  ts_data <- ts(df$adjusted, frequency = 30)

  # Decompose the time series
  decomp <- forecast::mstl(ts_data) |>
    as_tibble() |>
    mutate(date = seq(min(df$date), by = "day", length.out = nrow(df)))

  trend_plot <- forecast::mstl(ts_data) |> autoplot()

  pred_stl_ets <- forecast::stlf(
    ts_data,
    h = 30) |>
      as_tibble() |>
      select(adjusted = "Point Forecast") |>
      mutate(
        date = seq(
        end,
        by = "day",
        length.out = 30)
        ) |>
    mutate(symbol = code)

  df_basis <- df |>
    bind_rows(pred_stl_ets) |>
    mutate(type = ifelse(date >= end, "Forecast", "Real")) |>
    arrange(date)

  forecast_plot <- forecast::stlf(ts_data, h = 30) |>
      autoplot()

  gains_forecast <- df_basis |>
    filter(type == "Forecast") |>
    compute_gains()

  res <- list(pred_df = df_basis,
              gains_forecast = gains_forecast,
              trend_plot = trend_plot,
              forecast_plot = forecast_plot)

  return(res)
}


#' @title Plot Stock Lines
#' @description
#' Plot the change of a value in a stock (or multiple ones) as lines.
#'
#' @param df data frame (usually outcome of get_yahoo_data())
#'
#' @return ggplot
#' @export
plot_stock_lines <- function(df){

    plt <- df |>
      ggplot(aes(x = date, y = adjusted, col = symbol)) +
      geom_line() +
      ggtitle("Stock Chart")+
      theme_tq()

    return(plt)
}
