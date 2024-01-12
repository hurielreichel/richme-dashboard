library(shiny)
library(shinycssloaders)
library(richme)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("richme: Portfolio Management Dashboard"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          textInput("stock_symbols", "Enter stock symbols (comma-separated):", ""),
          textInput("investment_amounts", "Enter investment amounts (comma-separated):", ""),
          dateRangeInput("dates",
                         start = lubridate::today() - lubridate::years(4),
                         end = lubridate::today(),
                         max = lubridate::today(),
                         h3("Date range")),
          actionButton(inputId = "press", label = "Launch", icon = icon("rocket"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("simpleview") |> withSpinner(),

           tableOutput("pastport") |> withSpinner(),

           textOutput("pastgains") |> withSpinner(),

           plotOutput("futureport") |> withSpinner(),

           tableOutput("pastporttab") |> withSpinner(),

           textOutput("futuregains") |> withSpinner()

        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$simpleview <- renderPlot({

      if (input$press == 0) return()
      input$press

      date1 <- input$dates[1]
      date2 <- input$dates[2]
      codes_str <- input$stock_symbols
      codes <- strsplit(codes_str, ",")[[1]]

      df <- get_yahoo_data(code = codes, start = date1, end = date2)
      df |> plot_stock_lines()

    })

    output$pastport <- renderTable({

      if (input$press == 0) return()
      input$press

      date1 <- input$dates[1]
      date2 <- input$dates[2]
      codes_str <- input$stock_symbols
      codes <- strsplit(codes_str, ",")[[1]]
      inves_str <- input$investment_amounts
      inves <- strsplit(inves_str, ",")[[1]] |> as.numeric()

      investment_df <- tibble(symbol = codes, investment = inves)

      df <- get_yahoo_data(code = codes, start = date1, end = date2)
      df <- df |>
        simulate_portfolio_past(investment_df,
                                start = date1,
                                end = date2)

    })

    output$pastgains <- renderText({

      if (input$press == 0) return()
      input$press

      date1 <- input$dates[1]
      date2 <- input$dates[2]
      codes_str <- input$stock_symbols
      codes <- strsplit(codes_str, ",")[[1]]
      inves_str <- input$investment_amounts
      inves <- strsplit(inves_str, ",")[[1]] |> as.numeric()

      investment_df <- tibble(symbol = codes, investment = inves)

      df <- get_yahoo_data(code = codes, start = date1, end = date2)
      gains <- df |>
        simulate_portfolio_past(investment_df,
                                start = date1,
                                end = date2)

      return = gains |>
        pull(return) |>
        sum() |>
        round(2)

      investment = gains |>
        pull(investment) |>
        sum() |>
        round(2)

      res = (return - investment) |> round(2)

      paste("Result of", res, "!")

    })

    output$futureport <- renderPlot({

      if (input$press == 0) return()
      input$press

      date1 <- input$dates[1]
      date2 <- input$dates[2]

      codes_str <- input$stock_symbols
      codes <- strsplit(codes_str, ",")[[1]]
      inves_str <- input$investment_amounts
      inves <- strsplit(inves_str, ",")[[1]] |> as.numeric()

      investment_df <- tibble(symbol = codes, investment = inves)

      df <- get_yahoo_data(code = codes, start = date1, end = date2)
      fut <- df |>
        simulate_portfolio_future(investment_df)

      fut$portfolio_plot

    })

    output$pastporttab <- renderTable({

      if (input$press == 0) return()
      input$press

      date1 <- input$dates[1]
      date2 <- input$dates[2]
      codes_str <- input$stock_symbols
      codes <- strsplit(codes_str, ",")[[1]]
      inves_str <- input$investment_amounts
      inves <- strsplit(inves_str, ",")[[1]] |> as.numeric()

      investment_df <- tibble(symbol = codes, investment = inves)

      df <- get_yahoo_data(code = codes, start = date1, end = date2)
      forecast <- df |>
        simulate_portfolio_future(investment_df)

      forecast$gains

    })

    output$futuregains <- renderText({

      if (input$press == 0) return()
      input$press

      date1 <- input$dates[1]
      date2 <- input$dates[2]
      codes_str <- input$stock_symbols
      codes <- strsplit(codes_str, ",")[[1]]
      inves_str <- input$investment_amounts
      inves <- strsplit(inves_str, ",")[[1]] |> as.numeric()

      investment_df <- tibble(symbol = codes, investment = inves)

      df <- get_yahoo_data(code = codes, start = date1, end = date2)
      gains <- df |>
        simulate_portfolio_future(investment_df)

      return = gains$gains |>
        pull(return) |>
        sum() |>
        round(2)

      investment = gains$gains |>
        pull(investment) |>
        sum() |>
        round(2)

      res = (return - investment) |> round(2)

      paste("Forecasted Result of", res, " in the next month!")

    })




}

# Run the application
shinyApp(ui = ui, server = server)
