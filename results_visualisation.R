#example of visualization I hope to achieve when all accelerators of EMEA are implemented.
#results is to be read from mc_simulations results

library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)

ui <- fluidPage(
  titlePanel("Interactive NPV Distribution: Toggle Components"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("n_runs", "Number of simulations:", value = 1000, min = 100),
      actionButton("run", "Run Simulation")
    ),
    
    mainPanel(
      plotlyOutput("npvPlot"),
      br(),
      verbatimTextOutput("summaryStats")
    )
  )
)

server <- function(input, output, session) {
  
  # Function: run Monte Carlo simulation for each component
  run_all_simulations <- reactive({
    input$run
    isolate({
      n <- input$n_runs
      
      base <- rnorm(n, mean = 10000, sd = 2000)
      subsidy <- base + 1500
      loan <- base - 1000 + rnorm(n, mean = 0, sd = 500)
      both <- base + 500
      
      tibble(
        Scenario = rep(c("Base", "With Subsidy", "With Loan", "With Subsidy + Loan"), each = n),
        NPV = c(base, subsidy, loan, both)
      )
    })
  })
  
  # Plot with interactive legend
  output$npvPlot <- renderPlotly({
    df <- run_all_simulations()
    
    p <- ggplot(df, aes(x = NPV, fill = Scenario)) +
      geom_density(alpha = 0.5) +
      theme_minimal() +
      labs(
        title = "NPV Distribution (click legend to show/hide components)",
        x = "Net Present Value (€)",
        y = "Density"
      ) +
      scale_fill_brewer(palette = "Set2")
    
    ggplotly(p) %>%
      layout(legend = list(title = list(text = "Components")))
  })
  
  # Summary stats
  # output$summaryStats <- renderPrint({
  #   df <- run_all_simulations()
  #   df %>%
  #     group_by(Scenario) %>%
  #     summarise(
  #       Mean_NPV = mean(NPV),
  #       Median_NPV = median(NPV),
  #       P5 = quantile(NPV, 0.05),
  #       P95 = quantile(NPV, 0.95)
  #     )
  # })
}

shinyApp(ui, server)
