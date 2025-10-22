library(shiny)
library(bslib)

ui <- page_sidebar(
  title = "Confidence Interval Calculator for Proportions",
  # Add MathJax support with proper configuration
  tags$head(
    tags$script(type = "text/javascript", async = NA, 
                src = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js"),
    tags$script(HTML("
      window.MathJax = {
        tex: {
          inlineMath: [['$', '$'], ['\\\\(', '\\\\)']],
          displayMath: [['$$', '$$'], ['\\\\[', '\\\\]']],
          processEscapes: true
        },
        startup: {
          ready: () => {
            MathJax.startup.defaultReady();
            MathJax.startup.promise.then(() => {
              // Re-render math when Shiny updates
              $(document).on('shiny:value', function(event) {
                MathJax.typesetPromise();
              });
            });
          }
        }
      };
    "))
  ),
  sidebar = sidebar(
    div(
      HTML('<label class="control-label" for="sample_prop">Sample Proportion (\\(\\hat{p}\\)):</label>'),
      numericInput(
        "sample_prop",
        label = NULL,
        value = 0.5,
        min = 0,
        max = 1,
        step = 0.01
      )
    ),
    div(
      HTML('<label class="control-label" for="sample_size">Sample Size (\\(n\\)):</label>'),
      numericInput(
        "sample_size",
        label = NULL,
        value = 100,
        min = 1,
        step = 1
      )
    ),
    numericInput(
      "confidence_level",
      "Confidence Level (%):",
      value = 95,
      min = 1,
      max = 99.9,
      step = 0.1
    )
  ),
  
  card(
    card_header("Results"),
    card_body(
      h4("Standard Error"),
      verbatimTextOutput("standard_error"),
      br(),
      h4("Confidence Interval"),
      verbatimTextOutput("confidence_interval"),
      br(),
      h4("Interpretation"),
      textOutput("interpretation")
    )
  )
)

server <- function(input, output, session) {
  
  # Re-render MathJax when the app loads
  observe({
    session$sendCustomMessage("mathjax", "")
  })
  
  # Calculate standard error
  standard_error <- reactive({
    p_hat <- input$sample_prop
    n <- input$sample_size
    
    # Standard error formula: sqrt(p̂(1-p̂)/n)
    se <- sqrt(p_hat * (1 - p_hat) / n)
    return(se)
  })
  
  # Calculate confidence interval
  confidence_interval <- reactive({
    p_hat <- input$sample_prop
    conf_level <- input$confidence_level / 100
    se <- standard_error()
    
    # Calculate critical value (z-score)
    alpha <- 1 - conf_level
    z_critical <- qnorm(1 - alpha/2)
    
    # Calculate margin of error
    margin_error <- z_critical * se
    
    # Calculate confidence interval bounds
    lower_bound <- p_hat - margin_error
    upper_bound <- p_hat + margin_error
    
    # Ensure bounds are within [0, 1]
    lower_bound <- max(0, lower_bound)
    upper_bound <- min(1, upper_bound)
    
    return(c(lower_bound, upper_bound))
  })
  
  # Output standard error
  output$standard_error <- renderText({
    se <- standard_error()
    paste0("SE = ", round(se, 6))
  })
  
  # Output confidence interval
  output$confidence_interval <- renderText({
    ci <- confidence_interval()
    conf_level <- input$confidence_level
    paste0(conf_level, "% CI: (", round(ci[1], 4), ", ", round(ci[2], 4), ")")
  })
  
  # Output interpretation
  output$interpretation <- renderText({
    conf_level <- input$confidence_level
    ci <- confidence_interval()
    paste0("We are ", conf_level, "% confident that the true population proportion ",
           "lies between ", round(ci[1], 4), " and ", round(ci[2], 4), ".")
  })
}

# Add custom message handler for MathJax
addResourcePath("js", system.file("www", package = "shiny"))

shinyApp(ui = ui, server = server)
