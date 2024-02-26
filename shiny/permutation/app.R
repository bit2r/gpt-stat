# Shiny 패키지를 불러온다
library(shiny)

# factorial 함수를 정의한다
factorial_func <- function(n) {
  if (n <= 0) {
    return(1)
  } else {
    return(n * factorial_func(n - 1))
  }
}

# UI 부분을 정의한다
ui <- fluidPage(
  titlePanel("Permutation Calculator"),
  sidebarLayout(
    sidebarPanel(
      numericInput("n", "Total number of elements (n):", 5, min = 0),
      numericInput("r", "Number of elements to permute (r):", 2, min = 0),
      actionButton("calculate_button", "Calculate")
    ),
    mainPanel(
      textOutput("result")
    )
  )
)

# 서버 로직을 정의한다
server <- function(input, output, session) {
  observeEvent(input$calculate_button, {
    n <- as.integer(input$n)
    r <- as.integer(input$r)

    if (n >= r && n >= 0 && r >= 0) {
      permutation <- factorial_func(n) / factorial_func(n - r)
      output$result <- renderText({
        return(paste("Permutation (nP", r, "): ", permutation))
      })
    } else {
      output$result <- renderText({
        return("Invalid input. Ensure that n >= r and both are non-negative.")
      })
    }
  })
}

# Shiny 앱을 실행한다
shinyApp(ui = ui, server = server)
