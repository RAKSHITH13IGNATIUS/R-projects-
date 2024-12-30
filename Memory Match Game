library(shiny)

# UI
ui <- fluidPage(
  titlePanel("Memory Match Game"),
  fluidRow(
    uiOutput("cards_ui")
  ),
  br(),
  actionButton("reset", "Reset Game")
)

# Server
server <- function(input, output, session) {
  values <- reactiveValues(cards = sample(1:8, 8, replace = TRUE), flipped = c(), matches = 0)
  
  observeEvent(input$reset, {
    values$cards <- sample(1:8, 8, replace = TRUE)
    values$flipped <- c()
    values$matches <- 0
  })
  
  output$cards_ui <- renderUI({
    lapply(seq_along(values$cards), function(i) {
      if (i %in% values$flipped) {
        actionButton(paste0("card_", i), values$cards[i])
      } else {
        actionButton(paste0("card_", i), "❓")
      }
    })
  })
  
  observe({
    flipped <- which(sapply(seq_along(values$cards), function(i) input[[paste0("card_", i)]] > 0))
    if (length(flipped) == 2) {
      if (values$cards[flipped[1]] == values$cards[flipped[2]]) {
        values$matches <- values$matches + 1
      }
      values$flipped <- flipped
    }
  })
}

shinyApp(ui, server)
