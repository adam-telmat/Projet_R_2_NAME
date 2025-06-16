# VERSION DEBUG - IDENTIFICATION DU PROBLÈME
library(shiny)
library(shinydashboard)

# Test de la fonction problématique
test_function <- function() {
  # Test des conditions qui causent l'erreur
  pl <- 4.5
  pw <- 1.5
  
  # Test 1: Condition simple
  if (pl < 2.5) {
    return("setosa")
  }
  
  # Test 2: Condition avec &&
  is_critical <- (pl >= 4.0) && (pl <= 6.0) && (pw >= 1.0) && (pw <= 2.0)
  
  if (is_critical) {
    return("zone critique")
  }
  
  return("autre")
}

# Test immédiat
cat("Test de la fonction:", test_function(), "\n")

ui <- dashboardPage(
  dashboardHeader(title = "DEBUG"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Test", tabName = "test")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "test",
        fluidRow(
          box(width = 12, title = "Test Debug",
            h3("Application de debug"),
            p("Si vous voyez ceci, l'app fonctionne !"),
            actionButton("test_btn", "Test"),
            verbatimTextOutput("result")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$test_btn, {
    result <- test_function()
    
    output$result <- renderText({
      paste("Résultat du test:", result)
    })
  })
}

cat("Lancement de l'application debug...\n")
shinyApp(ui = ui, server = server) 