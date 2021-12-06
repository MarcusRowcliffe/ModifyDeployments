library(shiny)
require(devtools)
source_url("https://raw.githubusercontent.com/MarcusRowcliffe/CTtracking/master/CTtracking.r")

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("models", "Choose deployment model file (.rds)", accept=".rds"),
      tags$hr(),
      uiOutput("selectmod"),
      tags$hr(),
      uiOutput("b1"),
      uiOutput("b2"),
      uiOutput("b3"),
      uiOutput("b4"),
      uiOutput("re"),
      tags$hr(),
      fluidRow(
        column(5, actionButton("save", "Save model")),
        column(5, downloadButton("deployment_models.rds", "Download models"))),
      textOutput("txt")
    ),
    mainPanel(
      tags$hr(),
      fluidRow(
        column(2, actionButton("back", "Back"), offset=4),
        column(2, actionButton("forward", "Forward"))),
      plotOutput("image")
    )  
  )
)

server <- function(input, output, session) {
  # Image scroll buttons
  img <- reactiveValues(i=1)
  
  observeEvent(input$forward, {
    img$i <- if(img$i==nrow(dat())) 1 else img$i+1
  })
  
  observeEvent(input$back, {
    img$i <- if(img$i==1) nrow(dat()) else img$i-1
  })
  
  # Load models and extract components
  dmodsOriginal <- eventReactive(input$models, {
    readRDS(input$models$datapath)
  })
  
  dmods <- reactiveVal()
  
  output$selectmod <- renderUI({
    selectInput("mod", "Select model", names(dmodsOriginal()))
  })
  
  observeEvent(input$models, {
    dmods(dmodsOriginal())
  })
  
  mod <- reactive({
    req(input$mod)
    dmods()[[input$mod]]
  })
  
  cfs <- reactive({
    unname(mod()$model$coefs)
  })
  
  dat <- reactive({
    mod()$data
  })
  
  # Set up parameter value sliders and reset buttons
  output$b1 <- renderUI({
    b <- round(cfs()[1], 2)
    fluidRow(
      column(9, sliderInput("b1", "Parameter 1", 0.5*b, 1.5*b, b)),
      column(2, actionButton("re1", "Reset")))
  })
  
  output$b2 <- renderUI({
    b <- round(cfs()[2], 2)
    fluidRow(
      column(9, sliderInput("b2", "Parameter 2", 0.5*b, 1.5*b, b)),
      column(2, actionButton("re2", "Reset")))
  })
  
  output$b3 <- renderUI({
    b <- round(cfs()[3], 2)
    fluidRow(
      column(9, sliderInput("b3", "Parameter 3", b-0.5, b+0.5, b)),
      column(2, actionButton("re3", "Reset")))
  })
  
  output$b4 <- renderUI({
    b <- round(cfs()[4], 2)
    fluidRow(
      column(9, sliderInput("b4", "Parameter 4", 0.5*b, 1.5*b, b)),
      column(2, actionButton("re4", "Reset")))
  })
  
  output$re <- renderUI({
    fluidRow(
      column(5, actionButton("reset", "Reset all parameters")),
      column(5, actionButton("restore", "Restore model")))
  })
  
  # Reset / restore parameter slider values
  observeEvent(input$re1, {
    updateSliderInput(session, "b1", value=cfs()[1])
  })
  
  observeEvent(input$re2, {
    updateSliderInput(session, "b2", value=cfs()[2])
  })
  
  observeEvent(input$re3, {
    updateSliderInput(session, "b3", value=cfs()[3])
  })
  
  observeEvent(input$re4, {
    updateSliderInput(session, "b4", value=cfs()[4])
  })
  
  observeEvent(input$reset, {
    updateSliderInput(session, "b1", value=cfs()[1])
    updateSliderInput(session, "b2", value=cfs()[2])
    updateSliderInput(session, "b3", value=cfs()[3])
    updateSliderInput(session, "b4", value=cfs()[4])
  })
  
  observeEvent(input$restore, {
    tempmods <- dmods()
    tempmods[[input$mod]] <- dmodsOriginal()[[input$mod]]
    dmods(tempmods)
  })
  
  # Dynamic coefficients vector
  coefs <- reactive({
    c(input$b1, input$b2, input$b3, input$b4)
  })
  
  # Render image
  output$image <- renderPlot({
    req(dat(), coefs())
    plot_deployment(dat(), coefs(), img$i)
  }, width=1000, height=1000)
  
  # Save and download models
  observeEvent(input$save, {
    savemods <- dmods()
    savemods[[input$mod]]$model$coefs <- coefs()
    dmods(savemods)
  })
  
  output$deployment_models.rds <- downloadHandler(
    filename = function() {
      "deployment_models.rds"
    },
    content = function(file) {
      saveRDS(dmods(), file)
    }
  )
}

shinyApp(ui, server)
