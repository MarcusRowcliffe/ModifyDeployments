library(shiny)
require(devtools)
source_url("https://raw.githubusercontent.com/MarcusRowcliffe/CTtracking/V0.4.0/CTtracking.r")

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
      uiOutput("b5"),
      uiOutput("re"),
      tags$hr(),
      fluidRow(
        column(5, actionButton("save", "Save model")),
        column(5, downloadButton("deployment_models.rds", "Download models")))
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
  options(shiny.maxRequestSize=50*1024^2) 
  
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
      column(9, sliderInput("b1", "Contour spacing", 0, 2*b, b, b/100)),
      column(2, actionButton("re1", "Reset")))
  })
  
  output$b2 <- renderUI({
    b <- round(1 - cfs()[2]^(1/cfs()[5]), 2)
    fluidRow(
      column(9, sliderInput("b2", "Horizon at image centre", b-0.5, b+0.5, b, 0.01)),
      column(2, actionButton("re2", "Reset")))
  })
  
  output$b3 <- renderUI({
    b <- round(cfs()[3], 2)
    fluidRow(
      column(9, sliderInput("b3", "Contour slope", b-1, b+1, b, 0.01)),
      column(2, actionButton("re3", "Reset")))
  })
  
  output$b4 <- renderUI({
    b <- round(cfs()[4], 2)
    fluidRow(
      column(9, sliderInput("b4", "Contour spacing trend", b-1, b+1, b, 0.01)),
      column(2, actionButton("re4", "Reset")))
  })
  
  output$b5 <- renderUI({
    b <- round(cfs()[5], 2)
    fluidRow(
      column(9, sliderInput("b5", "Contour curvature", b-1, b+1, b, 0.01)),
      column(2, actionButton("re5", "Reset")))
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
    updateSliderInput(session, "b2", value=1 - cfs()[2]^(1/cfs()[5]))
  })
  
  observeEvent(input$re3, {
    updateSliderInput(session, "b3", value=cfs()[3])
  })
  
  observeEvent(input$re4, {
    updateSliderInput(session, "b4", value=cfs()[4])
  })
  
  observeEvent(input$re5, {
    updateSliderInput(session, "b5", value=cfs()[5])
  })
  
  observeEvent(input$reset, {
    updateSliderInput(session, "b1", value=cfs()[1])
    updateSliderInput(session, "b2", value=1 - cfs()[2]^(1/cfs()[5]))
    updateSliderInput(session, "b3", value=cfs()[3])
    updateSliderInput(session, "b4", value=cfs()[4])
    updateSliderInput(session, "b5", value=cfs()[5])
  })
  
  observeEvent(input$restore, {
    tempmods <- dmods()
    tempmods[[input$mod]] <- dmodsOriginal()[[input$mod]]
    dmods(tempmods)
  })
  
  # Dynamic coefficients vector
  coefs <- reactive({
    c(b1=input$b1, b2=(1-input$b2)^input$b5, b3=input$b3, b4=input$b4, b5=input$b5)
  })
  
  # Render image
  output$image <- renderPlot({
    req(coefs())
    plot_deployment_image(mod(), coefs(), img$i)
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

#shinyApp(ui, server)
