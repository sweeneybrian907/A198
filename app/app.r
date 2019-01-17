library(shiny)
library(readxl)
# shiny app user interface
source("./global.R")

mainP <- mainPanel(
  tabsetPanel(
  tabPanel("Plot", plotOutput("plotOut")),
  tabPanel("Data", tableOutput("tabIn"))
  )
)

ui <- fluidPage(
  titlePanel("A-198 Auswertung"),
  h5("KA Daten in xlsx Format einlesen um Daten interactive anzuschauen"),
  br(),
  hr(),
  sidebarLayout(
  sidebarPanel(
  helpText("Bitte Datei einlesen"),
  fileInput('xlsxIn', label = 'xlsx Datei auswählen', multiple=FALSE, accept=c(".xlsx")),
  selectInput("plotType", "Grafiktyp", selected = "Multivariate",
    choices = c("Multivariate", "Cumsum", "Wirkungsgrad")),
  helpText("Eingangs- und Ausgangskonzentrationen auswählen für die Grafiken."),
  # conditional input multivariate plot
  conditionalPanel(condition = "input.plotType == 'Multivariate'",
                   uiOutput("paramInMulti"),
                   uiOutput("paramOutMulti"),
                   uiOutput("twMulti") ),
  # conditiaonl input cumulative sum plot
  conditionalPanel(condition = "input.plotType == 'Cumsum'",
                   uiOutput("cumParam"),
                   uiOutput("twCumSum")),
  # conditional input Wirkungsgrad
  conditionalPanel(condition = "input.plotType == 'Wirkungsgrad'",
                   uiOutput("paramIn"),
                   uiOutput("paramOut"),
                   uiOutput("dateCol"),
                   uiOutput("year"),
                   numericInput("grenzwert", label="Param. Grenzwert", value=2.0),
                   uiOutput("tw")),
  
  textInput("xlab", "X-axen Beschriftung"), #,
  textInput("ylab", "Y-axen Beschriftung") #,
  # TODO - create download link for displayed graphic
  #actionButton('download', 'Grafiken herunterladen'),
  #textInput("save_as", "Namen der zu speichernden Grafik")
  ),
  mainP, position="left")
)


# shiny app server
server <- function(input, output, session) {
  options(shiny.maxRequestSize=50*1024^2) #increasing maximum upload size
  
  tabIn <- reactive({
    xlsxIn <- input$xlsxIn
    if (is.null(xlsxIn)){
      return(NULL)
    }
    read_xlsx(xlsxIn$datapath)
    })
  
  colsIn <- reactive({
    df <- tabIn()
    if (is.null(df)) return(NULL)
    names(df)
  })
  
  output$tabIn <- renderTable({head(tabIn())})

  observeEvent(input$plotType, { 
    req(tabIn())
    if(input$plotType == "Multivariate"){
      output$paramInMulti <- renderUI({
        selectInput("paramInMulti", "Eingangskonz. (input)", choices = colsIn())})
      output$paramOutMulti <- renderUI({
        selectInput("paramOutMulti", "Ausgangskonz. (output)", choices = colsIn())})
      output$twMulti <- renderUI({
        selectInput("tw", "Trockenwetter (1/0) filter", choices = colsIn())})
    } else if(input$plotType == "Wirkungsgrad"){
      output$paramIn <- renderUI({
        selectInput("paramIn", "Eingangskonz. (input)", choices = colsIn())})
      output$paramOut <- renderUI({
        selectInput("paramOut", "Ausgangskonz. (output)", choices = colsIn())})
      output$tdateCol<- renderUI({
        selectInput("dateCol", "Datum Spalte", choices = colsIn())})
      output$year <- renderUI({
        selectInput("year", "Jahr filter", choices = colsIn())})
      output$tw <- renderUI({
        selectInput("tw", "Trockenwetter (1/0) filter", choices = colsIn())})
    } else if(input$plotType == "Cumsum"){
      output$cumParam <- renderUI({
        selectInput("cumParam", "Parameter kumulative Summe", choices = colsIn())})
      output$twCumSum <- renderUI({
        selectInput("twCumSum", "Trockenwetter (1/0) filter", choices = colsIn())})
      if(is.null(input$twCumSum)){
        output$plotOut <- renderPlot({ggplot_fun_tw(tabIn(), input$cumParam, input$xlab)})
      } 
      else {
        filt <- which(tabIn[,input$twCumSum] == 1)
        output$plotOut <- renderPlot({ggplot_fun_tw(tabIn()[filt,], input$cumParam, input$xlab)})
      }
    }
  })
}

shinyApp(ui, server)