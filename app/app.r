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
  # hr(),
  sidebarLayout(
  sidebarPanel(
  selectInput("plotType", "Grafiktyp", selected = "NULL",
    choices = c("NULL", "Multivariate", "Cumsum", "Wirkungsgrad")),
  helpText("Grafikparameter auswählen."),
  # conditional input multivariate plot
  conditionalPanel(condition = "input.plotType == 'Multivariate'",
                   uiOutput("paramX"),
                   uiOutput("paramY")) ,
                   #uiOutput("twMulti") ),
  # conditiaonl input cumulative sum plot
  conditionalPanel(condition = "input.plotType == 'Cumsum'",
                   uiOutput("cumParam")) ,
                   #uiOutput("twCumSum")),
  # conditional input Wirkungsgrad
  conditionalPanel(condition = "input.plotType == 'Wirkungsgrad'",
                   uiOutput("paramIn"),
                   uiOutput("paramOut"),
                   uiOutput("dateCol"),
                   uiOutput("year"),
                   numericInput("grenzwert", label="Param. Grenzwert", value=2.0)),
                   #uiOutput("tw")),
  uiOutput("tw"),
  helpText("HINWEIS: Datumspalte muss im Format DD.MM.YYYY sein"),
  fileInput('xlsxIn', label = 'xlsx Datei auswählen', multiple=FALSE, accept=c(".xlsx"))
  # textInput("xlab", "X-axen Beschriftung"), #,
  # textInput("ylab", "Y-axen Beschriftung") #,
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
  
  # data frame after converting date values
  dfDates <- reactive({
    df <- dfTW()
    dateCol <- input$dateCol
    if (dateCol == "NULL") {
      return(df)
    }
    else{
      chrDates <- apply(as.data.frame(df[,dateCol]), 1, as.character)
      dates <- parse_date_time(chrDates, "%d.%m.%Y")   #format="%d.%m.%Y",
      df[,dateCol] <- dates
      return(df)
    }
  })
 
  # reactive year values 
  years <- reactive({
    df <- dfTW()
    dateCol <- input$dateCol
    if(dateCol == "NULL"){
      return(NULL)
    }
    else{
      chrDates <- apply(as.data.frame(df[,dateCol]), 1, as.character)
      dates <- parse_date_time(chrDates, "%d.%m.%Y")
      return(sort(unique(year(dates))))
    }
  })
  
 dfTW <- reactive({ 
    if(input$tw == "NULL"){
      df <- tabIn()
      return(df)
    }
    else {
      df <- tabIn()
      filt <- which(df[,input$tw] == 1)
      df <- df[filt,]
      return(df)
    }
 })
 
  output$tabIn <- renderTable({head(tabIn())})

  observeEvent(input$plotType, {
    req(tabIn())
    output$tw <- renderUI({
      selectInput("tw", "Trockenwetter (1/0) filter", selected="NULL", choices = c("NULL", colsIn()))})
    
    if(input$plotType == "Multivariate"){
      output$paramX <- renderUI({
        selectInput("paramX", "Parameter X", choices = colsIn())})
      output$paramY <- renderUI({
        selectInput("paramY", "Parameter Y", choices = colsIn())})
      # output$twMulti <- renderUI({
      #   selectInput("tw", "Trockenwetter (1/0) filter", choices = colsIn())})
      output$plotOut <- renderPlot(gg_mulitvar(dfTW(), input$paramX, input$paramY))
    } else if(input$plotType == "Wirkungsgrad"){
      output$paramIn <- renderUI({
        selectInput("paramIn", "Eingangskonz. (input)", choices = colsIn())})
      output$paramOut <- renderUI({
        selectInput("paramOut", "Ausgangskonz. (output)", choices = colsIn())})
      output$dateCol<- renderUI({
        selectInput("dateCol", "Datum Spalte", selected="NULL", choices = c("NULL", colsIn()))})
      # output$tw <- renderUI({
      #   selectInput("tw", "Trockenwetter (1/0) filter", choices = colsIn())})
      # get years from parsing date column, once date column is selected
      output$year <- renderUI({
          selectInput("year", "Jahr filter", choices = years())})
      output$plotOut <- renderPlot({gg_barplot(dfDates(), input$dateCol, input$year,
                                               input$paramIn, input$paramOut,
                                               input$grenzwert, input$xlab)})
      output$tabIn <- renderTable({head(dfDates())})
    } else if(input$plotType == "Cumsum"){
      output$cumParam <- renderUI({
        selectInput("cumParam", "Parameter kumulative Summe", choices = colsIn())})
      # output$twCumSum <- renderUI({
      #   selectInput("twCumSum", "Trockenwetter (1/0) filter", choices = colsIn())})
      output$plotOut <- {renderPlot({ggplot_fun_tw(dfTW(), input$cumParam, input$xlab)})
      }
    }
  })
}

shinyApp(ui = ui, server = server)