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
  helpText("Eingangs- und Ausgangskonzentrationen auswählen für die Grafiken."),
  uiOutput("paramIn"),
  uiOutput("paramOut"),
  uiOutput("cumParam"),
  uiOutput("tw"),
  selectInput("plot_type", "Grafiktyp", selected = "Multivariate",
  choices = c("Multivariate", "Cumsum", "Wirkungsgrad")),
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
  #   tryCatch({
  #     tabIn <- read_xlsx(input$xlsxIn$datapath)
  #   },
  #   error = function(e){
  #     # return safe error if parsing error occurs
  #     stop(safeError(e))
  #   }
  #   return(tabIn)
  # )})
  observeEvent(input$xlsxIn,
    observeEvent(input$plot_type, {
      req(input$xlsxIn)
      tabIn <- read_xlsx(input$xlsxIn$datapath)
      output$tabIn <- renderTable({
      return(head(tabIn))
    })
    if(input$plot_type %in% c("Multivariate", "Wirkungsgrad")){
      output$paramIn <- renderUI({
        selectInput("paramIn", "Eingangskonz. (input)", choices = colnames(tabIn))})
      output$paramOut <- renderUI({
        selectInput("paramOut", "Ausgangskonz. (output)", choices = colnames(tabIn))})
      output$tw <- renderUI({
        selectInput("tw", "Trockenwetter (1/0) filter", choices = colnames(tabIn))})
    }
    else(
      if(input$plot_type == "Cumsum"){
        output$cumParam <- renderUI({
          selectInput("cumParam", "Parameter kumulative Summe", choices = colnames(tabIn))})
        output$tw <- renderUI({
          selectInput("tw", "Trockenwetter (1/0) filter", choices = colnames(tabIn))})
        if(is.null(input$tw)){
          output$plotOut <- renderPlot({ggplot_fun_tw(tabIn, input$cumParam, input$xlab)})
        } else({
          filt <- which(tabIn[,input$tw] == 1)
          output$plotOut <- renderPlot({ggplot_fun_tw(tabIn[filt,], input$cumParam, input$xlab)})
        })
    })
  })
)
#
#   observeEvent(iput$paramIn, {
#             output$paramIn <- renderUI({
#               selectInput("paramIn", "Eingangskonz. (input)", choices = colnames(tabIn))})
#   })
#   observeEvent(
#             output$paramOut <- renderUI({
#               selectInput("paramOut", "Ausgangskonz. (output)", choices = colnames(tabIn))})
#
#             output$tw <- renderUI({
#        selectInput("cum_sum", "Kumulative Summe", choices = colnames(tabIn))})
#
#         setProgress(1)
#         Sys.sleep(.35)
# filename<- reactiveValues(input$save_as)
# observe(input$download, {
#   if(input$save_as > 0) {
#   dir.create("./plots/", showWarnings = FALSE )
#   ggsave(filename= filename(), path = "./plots/")}
# })
}
shinyApp(ui, server)