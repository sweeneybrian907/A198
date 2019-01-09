library(shiny)
# shiny app user interface
source("./global.R")

mainP <- mainPanel(
    tabsetPanel(
      tabPanel("Multivariate Plot", plotOutput("compPlot")),
      tabPanel("85% Plot", plotOutput("eightyFivePlot")),
      tabPanel("Wirkungsgrad", plotOutput("wirkungsgrad"))
    )
  )

ui <- fluidPage(
  titlePanel("A-198 Auswertung"),
  h5("KA Daten in xlsx Format einlesen um Daten interactive anzuschauen"),
  br(),
  hr(),
  
  sidebarLayout(
     sidebarPanel(
      helpText("Bitte Datei einlesen. Achtung die Datei müssen in der ersten Spalte das Datum haben (ohne Uhrzeit) im Format: Tag Monat Jahr (Trennzeichen egal)."),
      fileInput('xlsxIn', label = 'xlsx Datei auswählen', multiple=FALSE, accept=c(".xlsx")),
      textOutput("spalten_namen"),
      tableOutput("table1"),
      helpText("Jetzt die Eingangs- und Ausgangskonzentrationen auswählen für die Grafiken."),
      uiOutput("paramIn"),
      uiOutput("paramOut"),
      uiOutput("tw"),
      textInput("xlab", "x-axen Beschriftung") #,
# TODO - create download link for displayed graphic
      #actionButton('download', 'Grafiken herunterladen'),
      #textInput("save_as", "Namen der zu speichernden Grafik")
    ),
  mainP, position="left")
)

# shiny app server
server <- function(input, output, session) {
  options(shiny.maxRequestSize=50*1024^2) #increasing maximum upload size

   observeEvent(input$xlsIn,{
       withProgress(message = 'Daten lesen', value = 0.14, {
            list_2 <- read_files(readDirectoryInput(session, 'directory'))
            mydata <<- as.tibble(list_2[[1]])
       setProgress(.5)
          date_name <- names(mydata)[1]
          remove_cols <- c(date_name, "date")
          duplicates <- duplicated(mydata[,1]) %>% which()
          #removing duplicates
          if(length(duplicates) > 0) {mydata <- mydata[-c(duplicates),]}

          #removing columns
          check <- mydata %>%
            is.na() %>%
            colSums()
          nas_tib  <-   which(check==nrow(mydata))
          if(length(nas_tib) >0)   mydata<- mydata[,-nas_tib]
          setwd(readDirectoryInput(session, 'directory'))
          mydata <- mydata %>%
            mutate_at(date_name, funs(date = dmy(.))) %>%  #format date
          arrange(date) %>%
          select(-one_of(date_name)) %>%
          select(date, everything())
          setProgress(.7)
            write_xlsx(mydata, path="./klaeranlagen_daten.xlsx")
            #write.xlsx(mydata, file="./klaeranlagen_daten.xlsx") # fails if data set too large
            #WriteXLS("mydata", "./klaeranlagen_daten.xlsx", perl = "B:/Programme/Perl_strawberry/perl/bin/perl") #perl must be installed change path if necessary
            
            output$input <- renderUI({
       selectInput("paramIn", "Eingangskonz. (input)", choices = colnames(tabIn))})

            output$output <- renderUI({
       selectInput("paramOut", "Ausgangskonz. (output)", choices = colnames(tabIn))})

            output$tw <- renderUI({
       selectInput("cum_sum", "Kumulative Summe", choices = colnames(tabIn))})    
             
        setProgress(1)
        Sys.sleep(.35)
      
          
       })
output$message<- renderText({paste(list_2[[2]])})
showNotification("Erfolgreich umgewandelt. Bitte überprüfen Sie die Tabelle. Sie ist abgespeichert im Arbeitsverzeichnis: klaeranlagen_daten.xlsx. Wenn alles stimmt können jetzt die entsprechenden Grafiken erstellt werden.", type="message", duration = NULL)
     })
      output$eightyFivePlot <- renderPlot( {ggplot_fun_tw(input$cum_sum, input$xlab)
      })

     # filename<- reactiveValues(input$save_as)

# observe(input$download, {  
#   if(input$save_as > 0) {
#   dir.create("./plots/", showWarnings = FALSE )
#   ggsave(filename= filename(), path = "./plots/")}
# }) 

}  

shinyApp(ui, server)
