library(writexl)
server <- function(input, output, session) {
  options(shiny.maxRequestSize=50*1024^2) #increasing maximum upload size
   observeEvent(
     ignoreNULL = TRUE,
     eventExpr = {
       input$directory
     },
     handlerExpr = {
       if (input$directory > 0) {
        path = choose.dir(default = readDirectoryInput(session, 'directory'))
        updateDirectoryInput(session, 'directory', value = path)
        }
     }
   )
 

   observeEvent(input$upload,{
   
       withProgress(message = 'Generating data', value = 0.14, {
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
       selectInput("input", "Eingangskonz. (input)", choices = colnames(mydata))})

            output$output <- renderUI({
       selectInput("output", "Ausgangskonz. (output)", choices = colnames(mydata))})

            output$tw <- renderUI({
       selectInput("cum_sum", "Kumulative Summe", choices = colnames(mydata))})    
             
        setProgress(1)
        Sys.sleep(.35)
      
          
       })
output$message<- renderText({paste(list_2[[2]])})
showNotification("Erfolgreich umgewandelt. Bitte überprüfen Sie die Tabelle. Sie ist abgespeichert im Arbeitsverzeichnis: klaeranlagen_daten.xlsx. Wenn alles stimmt können jetzt die entsprechenden Grafiken erstellt werden.", type="message", duration = NULL)
     })
      output$dry_cum_sum <- renderPlot( {ggplot(mydata, aes_string(input$cum_sum))+
      stat_ecdf(geom = "step")+
      theme_bw()+
      ylab("Summenhäufigkeit")+
      xlab(paste("Trockenwetter Abfluss",input$xlab))+
      geom_hline(yintercept=.85, col=2, lwd=1.2)
      })

     # filename<- reactiveValues(input$save_as)

# observe(input$download, {  
#   if(input$save_as > 0) {
#   dir.create("./plots/", showWarnings = FALSE )
#   ggsave(filename= filename(), path = "./plots/")}
# }) 

}  


