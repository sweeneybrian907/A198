ui <- fluidPage(
  
   
  helpText("Bitte Arbeitsverzeichnis einlesen (neues Fenster öffnet sich) und auf Hochladen drücken. Dies kann einige Minuten Dauern!. Achtung die Datein müssen in der ersten Spalte das Datum haben (nicht auch Uhrzeit) im Format: Tag Monat Jahr (Trennzeichen egal)."),
  directoryInput('directory', label = 'select a directory'),
  actionButton("upload", label="Hochladen"),
  #actionButton("show_data", "Tabelle zeigen"),
  #tableOutput("df"),

  #actionButton("action_format", label = "Jetzt formatieren"),
  #downloadButton("download", label="Runterladen"),
  "Gefundene Dateien",
  br(),
  textOutput(outputId = "message"),
  # textOutput(outputId = "date"),
 # uiOutput("found_data"),
#radioButtons("rb", "Please select which graph you want to draw", choices = c("", "")),
 
 # fileInput("updated_table",label="Bearbeitete Tabelle (als csv Datei)"),
 textOutput("spalten_namen"),
 tableOutput("table1"),
 helpText("Jetzt die EIngangs- und Ausgangskonzentrationen auswählen für die Grafiken."),
 uiOutput("input"),
 uiOutput("output"),
uiOutput("tw"),
 plotOutput("dry_cum_sum"),
 textInput("xlab", "x-axen Beschriftung"),
 actionButton('download', 'Grafiken runter laden'),
textInput("save_as", "Namen der zu speichernden Grafik")
  
)

# es werden keine unterordner durchsucht