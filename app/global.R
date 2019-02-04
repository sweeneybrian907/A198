#install.packages(c('zoo', 'readxl',"shiny", 'stringr', 'lattice', 'ggplot2', 'reshape2', 'lubridate', 'scales', 'tidyverse', 'WriteXLS', 'writexl'))
sapply(c("grDevices",'zoo', 'readxl',"shiny", "stringr", 'lattice', 'ggplot2',"reshape2", "lubridate", "scales", "writexl"), require, character.only = T)
# "WriteXLS",
#library("tidyverse")

# global functions
read_files <- function(inDir, pat="*.csv", readMe=read_csv2){
  files = list.files(inDir, pattern=pat)
  files = lapply(files, function(x) file.path(inDir, x))
  df = do.call(rbind, lapply(files, readMe))
  return(list(df, files))
}

ggplot_fun_tw <- function(df, colnamesx, xlab){
  anno = paste("85% = ", quantile(as.data.frame(df[,colnamesx]), probs=c(0.85), na.rm=TRUE))
  posx = quantile(as.data.frame(df[,colnamesx]), probs=c(0.01), na.rm=TRUE)
  p = ggplot(df, aes_string(colnamesx)) +
      stat_ecdf(geom = "step") +
      annotate("text", x = posx, y = 0.88, label = anno)  +
      theme_bw() +
      ylab("Summenhäufigkeit") +
      xlab(xlab) +
      geom_hline(yintercept=.85, col=2, lwd=1)
  return(p)
}

meanNA <- function(x){mean(x, na.rm = TRUE)}

filter_date_df <- function(dfIn, datecol, yr, sub_in, sub_out){
  # filter by date and year 
  filtYear <- year(dfIn[,datecol]) == yr
  filtVars <- c(datecol, sub_in, sub_out)
  dfEval <- dfIn[filtYear, filtVars]
  names(dfEval[,datecol]) <- "Date"
  dfEval$Monat <- month(dfEval$Date)
  
  # monthly average for every year ####
  mean_Month <- aggregate(dfEval[,c(sub_in, sub_out)], list(dfEval$Monat), meanNA)
  #change column name
  names(mean_Month)[1] <- "Monat"
    
  #convert into long format for ggplot
  long_df <- melt(mean_Month, variable.name="Param", value.name="Conz", id.vars = "Monat")
  long_df$Monat <- as.factor(long_df$Monat)
  return(long_df)
}

## 
# Inputs:
# df (dataframe): dataframe to be evaluated
# datecol (str): name of column that has date values
# year (num): year to be evaluated
# sub_in (num): concentration of substance at inlet
# sub_out (num): concentration of substanc at outflow
# grenzwert (num): given threshold value for the discharge concentration
# paramname (str): name of substance to be evaluated
gg_barplot <- function(dfIn, datecol, yearx, sub_in, sub_out, grenzwert, paramName){
  # prepare data to plot
  df <- filter_date_df(as.data.frame(dfIn), datecol, yearx, sub_in, sub_out)
  yearChr <- as.character(yearx)
  
  # prepare plot
  p = ggplot(df, aes_string(x="Monat", y="Conz"))+
      geom_bar(aes_string(fill="Param"), stat="identity", position="identity") + 
      coord_cartesian(ylim = c(0, max(df$Conz, na.rm=TRUE))) +
      geom_hline(yintercept=grenzwert, col="2", size=1.5, show.legend = T) +
      scale_x_discrete(df$Monat, name=paste("Monat", yearChr)) + #, date_breaks = "months", labels = date_format("%b")) +
      scale_fill_manual(values=c("chocolate4", "blue"),
                        name="Legende",
                        labels=c("Ablauf", "Zulauf"),
                        guide=guide_legend(reverse = TRUE)) +
      scale_y_continuous(paste(paramName, "[mg/l]")) +
      theme_bw()
  
  return(p)
  }

gg_mulitvar <- function(dfIn, var_x, var_y){
  # prepare plot
  p <-  ggplot(dfIn, aes_string(x=var_x, y=var_y))+
        geom_point(na.rm=TRUE) +
        theme_bw()
  
  return(p)
  }

## save ggplot
# inputs:
# plotIn (ggplot obj): ggplot object to be plotted
# pathOut (str): path where png file is to be saved (does not check file endings)
save_plot <- function(plotIn, pathOut, res=200, width=15.5, height=9.62, units="cm"){
  png(pathOut, res=res, width=width, height=height, units=units)
  print({plotIn})
  dev.off()
}


# testing
# uncomment to run tests

# df <- read_xlsx("../../KA_data/betriebstagebücher/collate.xlsx")
# ggplot_fun_tw(df, "Abl_Laton_dval", "Laton")
# ggplot_fun_tw(df[df$TW == 1,], "Abl_Laton_dval", "Laton")
# 
# parse_date_time(as.character(df$Date), "%d.%m.%Y")
# df$Date <- as.POSIXct(strptime(df$Date, format="%d.%m.%Y", tz="GMT"))
# x <- filter_date_df(as.data.frame(df), "Date", 2017, "Zul_Laton_dval", "Abl_Laton_dval")
# 
# gg_barplot(df, "Date", 2017, "Zul_Laton_dval", "Abl_Laton_dval", 2.0, "Laton")
# gg_mulitvar(df, "Zul_Laton_dval", "Abl_Laton_dval")
