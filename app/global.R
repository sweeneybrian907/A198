#install.packages(c('zoo', 'readxl',"shiny", 'stringr', 'lattice', 'ggplot2', 'reshape2', 'lubridate', 'scales', 'tidyverse', 'WriteXLS', 'writexl'))
sapply(c("grDevices",'zoo', 'readxl',"shiny", "stringr", 'lattice', 'ggplot2',"reshape2", "lubridate", "scales", "tidyverse", "WriteXLS", "writexl", "tcltk"), require, character.only = T)

# global functions
read_files = function(inDir, pat="*.csv", readMe=read_csv2){
  files = list.files(inDir, pattern=pat)
  files = lapply(files, function(x) file.path(inDir, x))
  df = do.call(rbind, lapply(files, readMe))
  return(list(df, files))
}

ggplot_fun_tw <- function(df, colnamesx, xlab){
  p = ggplot(df, aes_string(colnamesx)) +
      stat_ecdf(geom = "step") +
      theme_bw() +
      ylab("Summenhäufigkeit") +
      xlab(xlab) +
      geom_hline(yintercept=.85, col=2, lwd=1.2)
  return(p)
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
gg_barplot <- function(df, datecol, year, sub_in, sub_out, grenzwert, paramame){
  # prepare data to plot
  part <- df[year(df[,datecol]) == yearx,]
  part <- rbind(part[grep(sub_in, part$variable),],part[grep(sub_out, part$variable),]) 
  year_st <- as.character(yearx)
  
  # prepare plot
  p = ggplot(part, aes(x=Monat,y=substance))+
      geom_bar(aes(fill=variable), stat="identity", position="identity") + 
      coord_cartesian(ylim = c(0, max(mean_Monat[,grep(sub_in, colnames(mean_Monat))], na.rm=T))) +
      geom_hline(yintercept=grenzwert, col="2", size=2, show.legend = T) +
      scale_x_date(year_st, date_breaks = "months", labels = date_format("%b")) +
      scale_fill_manual(values=c("blue","chocolate4"),
                        name="Legende",
                        labels=c("Ablauf", "Zulauf"),
                        guide=guide_legend(reverse = TRUE)) +
    scale_y_continuous(paste(paramName, "[mg/l]")) +
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