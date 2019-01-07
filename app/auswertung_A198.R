# Auswertung der KA Betriebstagebücher - A198
# Brian Sweeney - 17.4.2018
# Raphael Menke - 5.2018
# Brian Sweeney - 11.2018
#---------------------------------------------
#### load packages ####
sapply(c("grDevices","zoo", "lattice", "ggplot2","reshape2", "lubridate", "scales", "readxl", "tidyverse"), require, character.only = T)

# set system time to "GMT"
Sys.setenv(TZ = "GMT")

# if needed change the working directory to the needed path
# setwd("P:/02/ELZ/02ELZ17012/531_Wasser/04_Berechn/Betriebstagebuecher")
# setwd("./02ELZ17012_R_auswertung")
# pathToExcel = "P:/02/ELZ/02ELZ17012/531_Wasser/04_Berechn/Betriebstagebuecher/alt/02ELZ17012_Reinigungsleistung_2012-2017.xlsx" 
# exportpfad <- paste0(getwd(),"/r_graphics/")
# pathToCsv <- "P:/02/ELZ/02ELZ17012/531_Wasser/06_Grundlagen/Elzach_Betriebstageb?cher/"
# sheetName = 'gesamt'
# getwd()

# import betriebstagebücher von excel -- (Betreibstagebücher were merged in Excel in the sheet "gesamt")
# change variables above to import another file
xcelSheet = read_excel(pathToExcel, sheet = sheetName)
csv_2017 <- read_csv2(paste0(pathToCsv, "2017.csv"))
csv_2017$Date <- dmy(csv_2017$Date)
csv_2018 <- read_csv2(paste0(pathToCsv, "2018.csv"))
csv_2018$Date <- dmy(csv_2018$Date)

tib = xcelSheet[,c(1, grep("*dval", colnames(xcelSheet)))]
tib2017 <- csv_2017[,c(1, grep("*dval", colnames(csv_2017)))]
tib2018 <- csv_2018[,c(1, grep("*dval", colnames(csv_2018)))]

# combining to one long df

start_copy <- which(tib2017$Date == "2017-11-28")
tib <- rbind(tib, tib2017[start_copy:(nrow(tib2017)),], tib2018)
range(tib$Date)

remove(tib2017,tib2018)
#save excel sheet with removed columns
check <- tib %>% 
  is.na() %>% 
  colSums() 
nas_tib  <-   which(check==nrow(tib)) #this row needs to be rewritten for every df read into R change: "nas_tib"  and "tib"

no_na<- tib[,-nas_tib]

write.xlsx(no_na, "../Reinigungsleistung_2012-2018_no_NA.xlsx", showNA=F)

tib <- no_na
remove(no_na)
df = as.data.frame(tib)

# extract data during dry weather discharge
TW <- subset(tib, tib$Wetter_dval < 3) # 1 and 2 mean dry weather

# the day before dry weather measurement must be a dry weather day
TW$date_diff <- c(1,diff(TW$Date))
tw_corrected <- subset(TW, TW$date_diff == 1)
remove(TW)

#defining the values to be drawn
colnamesx <- colnames(df[,grep("CSB_Frach|Laton_Frach|PO4P_Frach", colnames(df))])
colnamesx <- colnamesx[grep("dvaltm|VKB",colnamesx, invert = T)]
#defining the x axis labels, has to be same length as colnamesx!
xlabs <- c("CSB Fracht Zulauf [kg/d]", "Nges Fracht Zulauf [kg/d]","PO4 Fracht Zulauf [kg/d]")
xlabs_tw <- c("CSB Fracht TW [kg/d]", "Nges Fracht TW [kg/d]","PO4 Fracht TW [kg/d]") #for dry weather graphics
#plots cum sum ####

#dry weather####

ggplot_fun_tw <- function(colnamesx, xlab){
  png(paste0(exportpfad, colnamesx,"TW.png"), res=200, width = 15.5, height = 9.62, units="cm")
  print({
    p <- ggplot(tw_corrected, aes_string(colnamesx)) +
      stat_ecdf(geom = "step")+
      theme_bw()+
      ylab("Summenhäufigkeit")+
      xlab(paste("Trockenwetter Abfluss",xlab))+
      geom_hline(yintercept=.85, col=2, lwd=1.2)
  })
  dev.off()
  
}

mapply(ggplot_fun_tw, colnamesx, xlabs_tw)

#wet weather####


ggplot_fun <- function(colnamesx, xlab){
png(paste0(exportpfad, colnamesx,".png"), res=200, width = 15.5, height = 9.62, units="cm")
  print({
    p <- ggplot(df, aes_string(colnamesx)) +
  stat_ecdf(geom = "step")+
  theme_bw()+
  ylab("Summenhäufigkeit")+
  xlab(xlab)+
      geom_hline(yintercept=.85, col=2, lwd=1.2)
  })
  dev.off()

}

mapply(ggplot_fun, colnamesx, xlabs)

## calculating schlammindex and Trockensubstanz ####
tib$ts_fracht = tib$Abwassermenge_dval*tib$Bel_TS_3_dval #in  #qm/d * g/l -> kg/d 
tib$schlammindex <- tib$Bel_SV_3_dval/tib$Bel_TS_3_dval #l/kg

#writing mean to excel 
means <- data.frame(row.names = c("TS Fracht", "schlammvolumenindex"),matrix(ncol=2, nrow=2))

means[1,1] <- mean(tib$ts_fracht, na.rm = T)
means[2,1] <- mean(tib$schlammindex , na.rm = T) #mean of schlammvolumenindex
means[1,2] <- "kg/d"
means[2,2] <- "l/kg"
colnames(means) <- c("mean", "unit")

write.xlsx(means, "means_ts_isv.xlsx", row.names = T)

#plotting schlamindex ####

# summenhäufigkeit

png(paste0(exportpfad, "Schlammindex",".png"), res=200, width = 30, 
    height = 21, units="cm")
  tib %>% 
  filter(Date >="2015-01-01" & Date < "2018-01-01") %>% 
  ggplot(aes(schlammindex)) +
    stat_ecdf(geom = "step")+
    theme_bw()+
    ylab("Summenh?ufigkeit")+
    xlab("Schlammindex [l/kg]")+
    geom_hline(yintercept=.85, col=2, lwd=1.2)+
    theme(text = element_text(size=20))
  #  geom_text(aes(x=s, y=F2, size = 2.5))

dev.off()

# timeseries schlammindex 

png(paste0(exportpfad,"2_wk_avg_ISV.png"), res=200, width = 30, height = 21, units="cm")
  tib %>% filter(Date >="2015-01-01" & Date < "2018-01-01") %>% 
  ggplot() +
  geom_line(aes (y=c(rep(NA,13),
                     rollapply(schlammindex, 14, mean, na.rm=T, align="right")),
                 x= Date), color=2, lwd=1)+
  ylab("Schlammvolumenindex ISV [ml/g]")+
  xlab("")+
    theme_bw()+
    theme(text = element_text(size=20))
 dev.off()

#extract 85% value in kg/t ####
 
 since_2015 <- tib %>% 
   filter(Date >="2015-01-01" & Date < "2018-01-01") 
 
 quant_values <- matrix(nrow=6, ncol=2)
wet_weather <-   tib %>% 
   filter(Date >"2015-01-01"& Date < "2018-01-01") %>% 
   select(c(colnamesx))  %>%  
   summarise_if(is.numeric, quantile, probs=.85,na.rm = TRUE)


#brian hatte 51
length(which(!is.na(tw_corrected$Zul_CSB_Fracht_dval[which(tw_corrected$Date >"2015-01-01 UTC")[1]:636])))
 
dry_weather <- tw_corrected %>% 
  filter(Date >"2015-01-01"& Date < "2018-01-01") %>% 
  select(colnamesx) %>%  
  summarise_if(is.numeric, quantile, probs=.85,na.rm = TRUE)


colnames(quant_values) <-  c("Gesamtabfluss", "TW") 
quant_values2 <- rbind(wet_weather, dry_weather) %>% 
  as.data.frame() %>%
  t() 
rownames(quant_values) <-  c(rownames(quant_values2),"schlammindex", "ts_fracht", "EGW")

quant_values[4,1] <- quantile(ecdf(since_2015$schlammindex), probs=.85)
quant_values[5,1] <- quantile(ecdf(since_2015$ts_fracht), probs=.85)
quant_values[6,1] <- mean(tib$EGW_dval, na.rm = T)


write.xlsx(quant_values, "85_values.xlsx", row.names = T, showNA = F)



# monthly average for every year ####
mean_Monat <- df %>% mutate(date = ymd(Date)) %>% 
  mutate(Monat = ymd(paste(year(date), month(date), "15"))) %>% 
  group_by(Monat) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
  as.data.frame()


#convert into long format for ggplot
long_df <- melt(mean_Monat, value.name="substance", id.vars = "Monat")

#barblot by year and substance####

gg_barplot <- function(yearx, substance_in, substance_out, grenzwert, name){
  png(paste0(exportpfad, name,yearx,".png"), res=200, width = 15.5, height = 9.62, units="cm")
  part <- long_df[year(long_df$Monat) == yearx,]
  part <- rbind(part[grep(substance_in, part$variable),],part[grep(substance_out, part$variable),]) 
  year_st <- as.character(yearx)
  print({
bc_CSB <- ggplot(part, aes(x=Monat,y=substance))+
    geom_bar(aes(fill=variable),stat="identity", position="identity")+ 
    coord_cartesian(ylim = c(0, max(mean_Monat[,grep(substance_in, colnames(mean_Monat))], na.rm=T)))+
    geom_hline(yintercept=grenzwert, col="2", size=2, show.legend = T)+
    scale_x_date(year_st, date_breaks = "months", labels = date_format("%b"))+
       scale_fill_manual(values=c("blue","chocolate4"), name="Legende",labels=c("Ablauf", "Zulauf"),
                         guide=guide_legend(reverse = TRUE))+
    scale_y_continuous(paste(name, "[mg/l]"))+
  theme_bw()
})
  dev.off()
  
    
  }

out <- colnames(df[,grep("Abl_CSB_dval|Abl_Laton_dval|Abl_PO4_dval", colnames(df))])
out <- out[grep("dvaltm",out, invert = T)]
years <- year(long_df$Monat) %>% range() 
numberYears <- diff(years)+1
years <- c(years[1]:years[2]) %>% rep(length(out)) #because we are looking at 3 elemts:csb, laton and po4
out <- rep(out, each=numberYears)
inlet <- colnames(df[,grep("Zul_CSB_dval|Zul_Laton_dval|Zul_PO4_dval", colnames(df))])
inlet <- inlet[grep("dvaltm",inlet, invert = T)]
inlet <- rep(inlet, each=numberYears)
grenzen <- rep(c(70, 14, 2),each=numberYears ) #ab 2018 Grenzwert 1.3
name <- rep(c("CSB", "Laton", "PO4"), each=numberYears)

mapply(gg_barplot, years , inlet, out, grenzen, name)


# monthly average over all years
mean_years <- tib %>% 
  mutate(date = ymd(Date)) %>% 
  mutate(month = month(date))%>% 
  filter(date >="2015-01-01"& date < "2018-01-01") %>%  # i added this line the function uses this data
  group_by(month) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
  melt(value.name="substance", id.vars = "month")



gg_barplot_entire <- function(substance_in_e, substance_out_e, grenze_entire, name_entire){
  png(paste0(exportpfad, name_entire,"_entire.png"), res=200, width = 15.5, height = 9.62, units="cm")
  part <- rbind(mean_years[grep(substance_in_e, mean_years$variable),],mean_years[grep(substance_out_e, mean_years$variable),]) 
  print({
    bp <-  ggplot(data=part, aes(x=factor(month), y=substance))+
    geom_bar(aes(fill=variable),stat="identity", position="identity")+
    geom_hline(yintercept=grenze_entire, col=2, size=2, show.legend = T)+
    scale_fill_manual(values=c("blue","chocolate4"), name="Legende",labels=c("Ablauf", "Zulauf"),
                      guide=guide_legend(reverse = TRUE))+
    scale_y_continuous(paste(name_entire, "[mg/l]"))+
      scale_x_discrete(breaks=1:12,
                       labels=c("Jan","Feb","M?r","Apr","Mai","Jun","Jul","Aug","Sep","Oct","Nov","Dez"))+
      xlab("Monat")+
      theme_bw()
      
  })
  dev.off()
 
  
}
   
out_e <- colnames(df[,grep("Abl_CSB_dval|Abl_Laton_dval|Abl_PO4_dval", colnames(df))]) #this is only for the name the actual data is transformed in line 242
out_e <- out_e[grep("dvaltm",out_e, invert = T)]
inlet_e <- colnames(df[,grep("Zul_CSB_dval|Zul_Laton_dval|Zul_PO4_dval", colnames(df))])
inlet_e <- inlet_e[grep("dvaltm",inlet_e, invert = T)]
grenze_entire <- c(70, 14, 2) #ab 2018 Grenzwert 1.3 für po4
name_entire <- c("CSB", "Laton", "PO4")

mapply(gg_barplot_entire, inlet_e, out_e, grenze_entire, name_entire)

# auswertung an TW Tagen ####


tw_csb <-  tw_corrected%>% filter(Date> "2015-01-01" & Date < "2018-01-01") %>% 
  mutate(Date = ymd(Date)) %>% 
  select(Date, Wetter_dval, Abwasserm_TW_dval, Zul_CSB_dval, Zul_CSB_Fracht_dval, Abl_CSB_dval) %>% 
  filter(!is.na(Zul_CSB_dval)) %>% 
  as.data.frame() 
  
tw_laton <-  tw_corrected%>% filter(Date> "2015-01-01" & Date < "2018-01-01") %>% 
  mutate(Date = ymd(Date)) %>% 
  select(Date, Wetter_dval, Abwasserm_TW_dval, Zul_Laton_dval, Zul_Laton_Frach_dval, Abl_Laton_dval) %>% 
  filter(!is.na(Zul_Laton_dval)) %>% 
  as.data.frame() 

tw_po4 <-  tw_corrected %>% 
  filter(Date> "2015-01-01" & Date < "2018-01-01")%>% 
  mutate(Date = ymd(Date)) %>% 
  select(Date, Wetter_dval, Abwasserm_TW_dval, Zul_PO4_dval, Zul_PO4P_Fracht_dval, Abl_PO4_dval) %>% 
  filter(!is.na(Zul_PO4_dval)) %>% 
  as.data.frame() 

write.xlsx(tw_csb, "tw_csb.xlsx", row.names = FALSE, showNA = FALSE)
write.xlsx(tw_laton, "tw_laton.xlsx", row.names = FALSE, showNA = FALSE)
write.xlsx(tw_po4, "tw_po4.xlsx", row.names = FALSE, showNA = FALSE)



 
#save.image(file = paste0(localLib,"workspace.Rdata"))

####long term trend####
summary(glm(tib$Zul_CSB_Fracht_dval/tib$Abwassermenge_dval ~ tib$Date))
summary(glm(mean_Monat$Zul_CSB_Fracht_dval/mean_Monat$Abwassermenge_dval ~ mean_Monat$Monat))
summary(glm(tib$Zul_CSB_Fracht_dval ~ tib$Date))

plot(mean_Monat$Zul_CSB_Fracht_dval/mean_Monat$Abwassermenge_dval ~ mean_Monat$Monat)
plot(tib$Zul_CSB_Fracht_dval/tib$Abwassermenge_dval~ tib$Date)
plot(mean_Monat$Abwassermenge_dval ~ mean_Monat$Monat, type="l")
