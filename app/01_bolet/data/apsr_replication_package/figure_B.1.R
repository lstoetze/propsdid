#############################################################################################################
#################Figure B.1: Share of media coverage on just transition in national vs regional newspapers
#############################################################################################################

#load packages
library(readxl)
library(openxlsx)
library(quanteda)
library(ggplot2)
library(dplyr)
library(dygraphs)
library(xts)

#set your working directory
setwd("~/replication_just_transition")
article_data <- read_excel("articles_full.xlsx")
data <- data.frame(article_data)

data$jt <- (ifelse (grepl("transición just|transición ecológica| minas|Teresa Ribera| minería del carbón |ley de Cambio Climático|centrales de carbón|Hunosa|acuerdo minero|cuencas mineras|transición energé| carbón|comarcas mineras|acuerdo minería|
                          acuerdo marco| SOMA |sindicato de los obreros mineros de Asturias|descarboni|térmicas de carbón|plantas de carbón" ,  data$title, ignore.case = TRUE), 1, 0))

data$psoe <- (ifelse (grepl(("PSOE| Partido Socialist"),  data$title, ignore.case = TRUE) & data$jt ==1, 1, 0))

#count total number of articles
data$count <- 1
total <- aggregate(count ~ date+paper, data,sum)

#creating data set for plotting
data$date <- as.Date(data$date, "%m/%d/%y")
df <- data[,2:4]

#data set for total counts of mentions
count <- aggregate(jt ~ date+paper, df, sum)
#differentiate papers by national or regional
count$cat <- (ifelse(grepl("elPais|elMundo",count$paper), "National", "Regional"))

#merging the JT and total data sets

clean.data <- merge(count,total, by=c("date","paper"))

#calculate proportion of JT to total articles
clean.data$prop <- (clean.data$jt/clean.data$count)*100

#graphing
p <- ggplot(clean.data, aes(x=date, y=prop, color=cat)) + geom_line() + scale_x_date(date_labels = "%m %y") + scale_y_continuous(breaks = seq(0, 6, 2))
write.csv(count, "justtransition_final.csv")

clean.data <- merge(count,total, by=c("date","paper"))
d<-clean.data[!(clean.data$paper=="laVoz"),]
d$Newspapers <- (ifelse(grepl("elPais|elMundo",d$paper), "National", "Regional"))
d$prop1 <- (d$jt/d$count)*100
p <- ggplot(d, aes(x=date, y=prop1, color=Newspapers)) + geom_line() + scale_x_date(date_labels = "%m %y")

startTime <- as.Date("2018-01-01")
endTime <-as.Date("2019-05-01")
start.end <- c(startTime,endTime)

dates_vline <- as.Date(c("2018-06-08", "2018-07-10","2018-07-15", "2018-10-24","2018-12-17","2018-12-21", "2019-01-22"))                 # Define positions of vline
dates_vline <- which(d$date %in% dates_vline)

p_final <- ggplot(d, aes(x=date, y=prop1, color=Newspapers)) + geom_line() + scale_x_date(limits = start.end, breaks = "3 months", date_labels=("%b %y")) +  xlab("Date") + 
  ylab("Share of Media Coverage on Just Transition (in %)")
p1 <- p_final + scale_color_manual(values= c("black", "#69b3a2")) + theme_bw() 
p2 <- p1+ annotate("text", x = as.Date("2018-10-24") ,y=8.7,label = "Agreement", color = "black", size=2.7) +
  geom_vline(xintercept = as.Date("2018-10-24"),lty='dashed')
p3 <- p2 + annotate("text", x = as.Date("2019-01-22"),y=8.7,label = "Law", colour = "black", size=2.7) +
  geom_vline(xintercept = as.Date("2019-01-22"),lty='dashed')
p3

ggsave(p3, filename="figure_b1.pdf")
