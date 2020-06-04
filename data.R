library(ggplot2)
library(gganimate)
library(httr)
library(dplyr)
library(stringi)
library(esquisse)
library(GGally)
library(ggdark)

time_series_covid19_confirmed_global <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
ts<-time_series_covid19_confirmed_global[,c(2,5:ncol(time_series_covid19_confirmed_global))]

corona_country<-as.character(unique(ts$Country.Region))
series<-function(x){colSums(ts[ts$Country.Region==x,2:ncol(ts)])}

cdata<-matrix(ncol =(ncol(ts)),
              nrow = length(corona_country))

for (i in 1:length(corona_country)){
  
  cdata[i,1]<-corona_country[i]
  
  for (j in 1:(ncol(ts)-1)){
    timeline<-series(corona_country[i])[j]
    cdata[i,j+1]<-timeline
  }
}

corona_data<-as.data.frame(t(cdata))
corona_data<-data.frame(lapply(corona_data, as.character), stringsAsFactors=FALSE)
names(corona_data)<-as.character(corona_data[1,])
corona_data<-corona_data[-1,]
corona_data<-data.frame(lapply(corona_data, as.numeric), stringsAsFactors=FALSE)

timeline<-names(ts)[-1]
timeline<-gsub("X","0",timeline)
stri_sub(timeline[nchar(timeline)==7],4,3)<-0
timeline<-as.Date(as.character(timeline), format="%m.%d.%y")
corona_data<-cbind(Time=timeline,corona_data)
Total<-rowSums(corona_data[,2:ncol(corona_data)])
corona_data<-cbind(corona_data,Total)
days<-seq(1,nrow(corona_data),1)
corona_data<-cbind(corona_data,days)
####################################


longi<-function (x){
  sub_series_name<-rep(x,length(corona_data[,x]))
  subseries<-data.frame(Time=corona_data[,1],sub_series_name,Cases=corona_data[,x],days=corona_data[,c("days")])
  return(subseries)
}



countries<-c("China","US","India","Germany","Russia","Brazil")
long_data<-longi(countries[1])

for (k in 2:length(countries)){
  temp_long_data<-longi(countries[k])
  long_data<-rbind(long_data,temp_long_data)
}
long_data$sub_series_name<-as.character(long_data$sub_series_name)
names(long_data)[2]<-"Countries"




e<-ggplot(data = long_data, aes(x=Time, y=Cases)) +
  geom_point(aes(colour=Countries),size=1.5)+
  dark_theme_classic()+
  scale_color_brewer(palette="RdBu")

e+transition_time(days) +
  labs(title = "Days ela: {round(frame_time)}")+
  shadow_mark(size = 1.5)


