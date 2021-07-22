#### Special thanks to Jean Yang (https://www.maths.usyd.edu.au/u/jeany/) for the core materials


confirmed_world <- read.csv("time_series_covid19_confirmed_global.csv",
                            stringsAsFactors = FALSE,
                            check.names =  FALSE)

hubei <- read.csv("time_series_covid19_hubei.csv",
                            stringsAsFactors = FALSE,
                            check.names =  FALSE)
hubei$Confirmed<-as.numeric(hubei$Confirmed)
hubei$Deaths<-as.numeric(hubei$Deaths)
hubei$Recovered<-as.numeric(hubei$Recovered)
hubei$Date<-as.Date(as.character(hubei$Date), format = c("%m/%d/%y"))
ggplot() + 
    geom_line(data = hubei, aes(x = Date, y = Confirmed), color = "blue") +
    geom_line(data = hubei, aes(x = Date, y = Deaths), color = "red") +
    geom_line(data = hubei, aes(x = Date, y = Recovered), color = "purple") +
    labs(x = "Date",
         y = "Number of People",title="Hubei, China")+theme_minimal()+
    theme(text = element_text(size = 14, face = "bold"),
          axis.text.x = element_text(angle = 90), legend.position = "right")+
    scale_x_date(date_labels = "%m-%d",
                 date_breaks = "1 day")+
    annotate(geom="text", x=as.Date("2020-03-28"), y=70000, label="Confirmed",
                                                 color="blue",fontface="bold",size=9) +
    annotate(geom="text", x=as.Date("2020-03-28"), y=5000, label="Deaths",
             color="red",fontface="bold",size=9) +
    annotate(geom="text", x=as.Date("2020-04-03"), y=66000, label="Recovered",
             color="purple",fontface="bold",size=9)


dim(confirmed_world)

names(confirmed_world)

head(confirmed_world[,1:6],10)

#install.packages("reshape2")

library(reshape2)

### again, we will use the package reshape2 to transform this data set to a more convenient format


confirmed_world <- reshape2::melt(confirmed_world, id.vars = c("Province/State", "Country/Region","Population","Lat", "Long"), variable.name = "Date", value.name = "Confirmed")
head(confirmed_world)

confirmed_world[confirmed_world["Province/State"]=="Hubei"]

##### load death counts and recovered counts
death_world <- read.csv("time_series_covid19_deaths_US.csv",stringsAsFactors = FALSE,
                        check.names =  FALSE)

recovered_world <- read.csv("time_series_covid19_recovered_global.csv",stringsAsFactors = FALSE, check.names =  FALSE)

head(death_world[.1:6])

head(recovered_world[,1:6])


dim(death_world)

dim(recovered_world)

death_world <- reshape2::melt(death_world, id.vars = c("Province_State", "Country_Region", "Lat", "Long_"), variable.name = "Date", value.name = "Death")

recovered_world <- reshape2::melt(recovered_world, id.vars = c("Province/State", "Country/Region", "Lat", "Long"), variable.name = "Date", value.name = "Recovered")


head(death_world)

head(recovered_world)


world_history_data <- dplyr::left_join(confirmed_world, death_world, by = c("Province_State", "Country_Region", "Lat", "Long_", "Date"))

head(world_history_data)

world_history_data <- dplyr::left_join(world_history_data, recovered_world, by = c("Province/State", "Country/Region", "Lat", "Long", "Date"))


head(world_history_data,20)

world_history_data$Date <- as.Date(as.character(world_history_data$Date), format = c("%m/%d/%y"))
head(world_history_data,20)


colnames(world_history_data) <- make.names(colnames(world_history_data))
colnames(world_history_data)

head(world_history_data,20)



library(RColorBrewer)

cols <- matrix(c(brewer.pal(9,"Set1"),brewer.pal(11,"Set3")),ncol=1)

length(unique(world_history_data$Province_State))

library(plyr)

world.summary.data <- ddply(world_history_data,.(Province_State, Date, Lat, Long_),function(x){
    colSums(x[,c("Confirmed","Death")])
})


dim(world.summary.data)

#### too many of them

#### look at the countrires with the most confirmed cases

lastday <- max(world.summary.data$Date)
lastday <- "2020-04-08"

lastday <- read.csv("time_series_covid19_lastday_global.csv",stringsAsFactors = FALSE, check.names =  FALSE)

world.summary.data <- world.summary.data[world.summary.data$Date<=lastday,]

yesterday.data <- world.summary.data[world.summary.data$Date==lastday,]

dim(yesterday.data)

head(yesterday.data)

sort.index <- sort(yesterday.data$Confirmed,decreasing=TRUE,index.return=TRUE)$ix

yesterday.data.major  <- yesterday.data[sort.index[1:20],]

yesterday.data.major

yesterday.data.major  <- data.frame(Province_State=yesterday.data$Province_State[sort.index[1:20]])

yesterday.data.major

yesterday.data.major$Province_State  <- as.character(yesterday.data.major$Province_State )


major.summary.data <- dplyr::inner_join(world.summary.data,yesterday.data.major,by = "Province_State")


dim(major.summary.data)

head(major.summary.data)



length(unique(major.summary.data$Country.Region))


rownames(cols) <- unique(major.summary.data$Country.Region)


major.summary.data$Country.Region <- factor(major.summary.data$Country.Region,
    levels = rev(yesterday.data.major$Country.Region))


library(ggplot2)



ggplot(major.summary.data[major.summary.data$Date==lastday,], aes(x = Country.Region, y = Confirmed, fill = Country.Region)) +
    geom_bar(position="stack", stat="identity")+geom_col() + scale_fill_manual(values = cols) + theme_minimal() +
    ylab("Confirmed") + xlab("") + labs(color = "Country/Region") + coord_flip() +
    theme(text = element_text(size = 12), axis.text.x = element_text(angle = 90),
        legend.position = "none")




ggplot(major.summary.data[major.summary.data$Date==lastday,], aes(x = Country.Region, y = log(Confirmed), fill = Country.Region)) +
    geom_col() + scale_fill_manual(values = cols) + theme_minimal() +
    ylab("log-Confirmed") + xlab("") + labs(color = "Country/Region") + coord_flip() +
    theme(text = element_text(size = 12), axis.text.x = element_text(angle = 90),
        legend.position = "none")


major.summary.data$Mortality<-(major.summary.data$Death*1000)/major.summary.data$Confirmed
major.summary.data$recoverrate<-(major.summary.data$Recovered*1000)/major.summary.data$Confirmed

### Generate a line graph to visualize the confirmed cases since Jan 21 (except mainland China)

ggplot(major.summary.data,
    aes(x = Date, y = Mortality, col = Country.Region)) + geom_line(lwd = 1) + geom_point(size = 2) +
    scale_color_manual(values = cols) + theme_minimal() + ylab("Mortality Rate") +
    xlab("") + labs(color = "Country/Region") + scale_x_date(date_labels = "%m-%d",
    date_breaks = "1 day") + theme(text = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 90), legend.position = "right")


### Q1: Use a graphical summary to visualize the confirmed cases within US, stratified by state.

world_history_data <- world_history_data[world_history_data$Date<=lastday,]


US.data <- world_history_data[world_history_data$Country_Region=="US",]

unique(US.data$Province_State)

### As can be seen, the result is messy. You need to modify the data.

### the cases are counted within each county and also in the state level.

PS <- unique(world_history_data$Province_State)


grep("County",PS)


new.PS <- PS[-grep("County",PS)]
new.PS

### So the "County" is not sufficient

remove.index <- grep(",",PS)
which(remove.index==123)

remove.index <- remove.index[-71]


new.PS <- PS[-remove.index]

new.PS <- new.PS[-59]


new.PS

#### another way to join


any(US.data$Province_State[1]==new.PS)

unlist(lapply(US.data$Province.State,function(x){
    any(x==new.PS)
}))

US.data <- US.data[unlist(lapply(US.data$Province.State,function(x){
    any(x==new.PS)
})),]

unique(US.data$Province.State)


ggplot(confirmed_world, aes(x = Long_,
    y = Lat, col = Confirmed)) + geom_line(lwd = 1) + geom_point(size = 2)  + ylab("Confirmed") + xlab("") + labs(color = "States and Territories") +
    scale_x_date(date_labels = "%m-%d", date_breaks = "1 day") + theme_minimal() +
    theme(text = element_text(size = 14, face = "bold"), axis.text.x = element_text(angle = 90),
          legend.position = "right")

-
### again, this is too many. You need to focus on the major signal
specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
condition <- rep(c("normal" , "stress" , "Nitrogen") , 4)
value <- abs(rnorm(12 , 0 , 15))
data <- data.frame(specie,condition,value)
ggplot(lastday, aes(fill=Type, y=Value, x=Country)) + 
    geom_bar(position="stack", stat="identity")+coord_flip()
install.packages("plotly")
library(plotly)
mapDetails <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    showlakes = TRUE,
    lakecolor = toRGB('white')
)

usaMap <- plot_geo(confirmed_world, locationmode = 'USA-states') %>%
    add_trace(
        z = confirmed_world$Confirmed, locations = confirmed_world)
library(ggplot2)
library(dplyr)
choro <- left_join(
    map_data("state"), 
    confirmed_world %>% 
        add_rownames("region") %>% 
        mutate(region=tolower(region))
)
ggplot(choro, aes(long, lat)) +
    geom_polygon(aes(group = group, fill = Confirmed)) + 
    coord_quickmap()













