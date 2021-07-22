#### Special thanks to Jean Yang (https://www.maths.usyd.edu.au/u/jeany/) for the core materials


confirmed_world <- read.csv("time_series_covid19_confirmed_global.csv",stringsAsFactors = FALSE,
                            check.names =  FALSE)



dim(confirmed_world)

names(confirmed_world)

head(confirmed_world[,1:6],10)

#install.packages("reshape2")

library(reshape2)

### again, we will use the package reshape2 to transform this data set to a more convenient format


confirmed_world <- reshape2::melt(confirmed_world, id.vars = c("Province/State", "Country/Region","Population","Lat", "Long"), variable.name = "Date", value.name = "Confirmed")
head(confirmed_world)
confirmed_world$Date<-as.Date(as.character(confirmed_world$Date), format = c("%m/%d/%y"))
lastday <- "2020-08-15"


##### load death counts and recovered counts
death_world <- read.csv("time_series_covid19_deaths_global.csv",stringsAsFactors = FALSE,
                        check.names =  FALSE)

recovered_world <- read.csv("time_series_covid19_recovered_global.csv",stringsAsFactors = FALSE,
                            check.names =  FALSE)

head(death_world[.1:6])

head(recovered_world[,1:6])


dim(death_world)

dim(recovered_world)

death_world <- reshape2::melt(death_world, id.vars = c("Province/State", "Country/Region", "Lat", "Long"), variable.name = "Date", value.name = "Death")

recovered_world <- reshape2::melt(recovered_world, id.vars = c("Province/State", "Country/Region", "Lat", "Long"), variable.name = "Date", value.name = "Recovered")


head(death_world)

head(recovered_world)


world_history_data <- dplyr::left_join(confirmed_world, death_world, by = c("Province/State", "Country/Region", "Lat", "Long", "Date"))

head(world_history_data)

world_history_data <- dplyr::left_join(world_history_data, recovered_world, by = c("Province/State", "Country/Region", "Lat", "Long", "Date"))


head(world_history_data,20)

world_history_data$Date <- as.Date(as.character(world_history_data$Date), format = c("%m/%d/%y"))
head(world_history_data,20)
world_history_data$Population<-world_history_data$Population/1000000

colnames(confirmed_world) <- make.names(colnames(confirmed_world))
colnames(world_history_data)

head(world_history_data,20)



library(RColorBrewer)

cols <- matrix(c(brewer.pal(9,"Set1"),brewer.pal(11,"Set3")),ncol=1)

length(unique(world_history_data$Country.Region))

library(plyr)

world.summary.data <- ddply(world_history_data,.(Country.Region, Date),function(x){
    colSums(x[,c("Long","Lat","Confirmed","Death","Recovered")])
})


dim(world.summary.data)

#### too many of them

#### look at the countrires with the most confirmed cases

lastday <- max(world.summary.data$Date)
lastday <- "2020-08-15"

world.summary.data$Infectedpermil<-world.summary.data$Confirmed/world.summary.data$Population
world.summary.data <- world.summary.data[world.summary.data$Date<=lastday,]

yesterday.data <- confirmed_world[confirmed_world$Date==lastday,]

dim(yesterday.data)

head(yesterday.data,20)


sort.index <- sort(yesterday.data$Infectedpermil,decreasing=TRUE,index.return=TRUE)$ix

yesterday.data.major  <- yesterday.data[c(75,144,4,118,102,78,157,162,85,113,100,17,62,10,66,177,136,83,121,127),]

yesterday.data.major

yesterday.data.major  <- data.frame(Country.Region=yesterday.data$Country.Region[c(75,144,4,118,102,78,157,162,85,113,100,17,62,10,66,177,136,83,121,127)])

yesterday.data.major

yesterday.data.major$Country.Region  <- as.character(yesterday.data.major$Country.Region )


major.summary.data <- dplyr::inner_join(world.summary.data,yesterday.data.major,by = "Country.Region")


dim(major.summary.data)

head(major.summary.data)



length(unique(major.summary.data$Country.Region))


rownames(cols) <- unique(major.summary.data$Country.Region)


major.summary.data$Country.Region <- factor(major.summary.data$Country.Region,
    levels = rev(yesterday.data.major$Country.Region))


library(ggplot2)

ggplot(major.summary.data[major.summary.data$Date==lastday,], aes(x = Country.Region, y = Infectedpermil, fill = Country.Region)) +
    geom_col() + scale_fill_manual(values = cols) + theme_minimal() +
    ylab("Infected Per Million") + xlab("") + labs(color = "Country/Region") + coord_flip() +
    theme(text = element_text(size = 12), axis.text.x = element_text(angle = 90),
        legend.position = "none")




ggplot(major.summary.data[major.summary.data$Date==lastday,], aes(x = Country.Region, y = log(Confirmed), fill = Country.Region)) +
    geom_col() + scale_fill_manual(values = cols) + theme_minimal() +
    ylab("log-Confirmed") + xlab("") + labs(color = "Country/Region") + coord_flip() +
    theme(text = element_text(size = 12), axis.text.x = element_text(angle = 90),
        legend.position = "none")



major.summary.data$Mortality<-(major.summary.data$Death*1000)/major.summary.data$Population
### Generate a line graph to visualize the confirmed cases since Jan 21 (except mainland China)

ggplot(major.summary.data[major.summary.data$Country.Region != "China", ],
    aes(x = Date, y = Mortality, col = Country.Region)) + geom_line(lwd = 1) + geom_point(size = 2) +
    scale_color_manual(values = cols) + theme_minimal() + ylab("Mortality Rate") +
    xlab("") + labs(color = "Country/Region") + scale_x_date(date_labels = "%m-%d",
    date_breaks = "1 day") + theme(text = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 90), legend.position = "right")


### Q1: Use a graphical summary to visualize the confirmed cases within US, stratified by state.

world_history_data <- world_history_data[world_history_data$Date<=lastday,]


US.data <- world_history_data[world_history_data$Country.Region=="US",]

unique(US.data$Province.State)

### As can be seen, the result is messy. You need to modify the data.

### the cases are counted within each county and also in the state level.

PS <- unique(US.data$Province.State)


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


any(US.data$Province.State[1]==new.PS)

unlist(lapply(US.data$Province.State,function(x){
    any(x==new.PS)
}))

US.data <- US.data[unlist(lapply(US.data$Province.State,function(x){
    any(x==new.PS)
})),]

unique(US.data$Province.State)


ggplot(US.data[US.data$Date > "2020-02-20", ], aes(x = Date,
    y = Confirmed, col = Province.State)) + geom_line(lwd = 1) + geom_point(size = 2)  + ylab("Confirmed") + xlab("") + labs(color = "States and Territories") +
    scale_x_date(date_labels = "%m-%d", date_breaks = "1 day") + theme_minimal() +
    theme(text = element_text(size = 14, face = "bold"), axis.text.x = element_text(angle = 90),
          legend.position = "right")

### again, this is too many. You need to focus on the major signal

pop_data <- wb(indicator = "SP.POP.TOTL", startdate = 2018, enddate = 2018)


map("world", col="gray30",  border="gray10", fill=TRUE, bg="black")
map1<- map_data("world")
ggplot()+
    geom_polygon(data = map1, aes(x=long, y = lat,group=group), fill="grey", alpha=0.3)+
    geom_point( data=yesterday.data, aes(x=Long, y=Lat,size=Confirmed,color=Confirmed))+
    scale_size_continuous(range=c(3,14))+scale_color_viridis(trans="log")+theme_void() 

