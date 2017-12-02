#BUBBLE MAP : LOCATION AND NUMBER OF STARTUPS IN EACH

library(RJSONIO)
library(plotly)
library(ggmap)

all_locs<-as.vector(unique(mydata$CityLocation))
all_locs
all_locs<- all_locs[-which(all_locs=="")]

lat_long<-data.frame(matrix(nrow=length(all_locs),ncol=2))
names(lat_long)<-c("lat","long")
lat_long<-cbind(lat_long,all_locs)



lat_list<-c('12.8191198', '18.7721862', '17.2968661', '22.8616238', '28.4858773',
            '28.4126442', '18.3603062', '28.4812633', '12.8519562', '22.4077459', 
            '26.7561293', '35.995683', '15.3389946', '25.56', '22.0266672',
            '1.1304753', '14.7531189', '22.137076', '-7.3500335', '8.0960452',
            '30.5594022', '10.8418115', '26.6781', '28.4917178', '8.3458909',
            '37.6043663', '47.4810022', '2.78168', '25.1756491', '23.0930923',
            '-14.7608358', '49.674', '26.3009135', '26.2059842', '-14.7608358',
            '42.227654', '37.2854725', '40.477399', '6.5546079', '15.6972666',
            '37.0479139', '40.763858', '43.7281471', '32.613216', '13.1819169')
long_list<-c('77.4312997', '72.6708337', '78.2772487', '72.4197068', '77.007847',
             '77.1947609', '73.6943185', '77.0485147', '80.1413891', '88.1876023', 
             '75.6604056', '-95.7741441', '73.6682141', '77.56', '75.109998',
             '103.6920359', '73.3287099', '73.0357373', '110.6466887', 
             '74.6406104', '76.6046552', '76.8028425', '80.7746001', 
             '77.0619388', '76.7970481', '-122.4027829', '-122.459696', 
             '44.06159', '82.8476292', '77.2362718', '-180', '-14.015517',
             '80.1617588', '72.9485753', '-180', '-71.19126', '-122.2026563',
             '-79.761944', '68.1113787', '74.3469343', '-122.588177', '-73.9548573',
             '-79.3498098', '-97.000482', '74.5873232')
 for (i in 1:nrow(lat_long)){
#   if(i %% 2 == 0) Sys.sleep(3)
#   r<-toString(lat_long[i,]$all_locs)
#   address <- findGPS(r)
#   json_of_address <- fromJSON(address)
#   #print(json_of_address)
#   latitude <- json_of_address$results[[1]]$geometry$location["lat"]
#   longitude <- json_of_address$results[[1]]$geometry$location["lng"]
  lat_long[i,]$lat<-as.numeric(lat_list[i])
  lat_long[i,]$long<-as.numeric(long_list[i])
#   Sys.sleep(1)
}

lat_long$lat<-lapply(lat_long$lat,abs)
lat_long$long<-lapply(lat_long$long,abs)
lat_long<-subset(lat_long,!is.na(lat_long$lat))


latitude_list <- c()
longitude_list <- c()

loc_fund<-as.data.frame(table(mydata$CityLocation))
names(loc_fund)<-c("CityLocation","count")
loc_fund<-subset(loc_fund,!loc_fund$count==0)


for(i in 1:nrow(loc_fund)){
  latitude=-1
  longitude=-1
  if(loc_fund[i,]$count!=0){
    row<-loc_fund[i,]
    
    location<-toString(row$CityLocation)
    
    if(is.na(location)==FALSE && location!=""){
      if(is.element(location,lat_long$all_locs)==TRUE){
        index<-which(lat_long$all_locs %in% location)
        latitude<-as.numeric(lat_long[index,]$lat)
          longitude<-as.numeric(lat_long[index,]$long)
          print(c(location,latitude,longitude))
      }
  
    }
  }
    latitude_list<-c(latitude_list,latitude)
    longitude_list<-c(longitude_list,longitude)
  
}

print(c(length(latitude_list),length(longitude_list)))
loc_fund<-cbind(loc_fund,latitude_list,longitude_list)


# removeRows <- function(rowNum, data) {
#   newData <- data[-rowNum, , drop = FALSE]
#   rownames(newData) <- NULL
#   return(newData)
# }
# 
# x<-which(loc_fund$latitude_list==-1)
# for( i in x){
#   loc_fund<-removeRows(i,loc_fund)
# }
# map parameters
g <- list(
  scope = 'india',
  projection =  list(type = 'Mercator'),         # set which version of the world to use
  showland = T,                                      # show the land
  landcolor = toRGB("gray95"),                       # set the color of the land
  showcoastlines = T,                                # show the coastlines
  coastlinecolor = toRGB("white"),                   # set the color of the coastlines
  showsubunits = T,                                  # show the country boundaries
  subunitcolor = toRGB("gray85"),                    # set the color of the country boundaries
  subunitwidth = 0.5,                                # set the width of the state boundaries
  #countrycolor = toRGB("gray85"),
  #countrywidth = 0,
  showframe = F,                                     # don't frame the map
  showocean = T,                                     # show the ocean
  oceancolor = "#FFFFFF"                             # set the color of the ocean
)

loc_fund<-loc_fund[-61,]
loc_fund$q <- with(loc_fund, cut(count, unique(quantile(count))))
levels(loc_fund$q) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
loc_fund$q <- as.ordered(loc_fund$q)

world <- plot_ly(loc_fund, lat = loc_fund$latitude_list, lon = loc_fund$longitude_list, text = loc_fund$CityLocation,
                 type = 'scattergeo', locationmode = 'country names', mode = 'markers',
                x = loc_fund$latitude_list, 
                y= loc_fund$longitude_list,size=loc_fund$count,sizes=c(1,1000),
                color=loc_fund$q
                ) %>%
  layout(title = 'Map showing number of startups in different cities of India', geo = g) 


world


#################################################################



