## script to create the dataset of each of the college basketball rosters for the teams in the 2016 tournament
require(XML)
require(sqldf)
require(plotly)
require(ggmap)
require(maps)
require(shiny)


schools <- c("kansas", "austin-peay", "colorado", "connecticut", "maryland", 
             "south-dakota-state", "california", "hawaii", "arizona", "vanderbilt",
             "wichita-state", "miami-fl", "buffalo", "iowa", "temple", "villanova", 
             "north-carolina-asheville", "oregon", "holy-cross", "southern", "saint-josephs", 
             "cincinnati", "baylor", "yale", "duke", "north-carolina-wilmington",
             "texas", "northern-iowa", "texas-am", "green-bay", "oregon-state", "virginia-commonwealth",
             "oklahoma", "cal-state-bakersfield", "north-carolina", "fairleigh-dickinson", "florida-gulf-coast", 
             "southern-california", "providence", "indiana", "chattanooga", "kentucky", "stony-brook", "notre-dame",
             "michigan", "tulsa", "west-virginia", "stephen-f-austin", "wisconsin", 
             "pittsburgh", "xavier", "weber-state", "virginia", "hampton", "texas-tech",
             "butler", "purdue", "arkansas-little-rock", "iowa-state", "iona", "seton-hall", "gonzaga",
             "utah", "fresno-state", "dayton", "syracuse", "michigan-state", "middle-tennessee")
seeds <- c(1,16,8,9,5,12,4,13,6,11,11,3,14,7,10,2,15,1,16,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15,
           1,16,16,8,9,5,12,4,13,6,11,11,3,14,7,10,2,15,1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15)

lookup<- cbind(schools, seeds)

Player.Table <- as.data.frame(matrix(c(rep(NA,11)), nrow=1))
names(Player.Table) <- c("Player", "#", "Class", "Pos", "Height", "Weight", "Hometown", "High School", "Summary", "College", "Conference") 

for(school in schools){
  temp <- readHTMLTable(paste0("http://www.sports-reference.com/cbb/schools/",school,"/2016.html#all_roster"), which=1)
  conference <- xmlValue(htmlParse(paste0("http://www.sports-reference.com/cbb/schools/",school,"/2016.html#all_roster"))["//a[contains(@href,'conference')]"][[2]])
  temp$College <- school
  temp$Conference <- conference
  Player.Table <- rbind(Player.Table, temp)
  temp<- NULL
  print(school)
}

## merge on the seeds for each school
Player.Table <- sqldf("select a.*, b.seeds from `Player.Table` as a left join lookup as b on a.College = b.schools")

## where are all of these kids from?
## first need to turn their hometowns into geographic coordinates...
head(Player.Table)
Player.Table <- Player.Table[!(is.na(Player.Table$Player)), ]

hometown.coordinates <- as.data.frame(matrix(rep(NA,2), nrow=1))
highschool.coordinates <- as.data.frame(matrix(rep(NA,2), nrow=1))
names(hometown.coordinates)<- names(highschool.coordinates) <- c("lon", "lat")

for(i in 1:nrow(Player.Table)){
  hometown.coordinates <- rbind(hometown.coordinates, geocode(Player.Table$Hometown[i]))
  highschool.coordinates <- rbind(highschool.coordinates, geocode(Player.Table$`High School`[i]))
}

## combine the lat/lon data with the Player table
hometown.coordinates2 <- hometown.coordinates[-1,]
highschool.coordinates2 <- highschool.coordinates[-1,]
Player.Table <- cbind(Player.Table, hometown.coordinates2)
names(Player.Table) <- c("Player", "#", "Class", "Pos", "Height", "Weight", "Hometown", "High School", "Summary", "College", "Conference", "HometownLon", "HometownLat") 
Player.Table <- cbind(Player.Table, highschool.coordinates2)
names(Player.Table) <- c("Player", "#", "Class", "Pos", "Height", "Weight", "Hometown", "High School", "Summary", "College", "Conference", "HometownLon", "HometownLat", "HighSchoolLon", "HighSchoolLat") 

## grab the seeds from the teams
lookup <- as.data.frame(lookup)
Player.Table <- sqldf("select a.*, b.seeds from `Player.Table` as a left join lookup as b on a.College = b.schools")

## create a plotly map of all the players
Player.Table$hover <- with(Player.Table, paste(Player, Hometown, College, Class, "POS: ", Pos))

## export the dataset that was created with lat/lon
write.csv(Player.Table, "C:/Users/Ben/Documents/GitHub/College Bball/College BBall Player Table.csv")

## read back in the player table dataset
Player.Table <- read.csv("C:/Users/Ben/Documents/GitHub/College Bball/College BBall Player Table.csv")


all_states <- map_data("state")


#plot all states with ggplot
temp <- sqldf(paste0("select * from `Player.Table` where HometownLon >=-150 and HometownLon <=-50 and HometownLat>=23 and HometownLat <=50"))
p <- ggplot()
p <- p + geom_polygon( data=all_states, aes(x=long, y=lat, group = group),colour="white", fill="grey10" )
p <- p + geom_point(data=temp, aes(x=HometownLon, y=HometownLat, color=Pos), size=5, shape = 1) + theme_classic() + theme(panel.grid.major = element_blank(),
                                                                                                                           panel.grid.minor = element_blank(),
                                                                                                                           panel.border = element_blank(),
                                                                                                                           panel.background = element_blank(),
                                                                                                                           axis.line = element_blank(),
                                                                                                                           axis.text = element_blank(),
                                                                                                                           axis.title = element_blank(),
                                                                                                                          axis.ticks = element_blank())

ggsave(filename = "C:/Users/Ben/Documents/GitHub/College Bball/All-Players-Map.png", plot = p)

## create a map for each position -- is there a trend?
for(positions in unique(Player.Table$Pos)){
  temp <- sqldf(paste0("select * from `Player.Table` where HometownLon >=-150 and HometownLon <=-50 and HometownLat>=23 and HometownLat <=50 and Pos ='", positions, "'"))
  
  p <- ggplot()
  p <- p + geom_polygon( data=all_states, aes(x=long, y=lat, group = group),colour="white", fill="grey10" )
  p <- p + geom_point(data=temp, aes(x=HometownLon, y=HometownLat, color=Pos), size=10, shape = 1) + theme_classic() + theme(panel.grid.major = element_blank(),
                                                                                                                             panel.grid.minor = element_blank(),
                                                                                                                             panel.border = element_blank(),
                                                                                                                             panel.background = element_blank(),
                                                                                                                             axis.line = element_blank(),
                                                                                                                             axis.text = element_blank(),
                                                                                                                             axis.title = element_blank())
  
  assign(x=positions, value = p)
}

## maybe the best seeded teams are indicative of where kids are from?
Player.Table$seeds <- as.numeric(as.character(Player.Table$seeds)) 
seed.summary <- sqldf("select * from `Player.Table` where HometownLon >=-150 and HometownLon <=-50 and HometownLat>=23 and HometownLat <=50")
seed.map <- ggplot()
seed.map <- seed.map + geom_polygon( data=all_states, aes(x=long, y=lat, group = group),colour="white", fill="grey10" )
seed.map <- seed.map + geom_point(data=seed.summary, aes(x=HometownLon, y=HometownLat, color = seeds), size=5, alpha = .5)+
                      scale_colour_gradient(low = "darkred", high = "white") + theme_classic() + theme(panel.grid.major = element_blank(),
                                                                                                                          panel.grid.minor = element_blank(),
                                                                                                                          panel.border = element_blank(),
                                                                                                                          panel.background = element_blank(),
                                                                                                                          axis.line = element_blank(),
                                                                                                                          axis.text = element_blank(),
                                                                                                                          axis.title = element_blank(),
                                                                                                                          axis.ticks = element_blank())
ggsave(filename = "C:/Users/Ben/Documents/GitHub/College Bball/NCAA Player Map - Shaded by tournament Seed.png", plot = seed.map)

for(conferences in unique(Player.Table$Conference)){
  temp <- sqldf(paste0("select * from `Player.Table` where HometownLon >=-150 and HometownLon <=-50 and HometownLat>=23 and HometownLat <=50 and Conference ='", conferences, "'"))
  
  p <- ggplot()
  p <- p + geom_polygon( data=all_states, aes(x=long, y=lat, group = group),colour="white", fill="grey10" )
  p <- p + geom_point(data=temp, aes(x=HometownLon, y=HometownLat, color=Pos), size=10, shape = 1) + theme_classic() + theme(panel.grid.major = element_blank(),
                                                                                                                             panel.grid.minor = element_blank(),
                                                                                                                             panel.border = element_blank(),
                                                                                                                             panel.background = element_blank(),
                                                                                                                             axis.line = element_blank(),
                                                                                                                             axis.text = element_blank(),
                                                                                                                             axis.title = element_blank())
  
  assign(x=conferences, value = p)
}



#######################################################################
### Calculate the distance between all of the players for each team ###
#######################################################################
schools <- unique(as.character(Player.Table$College))
distances <- as.data.frame(cbind(schools, rep(NA, 68)))
names(distances) <- c("School", "AverageDist")
distances$AverageDist <- 0

for(schoolz in unique(Player.Table$College)){
  temp <- Player.Table[Player.Table$College == schoolz & !(is.na(Player.Table$HometownLon)), ]
  combinations <- combinations <- combn(nrow(temp),2, simplify=FALSE)
  temp2 <- rep(NA, length(combinations))
  for(i in 1:length(combinations)){
      temp2[i] <- gdist(temp$HometownLon[combinations[[i]][1]], temp$HometownLat[combinations[[i]][1]], temp$HometownLon[combinations[[i]][2]], temp$HometownLat[combinations[[i]][2]], units = "miles")
}
  distances[distances$School==schoolz,]$AverageDist <- sum(temp2)/length(combinations)
}

distances <- distances[order(distances$AverageDist), ]
write.csv(x = distances, file = "C:/Users/Ben/Documents/GitHub/College Bball/Team Spread.csv")

## northern iowa looks pretty nuts -- everyone from Iowa??
## not surprisingly hawaii has people from all over... but it is 1,000 mile more on average!?

NI <- sqldf(paste0("select * from `Player.Table` where HometownLon >=-150 and HometownLon <=-50 and HometownLat>=23 and HometownLat <=50 and College = 'northern-iowa'"))
NI.p <- ggplot()
NI.p <- NI.p + geom_polygon( data=all_states, aes(x=long, y=lat, group = group),colour="white", fill="grey10" )
NI.p <- NI.p + geom_point(data=NI, aes(x=HometownLon, y=HometownLat, color=Pos), size=10, shape = 1) + theme_classic() + theme(panel.grid.major = element_blank(),
                                                                                                                           panel.grid.minor = element_blank(),
                                                                                                                           panel.border = element_blank(),
                                                                                                                           panel.background = element_blank(),
                                                                                                                           axis.line = element_blank(),
                                                                                                                           axis.text = element_blank(),
                                                                                                                           axis.title = element_blank())
NI.p

## villanova is also crazy centralized
v <- sqldf(paste0("select * from `Player.Table` where HometownLon >=-150 and HometownLon <=-50 and HometownLat>=23 and HometownLat <=50 and College = 'villanova'"))
v.p <- ggplot()
v.p <- v.p + geom_polygon( data=all_states, aes(x=long, y=lat, group = group),colour="white", fill="grey10" )
v.p <- v.p + geom_point(data=v, aes(x=HometownLon, y=HometownLat), size=8, alpha = .5, color = "steelblue2") + theme_classic() + theme(panel.grid.major = element_blank(),
                                                                                                                               panel.grid.minor = element_blank(),
                                                                                                                               panel.border = element_blank(),
                                                                                                                               panel.background = element_blank(),
                                                                                                                               axis.line = element_blank(),
                                                                                                                               axis.text = element_blank(),
                                                                                                                               axis.title = element_blank(),
                                                                                                                               axis.ticks = element_blank())
v.p

ggsave(filename = "C:/Users/Ben/Documents/GitHub/College Bball/Villanova.png", plot = v.p)



## villanova is also crazy centralized
nc <- sqldf(paste0("select * from `Player.Table` where HometownLon >=-150 and HometownLon <=-50 and HometownLat>=23 and HometownLat <=50 and College = 'north-carolina'"))
nc.p <- ggplot()
nc.p <- nc.p + geom_polygon( data=all_states, aes(x=long, y=lat, group = group),colour="white", fill="grey10" )
nc.p <- nc.p + geom_point(data=nc, aes(x=HometownLon, y=HometownLat), size=8, alpha = .5, color = "skyblue") + theme_classic() + theme(panel.grid.major = element_blank(),
                                                                                                                                        panel.grid.minor = element_blank(),
                                                                                                                                        panel.border = element_blank(),
                                                                                                                                        panel.background = element_blank(),
                                                                                                                                        axis.line = element_blank(),
                                                                                                                                        axis.text = element_blank(),
                                                                                                                                        axis.title = element_blank(),
                                                                                                                                       axis.ticks = element_blank())
nc.p

ggsave(filename = "C:/Users/Ben/Documents/GitHub/College Bball/North-Carolina.png", plot = nc.p)
