data <- read.csv(data.name)                                  
folder.location <- dirname(data.name)   

# convert the date to R date-time format ------
data[,DateTimeFrom]<-mdy_hm(as.character(data[,DateTimeFrom]))
data[,DateTimeTo]<-mdy_hm(as.character(data[,DateTimeTo]))

# aoristic.df ------
data2 <- aoristic.df(data=data, DateTimeFrom=DateTimeFrom, DateTimeTo=DateTimeTo)

# aoristic.all.graph -------
cat("#############################\n")
cat("# Generating an Aoristic Graph for the Entire Study Area...\n")
cat("#############################\n")
graph <- aoristic.all.graph(data=data)
ggplot(graph, aes(x=hour, y=freq)) + geom_bar(stat="identity") + 
         ggtitle("Aoristic Graph for the Entire Study Area")

# creating aoristic SPDF
cat("#############################\n")
cat("# Generating Spatial Aoristic Data...\n")
cat("#############################\n")
data.spdf <- aoristic.spdf(data=data, DateTimeFrom=DateTimeFrom, 
                           DateTimeTo=DateTimeTo, lon=lon, lat=lat)

# create output location
dir.create(file.path(folder.location, "output"), showWarnings = FALSE)
dir.create(file.path(folder.location, "output", "Grid"), showWarnings = FALSE)

# est output location
setwd(file.path(folder.location, "output", "Grid"))

# generate a grid kml file
cat("#############################\n")
cat("# Generating a KML file for Grid Count Aoristic Graph...\n")
cat("#############################\n")
aoristic.grid(data.spdf)

# Open the grid kml file with Google Earth
browseURL(file.path(folder.location, "output", "Grid", "Aoristic_Grid.kml"))


# create and output location 
dir.create(file.path(folder.location, "output", "Density and Contour"), showWarnings = FALSE)
setwd(file.path(folder.location, "output", "Density and Contour"))

# generate a grid kml file
cat("#############################\n")
cat("# Generating a KML file for Kernel Density Aoristic Graph...\n")
cat("#############################\n")
aoristic.kernel(data.spdf)

# Open the grid kml file with Google Earth
browseURL(file.path(folder.location, "output", "Density and Contour", "Density.kml"))
Sys.sleep(10)
browseURL(file.path(folder.location, "output", "Density and Contour", "Aoristic_Contour.kml"))

cat("#############################################\n")
cat("# Done!\n")
cat("# Please use Google Earth to conduct your Aoristic Analysis\n")
cat("#############################################\n")

Sys.sleep(5)
browseURL(file.path(folder.location, "output"))

cat("Quitting R in 10 seconds\n")
Sys.sleep(10)
quit(save = "no", status = 0, runLast = TRUE)
	
