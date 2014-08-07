cat("#############################################\n")
cat("# Conducting Aoristic Analysis...\n")
cat("#############################################\n")
cat("#############################################\n")
cat("# Creating an Aoristic Graph for the Entire Study Area...\n")
cat("#############################################\n")

# folder.location <- dirname(svalue(browse.file))
folder.location <- svalue(out_dir)

setwd(folder.location)

# output file folder
dir.create(file.path(folder.location, "output"), showWarnings = FALSE)

# read data
data <- read.table(svalue(browse.file), header=TRUE, sep=",")
# select columns
data <- data[, c(svalue(FromDateTime), svalue(ToDateTime), svalue(Longitude), svalue(Latitude))]
# assign column names
names(data) <- c("FromDateTime", "ToDateTime", "lon", "lat")
# remove missing data
data <- data[!is.na(data$lon),]
data <- data[!is.na(data$lat),]

# format date fields------------
#data$FromDateTime <- mdy_hm(as.character(data$FromDateTime), quiet=TRUE)
#data$ToDateTime <- mdy_hm(as.character(data$ToDateTime), quiet=TRUE)
dtFormat <- c("mdy R*", "ymd R*") # see date_time_parse help for additional formatting
data$FromDateTime <- parse_date_time(as.character(data$FromDateTime), orders=dtFormat, quiet=TRUE)
data$ToDateTime   <- parse_date_time(as.character(data$ToDateTime),   orders=dtFormat, quiet=TRUE)

if (grepl("TRUE", paste(names(table(is.na(data$FromDateTime))), collapse=""))){
  stop("# Possible errors with FROM Date Time values. (e.g., missing Date/Time values)\n")
}

# check lat/lon values
if (!class(data$lon)=="numeric") {stop ("Longitude column is not numeric")}
if (!class(data$lat)=="numeric") {stop ("Latitude column is not numeric")}

if (min(data$lon) < -180 | max(data$lon) > 180) {stop ("Longitude values have invalid values (i.e., outside of (-180, 180) range")}
if (min(data$lat) < -90  | max(data$lat) > 90) {stop ("Latitude values have invalid values (i.e., outside of (-90, 90) range")}

# create duration variables
duration <- as.numeric(difftime(data$ToDateTime, data$FromDateTime, units="hours") + 1 )
HourFrom <- hour(data$FromDateTime)
duration[duration>24] <- 24
duration <- ceiling(duration)
# recode duration as 1 hour if timeTo is missing
## different method may be chosen in a function
duration[is.na(duration)] <- 1

# error handling of date
e1 <- "FALSE"
if (grepl("TRUE", paste(names(table(duration<0)), collapse=""))){
  cat("#############################################\n")
  cat("# Possible errors with FROM/TO Date Time values.\n")
  cat("# Suggestions:\n")
  cat("# 1) Check the format of the date-time fields using Excel.\n")
  cat("#    Once you open your crime incident file with Excel, select these date-time columns\n")
  cat("#    and reset the date-time format by going to\n")
  cat("#    \"Format Cells\" --> \"Custom\" --> \"m/d/yyyy h:mm\".\n")
  cat("# 2) Check if all of the TO Date Time values occur later in time than the FROM Date Time values.\n")
  cat("#############################################\n")
  e1 <- "TRUE"
  duration[duration<0] <- 1  
  Sys.sleep(10)
}

# create df for aoristic --------------------
id <- seq(1,nrow(data), 1)

temp.df <- data.frame(time0=numeric(), time1=numeric(), time2=numeric(), time3=numeric(), time4=numeric(), time5=numeric(), time6=numeric(), time7=numeric(),
                      time8=numeric(), time9=numeric(), time10=numeric(), time11=numeric(), time12=numeric(), time13=numeric(), time14=numeric(), time15=numeric(),
                      time16=numeric(), time17=numeric(), time18=numeric(), time19=numeric(), time20=numeric(), time21=numeric(), time22=numeric(), time23=numeric())

for (i in 1:nrow(data)){
  h <- HourFrom[i]
  d <- duration[i]
  
  if(d >= 24){ 
    temp.df[i,1:24] <- 1/d
  } else if (h+d < 24){ 
    temp.df[i,(h+1):(h+d)] <- 1/d
  } else {
    temp.df[i,(h+1):24] <- 1/d
    temp.df[i,1:(h+d-23)] <- 1/d
  }
}

temp.df[is.na(temp.df)] <- 0

data2 <- cbind(id, HourFrom, duration, temp.df)
data2 <- as.data.frame(data2)

# creating all area aoristic graph -----------
data2.temp <- melt(data2, id.vars=c("id", "HourFrom", "duration"))
data2.temp$variable <- as.numeric(gsub("time", "", as.character(data2.temp$variable)))
# tail(data2.temp)

graph <- aggregate(data2.temp$value, by=list(data2.temp$variable), FUN=sum)
names(graph) <- c("hour", "freq")
graph$hour <- as.numeric(graph$hour) 

# change the factor order 
graph$hour <- factor(graph$hour, 
                      levels=c("6", "7", "8", "9", "10", "11", "12", 
                               "13", "14", "15", "16", "17", "18", "19", "20",
                               "21", "22", "23", "0", "1", "2", "3", "4", "5"))

graph <- graph[order(graph$hour),]

# set the output location
setwd(file.path(folder.location, "output"))

# create All Areas Aoristic Graph
ggplot(graph, aes(x=hour, y=freq)) + geom_bar(stat="identity") + ggtitle("Aoristic Graph for the Entire Study Area")
ggsave("allAreasAoristicGraph.png", width = 6, height = 4)

							   
							   
# read shp file # create an if-else statement for Mac ######

# create point data----------
proj.WGS84 <- CRS("+proj=longlat +datum=WGS84")
data.spdf <- SpatialPointsDataFrame(data=data2, coords=matrix(c(data$lon, data$lat), ncol=2), 
                                    proj4string=proj.WGS84)
data.spdf <- suppressMessages(reproject(data.spdf, proj.WGS84@projargs, show.output.on.console=FALSE))

# check if the GIS boundary file was selected									
if (!svalue(shp.file)==""){

  # gis.true <- "TRUE"
  
	cat("#############################################\n")
	cat("# Creating Aoristic Graphs using the GIS boundary file...\n")
	cat("#############################################\n")

	dir.create(file.path(folder.location, "output", "GISboundary"), showWarnings = FALSE)

	shp.file <- svalue(shp.file)
	
	# windows and Mac/Unix ###############
	if (Sys.info()['sysname']=="Windows"){ 
	dsn <- gsub("(.*)\\\\{1}.*.shp", "\\1", shp.file)
	# dsn <- gsub("(.*\\\\.*)\\\\{1}.*.shp", "\\1", shp.file)
	
	shp.file <- gsub(".*\\\\{1}(.*).shp", "\\1", shp.file)
	# shp.file <- gsub(".*\\\\.*\\\\{1}(.*).shp", "\\1", shp.file)
	 } else {
	dsn <- gsub("(.*)/{1}.*.shp", "\\1", shp.file)
	shp.file <- gsub(".*/{1}(.*).shp", "\\1", shp.file)
	}
	
	area.shp <-  readOGR(dsn=dsn, layer=shp.file, verbose=FALSE)
	
	## changed 2014/07/29
	# area.shp@proj4string <- proj.WGS84
	if (!check_projection(area.shp)){
		area.shp <- reproject(area.shp, proj.WGS84@projargs)
	}
	# project again even in order to use the "identical" projection 
	area.shp <- suppressMessages(reproject(area.shp, proj.WGS84@projargs, show.output.on.console=FALSE)) 
  ########
	
	area.shp@data$sortID <- seq(1, nrow(area.shp@data), 1)
	area.shp <- area.shp[order(area.shp@data$sortID),]

	# aggregate aoristic count per GIS boundary through for-loop----------
	data.spdf@data$Total <- 1 # dummy count (2014/08/05)
	for (i in 4:28){
		agg <- aggregate(data.spdf[i], area.shp, FUN=sum)
		agg@data[is.na(agg@data)] <- 0
		area.shp <- spCbind(area.shp, agg@data)
	}
  	
	# graph by areas ------------
	graph2 <- subset(area.shp@data, select=c(sortID, time0:time23))

	graph2 <- melt(graph2, id.vars="sortID")
	graph2$variable <- as.numeric(gsub("time", "", graph2$variable))
	# tail(graph2)

	# factor 
	graph2$variable <- factor(graph2$variable, 
                          levels=c("6", "7", "8", "9", "10", "11", "12", 
                                  "13", "14", "15", "16", "17", "18", "19", "20",
                                   "21", "22", "23", "0", "1", "2", "3", "4", "5"))

	names(graph2)[2:3] <- c("hour", "freq")
	# sort by area and hour
	graph2 <- graph2[order(graph2[1], graph2$hour),]

	# set output directory
	setwd(file.path(folder.location, "output", "GISboundary"))
	
	# aoristic graphs by GIS area through for-loop
	for (i in 1:nrow(area.shp@data)){
  
		graph.temp<-graph2[graph2$sortID==i,]
		p <- ggplot(graph.temp, aes(x=hour, y=freq)) + 
          geom_bar(stat="identity") +
          ylim(0, max(graph2$freq))
  
	ggsave(filename=paste("sortID_", i, ".png", sep=""), plot=p, width = 6, height = 4)
	area.shp@data$img[i] <-  paste("sortID_", i, ".png", sep="")
	}

	####
	## create KML (shapefile)--------
	
	if (nrow(area.shp@data)>9){
	nclr <- 9 # the number of classification categories
	} else {
	nclr <- round(nrow(area.shp@data)/2)
	}
	
	plotclr <- brewer.pal(nclr,"YlOrRd")

	# area.shp@data$Total <- rowSums(area.shp@data[,c("time0", "time1", "time2", "time3", "time4", 
  #                                              "time5", "time6", "time7", "time8", "time9", 
  #                                              "time10", "time11", "time12", "time13", "time14",
  #                                              "time15", "time16", "time17", "time18", "time19",
  #                                              "time20", "time21", "time22", "time23")])
	plotvar <- area.shp@data$Total

	class <- classIntervals(plotvar, nclr, style="jenks") 
	colcode <- findColours(class, plotclr, digits=4)
	area.shp@data$col <- add.alpha(colcode, 0.75)

	# relative path                          
	out <- sapply(slot(area.shp, "polygons"), function(x) { kmlPolygon(x,
                # name=as(area.shp, "data.frame")[slot(x, "ID"), "DISTRICTDE"], 
                name=paste("Crime Count: ", round(as(area.shp, "data.frame")[slot(x, "ID"), "Total"]), sep=""), 
              	col =as(area.shp, "data.frame")[slot(x, "ID"), "col"], lwd=1, border='black', 
                description=paste("<img src=", 
                                   as(area.shp, "data.frame")[slot(x, "ID"), "img"], " width=\"450\">", sep=""))})
                                                    
	kml.folder <- setwd(file.path(folder.location, "output", "GISboundary"))
	tf <- file.path(kml.folder, "Aoristic_GIS_boundary.kml")
						
	kmlFile <- file(tf, "w")
	cat(kmlPolygon(kmlname="Aoristic_GIS_boundary")$header,
    file=kmlFile, sep="\n")
	cat(unlist(out["style",]), file=kmlFile, sep="\n")
	cat(unlist(out["content",]), file=kmlFile, sep="\n")
	cat(kmlPolygon()$footer, file=kmlFile, sep="\n")
	close(kmlFile)

	# opening a KML file
		browseURL(file.path(folder.location, "output", "GISboundary", "Aoristic_GIS_boundary.kml"))
}


#########################################3
# Grid count method -------------
# http://www.bias-project.org.uk/ASDARcourse/unit5_slides.pdf

cat("#############################################\n")
cat("# Creating Aoristic Graphs by Grid Count...\n")
cat("#############################################\n")

dir.create(file.path(folder.location, "output", "Grid"), showWarnings = FALSE)
setwd(file.path(folder.location, "output", "Grid"))

# using GIS as the boundary of the grid count
gis.true <- svalue(gis.true)

if (gis.true =="TRUE"){
  area.shp <-  readOGR(dsn=dsn, layer=shp.file, verbose=FALSE)
  if (!check_projection(area.shp)){
    area.shp <- reproject(area.shp, proj.WGS84@projargs)
  }
  area.shp <- suppressMessages(reproject(area.shp, proj.WGS84@projargs, show.output.on.console=FALSE)) 
  
  data.spdf@bbox["coords.x1", "min"] <- area.shp@bbox["x", "min"]
  data.spdf@bbox["coords.x1", "max"] <- area.shp@bbox["x", "max"]
  data.spdf@bbox["coords.x2", "min"] <- area.shp@bbox["y", "min"]
  data.spdf@bbox["coords.x2", "max"] <- area.shp@bbox["y", "max"]
    
  data.ppp <- as(data.spdf, "ppp")
  
  # remove points outside of the gis boundary
  win <- owin(c(area.shp@bbox["x", "min"], area.shp@bbox["x", "max"]), c(area.shp@bbox["y", "min"], area.shp@bbox["y", "max"]))
  data.ppp <- data.ppp[win]
  
  } else {
  
  data.ppp <- as(data.spdf, "ppp")
    
}

## changed 2014/07/29 (spatstat-deprecated)
## Don't need this code?  bb may have been used initially to check missing coordinate values?
## bb <- boundingbox(data.ppp) ## bb <- bounding.box(data.ppp)

nxy <- as.numeric(svalue(nxy))
if (nxy<2){
  cat("the number of grids in xy directions needs to be 2 or greater. the default value of 5 is used\n")
  nxy <- 5
}

qc <- quadratcount(data.ppp, nx=nxy, ny=nxy)

x <- list()
for (i in 1:(length(attr(qc, "tess")$xgrid)-1)){
  x[[i]] <- attr(qc, "tess")$xgrid[i]
  x[[i]] <- rbind(x[[i]], attr(qc, "tess")$xgrid[i+1])
  x[[i]] <- rbind(x[[i]], attr(qc, "tess")$xgrid[i+1])
  x[[i]] <- rbind(x[[i]], attr(qc, "tess")$xgrid[i])
  x[[i]] <- rbind(x[[i]], attr(qc, "tess")$xgrid[i])
}

y <- list()
for (i in 1:(length(attr(qc, "tess")$ygrid)-1)){
  y[[i]] <- attr(qc, "tess")$ygrid[i]
  y[[i]] <- rbind(y[[i]], attr(qc, "tess")$ygrid[i])
  y[[i]] <- rbind(y[[i]], attr(qc, "tess")$ygrid[i+1])
  y[[i]] <- rbind(y[[i]], attr(qc, "tess")$ygrid[i+1])
  y[[i]] <- rbind(y[[i]], attr(qc, "tess")$ygrid[i])
}

poly <- list()
for (i in 1:(length(attr(qc, "tess")$ygrid)-1)){
  for (j in 1:(length(attr(qc, "tess")$ygrid)-1)){
    xy <- cbind(x[[i]], y[[j]])
    
    p <- Polygon(xy)
    ps <- Polygons(list(p), paste(i, "_", j, sep=""))
    sps.temp <- SpatialPolygons(list(ps))
    
    if (i==1 & j==1){
      sps <- sps.temp
    } else {
      sps <- rbind(sps, sps.temp) 
    }
  }
}
proj4string(sps) <- proj.WGS84

# create SPDF for quadrat count ------  		
area.shp <- sps
area.shp@proj4string <- proj.WGS84
area.shp <- as(area.shp, "SpatialPolygonsDataFrame")
area.shp@data$dummy <- seq(1, length(area.shp), 1)
names(area.shp@data) <- "sortID"

# aggregate aoristic count through for-loop (Quadrat)----------

data.spdf@data$Total <- 1 # dummy count (2014/08/05)
for (i in 4:28){
  agg <- aggregate(data.spdf[i], area.shp, FUN=sum)
  agg@data[is.na(agg@data)] <- 0
  area.shp <- spCbind(area.shp, agg@data)
}


# graph by areas (Quadrat) ------------
graph2 <- subset(area.shp@data, select=c(sortID, time0:time23))

graph2 <- melt(graph2, id.vars="sortID")
graph2$variable <- as.numeric(gsub("time", "", graph2$variable))

# factor 
graph2$variable <- factor(graph2$variable, 
                          levels=c("6", "7", "8", "9", "10", "11", "12", 
                                   "13", "14", "15", "16", "17", "18", "19", "20",
                                   "21", "22", "23", "0", "1", "2", "3", "4", "5"))

names(graph2)[2:3] <- c("hour", "freq")
# sort by area and hour
graph2 <- graph2[order(graph2[1], graph2$hour),]

for (i in 1:nrow(area.shp@data)){
  
  graph.temp<-graph2[graph2$sortID==i,]
  p <- ggplot(graph.temp, aes(x=hour, y=freq)) + 
    geom_bar(stat="identity") +
    ylim(0, max(graph2$freq))
  
  ggsave(filename=paste("sortID_", i, ".png", sep=""), plot=p, width = 6, height = 4)
  area.shp@data$img[i] <-  paste("sortID_", i, ".png", sep="")
}

## create KML (Quadrat) --------
nclr <- 9 # the number of classification categories

plotclr <- brewer.pal(nclr,"YlOrRd")

# area.shp@data$Total <- rowSums(area.shp@data[,c("time0", "time1", "time2", "time3", "time4", 
#                                                "time5", "time6", "time7", "time8", "time9", 
#                                                "time10", "time11", "time12", "time13", "time14",
#                                                "time15", "time16", "time17", "time18", "time19",
#                                                "time20", "time21", "time22", "time23")])
plotvar <- area.shp@data$Total
class <- classIntervals(plotvar, nclr, style="jenks") 
colcode <- findColours(class, plotclr, digits=4)
area.shp@data$col <- add.alpha(colcode, 0.75)

# relative path                          
out <- sapply(slot(area.shp, "polygons"), function(x) { kmlPolygon(x,
              name=paste("Crime Count: ", round(as(area.shp, "data.frame")[slot(x, "ID"), "Total"]), sep=""), 
              col =as(area.shp, "data.frame")[slot(x, "ID"), "col"], lwd=1, border='black', 
              description=paste("<img src=", 
              as(area.shp, "data.frame")[slot(x, "ID"), "img"], " width=\"450\">", sep=""))})

kml.folder <- setwd(file.path(folder.location, "output", "Grid"))
tf <- file.path(kml.folder, "Aoristic_Grid.kml")
				  
kmlFile <- file(tf, "w")
cat(kmlPolygon(kmlname="Aoristic_Grid")$header,
    file=kmlFile, sep="\n")
cat(unlist(out["style",]), file=kmlFile, sep="\n")
cat(unlist(out["content",]), file=kmlFile, sep="\n")
cat(kmlPolygon()$footer, file=kmlFile, sep="\n")
close(kmlFile)

# opening a KML file
browseURL(file.path(folder.location, "output", "Grid", "Aoristic_Grid.kml"))


#########################################
# kerneldensity, contour ----------------

cat("#############################################\n")
cat("# Creating Aoristic Graphs with Kernel Density\n")
cat("#############################################\n")

# create output folder
dir.create(file.path(folder.location, "output", "Density and Contour"), showWarnings = FALSE)
setwd(file.path(folder.location, "output", "Density and Contour"))

n.cell <- as.numeric(svalue(n.cell))

# check if GIS boundary is used to define KDE boundary
if (gis.true =="TRUE"){
    
    area.shp <-  readOGR(dsn=dsn, layer=shp.file, verbose=FALSE)
    if (!check_projection(area.shp)){
      area.shp <- reproject(area.shp, proj.WGS84@projargs)
    }
    area.shp <- suppressMessages(reproject(area.shp, proj.WGS84@projargs, show.output.on.console=FALSE)) 
    
    data.spdf@bbox["coords.x1", "min"] <- area.shp@bbox["x", "min"]
    data.spdf@bbox["coords.x1", "max"] <- area.shp@bbox["x", "max"]
    data.spdf@bbox["coords.x2", "min"] <- area.shp@bbox["y", "min"]
    data.spdf@bbox["coords.x2", "max"] <- area.shp@bbox["y", "max"]
    
    # create point data
    data.ppp <- as(data.spdf, "ppp")
        
    bbox <- c(min(data.ppp$window$xrange), max(data.ppp$window$xrange), min(data.ppp$window$yrange), max(data.ppp$window$yrange))
    kde <- kde2d(x=data.ppp$x, y=data.ppp$y, h=0.01, n=n.cell, lims=bbox) 
  } else {
    # create point data
    data.ppp <- as(data.spdf, "ppp")
    
    kde <- kde2d(x=data.ppp$x, y=data.ppp$y, h=0.01, n=n.cell) 
  }

# image(kde)
# quantile(kde$z, 0.99)
# contour(kde, levels=c(quantile(kde$z, 0.99)), add=TRUE)
# c <- contourLines(kde$x, kde$y, kde$z)
c <- contourLines(kde, levels=c(quantile(kde$z, 0.99)))
# convert a list to SpatialPolygons
unclosed <- list()
for (i in 1:length(c)){
  xy <- cbind(c[[i]]$x, c[[i]]$y)
  
  # checking if contour polygon closes. the error is likely to occur with a small sample size.
  unclosed[[i]] <- tryCatch(
    p <- Polygon(xy),
    error=function(e) e, {
      xy[nrow(xy), 1] <- xy[1, 1]
      xy[nrow(xy), 2] <- xy[1, 2]
      p <- Polygon(xy)
    }
  )
  
    ps <- Polygons(list(p), i)
    sps.temp <- SpatialPolygons(list(ps))
    
    if (!exists("c.sps")){
      c.sps <- sps.temp
    } else {
      c.sps <- rbind(c.sps, sps.temp) 
    }

}

id <- suppressWarnings(data.frame(id=getSpPPolygonsIDSlots(c.sps)))
row.names(id)<- id$id
  
proj4string(c.sps) <- proj.WGS84
c.sps <- SpatialPolygonsDataFrame(c.sps, data=id)

# create SPDF for kernel contour count ------    	
area.shp <- c.sps
area.shp@proj4string <- proj.WGS84
area.shp <- as(area.shp, "SpatialPolygonsDataFrame")
# area.shp@data$dummy <- seq(1, length(area.shp), 1)
names(area.shp@data) <- "sortID"
area.shp@data$sortID <- seq(1, length(area.shp), 1)

# aggregate aoristic count through for-loop (Kernel Contour)----------
data.spdf@data$Total <- 1 # dummy count (2014/08/05)
for (i in 4:28){
  agg <- aggregate(data.spdf[i], area.shp, FUN=sum)
  agg@data[is.na(agg@data)] <- 0
  area.shp <- spCbind(area.shp, agg@data)
}

# graph by areas (Kernel Contour) ------------
graph2 <- subset(area.shp@data, select=c(sortID, time0:time23))

graph2 <- melt(graph2, id.vars="sortID")
graph2$variable <- as.numeric(gsub("time", "", graph2$variable))

# factor 
graph2$variable <- factor(graph2$variable, 
                          levels=c("6", "7", "8", "9", "10", "11", "12", 
                                   "13", "14", "15", "16", "17", "18", "19", "20",
                                   "21", "22", "23", "0", "1", "2", "3", "4", "5"))

names(graph2)[2:3] <- c("hour", "freq")
# sort by area and hour
graph2 <- graph2[order(graph2[1], graph2$hour),]

for (i in 1:nrow(area.shp@data)){
  
  graph.temp<-graph2[graph2$sortID==i,]
  p <- ggplot(graph.temp, aes(x=hour, y=freq)) + 
    geom_bar(stat="identity") +
    ylim(0, max(graph2$freq))
  
  ggsave(filename=paste("sortID_", i, ".png", sep=""), plot=p, width = 6, height = 4)
  area.shp@data$img[i] <-  paste("sortID_", i, ".png", sep="")
}

## create KML (Kernel Contour) --------

# removed because of the dummy count above
# area.shp@data$Total <- rowSums(area.shp@data[,c("time0", "time1", "time2", "time3", "time4", 
#                                                "time5", "time6", "time7", "time8", "time9", 
#                                                "time10", "time11", "time12", "time13", "time14",
#                                                "time15", "time16", "time17", "time18", "time19",
#                                                "time20", "time21", "time22", "time23")])
	 
out <- sapply(slot(area.shp, "polygons"), function(x) {kmlPolygon(x,
                    name=paste("Crime Count: ", round(as(area.shp, "data.frame")[slot(x, "ID"), "Total"]), sep=""), 
					lwd=3, border='black', 
                    description=paste("<img src=", 
                    as(area.shp, "data.frame")[slot(x, "ID"), "img"], " width=\"450\">", sep=""))})

kml.folder <- setwd(file.path(folder.location, "output", "Density and Contour"))
tf <- file.path(kml.folder, "Aoristic_Contour.kml")
					
kmlFile <- file(tf, "w")

cat(kmlPolygon(kmlname="Aoristic_Contour")$header,
    file=kmlFile, sep="\n")
cat(unlist(out["style",]), file=kmlFile, sep="\n")
cat(unlist(out["content",]), file=kmlFile, sep="\n")
cat(kmlPolygon()$footer, file=kmlFile, sep="\n")
close(kmlFile)


# kernel density -> KML ------------------

if (gis.true =="TRUE"){
  
  area.shp <-  readOGR(dsn=dsn, layer=shp.file, verbose=FALSE)
  if (!check_projection(area.shp)){
    area.shp <- reproject(area.shp, proj.WGS84@projargs)
  }
  area.shp <- suppressMessages(reproject(area.shp, proj.WGS84@projargs, show.output.on.console=FALSE)) 

  data.spdf@bbox["coords.x1", "min"] <- area.shp@bbox["x", "min"]
  data.spdf@bbox["coords.x1", "max"] <- area.shp@bbox["x", "max"]
  data.spdf@bbox["coords.x2", "min"] <- area.shp@bbox["y", "min"]
  data.spdf@bbox["coords.x2", "max"] <- area.shp@bbox["y", "max"]
    
  sp.pix <- kde.points(data.spdf, h=0.01, n=n.cell, lims=data.spdf)
} else {
  sp.pix <- kde.points(data.spdf, h=0.01, n=n.cell)
}

sp.grd <- as(sp.pix, "SpatialGridDataFrame")
sp.grd@data$kde[sp.grd@data$kde < quantile(sp.grd@data$kde, 0.5)] <- NA

#kernel2KML.R
sp.grd.kml <- GE_SpatialGrid(sp.grd)

tf <- file.path(kml.folder, "Density")

png(file=paste(tf, ".png", sep=""), width=sp.grd.kml$width,
    height=sp.grd.kml$height, bg="transparent")

par(mar=c(0,0,0,0), xaxs="i", yaxs="i")

Lab.palette <-
  colorRampPalette(c("green", "yellow", "red"), space = "Lab")

image(as.image.SpatialGridDataFrame(sp.grd[1]), col=Lab.palette(10),
      xlim=sp.grd.kml$xlim, ylim=sp.grd.kml$ylim)

# kmlOverlay(sp.grd.kml, paste(tf, ".kml", sep=""), paste(tf, ".png", sep=""))
kmlOverlay(sp.grd.kml, kmlfile=paste(tf, ".kml", sep=""), imagefile="Density.png"), name="Density")

dev.off()

# opening KML files
browseURL(file.path(folder.location, "output", "Density and Contour", "Density.kml"))
Sys.sleep(2)
browseURL(file.path(folder.location, "output", "Density and Contour", "Aoristic_Contour.kml"))


# Creating Point KML file -----
cat("#############################################\n")
cat("# Creating Point KML file\n")
cat("#############################################\n")

  data <- read.table(svalue(browse.file), header=TRUE, sep=",")
  # remove missing data
  data <- data[!is.na(data[svalue(Longitude)]),]
  data <- data[!is.na(data[svalue(Latitude)]),]

  data.spdf <- SpatialPointsDataFrame(data=data, 
                                    coords=matrix(c(as.matrix(data[svalue(Longitude)]), as.matrix(data[svalue(Latitude)])), ncol=2), 
                                    proj4string=proj.WGS84)
 
  icon.url <- "https://dl.dropboxusercontent.com/u/121989515/kml/markers/icon57.png"
  
  kml.folder <- file.path(folder.location, "output")
  filename <- file(paste(kml.folder, "/", "Points.kml", sep=""), "w",  blocking=FALSE)
  
  write("<?xml version=\"1.0\" encoding=\"UTF-8\"?>", filename)
  write("<kml xmlns=\"http://earth.google.com/kml/2.2\">", filename, append = TRUE)
  write("<Document>", filename, append = TRUE)
  write("<name>Points</name>", filename, append = TRUE)
  write("<open>0</open>", filename, append = TRUE)
  
  write("<Style id=\"style1\">",filename, append = TRUE)
  write("<IconStyle>", filename, append = TRUE)
  write(paste("<Icon><href>", icon.url, "</href></Icon>", sep=""), filename, append = TRUE)
  write("<scale>0.3</scale>", filename, append = TRUE)
  write("</IconStyle>", filename, append = TRUE)
  write("</Style>", filename, append = TRUE)
   
  for (i in 1:nrow(data.spdf)) {
    write("<Placemark>", filename, append = TRUE)
    
    # write(paste("<name>", i, "</name>", sep=""), filename, append = TRUE)
    write("<name></name>", filename, append = TRUE)
    
    write("<styleUrl>#style1</styleUrl>", filename, append=TRUE)
    
    if (svalue(html)=="TRUE"){
      write("<description>\n", filename, append = TRUE)
      write(print(xtable(t(data.spdf@data[i,])), 
                type="html", 
                include.colnames=FALSE,
                html.table.attributes = "border=\"1\"")
          , filename, append=TRUE)
      write("</description>", filename, append = TRUE)          
      }    
      
    write("<Point>", filename, append = TRUE)
    write("<coordinates>", filename, append = TRUE)
    write(paste(coordinates(data.spdf)[i,1], coordinates(data.spdf)[i,2], sep=","), filename, append = TRUE)
    write("</coordinates>", filename, append = TRUE)
    write("</Point>", filename, append = TRUE)
    write("</Placemark>", filename, append = TRUE)      			
  }
  write("</Document>", filename, append = TRUE)
  write("</kml>", filename, append = TRUE)
  close(filename)
  
browseURL(file.path(folder.location, "output", "Points.kml"))


# ending message ----

alarm()

browseURL(file.path(folder.location, "output"))

# gmessage("Done! Please use Google Earth to conduct your Aoristic Analysis", title="message", icon = "info") 
if (!e1 == "TRUE"){
  
  cat("#############################################\n")
  cat("# Done!\n")
  cat("# Please use Google Earth to conduct your Aoristic Analysis\n")
  cat("#############################################\n")
    
  cat("Quitting R\n")
  
  Sys.sleep(7)
  quit(save = "no", status = 0, runLast = TRUE)
} else {
  cat("#############################################\n")
  cat("# Please see the warning messages about the date-time fields above.\n")
  cat("# Please use Google Earth to conduct your Aoristic Analysis.\n")
  cat("#############################################\n")
}		