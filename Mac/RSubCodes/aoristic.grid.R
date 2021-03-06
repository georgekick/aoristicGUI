aoristic.grid <- function(data.spdf){
  data.ppp <- as(data.spdf, "ppp")
  # bb <- bounding.box(data.ppp)
  qc <- quadratcount(data.ppp, nx=5, ny=5)

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
  
  proj4string(sps) <- CRS(data.spdf@proj4string@projargs)

# create SPDF for quadrat count ------    	
area.shp <- sps
area.shp@proj4string <- CRS(data.spdf@proj4string@projargs)
area.shp <- as(area.shp, "SpatialPolygonsDataFrame")
area.shp@data$dummy <- seq(1, length(area.shp), 1)
names(area.shp@data) <- "sortID"

# aggregate aoristic count through for-loop (Quadrat)----------

for (i in 4:27){
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

area.shp@data$Total <- rowSums(area.shp@data[,c("time0", "time1", "time2", "time3", "time4", 
                                                "time5", "time6", "time7", "time8", "time9", 
                                                "time10", "time11", "time12", "time13", "time14",
                                                "time15", "time16", "time17", "time18", "time19",
                                                "time20", "time21", "time22", "time23")])
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
}
