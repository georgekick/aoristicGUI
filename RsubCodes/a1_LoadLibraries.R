
cat("#############################\n")
cat("# Loading libraries...\n")
cat("#############################\n")


# check installed packages and read them
  options(repos='http://cran.rstudio.com/')  # options(repos='http://cran.cnr.Berkeley.edu')  
  
  required.packages <- c(
    "colorspace",
	"dichromat",
	"digest",
	"e1071",
	"gtable",
	"lubridate", 
    "ggplot2", 
    "ggmap", 
    "rgdal", 
    "reshape2", 
    "maptools", 
    "classInt",
    "RColorBrewer",
    "GISTools",
    "MASS",
    "spatstat",
    "splancs",
    "gWidgets",
	"gWidgetsRGtk2",
	"MASS",
	"deldir",
	"labeling",
	"mapproj",
	"maps",
	"memoise",
	"munsell",
	"plyr",
	"png",
	"proto",
	"rgeos",
	"RgoogleMaps",
	"rjson",
	"scales",
	"sp",
	"stringr",
	"plotKML"
    ) 
  new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) {
	suppressMessages(suppressWarnings(install.packages(new.packages)))
  }
  # read required library

suppressMessages(suppressWarnings(library(lubridate, quietly=TRUE)))
suppressMessages(suppressWarnings(library(ggplot2, quietly=TRUE)))
suppressMessages(suppressWarnings(library(ggmap, quietly=TRUE)))
suppressMessages(suppressWarnings(library(rgdal, quietly=TRUE)))
suppressMessages(suppressWarnings(library(reshape2, quietly=TRUE)))
suppressMessages(suppressWarnings(library(maptools, quietly=TRUE)))
suppressMessages(suppressWarnings(library(classInt, quietly=TRUE)))
suppressMessages(suppressWarnings(library(RColorBrewer, quietly=TRUE)))
suppressMessages(suppressWarnings(library(GISTools, quietly=TRUE)))
suppressMessages(suppressWarnings(library(MASS, quietly=TRUE)))
suppressMessages(suppressWarnings(library(spatstat, quietly=TRUE)))
suppressMessages(suppressWarnings(library(splancs, quietly=TRUE)))
suppressMessages(suppressWarnings(library(gWidgets, quietly=TRUE)))
suppressMessages(suppressWarnings(library(gWidgetsRGtk2, quietly=TRUE)))
	options("guiToolkit"="RGtk2")
suppressMessages(suppressWarnings(library(deldir, quietly=TRUE)))
suppressMessages(suppressWarnings(library(colorspace, quietly=TRUE)))
suppressMessages(suppressWarnings(library(dichromat, quietly=TRUE)))
suppressMessages(suppressWarnings(library(digest, quietly=TRUE)))
suppressMessages(suppressWarnings(library(e1071, quietly=TRUE)))
suppressMessages(suppressWarnings(library(gtable, quietly=TRUE)))
suppressMessages(suppressWarnings(library(labeling, quietly=TRUE)))
suppressMessages(suppressWarnings(library(mapproj, quietly=TRUE)))
suppressMessages(suppressWarnings(library(maps, quietly=TRUE)))
suppressMessages(suppressWarnings(library(memoise, quietly=TRUE)))
suppressMessages(suppressWarnings(library(munsell, quietly=TRUE)))
suppressMessages(suppressWarnings(library(plyr, quietly=TRUE)))
suppressMessages(suppressWarnings(library(png, quietly=TRUE)))
suppressMessages(suppressWarnings(library(proto, quietly=TRUE)))
suppressMessages(suppressWarnings(library(rgeos, quietly=TRUE)))
suppressMessages(suppressWarnings(library(RgoogleMaps, quietly=TRUE)))
suppressMessages(suppressWarnings(library(rjson, quietly=TRUE)))
suppressMessages(suppressWarnings(library(scales, quietly=TRUE)))
suppressMessages(suppressWarnings(library(sp, quietly=TRUE)))
suppressMessages(suppressWarnings(library(stringr, quietly=TRUE)))
suppressMessages(suppressWarnings(library(plotKML, quietly=TRUE)))

# source(file.path(folder.location, "RsubCodes/a2_SelectCSV.r")) 
source_github("https://raw.githubusercontent.com/georgekick/aoristicGUI/master/RsubCodes/a2_SelectCSV.r") 