cat("#############################################\n")
cat("# Step 2: Select a GIS boundary file (optional) and specify other parameters\n")
cat("# Please minimize or move the R Console screen, if you do not see the Step 2 dialog\n")
cat("#############################################\n")

# create a window 
w <- gwindow("Step 2: Parameter Specifications")

g <- ggroup(horizontal = FALSE, cont=w)
GISgroup <- gframe("Select a GIS boundary file (optional)", container = g, horizontal=FALSE)


# select a GIS file
shp.file <- try(gfilebrowse(text = "", type = "open", quote = FALSE, 
                         cont = GISgroup, toolkit = guiToolkit(),
                         filter = list("shp files" = list(patterns = c("*.shp")),
                                       "All files" = list(patterns = c("*")) 
                                      )        
                ) , silent=TRUE)

# use the boundary file's geographic extent for the grid/KDE analysis
gis.true <- gcheckbox(text=c("Use the boundary file's geographic extent in all analyses)"), 
                           checked=FALSE, cont=GISgroup) 

# select grid parameter
glabel("", cont = g)

GRIDgroup <- gframe("Grid Count Parameters", container = g, horizontal=FALSE)

glabel("The number of grids in xy directions", cont = GRIDgroup)
nxy <- gedit(text = "5",  , cont = GRIDgroup)

# select KDE parameters
glabel("", cont = g)

KDEgroup <- gframe("Kernel Density Parameters", container = g, horizontal=FALSE)

glabel("The number of cells in xy directions", cont = KDEgroup)
n.cell <- gedit(text = "256",  , cont = KDEgroup)

glabel("Bandwidth in lon/lat unit", cont = KDEgroup)
h.kde <- gedit(text = "0.01",  , cont = KDEgroup)


# kml point file with HTML table?
glabel("", cont = g)

KMLgroup <- gframe("KML point file", container = g, horizontal=FALSE)

html <- gcheckbox(text=c("Create a kml point file with a pop-up data table"), 
                           checked=TRUE, cont=KMLgroup) 


# select output file directory
glabel("", cont = g)

OUTPUTgroup <- gframe("Create analysis outputs in...", container = g, horizontal=FALSE)

out_dir <- gfilebrowse(text = "Select a directory ...",
                         quote = FALSE,
                         type = "selectdir", cont = OUTPUTgroup)
glabel("", cont = g)

addSpring(g)

next.button <- gbutton("Analyze! (click only once)", cont=g)
addHandlerChanged(next.button, handler=function(h,...){
  # source(file.path(folder.location, "RsubCodes/a4_AoristicAnalysis.r")) 
  source_github("https://raw.githubusercontent.com/georgekick/aoristicGUI/master/RsubCodes/a4_AoristicAnalysis.R")
  dispose(h$obj)
})
# visible(w) <- TRUE
