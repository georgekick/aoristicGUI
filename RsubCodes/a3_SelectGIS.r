cat("#############################################\n")
cat("# Step 2: Select a GIS boundary file (optional) and specify other parameters\n")
cat("# Please minimize or move the R Console screen, if you do not see the Step 2 dialog\n")
cat("#############################################\n")

# create a window 
w <- gwindow("Step 2: GIS and Other Parameter Specifications")

g <- ggroup(horizontal = FALSE, cont=w)

# select a GIS file
glabel("Select a GIS boundary file (optional)", cont=g)
shp.file <- try(gfilebrowse(text = "", type = "open", quote = FALSE, 
                         cont = g, toolkit = guiToolkit(),
                         filter = list("shp files" = list(patterns = c("*.shp")),
                                       "All files" = list(patterns = c("*")) 
                                      )        
                ) , silent=TRUE)

# use the boundary file's geographic extent for the grid/KDE analysis
gis.true <- gcheckbox(text=c("Use the boundary file's geographic extent for the grid and density analysis.\n (If unchecked, the geographic extent of the incident data will be used)"), 
                           checked=FALSE, cont=g) 

# select grid parameter
glabel("", cont = g)

glabel("Grid Count Parameters \n The number of grids in xy directions", cont = g)
nxy <- gedit(text = "5",  , cont = g)

# select KDE parameters
glabel("", cont = g)

glabel("Kernel Density Parameters \n The number of cells in xy directions", cont = g)
n.cell <- gedit(text = "256",  , cont = g)

glabel("Bandwidth in lon/lat unit", cont = g)
h.kde <- gedit(text = "0.01",  , cont = g)


# kml point file with HTML table?
glabel("", cont = g)

html <- gcheckbox(text=c("Create a kml point file with an HTML pop-up data table"), 
                           checked=TRUE, cont=g) 


# select output file directory
glabel("", cont = g)

glabel("Create analysis outputs in...", cont = g)
out_dir <- gfilebrowse(text = "Select a directory ...",
                         quote = FALSE,
                         type = "selectdir", cont = g)
addSpring(g)

next.button <- gbutton("Analyze! (click only once)", cont=g)
addHandlerChanged(next.button, handler=function(h,...){
  # source(file.path(folder.location, "RsubCodes/a4_AoristicAnalysis.r")) 
  source_github("https://raw.githubusercontent.com/georgekick/aoristicGUI/master/RsubCodes/a4_AoristicAnalysis.R")
  dispose(h$obj)
})
# visible(w) <- TRUE
