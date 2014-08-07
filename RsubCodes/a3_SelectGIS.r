cat("#############################################\n")
cat("# Step 2: Select a GIS boundary file (optional)\n")
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
gis.true <- gcheckboxgroup(items=c("Use the boundary file's geographic extent for the grid and density analysis. \n If unchecked, the geographic extent of your incident data will be used"), 
                           checked=FALSE, cont=g) 

# select grid parameter
glabel("", cont = g)

glabel("Specify the number of grids in xy directions for grid counts \n (a minimum of 2)", cont = g)
nxy <- gedit(text = "5",  , cont = g)

# select KDE cell parameter
glabel("", cont = g)

glabel("Specify the number of cells in xy directions for density analysis\n (recommended values: 128 - 256)", cont = g)
n.cell <- gedit(text = "128",  , cont = g)


# kml point file with HTML table?
glabel("", cont = g)

html <- gcheckboxgroup(items=c("Crate a kml point file with an HTML pop-up data table"), 
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
