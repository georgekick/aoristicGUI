cat("#############################################\n")
cat("# Step 2: Select a GIS boundary file (optional)\n")
cat("# Please minimize or move the R Console screen, if you do not see the Step 2 dialog\n")
cat("#############################################\n")

# create a window 
w <- gwindow("Step 2: Select a GIS boundary file (optional)")
# w <- gwindow("Step 2: Select a GIS boundary file (optional)", visible=FALSE)

g <- ggroup(horizontal = FALSE, cont=w)

# select a GIS file
glabel("Step 2: Select a GIS boundary file (optional)", cont=g)
shp.file <- gfilebrowse (text = "", type = "open", quote = FALSE, 
                         cont = g, toolkit = guiToolkit(),
                         filter = list("shp files" = list(patterns = c("*.shp")),
                                       "All files" = list(patterns = c("*")) 
                         )
)

glabel("Search in:", cont = g, anchor = c(-1,0))
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
