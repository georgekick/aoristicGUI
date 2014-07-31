
cat("#########################################\n")
cat("# Step 1: Selecting a CSV file\n")
cat("# Please minimize or move the R Console screen, if you do not see the Step 1 dialog\n")
cat("#########################################\n")

# http://stackoverflow.com/questions/17957099/widget-for-selecting-columns-of-a-dataset-with-mutual-exclusion

# defines a new environment to store data
myenv.data <- new.env()

# function for storing the data file in myenv.data
RR_data <- function(filename){ 
    path <- dirname(filename)
    setwd(path)
    dat0 <- read.csv(filename,header=TRUE)
    assign("dat0", dat0, envir=myenv.data)
}

### MAIN WIDGET ###
win <- gwindow("Step 1: Selecting a crime incident CSV file")
WIDGET <- ggroup(cont=win)
DataGroup <- gframe("DATA", container = WIDGET, horizontal=FALSE)
NextGroup <- gframe(container=WIDGET, horizontal = FALSE)
addSpring(NextGroup)

## WIDGET: LOAD DATA ## 
grp.file <- ggroup(horizontal=FALSE, container = DataGroup)
lbl.file <- glabel("File: ", container = grp.file)
browse.file <- gfilebrowse(text = "", container = grp.file, quote=FALSE, 
								filter = list("csv files" = list(patterns = c("*.csv")),
                                       "All files" = list(patterns = c("*")) 
								))

## WIDGET: SELECT COLUMNS ##
grp.load.data <- gbutton(text="Load data", container = DataGroup, 
    handler = function(h, ...) {
    enabled(grp.load.data) <- FALSE
    RR_data(svalue(browse.file))
    #
    dat0 <- get("dat0", envir=myenv.data)
    SelectGroup <<- gframe("Select columns ", container = DataGroup, horizontal=FALSE)  
    grp.select <<-  ggroup(horizontal=FALSE, container = SelectGroup)  
    dat.columns <- colnames(dat0)  
  #
    labels <- c("FROM DateTime \n(format: mm/dd/yyyy hh:mm)", "TO DateTime \n(format: mm/dd/yyyy hh:mm)", "Longitude", "Latitude")
    Insert.columns <- lapply(1:length(labels), function(i) {
      glabel(labels[i], container = grp.select) 
      gcombobox(dat.columns, selected=i, container=grp.select)
    })
    ## make exclusive
    sapply(1:length(Insert.columns), function(i) {
      addHandlerChanged(Insert.columns[[i]], handler=function(h,...) {
        all_selected <- sapply(Insert.columns, svalue)
        selected <- svalue(h$obj)    
        ind <- which(selected == all_selected)      
        if(length(ind) > 1) {
          j <- setdiff(ind, i)
          remaining <- setdiff(fac_levels, all_selected)
          tmp <- Insert.columns[[j]]
          svalue(tmp) <- remaining[1]
        }
      })
    })
	
    FromDateTime  <<- Insert.columns[[1]]
    ToDateTime	  <<- Insert.columns[[2]]
    Longitude 	  <<- Insert.columns[[3]]
    Latitude	  <<- Insert.columns[[4]]
	
	next.button <- gbutton("Next", cont=NextGroup)
	addHandlerChanged(next.button, handler=function(h,...){
		# source(file.path(folder.location, "RsubCodes/a3_SelectGIS.r")) 
	  source_github("https://raw.githubusercontent.com/georgekick/aoristicGUI/master/RsubCodes/a3_SelectGIS.R")
		dispose(h$obj)
	}
	) 
})
