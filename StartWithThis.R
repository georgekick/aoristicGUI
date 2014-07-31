options(repos='http://cran.rstudio.com/')
required.packages2 <- c("RGtk2", "RCurl") 
new.packages2 <- required.packages2[!(required.packages2 %in% installed.packages()[,"Package"])]
if(length(new.packages2)){ 
  
  cat("#############################\n")
  cat("# Installing libraries...\n")
  cat("# Please select Yes or OK if asked\n")
  cat("# This installation process may take a couple of minutes\n")
  cat("# Once R finishes installing the Gtk library, it needs to be restarted\n")
  cat("# (First time use only)\n")
  cat("#############################\n")

  suppressMessages(suppressWarnings(install.packages(new.packages2)))
  suppressMessages(suppressWarnings(library(RGtk2, quietly=TRUE))) 
  suppressMessages(suppressWarnings(library(RCurl, quietly=TRUE))) 
    
  cat("#############################\n")
  cat("# R needs to be restarted...\n")
  cat("# R will terminate its session in 20 seconds\n")
  cat("# Please restart R, go to 'File' --> 'Source R Code', and select 'StartWithThis.R' again\n")
  cat("#############################\n")

  alarm()
  
  Sys.sleep(20)
  q("no")
  
  # frame_files <- lapply(sys.frames(), function(x) x$ofile)
  # frame_files <- Filter(Negate(is.null), frame_files)
  # folder.location <- dirname(frame_files[[length(frame_files)]]) 
  # setwd(folder.location)

  # makeActiveBinding("refresh", function() { 
  # system("R --save")
  # source("StartWithThis.r", chdir=T) 
  # }, .GlobalEnv)
  # refresh
}

if(!exists("frame_files")){
 frame_files <- lapply(sys.frames(), function(x) x$ofile)
 frame_files <- Filter(Negate(is.null), frame_files)
 folder.location <- dirname(frame_files[[length(frame_files)]]) 
 setwd(folder.location)
}

# github added

source_github <- function(u) {
  # load package
  require(RCurl)
  
  # read script lines from website and evaluate
  script <- getURL(u, ssl.verifypeer = FALSE)
  eval(parse(text = script))
}  

source_github("https://raw.githubusercontent.com/georgekick/aoristicGUI/master/RsubCodes/a1_LoadLibraries.R")

# source("RsubCodes/a1_LoadLibraries.r", chdir=T) 
