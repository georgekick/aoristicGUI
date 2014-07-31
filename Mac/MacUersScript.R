################################################################
########## Aoristic Analysis with Spatial Outputs  #############
################################################################

### Step 1
########## Change the column names within the double quotes to fit your crime incident data column names.
########## For example, if your data have such a column name as "DateTimeFrom", 
########## change the below "FromDateTime" within the double quote to "DateTimeFrom. 

DateTimeFrom  <- "FromDateTime" # when you edit the column name, make sure you keep the double quote symbols ""
DateTimeTo    <- "ToDateTime"
lon           <- "lon"
lat           <- "lat"

### Step 2  
########## Copy the entire script in this file and paste it to R.  Then, hit the Enter key.
########## You will be asked to select a crime incident data.
########## For sample data analysis, select "ArlingtonPD_Burg_365DaysGeocoded.csv"   

### Step 3
########## Once R finishes running, Google Earth starts with KML outputs. 
########## R will terminate its session automatically.
  
################################################################
########## Do NOT alter the codes below  #######################
################################################################
data.name <- file.choose()

# install_github('aoristic', 'georgekick')
# library(aoristic)

DateTimeFrom  <- gsub(" ", ".", DateTimeFrom) # when you edit the column name, make sure you keep the double quote symbols ""
DateTimeTo    <- gsub(" ", ".", DateTimeTo)

options(repos='http://cran.rstudio.com/') 

  required.packages <- "devtools" 
  new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) {
	suppressMessages(suppressWarnings(install.packages(new.packages)))
  }
  suppressMessages(suppressWarnings(library(devtools, quietly=TRUE)))

source_url("https://sites.google.com/site/georgekick/LoadLibraries.R")
source_url("https://sites.google.com/site/georgekick/aoristic.spdf.R")
source_url("https://sites.google.com/site/georgekick/aoristic.grid.R")
source_url("https://sites.google.com/site/georgekick/aoristic.df.R")
source_url("https://sites.google.com/site/georgekick/aoristic.all.graph.R")
source_url("https://sites.google.com/site/georgekick/aoristic.kernel.R")
source_url("https://sites.google.com/site/georgekick/aoristic.analysis.code.R")

