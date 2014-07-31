aoristic.all.graph <- function(data){
  
  data2.temp <- melt(data2, id.vars=c("id", "HourFrom", "duration"))
  data2.temp$variable <- as.numeric(gsub("time", "", data2.temp$variable))

  graph <- aggregate(data2.temp$value, by=list(data2.temp$variable), FUN=sum)
  names(graph) <- c("hour", "freq")
  graph$hour <- as.numeric(graph$hour) 

  # convert as factor and reorder for a graph
  graph$hour <- factor(graph$hour, 
                     levels=c("6", "7", "8", "9", "10", "11", "12", 
                              "13", "14", "15", "16", "17", "18", "19", "20",
                              "21", "22", "23", "0", "1", "2", "3", "4", "5"))

  graph <- graph[order(graph$hour),]
  return(graph)
}