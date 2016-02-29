##PART1
pollutantmean <- function(directory, pollutant, id = 1:332) {
  files_list <- list.files(directory, full.names=TRUE) ##creates list of files
  dat <- data.frame() #creates empty data frame
  for (i in id) {
    #looping through and concatenating data
    dat <- rbind(dat, read.csv(files_list[i],header=TRUE))
  }
  ##get mean of those things getting rid of the NAs
  mean(dat[,pollutant], na.rm=TRUE)
}


##PART2
complete <- function(directory, id = 1:332) {
  files_list <- list.files(directory, full.names=TRUE) ##creates list of files
  dat <- data.frame( id = numeric(), nobs = numeric()) ##creates empty data frame
  for (i in id) { ##iterate through listed array
    filename <- files_list[i]
    ##read table and create new dataframe
    newrow <- data.frame( id = i, nobs = sum(complete.cases(read.csv(filename, header=TRUE)))) 
    ##add to aggregate data frame
    dat <- rbind(dat, newrow)}
  ##display aggregate data frame
  return(dat)}


##PART3
corr <- function(directory, threshold = 0) {
  files_list <- list.files(directory, full.names=TRUE) ##create list of files
  dat <- vector()
  
  for (i in 1:length(files_list)){
    read_table <- read.csv(files_list[i])
    calc_sum <- sum((!is.na(read_table$sulfate)) & (!is.na(read_table$nitrate)))
    if (calc_sum > threshold) {
      temp_var <- read_table[which(!is.na(read_table$sulfate)), ]
      sub_read_table <- temp_var[which(!is.na(temp_var$nitrate)), ]
      dat <- c(dat, cor(sub_read_table$sulfate, sub_read_table$nitrate))
    }
  }
  dat
}



