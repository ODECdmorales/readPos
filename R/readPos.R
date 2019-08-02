### FUNCTION TO READ DATA FILE DEFINED BY POSITIONS AND SAVE IT IN A DF


#' Function to read a data file defined by positions and save it in a DF
#'
#' @param datasource Path of a data file with plain text format (eg: .dat or .asc not associated with a xml)
#' @param positions Path of a csv that specifies the name of the variable, type and positions that have information in data file. The csv must be the following structure, otherwise this functions will not work:
#'    Four first columns (you can add more at the rigth of these four, they will be not used but can help you to add comments or somewhat)
#'    1 - variables: that column contains the name of the variables to work, the name that you write there will be the colname in R data frame.
#'    2 - type: specifies the type of the variable, allows "character", "single", "multiple" or "numeric".
#'    3 - start: positions that starts the range of data that will definie the variable.
#'    4 - finish: positions that finish the range of data that will definie the variable.
#' @return A data frame with as many columns as rows have the csv (with their "variable" names) and as many rows as rows have the data file.
#' @examples
#' readPos(datasource = "C:/Desktop/datum.dat", positions = "C:/Documents/map.csv" )
#'
#' CSV structure example(headers must be like this): 
#'
#'    variables,type,start,finish
#'    sex,single,5,6
#'    age,numeric,12,14
#'    satisfaction,character,75,100
#'
#' @export



readPos <- function(datasource, positions) {
  
  csv <- read.csv(positions)
  rawdata = read.delim(datasource, header=FALSE, sep="ç")
  
  df <-  data.frame(matrix("", ncol = nrow(csv), nrow = nrow(rawdata) ))
  colnames(df) <- csv$variable
  for (i in 1:nrow(csv))
  {
    df[,i] <- read.fwf(datasource,  widths = c(csv$start[i],(csv$finish[i] - csv$start[i])),header=FALSE, sep = "\t")[2]
  }
  return(df)
}

