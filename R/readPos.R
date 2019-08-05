### READ ### 


#' Function to read a data file defined by positions and save it in a DF
#'
#' @param datasource Path of a data file with plain text format (eg: .dat or .asc not associated with a xml)
#' @param positions Path of a csv that specifies the name of the variable, type and positions that have information in data file. The csv must be the following structure, otherwise this function will not work:
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





### WRITE ### 


#' Function to write plain text data file like .dat or .asc delimited by positions fixen in csv definition file.
#'
#' @param df Data frame where data is stored. Each column in input data frame will be a variable in plain text data file output.
#' @param file Path with the name of the file that will be generated. 
#' @param positions Path of a csv that specifies the name of the variable, type and positions where data will be stored in data file. The csv must be the following structure, otherwise this function will not work:
#'    Four five columns (you can add more at the rigth of these four, they will be not used but can help you to add comments or somewhat)
#'    1 - variables: that column contains the name of the variables to write. Text written here must match with some column name of the data frame.
#'    2 - type: specifies the type of the variable, allows "character", "single", "multiple" or "numeric".
#'    3 - start: positions that starts the range of data that will definie the variable.
#'    4 - finish: positions that finish the range of data that will definie the variable.
#'    5 - aligment: with "left" or "right" allowed values. Indicates where data will aligment in the positions range defined. Default value is "left" when the camp is empty.
#' @return A plain text data file like .asc or .dat that got the information of each data frame's column in sepecified positions by csv.
#' @examples
#' readPos(datasource = "C:/Desktop/datum.dat", positions = "C:/Documents/map.csv" )
#'
#' CSV structure example(headers must be like this): 
#'
#'    variables,type,start,finish,alignment
#'    sex,single,5,6,right
#'    age,numeric,12,14,
#'    satisfaction,character,75,100,left
#'
#' @export


writePos <- function(df,file,positions) {
  
   csv <- read.csv(positions)
   max <- max(csv$finish)
   dumblank <- " "
   for (yt in 1:max) {dumblank <- paste0(dumblank," ")}
   
   temp <- data.frame(matrix("", ncol = 1, nrow = nrow(df) ))
   temp[1] <- dumblank
   
   for (v in 1:ncol(df)) {
       for (pp in 1:nrow(df)) {
        if (csv$alignment[v] == "right") { 
            lendata <- nchar(df[pp,v])
            newstart <- csv$finish[v] - lendata + 1 
            temp[pp,1] <-paste0(substr(temp[pp,],1,newstart-1),df[pp,v],substr(temp[pp,],csv$finish[v]+1,max)) 
        }
        else {newend <- csv$start[v] + lendata
            lendata <- nchar(df[pp,v])
            temp[pp,1] <- paste0(substr(temp[pp,],1,csv$start[v]-1),df[pp,v],substr(temp[pp,],newend,max)) 
        } 
       }
   }
  
   write.table(temp, file, sep=" ", col.names=FALSE,row.names=FALSE, quote=FALSE)
    
}




