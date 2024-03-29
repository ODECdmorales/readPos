### READ ### 


#' Function to read a data file defined by positions and save it in a DF
#'
#' @param datasource Path of a data file with plain text format (eg: .dat or .asc not associated with a xml)
#' @param positions Path of a csv that specifies the name of the variable, type and positions that have information in data file. The csv must be the following structure, otherwise this function will not work:
#'    Four first columns (you can add more at the rigth of these four, they will be not used but can help you to add comments or somewhat)
#'    1 - variables: that column contains the name of the variables to work, the name that you write there will be the colname in R data frame. Names don't allow blank spaces, you must replace it with other character.
#'    2 - type: specifies the type of the variable, allows "character", "single", "multiple" or "numeric".
#'    3 - start: positions that starts the range of data that will definie the variable.
#'    4 - finish: positions that finish the range of data that will definie the variable.
#' @return A data frame with as many columns as rows have the csv (with their "variable" names) and as many rows as rows have the data file.
#' @examples
#' readPos(datasource = "C:/Desktop/datum.dat", positions = "C:/Documents/map.csv" )
#'
#' CSV structure example(headers must be like this): 
#'
#'    variable,type,start,finish
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
#'    1 - variables: that column contains the name of the variables to write. Text written here must match with some column name of the data frame. Names don't allow blank spaces, you must replace it with other character.
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
#'    variable,type,start,finish,alignment
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
  
  list<-colnames(df)
  
  
  for (v in 1:nrow(csv)) {
    
    csvsc<-csv$variable[v]
    number<-which(list==csvsc)
    for (pp in 1:nrow(df)) {
      if (csv$alignment[v] == "right") { 
        lendata <- nchar(df[pp,number])
        newstart <- csv$finish[v] - lendata + 1 
        temp[pp,1] <-paste0(substr(temp[pp,],1,newstart-1),df[pp,number],substr(temp[pp,],csv$finish[v]+1,max)) 
      }
      else {
        lendata <- nchar(df[pp,number])
        newend <- csv$start[v] + lendata
        temp[pp,1] <- paste0(substr(temp[pp,],1,csv$start[v]-1),df[pp,number],substr(temp[pp,],newend,max)) 
      } 
    }
  }
  
  write.table(temp, file, sep=" ", col.names=FALSE,row.names=FALSE, quote=FALSE)
  
}





### READ CARDS ### 


#' Function to read a data file defined by positions with disaggregated data by cards and save it in a DF
#'
#' @param datasource Path of a data file with plain text format and disaggregated data by cards (eg: .dat or .asc not associated with a xml)
#' @param positions Path of a csv that specifies the name of the variable, type and positions that have information in data file. The csv must be the following structure, otherwise this function will not work:
#'    Four first columns (you can add more at the rigth of these four, they will be not used but can help you to add comments or somewhat)
#'    1 - variables: that column contains the name of the variables to work, the name that you write there will be the colname in R data frame. Names don't allow blank spaces, you must replace it with other character. 
#'    2 - type: specifies the type of the variable, allows "character", "single", "multiple" or "numeric".
#'    3 - start: positions that starts the range of data that will definie the variable.
#'    4 - finish: positions that finish the range of data that will definie the variable.
#'    5 - card: number of card where data's variable is allowed. Should be numeric value (eg: "2" when the variable is in the second line of each card. In file of 5 cards it will the following rows: 2,6,11,16...)
#' @param ncards Number of cards of the file. Eg: when a file have 6 lines for each interviwed (register) the number of cards will be 6. Integer values allowed only.
#' @return A data frame with as many columns as rows have the csv (with their "variable" names) and as many rows as rows have the data file.
#' @examples
#' readPos(datasource = "C:/Desktop/datum.dat", positions = "C:/Documents/map.csv" )
#'
#' CSV structure example(headers must be like this): 
#'
#'    variable,type,start,finish,card
#'    sex,single,5,6,1
#'    age,numeric,12,14,1
#'    satisfaction,character,4,7,3
#'
#' @export


readCards <- function(datasource,positions,ncards) {
  
  csv <- read.csv(positions)
  finalrows = nrow(read.delim(datasource, header=FALSE, sep="ç")) / ncards
  df <-  as.matrix(data.frame(matrix("", ncol = nrow(csv), nrow = finalrows )))
  colnames(df) <- csv$variable
  for (i in 1:nrow(csv))  {
    temp <- as.matrix(read.fwf(datasource,  widths = c(csv$start[i]-1,(csv$finish[i] - csv$start[i])+1),header=FALSE, sep = "\t")[2])
    for (uu in 1:finalrows) {
      df[uu,i] <- temp[(uu*ncards)-ncards+csv$cards[i]]
    }
  }  
  df<-as.data.frame(df)
  return(df) 
  
}



### STA to MAP ### 


#' Function to scan a STA definition file and create a map with the same parameters for each variable
#'
#' @param stafile Path of the STA definition file taht will be scanned and being the base to generate de map.csv
#' @param map Name of the CSV that will be generated
#' @return A csv with many rows as variable defined in STA file.
#' @examples
#' scanSTA(STA = "C:/Desktop/definition.sta", map = "output.csv")
#'
#' @export


scanSTA <- function(stafile, map) {
  options( warn = -1 )
  sta <- as.character(readtext::readtext(stafile))
  names(sta)[1] <- "kkk"
  sta <- gsub("END;.*","",sta)
  sta <- as.data.frame(strsplit(sta,split=";"))
  csv <- as.matrix(data.frame(matrix("", ncol = 4, nrow = nrow(sta) )))
  colnames(csv) <- c("variable","type","start","finish")
  for (i in 1:nrow(sta)) {
    if (grepl("=", sta[i,]) == TRUE) {
      # VARIABLE COL
      csv[i,1] <- gsub(".*=(.+)\\\\:.*", "\\1",sta[i,])
      #TYPE COL
                  if (substr(gsub(".*\\:(.+)[[:space:]].*", "\\1", sta[i,1]),1,1) == "C")      {csv[i,2] <- "character"
                  } else if (substr(gsub(".*\\\\:(.+)[[:space:]].*", "\\1", sta[i,1]),1,1) == "S") {csv[i,2] <- "single"
                  } else if (substr(gsub(".*\\\\:(.+)[[:space:]].*", "\\1", sta[i,1]),1,1) == "M") {csv[i,2] <- "multiple"
                  } else if (substr(gsub(".*\\\\:(.+)[[:space:]].*", "\\1", sta[i,1]),1,1) == "I") {csv[i,2] <- "numeric"
                  } else if (substr(gsub(".*\\\\:(.+)[[:space:]].*", "\\1", sta[i,1]),1,1) == "L") {csv[i,2] <- "logic"}
      #START AND FINISH COLS   
                if (grepl("/[00001-99999]", sta[i,1]) == TRUE) {
                        csv[i,3] <- gsub(".*/(.+)-(.+).*","\\1",sta[i,1])
                        csv[i,4] <- gsub(".*/(.+)-(.+).*","\\2",sta[i,1])
                } else if (grepl("#[^CLAS]", sta[i,1]) == TRUE) { 
                      ii <- paste0(gsub(".*#(.+),.*", "\\1", sta[i,1]),":")
                      csv[i,3] <- gsub(".*/(.+)-(.+)","\\1",sta[grep(ii, sta[,1])[1],1] )
                      csv[i,4] <- gsub(".*/(.+)-(.+)","\\2",sta[grep(ii, sta[,1])[1],1] )
                }
      
       } else {csv[i,] <- NA} 
    
  }
 csv[,4] <- gsub("[001-999],.*","",csv[,4]) 
 csv <- na.omit(csv) 
 write.csv(csv, file = map, row.names =FALSE, fileEncoding = "UTF-8", quote = FALSE ) 
 
}


