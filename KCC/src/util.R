#### Utility functions for PROJECT_TEMPLATE #####

################ LIBRARIES ##################
#############################################

if (!require("pacman")) install.packages("pacman")
pacman::p_load(lubridate,
               ggplot2,
               ggfortify,
               plotly,
               dplyr,
               tidyr,
               DT,
               log4r,
               readr,
               stringr,
               mlbench,
               rmarkdown,
               knitr,
               caret,
               dygraphs,
               gridExtra,
               rpart,
               data.table,
               arules,
               ggrepel, 
               arulesViz, 
               RColorBrewer,
               kableExtra, 
               data.table, 
               RODBC,
               odbc,
               getPass,
               DBI,
               magrittr, 
               forecast, 
               xts, 
               zoo)


############## ENV VARIABLES ################
#############################################

paths <- list(home          = PROJECT_HOME,
              data          = file.path(PROJECT_HOME, "data"),
              visualisation = file.path(PROJECT_HOME, "visualisation"),
              reports       = file.path(PROJECT_HOME, "reports"),
              cache         = file.path(PROJECT_HOME, "cache"))



################ CUSTOM FUNCTIONS ##################
#############################################

# ------------LOAD FUNCTIONS-------------

# Load the data log file
load_data_log<-function(){
  df <- fread(file.path(paths$data, "data_log.csv"),na.strings=c("",NA,"NULL"), stringsAsFactors=FALSE)
  return(df)
}

# Load the data dictionary
load_data_dictionary<-function(){
  df <- fread(file.path(paths$data, "data_dictionary.csv"),na.strings=c("",NA,"NULL"), stringsAsFactors=FALSE)
  return(df)
}

# Load the daily call volume CIC data 2016-2017
load_CV_data<-function(){
    df <- fread(file.path(paths$data, "raw", "R005", "Data sample_fcst2016_2017.csv"),na.strings=c("",NA,"NULL"), stringsAsFactors=FALSE)
    return(df)
}

# Load the 15 min CIC data of 2017
load_CV15_data<-function(){
  df1 <- fread(file.path(paths$data, "raw", "R004", "Aanbod Totaal per kwartier_jan_jun_2017.csv"),na.strings=c("",NA,"NULL"), stringsAsFactors=FALSE, skip=6, drop = 1)
  df2 <- fread(file.path(paths$data, "raw", "R004", "Aanbod Totaal per kwartier_jul_dec_2017.csv"),na.strings=c("",NA,"NULL"), stringsAsFactors=FALSE, skip=6, drop = 1)
  colnames(df1)[1] <- "Time"
  colnames(df2)[1] <- "Time"
  df1<-df1[-1,] # dropping the redundant row between header and data
  df2<-df2[-1,] # dropping the redundant row between header and data
  df1[, grep("V", names(df1)) := NULL] #dropping the column names with a V (indicating an unnamed column)
  df2[, grep("V", names(df2)) := NULL] #dropping the column names with a V (indicating an unnamed column)
  df1<-melt(df1, id.vars = c("Time"), variable.name = "Datum", value.name = "CallVolume") 
  df2<-melt(df2, id.vars = c("Time"), variable.name = "Datum", value.name = "CallVolume") 
  
  df<-rbind(df1, df2)
  return(df)
}

# Load the 15 min call volume CIC data of 2017 AA werkgroup
load_AA_CV15_data<-function(){
  df1 <- fread(file.path(paths$data, "raw", "R003", "Aanbod AA per kwartier - jan-jun 2017.csv"),na.strings=c("",NA,"NULL"), stringsAsFactors=FALSE, skip=6, drop = 1)
  df2 <- fread(file.path(paths$data, "raw", "R003", "Aanbod AA per kwartier - jul-dec 2017.csv"),na.strings=c("",NA,"NULL"), stringsAsFactors=FALSE, skip=6, drop = 1)
  
  colnames(df1)[1] <- "Time"
  colnames(df2)[1] <- "Time"
  df1<-df1[-1,] # dropping the redundant row between header and data
  df2<-df2[-1,] # dropping the redundant row between header and data
  df1[, grep("V", names(df1)) := NULL] #dropping the column names with a V (indicating an unnamed column)
  df2[, grep("V", names(df2)) := NULL] #dropping the column names with a V (indicating an unnamed column)
  df1<-melt(df1, id.vars = c("Time"), variable.name = "Datum", value.name = "CallVolume.AA") 
  df2<-melt(df2, id.vars = c("Time"), variable.name = "Datum", value.name = "CallVolume.AA") 
  
  df<-rbind(df1, df2)
  return(df)
}

# Load the 15 min call volume CIC data of 2017 AV werkgroup
load_AV_CV15_data<-function(){
  df1 <- fread(file.path(paths$data, "raw", "R003", "Aanbod AV per kwartier - jan-jun 2017.csv"),na.strings=c("",NA,"NULL"), stringsAsFactors=FALSE, skip=6, drop = 1)
  df2 <- fread(file.path(paths$data, "raw", "R003", "Aanbod AV per kwartier - jul-dec 2017.csv"),na.strings=c("",NA,"NULL"), stringsAsFactors=FALSE, skip=6, drop = 1)
  
  colnames(df1)[1] <- "Time"
  colnames(df2)[1] <- "Time"
  df1<-df1[-1,] # dropping the redundant row between header and data
  df2<-df2[-1,] # dropping the redundant row between header and data
  df1[, grep("V", names(df1)) := NULL] #dropping the column names with a V (indicating an unnamed column)
  df2[, grep("V", names(df2)) := NULL] #dropping the column names with a V (indicating an unnamed column)
  df1<-melt(df1, id.vars = c("Time"), variable.name = "Datum", value.name = "CallVolume.AV") 
  df2<-melt(df2, id.vars = c("Time"), variable.name = "Datum", value.name = "CallVolume.AV") 
  
  df<-rbind(df1, df2)
  return(df)
}

# Load the 15 min call volume CIC data of 2017 Overig werkgroup
load_OV_CV15_data<-function(){
  df1 <- fread(file.path(paths$data, "raw", "R003", "Aanbod Overig per kwartier - jan-jun 2017.csv"),na.strings=c("",NA,"NULL"), stringsAsFactors=FALSE, skip=6, drop = 1)
  df2 <- fread(file.path(paths$data, "raw", "R003", "Aanbod Overig per kwartier - jul-dec 2017.csv"),na.strings=c("",NA,"NULL"), stringsAsFactors=FALSE, skip=6, drop = 1)
  
  colnames(df1)[1] <- "Time"
  colnames(df2)[1] <- "Time"
  df1<-df1[-1,] # dropping the redundant row between header and data
  df2<-df2[-1,] # dropping the redundant row between header and data
  df1[, grep("V", names(df1)) := NULL] #dropping the column names with a V (indicating an unnamed column)
  df2[, grep("V", names(df2)) := NULL] #dropping the column names with a V (indicating an unnamed column)
  df1<-melt(df1, id.vars = c("Time"), variable.name = "Datum", value.name = "CallVolume.OV") 
  df2<-melt(df2, id.vars = c("Time"), variable.name = "Datum", value.name = "CallVolume.OV") 
  
  df<-rbind(df1, df2)
  return(df)
}

# Load the 15 min call volume CIC data of 2017 Slimme meters werkgroup
load_SM_CV15_data<-function(){
  df1 <- fread(file.path(paths$data, "raw", "R003", "Aanbod Slimme Meter per kwartier - jan-jun 2017.csv"),na.strings=c("",NA,"NULL"), stringsAsFactors=FALSE, skip=6, drop = 1)
  df2 <- fread(file.path(paths$data, "raw", "R003", "Aanbod Slimme Meter per kwartier - jul-dec 2017.csv"),na.strings=c("",NA,"NULL"), stringsAsFactors=FALSE, skip=6, drop = 1)
  
  colnames(df1)[1] <- "Time"
  colnames(df2)[1] <- "Time"
  df1<-df1[-1,] # dropping the redundant row between header and data
  df2<-df2[-1,] # dropping the redundant row between header and data
  df1[, grep("V", names(df1)) := NULL] #dropping the column names with a V (indicating an unnamed column)
  df2[, grep("V", names(df2)) := NULL] #dropping the column names with a V (indicating an unnamed column)
  df1<-melt(df1, id.vars = c("Time"), variable.name = "Datum", value.name = "CallVolume.SM") 
  df2<-melt(df2, id.vars = c("Time"), variable.name = "Datum", value.name = "CallVolume.SM") 
  
  df<-rbind(df1, df2)
  return(df)
}

# Load the GSA planning data 
load_GSA_planning<-function(){
  df.OP <- fread(file.path(paths$data, "raw", "R002", "GSA_Planning_Opdrachten2016-2017.csv"),na.strings=c("",NA,"NULL"),stringsAsFactors=FALSE)
  df.AAN <- fread(file.path(paths$data, "raw", "R002", "GSA_Planning_Aanbiedingen2016-2017.csv"),na.strings=c("",NA,"NULL"),stringsAsFactors=FALSE)
  df.VER <- fread(file.path(paths$data, "raw", "R002", "GSA_Planning_Verslimmingen2016-2017.csv"),na.strings=c("",NA,"NULL"),stringsAsFactors=FALSE)
  colnames(df.OP) <- paste("Opdrachten", colnames(df.OP), sep = "_")
  colnames(df.AAN) <- paste("Aanbiedingen", colnames(df.AAN), sep = "_")
  colnames(df.VER) <- paste("Verslimmingen", colnames(df.VER), sep = "_")
  df<-df.OP[df.AAN, on = c("Opdrachten_JaarWeek" = "Aanbiedingen_JaarWeek")]
  df<-df[df.VER, on = c("Opdrachten_JaarWeek" = "Verslimmingen_JaarWeek")]
  return(df)
}

# Load the E001 external data: Bankholidays
load_bankholidays<-function(){
  df <- fread(file.path(paths$data, "external", "E001", "feestdagen_2016_2018.csv"),na.strings=c("",NA,"NULL"),stringsAsFactors=FALSE)
  return(df)
}


# --------------COMPUTATIONAL FUNCTIONS ----------
mse<-function(error){
  mean(error^2, na.rm=TRUE)
}

# Function that returns Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2, na.rm=TRUE))
}

# Function that returns Mean Absolute Error
mae <- function(error)
{
  mean(abs(error), na.rm=TRUE)
}


# ------------- LOADING AND SAVING FUNCTIONS -------------
load_mergeddata <- function(){
  load(file.path(paths$cache, "200_merged.data.rda"))
}

load_mergeddata <- function(){
  load(file.path(paths$cache, "210_merged.data.rda"))
}

load_mergeddata <- function(){
  load(file.path(paths$cache, "220_merged.data.rda"))
}


















# load_pos_data_old <- function() {
#   pos2014_2016 <- readr::read_csv(file.path(paths$data, "raw", "R001", "Andreas Franz - London stores 2014-15-16.csv"),col_names = FALSE)
#   header <- readr::read_csv(file.path(paths$data, "raw", "R003", "PoS_Header.csv"))
#   posdec2016 <- readr::read_csv(file.path(paths$data, "raw", "R006", "Dec2016.csv"))
#   names(pos2014_2016) <- names(header)
#   names(posdec2016) <- names(header)
#   df <- rbind(pos2014_2016,posdec2016)
#   df <- df[!nchar(df$SKU)<3,]
#   return(df)
# }
# 
# # new pos data load funtion (including the 2012-2013 data, the replacement dec data and the missing store 05H)
# load_pos_data <- function() {
#   pos2012_2013 <- read.csv(file.path(paths$data, "raw", "R018", "Data2012_2013.csv"),header = FALSE, fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
#   pos2014_2016 <- read.csv(file.path(paths$data, "raw", "R001", "Andreas Franz - London stores 2014-15-16.csv"),header = FALSE,  fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
#   posdec2016 <- read.csv(file.path(paths$data, "raw", "R017", "Andreas Franz - London stores 2016-12 December.csv"), header = FALSE,  fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
#   pos2014_2016_05H <- read.csv(file.path(paths$data, "raw", "R016", "Andreas Franz - London stores 05H 2014-15-15 till Nov 2016.csv"),header = FALSE,  fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
#   header <- readr::read_csv(file.path(paths$data, "raw", "R003", "PoS_Header.csv"))
# 
#   names(pos2012_2013) <- names(header)
#   names(pos2014_2016) <- names(header)
#   names(posdec2016) <- names(header)
#   names(pos2014_2016_05H) <- names(header)
# 
#   pos2014_2016 <- pos2014_2016 %>%
#     filter(lubridate::year(pos2014_2016$Trx_Date) != 2013) # remove the 2013 values in in the 2014-2016 data to avoid duplicates
# 
#   df.pos <- rbind(pos2012_2013, pos2014_2016_05H ,pos2014_2016,posdec2016)
#   df.pos <- df.pos[order(df.pos$shopid, df.pos$Trx_TimeStamp),]
#   df.pos <- df.pos[!nchar(df.pos$SKU)<3,]
#   return(df.pos)
# }
# 
# 
# load_data_receipts <- function() {
#   df <- readr::read_csv(file.path(paths$data, "data_receipts.csv"))
#   return(df)
# }
# 
# load_store_master <- function() {
#   df.store <- readr::read_csv(file.path(paths$data, "raw", "R004", "Store_Master.csv"))
#   df.store$Postcode <- gsub(" ", "", df.store$Postcode, fixed = TRUE)
#   return(df.store)
# }
# 
# load_bank_holidays <- function() {
#   # Source: https://www.gov.uk/bank-holidays
#   # Loads a list of bank data from a calendar file
#   bh <- readLines(file.path(paths$data, "external", "E001", "england-and-wales.ics"))
#   bh <- as.data.frame(bh)
#   bh$id <- cumsum(bh == "BEGIN:VEVENT")
#   bh <- bh %>%
#     dplyr::filter(bh$id > 0) %>%
#     separate(bh, ":", into=c("entity", "value")) %>%
#     filter(str_detect(entity, "SUMMARY|DTSTART")) %>%
#     spread(entity, value)
#   names(bh) <- c("id", "date", "description")
#   bh$date <- ymd(bh$date)
#   return(bh)
# }
# 
# 
# # This function refers to the 26 february hierarchical data (this file includes headers)
# load_product_hierarchies <- function(){
#   df.hrch1 <- read.csv(file.path(paths$data, "raw", "R019", "Andreas Franz - London stores - Product master.csv"), header = TRUE, fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
#   df.hrch2 <- read.csv(file.path(paths$data, "raw", "R019", "Product Master - Timberland.csv"), header = TRUE, fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
#   df.hrch <- rbind(df.hrch1,df.hrch2)
#   return(df.hrch)
# }
# 
# load_footfall_data <- function(){
#   FFfiles <- list.files(file.path(paths$data, "raw", "R012"),
#                         full.names=TRUE)
#   FFlist <- lapply(FFfiles, function(x) read.csv(x,
#                                                  fileEncoding="UTF-8-BOM",
#                                                  stringsAsFactors = FALSE))
#   #lapply(FFlist, dim) test nr of rows
#   df.FFTotal <- bind_rows(FFlist)
#   return(df.FFTotal)
# }
# 
# load_PCtoWard_data <- function(){
#   PCtoWard <- read.csv(file.path(paths$data, "external", "E006", "London_postcode-ONS-postcode-Directory-May15.csv"),
#                        fileEncoding="UTF-8-BOM",
#                        stringsAsFactors = FALSE)
#   PCtoWard <- PCtoWard[,c(1,8)]
#   PCtoWard$pcd <- gsub(" ", "", PCtoWard$pcd, fixed = TRUE)
#   return(PCtoWard)
# }
# 
# load_Mosaic_data <- function(){
#   df.mosaic <- read.csv(file.path(paths$data, "raw", "R021", "London Mosaic.csv"),
#                         fileEncoding="UTF-8-BOM",
#                         stringsAsFactors = FALSE)
#   return(df.mosaic)
# }
# 
# 
# 
# load_ecomm_data <- function(){
#   Ecomm.files <- list.files(file.path(paths$data, "raw", "R022"), full.names=TRUE)
#   Ecomm.list <- lapply(Ecomm.files, function(x) read.csv(x, 
#                                                          fileEncoding="latin1", 
#                                                          stringsAsFactors = FALSE,
#                                                          colClasses = c(rep("character", 6), rep("numeric", 3))))
#   df.ecomm <- bind_rows(Ecomm.list)
#   df.ecomm$Ship.ZIP.Code <- gsub(" ", "", df.ecomm$Ship.ZIP.Code, fixed = TRUE)
#   df.ecomm$Ship.ZIP.Code <- toupper(df.ecomm$Ship.ZIP.Code)
#   df.ecomm$ADSR.date <- as.POSIXct(strptime(df.ecomm$ADSR.date, format = "%Y-%m-%d"))
#   return(df.ecomm)
# }
# 
# load_stock_data <- function(){
#   Stockfiles <- list.files(file.path(paths$data, "raw", "R020"), full.names=TRUE)
#   Stocklist <- lapply(Stockfiles, function(x) read.csv(x,
#                                                        fileEncoding="UTF-8-BOM",
#                                                        stringsAsFactors = FALSE))
#   df.StockTotal <- bind_rows(Stocklist)
#   return(df.StockTotal)
# }
# 
# load_weather_data <- function(){
#     #list.files(file.path(paths$data, "external", "E005"), full.names=TRUE)
#     df.wth <- read.csv(file.path(paths$data, "external", "E005", "WeatherdataLondon2012-2016.tab"),sep="\t" ,fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
#     df.wth$DateHrGmt <-as.POSIXct(strptime(df.wth$DateHrGmt, format = "%m/%d/%Y %H:%M:%S"))
#     df.wth$DateHrLwt <-as.POSIXct(strptime(df.wth$DateHrLwt, format = "%m/%d/%Y %H:%M:%S"))
#     return(df.wth)
# }
# 
# load_demographics_data <- function(){
#   df.demo <- read.csv(file.path(paths$data, "external", "E010", "UK_Demographics_Data_2011_Final.csv"),
#                       sep="\t",
#                       fileEncoding="latin1", 
#                       stringsAsFactors = FALSE)
#   #df.demo$Trx_TimeStamp <- as.POSIXct(strptime(df.demo$Trx_TimeStamp, format = "%d/%m/%Y %H:%M:%S"))
#   #df.demo$Trx_TimeStamp_hour <- as.POSIXct(strptime(df.demo$Trx_TimeStamp_hour, format = "%d/%m/%Y %H:%M:%S"))
#   df.demo <- df.demo[,!apply(df.demo, 2, function(x) all(gsub(" ", "", x)=="", na.rm=TRUE))]
#   return(df.demo)
# }
# 
# 
# 
# # I001 contains the stored version. I003 contains the latest recalculated version.
# load_enriched_data <- function(){
#   df.enr <- read.csv(file.path(paths$data, "interim", "I001", "EnrichedPosdata.csv"),
#                      sep=",",
#                      fileEncoding="latin1", 
#                      stringsAsFactors = FALSE)
#   return(df.enr)
# }
# 
# load_latest_enriched_data <- function(){
#   df.enr <- read.csv(file.path(paths$data, "interim", "I003", "EnrichedPosdata.csv"),
#                      sep=",",
#                      fileEncoding="latin1", 
#                      stringsAsFactors = FALSE)
#   return(df.enr)
# }
# 
# load_londonward_lookup <- function(){
#   df.lwlkup <- read.csv(file.path(paths$data, "external", "E010", "LondonWardCodes_plus05HWard.csv"),
#                         sep=",",
#                         fileEncoding="latin1", 
#                         stringsAsFactors = FALSE)
#   return(df.lwlkup)
# }
# 
# load_postcode_longlat <- function(){
#   df.pcll <- read.csv(file.path(paths$data, "external", "E011", "ukpostcodes.csv"),sep="," ,fileEncoding="latin1", stringsAsFactors = FALSE)
#   df.pcll$postcode <- gsub(" ", "", df.pcll$postcode, fixed = TRUE)
#   return(df.pcll)
# }
# 
# 
# load_ecomm_latlong_data <- function(){
#   # Loads the enriched (using lat / long data) E-Comm file from the Interim data files
#   df.ecenr <- read.csv(file.path(paths$data, "interim", "I002", "EnrichedEcommdata.csv"), sep="," , fileEncoding="latin1", stringsAsFactors = FALSE)
#   return(df.ecenr)
# }
# 
# 
# load_style_lookup <- function(){
#   df.style_lookup <- read.csv(file.path(paths$data, "interim", "I007", "style_lookup.csv"), sep=",")
#   return(df.style_lookup)
# }
# 
# # WRITE FUNCTIONS
# 
# 
# write_enriched_data <- function(df){
#   write.csv(df,
#             file.path(paths$data, "interim", "I001", "EnrichedPosdata.csv"),
#             fileEncoding="latin1",
#             row.names = FALSE)
# }
# 
# write_ecomm_data <- function(df){
#   write.csv(df,
#             file.path(paths$data, "interim", "I002", "Ecommdata.csv"), 
#             fileEncoding="latin1",
#             row.names = FALSE)
# }
# 
# write_ecomm_latlong_data <- function(df){
#   write.csv(df,
#             file.path(paths$data, "interim", "I002", "EnrichedEcommdata.csv"), 
#             fileEncoding="latin1", 
#             row.names = FALSE)
# }
# 


