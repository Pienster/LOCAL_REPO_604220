#----------------------- DATA PREPARATION DAILY CALL VOLUME CIC data---------------------



#-------------------DATA CLEANSING ------------------------------
# Remove rows with no timestamp
df.CV<-df.CV[!is.na(df.CV$Samenv.),] # Remove rows with no timestamp

# There are two columns names with the name "ASA". Give unique column names
setnames(df.CV, make.unique(names(df.CV)))

# Remove weekends from the data
df.CV<-df.CV[!grepl(paste(c("Sat","Sun"), collapse="|"), Dag)]

# Replace the NA values in TOTAL FCST column with a zero 
df.CV<-df.CV[is.na(get("FCST-Totaal")), `FCST-Totaal` := 0] 

# Replace the NA values in the CIC "Aangeboden" call volume to 0 (this happens only on weekdays that are bank holidays)
df.CV<-df.CV[is.na(get("Aangeboden")), Aangeboden := 0] 



#------------------ FEATURE GENERATION ------------------------------
# converting dates to recognised date formats
df.CV$Datum <-as.POSIXct(strptime(df.CV$Samenv., format = "%d-%m-%y"))

# Extract the year from the date (will be used later for merging with the year and week in the GSA planning data)
df.CV$Jaar<-year(df.CV$Datum)

# Extract montth from the date (for visualisation purposes)
df.CV$Maand<-months(df.CV$Datum, abbr=TRUE)

# Create dummy variable for weekends (1=weekend, 0=weekday) --> dont, timeseries models dont handle zero values well
#df.CV[,IsWeekend:=ifelse(df.CV$Dag=="Sat"|df.CV$Dag=="Sun",1,0)]

# Create Error colum
df.CV$Error<-df.CV$Aangeboden-df.CV$`FCST-Totaal`

# ------------------ FEATURE SELECTION ------------------------------
# Remove unecessary variables (scoping of usecase)
df.CV<-df.CV[,c("Dag", "Week", "Maand", "Jaar", "Datum", "Aangeboden", "FCST-Totaal", "Error"),with = FALSE]
