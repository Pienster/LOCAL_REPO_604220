#----------------------- DATA PREPARATION 15 min CIC data per Werkgroup---------------------

#-------------------DATA CLEANSING ------------------------------

# Setting the time formats
df.CV15$Datetime <-as.POSIXct(paste(df.CV15$Datum, df.CV15$Time), format="%d-%m-%Y %H:%M") # converting date and time to recognised date formats
df.CV15$Datum<-as.POSIXct(df.CV15$Datum, format="%d-%m-%Y") # converting dates to recognised date formats
df.CV15<-df.CV15[order(Datetime)] # order by date time

# Make Call Volume integer
df.CV15$CallVolume<-as.integer(df.CV15$CallVolume)

# Removing weekends
df.CV15<-df.CV15[wday(df.CV15$Datetime)!=7 & wday(df.CV15$Datetime)!=1]

# Removing any timestamps that do not round to 15 minutes (i.e. 23:47) 
df.CV15<-df.CV15[minute(Datetime)%%5==0]

# Removing all timestamps outside working hours (08:00-17:00)
df.CV15<-df.CV15[hour(Datetime)>7 & hour(Datetime)<18]
df.CV15<-df.CV15[Time!="17:15"& Time!="17:30" & Time!="17:45"]

# Replace the NA values in the Call volume to 0 (if any, i.e. on holidays)
df.CV15<-df.CV15[is.na(get("CallVolume")), CallVolume := 0] 

# Test that all date have 96 time stamps: 
#YY<-df.CV15[, .(`Number of rows` = .N), by = Date]

# Test if any NA values in data
#df.CV15[is.na(CallVolume)=="TRUE"]

# ------------------ FEATURE GENERATION ------------------------------

# Extract week number in year from the date --> no weeks dont match with the daily & GSA week numbers...
#strftime(c("2014-03-16", "2014-03-17","2014-03-18", "2014-01-01"), format = "%V")


