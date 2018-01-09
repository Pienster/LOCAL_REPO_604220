
#----------------------- DATA PREPARATION 15 min CIC data---------------------

#-------------------DATA CLEANSING ----------------------------------------

# Setting time formats
# converting dates to recognised date formats
df.AA.CV15$Datetime <-as.POSIXct(paste(df.AA.CV15$Datum, df.AA.CV15$Time), format="%d-%m-%Y %H:%M")
df.OV.CV15$Datetime <-as.POSIXct(paste(df.OV.CV15$Datum, df.OV.CV15$Time), format="%d-%m-%Y %H:%M")
df.AV.CV15$Datetime <-as.POSIXct(paste(df.AV.CV15$Datum, df.AV.CV15$Time), format="%d-%m-%Y %H:%M")
df.SM.CV15$Datetime <-as.POSIXct(paste(df.SM.CV15$Datum, df.SM.CV15$Time), format="%d-%m-%Y %H:%M")

df.AA.CV15$Datum<-as.POSIXct(df.AA.CV15$Datum, format="%d-%m-%Y")
df.OV.CV15$Datum<-as.POSIXct(df.OV.CV15$Datum, format="%d-%m-%Y")
df.AV.CV15$Datum<-as.POSIXct(df.AV.CV15$Datum, format="%d-%m-%Y")
df.SM.CV15$Datum<-as.POSIXct(df.SM.CV15$Datum, format="%d-%m-%Y")

# Make Call Volume integer
df.AA.CV15$CallVolume.AA<-as.integer(df.AA.CV15$CallVolume.AA)
df.OV.CV15$CallVolume.OV<-as.integer(df.OV.CV15$CallVolume.OV)
df.AV.CV15$CallVolume.AV<-as.integer(df.AV.CV15$CallVolume.AV)
df.SM.CV15$CallVolume.SM<-as.integer(df.SM.CV15$CallVolume.SM)


# order by date time
df.AA.CV15<-df.AA.CV15[order(Datetime)]
df.OV.CV15<-df.OV.CV15[order(Datetime)]
df.AV.CV15<-df.AV.CV15[order(Datetime)]
df.SM.CV15<-df.SM.CV15[order(Datetime)]

#Removing weekends
df.AA.CV15<-df.AA.CV15[wday(Datetime)!=7 & wday(Datetime)!=1]
df.OV.CV15<-df.OV.CV15[wday(Datetime)!=7 & wday(Datetime)!=1]
df.AV.CV15<-df.AV.CV15[wday(Datetime)!=7 & wday(Datetime)!=1]
df.SM.CV15<-df.SM.CV15[wday(Datetime)!=7 & wday(Datetime)!=1]

# Removing all timestamps outside working hours (08:00-17:00)
df.AA.CV15<-df.AA.CV15[hour(Datetime)>7 & hour(Datetime)<18]
df.AA.CV15<-df.AA.CV15[Time!="17:15"& Time!="17:30" & Time!="17:45"]
df.OV.CV15<-df.OV.CV15[hour(Datetime)>7 & hour(Datetime)<18]
df.OV.CV15<-df.OV.CV15[Time!="17:15"& Time!="17:30" & Time!="17:45"]
df.AV.CV15<-df.AV.CV15[hour(Datetime)>7 & hour(Datetime)<18]
df.AV.CV15<-df.AV.CV15[Time!="17:15"& Time!="17:30" & Time!="17:45"]
df.SM.CV15<-df.SM.CV15[hour(Datetime)>7 & hour(Datetime)<18]
df.SM.CV15<-df.SM.CV15[Time!="17:15"& Time!="17:30" & Time!="17:45"]

#Removing time stamps that do not round to 15 min (i.e. 23:47)
df.AA.CV15<-df.AA.CV15[minute(Datetime)%%5==0]
df.OV.CV15<-df.OV.CV15[minute(Datetime)%%5==0]
df.AV.CV15<-df.AV.CV15[minute(Datetime)%%5==0]
df.SM.CV15<-df.SM.CV15[minute(Datetime)%%5==0]

# Replace the NA values in the Call volume to 0 
df.AA.CV15<-df.AA.CV15[is.na(get("CallVolume.AA")), CallVolume.AA := 0] 
df.OV.CV15<-df.OV.CV15[is.na(get("CallVolume.OV")), CallVolume.OV := 0] 
df.AV.CV15<-df.AV.CV15[is.na(get("CallVolume.AV")), CallVolume.AV := 0] 
df.SM.CV15<-df.SM.CV15[is.na(get("CallVolume.SM")), CallVolume.SM := 0] 


# Test that all date have 96 time stamps: 
#YY<-df.CV15[, .(`Number of rows` = .N), by = Date]


