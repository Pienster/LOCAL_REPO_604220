#----------------------- DATA MERGING -------------------------------------

# Create DAILY masterfile
df.master<-merge(df.CV, df.bh, by="Datum", all.x=T) # Merge daily Call volume data to bank holiday data (left join)
df.master<-merge(df.master, df.GSA, by.x=c("Jaar", "Week"), by.y=c("Jaar", "Week"), all.x=T) # Merge the CV data with the GSA planning data based on the year and week number columns
 
# Create 15MIN masterfile
df.master15<-merge(df.CV15, df.bh, by="Datum", all.x=T) # Merge 15min Call volume data to bank holiday data (left join)
df.master15<-merge(df.master15, df.CV, by.x=c("Datum"), by.y=c("Datum"), all.x=T) # Merge the CV15 data with the daily CVG data (to get week number and year and daily forecasts)
df.master15<-merge(df.master15, df.GSA, by.x=c("Jaar", "Week"), by.y=c("Jaar", "Week"), all.x=T) # Merge the CV data with the GSA planning data based on the year and week number columns

# Create 15MIN.WG masterfile
df.masterWG<-cbind(df.AA.CV15[,c("Time","Datum", "Datetime","CallVolume.AA"),with = FALSE], df.AV.CV15[,c("CallVolume.AV"),with = FALSE], df.OV.CV15[,c("CallVolume.OV"),with = FALSE], df.SM.CV15[,c("CallVolume.SM"),with = FALSE])
df.masterWG<-merge(df.masterWG,df.bh, by="Datum", all.x=T)
df.masterWG<-merge(df.masterWG, df.CV[,c("Week", "Jaar", "Datum", "Maand")], by.x=c("Datum"), by.y=c("Datum"),all.x=T)
df.masterWG<-merge(df.masterWG, df.GSA, by.x=c("Jaar", "Week"), by.y=c("Jaar", "Week"), all.x=T)

# -------------MERGING VALIDATION/CHECK DAILY DATA-------------------------------

# # MERGE CHECK BETWEEN CV DATA AND BH DATA
# # extract unique keys per data source
# x_Keys  <- unique(df.CV$datum)
# y_Keys <- unique(df.bh$datum)
# 
# length(dplyr::intersect(x_Keys, y_Keys)) #number of matching keys between x and y data (note: some holidays fall in the weekend so are automatically excluded)
# length(dplyr::setdiff(x_Keys, y_Keys)) # number of x keys with no y keys match
# length(dplyr::setdiff(y_Keys, x_Keys)) # number of y keys with no x keys match
# 
# # MERGE CHECK BETWEEN CV DATA AND GSA DATA (note GSA data is weekly and CIC data is daily)
# # extract unique keys per data source
# table(df.GSA$Week, df.GSA$Jaar) # should have 1 value per week
# table(df.CV$Week, df.CV$Jaar) # should have 5 days per week
# x_Keys  <- unique(sort(apply(expand.grid(df.CV$Jaar, df.CV$Week), 1, paste, collapse = "", sep = "")))
# y_Keys<-unique(sort(apply(expand.grid(df.GSA$Jaar, df.GSA$Week), 1, paste, collapse = "", sep = "")))
# length(dplyr::intersect(x_Keys, y_Keys)) #number of matching keys between x and y data
# length(dplyr::setdiff(x_Keys, y_Keys)) # number of x keys with no y keys match
# length(dplyr::setdiff(y_Keys, x_Keys)) # number of y keys with no x keys match

# -------------MERGING VALIDATION/CHECK 15MIN DATA-------------------------------

# # MERGE CHECK BETWEEN CV DATA AND BH DATA
# # extract unique keys per data source
# x_Keys  <- unique(df.CV15$datum)
# y_Keys <- unique(df.bh$datum)
# 
# length(dplyr::intersect(x_Keys, y_Keys)) #number of matching keys between x and y data (note: some holidays fall in the weekend so are automatically excluded)
# length(dplyr::setdiff(x_Keys, y_Keys)) # number of x keys with no y keys match
# length(dplyr::setdiff(y_Keys, x_Keys)) # number of y keys with no x keys match
# 
# # MERGE CHECK BETWEEN CV 15 MIN DATA AND DAILY CV DATA 
# # extract unique keys per data source
# x_Keys  <- unique(df.CV15$datum)
# y_Keys <- unique(df.CV$datum)
# 
# length(dplyr::intersect(x_Keys, y_Keys)) #number of matching keys between x and y data (note: some holidays fall in the weekend so are automatically excluded)
# length(dplyr::setdiff(x_Keys, y_Keys)) # number of x keys with no y keys match
# length(dplyr::setdiff(y_Keys, x_Keys)) # number of y keys with no x keys match
#
# # MERGE CHECK BETWEEN CV15MIN DATA AND GSA DATA 
# # extract unique keys per data source
# table(df.master15$Week, df.master15$Jaar) # should have 96*5 values per week. Last week is incomplete
# table(df.GSA$Week, df.GSA$Jaar) # should have 1 value per week
# x_Keys  <- unique(sort(apply(expand.grid(df.master15$Jaar, df.CV$Week), 1, paste, collapse = "", sep = "")))
# y_Keys<-unique(sort(apply(expand.grid(df.GSA$Jaar, df.GSA$Week), 1, paste, collapse = "", sep = "")))
# length(dplyr::intersect(x_Keys, y_Keys)) #number of matching keys between x and y data
# length(dplyr::setdiff(x_Keys, y_Keys)) # number of x keys with no y keys match
# length(dplyr::setdiff(y_Keys, x_Keys)) # number of y keys with no x keys match


# ------------- MISSING VALUE HANDLING --------------------------------

# Replace the NA values in bankholiday flag column with a zero 
df.master<-df.master[is.na(get("FeestdagVlag")), FeestdagVlag := 0] 

# Replace the NA values in bankholiday flag column with a zero 
df.master15<-df.master15[is.na(get("FeestdagVlag")), FeestdagVlag := 0] 

# Replace the NA values in bankholiday flag column with a zero 
df.masterWG<-df.masterWG[is.na(get("FeestdagVlag")), FeestdagVlag := 0]

# ------------- SAVE DATA AS INTERIM FILES -----------------------

# Saving cache for later usage as an .rda file
save(df.master, file=file.path(paths$cache, "200_merged.data.rda"))

# Saving cache for later usage as an .rda file
save(df.master15, file=file.path(paths$cache, "210_merged.data.rda"))

# Saving cache for later usage as an .rda file
save(df.masterWG, file=file.path(paths$cache, "220_merged.data.rda"))
