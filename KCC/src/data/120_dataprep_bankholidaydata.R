#----------------------- DATA PREPARATION bank holiday data---------------------

# SETTING TIME FORMATS
# 1. converting dates to recognised date formats
df.bh$Datum <-as.POSIXct(strptime(df.bh$Datum, format = "%d-%m-%y"))

