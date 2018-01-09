#----------------------- DATA PREPARATION GSA planning data---------------------

#------------------ FEATURE GENERATION ------------------------------
# extracting the year from the JaarWeek column and the week into 2 separate columns
df.GSA$Jaar<-as.integer(paste0("20",substr(df.GSA$Opdrachten_JaarWeek, 1, 2) ))
df.GSA$Week<-as.integer(substr(df.GSA$Opdrachten_JaarWeek, 3, 4) )

# ------------------ FEATURE SELECTION ------------------------------
# Remove unecessary variables (scoping of usecase)
df.GSA<-df.GSA[,c("Jaar", "Week", "Opdrachten_Totaal", "Aanbiedingen_Totaal"),with = FALSE]