# ------------------------------ DATA EXPLORATION -----------------------

# ----------------VISUALISATION -------------------
# Plot the CIC call volume data
ggplot(df.master, aes(x=datum, y=Aangeboden))+ geom_line() + ggtitle("Call Volume 2017")

#plotly
plot_ly(df.master, x = df.master$datum, y = df.master$Aangeboden, type = "bar") 

# Plot together with the forecasted values
ggplot(df.master, aes(datum, y = value, color = variable)) + geom_line(aes(y = Aangeboden, col = "Aangeboden")) +  geom_line(aes(y = FCST_Totaal, col = "FCAST_Totaal")) + ggtitle("Call Volume 2017 vs FCST Total")

# Plot the error
df.master$Error<-df.master$Aangeboden-df.master$FCST_Totaal
ggplot(df.master, aes(x=datum, y=Error))+ geom_line() + ggtitle("Error (Actuals - Fcst")


# Plot the GSA planning over 2017 (note this is weekly data)
ggplot(df.master, aes(x=datum, y=Opdrachten_Totaal))+ geom_line() + ggtitle("GSA Opdrachten Totaal")


#--------------CURRENT PERFORMANCE ----------------

mse(df.master$Error)
rmse(df.master$Error)
mae(df.master$Error)

#------------ TIMESERIES EXPLORATION --------------
# create timeseries object of call volume daily frequency
# CV.ts<-ts(df.master[, "Aangeboden"], start = c(2017, 1), frequency = 250)
# autoplot(CV.ts)

CV.ts.weeklyperiod<-ts(df.master[, "Aangeboden"], frequency = 5)
autoplot(CV.ts.weeklyperiod)
#ggseasonplot(CV.ts.weeklyperiod)

gglagplot(CV.)
ggAcf()
ccf()
Box.test() #As you learned in the video, white noise is a term that describes purely random data. You can conduct a Ljung-Box test using the function below to confirm the randomness of a series; a p-value greater than 0.05 suggests that the data are not significantly different from white noise.