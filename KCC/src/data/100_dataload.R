#----------------------- DATA LOADING ---------------------

# Load all internal raw data
df.CV<-load_CV_data()
df.CV15<-load_CV15_data()
df.AA.CV15<-load_AA_CV15_data()
df.OV.CV15<-load_OV_CV15_data()
df.AV.CV15<-load_AV_CV15_data()
df.SM.CV15<-load_SM_CV15_data()
df.GSA<-load_GSA_planning()

# Load all external data
df.bh<-load_bankholidays()