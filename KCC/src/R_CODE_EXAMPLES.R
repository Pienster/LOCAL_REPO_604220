# EXAMPLE CODE



# -------------Executing from a Masterfile -------------------
OS <- .Platform$OS.type
US <- Sys.info()["user"]

if(OS=='unix'){
  PROJECT_HOME <- file.path("~", "Github", "PROJECT_TEMPLATE")
  Sys.setenv(RSTUDIO_PANDOC="/Applications/RStudio.app/Contents/MacOS/pandoc")
} else if(US=='Administrator' | US=="username"){
  PROJECT_HOME <- file.path("C:", "Github", "PROJECT_TEMPLATE")
  Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc")
} else {
  PROJECT_HOME <- file.path("C:", "Users", "Penelope Rammos", "Documents", "Github", "PROJECT_TEMPLATE")
  Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc")
}

source(file.path(PROJECT_HOME, "src", "util.R"))

# -------------EXECUTION FROM A MARKDOWN FILE ------------------------

# include this a initiating chunk {r setup, include=FALSE}

knitr::opts_chunk$set(echo = FALSE, warning=FALSE, message = FALSE)
options(scipen = 999)
OS<-.Platform$OS.type
US<-Sys.info()["user"]

if(OS=='unix'){
  PROJECT_HOME <- file.path("~", "Github", "VFC")
} else if(US=='Administrator'){
  PROJECT_HOME <- file.path("C:", "Github", "VFC")
} else {
  PROJECT_HOME <- file.path("C:", "Users", "My Documents", "Github", "PROJECT NAME")
}

source(file.path(PROJECT_HOME, "src", "util.R")) # does library & helper function load


# -------------MERGING EXAMPLE df.x & df.y----------------------------
df.merged <- left_join(df.x,
                             df.y,
                             by=c("osward" = "Dem.Ward.Code"))

df.merged <- dplyr::left_join(df.x,
                                   df.y,
                                   by=c("Postcode" = "pcd"))

df.merged <- plyr::join(df.x,
                     df.y[,c("shopid", "GEO_ID", "Postcode","shop_brand", "Latitude", "Longitude", "currencyid")],
                     by="shopid",
                     type="left")

# -------------MERGING VALIDATION/CHECK-------------------------------

# extract unique keys per data source
x_Keys  <- unique(df.x$Key)
y_Keys <- unique(df.y$Key)

length(dplyr::intersect(x_Keys, y_Keys)) #number of matching keys between x and y data
length(dplyr::setdiff(x_Keys, y_Keys)) # number of x keys with no y keys match
length(dplyr::setdiff(y_Keys, x_Keys)) # number of y keys with no x keys match

# -------------DATA CLEANSING FUNCTION EXAMPLES-----------------------

# trim leading and trailing spaces in a category & remove spaces entirely
df.hrch$SKU <- trimws(df.hrch$SKU)
df.hrch$SKU <- gsub(" ", "", df.hrch$SKU, fixed = TRUE)

# removing empty keys
df.pos <- df.pos[!nchar(df.pos$SKU)<3,] # Remove empty SKU's (SKU names with *, or SKUs with less than 3 characters)

# Filtering, i.e. only include TBL, VANS and TNF in this exploration
df.pos <- df.pos %>%
  filter(shop_brand == "TIMBERLAND"| shop_brand == "TNF"| shop_brand == "VANS")

# removing specific columns
df.pos[, c("ProdGrp1", "ProdGrp2")] <- list(NULL)

# Aggregating by hour and summing the counts per footfall device 
df.ff$fulldate <- as.POSIXct(strptime(df.ff$fulldate, format = "%Y-%m-%d %H:%M:%S"))
df.ff <- df.ff[order(df.ff$shopid, df.ff$fulldate),]
df.ff$datehour <- cut(df.ff$fulldate, breaks="hour")
df.ff$datehour <- as.POSIXct(strptime(df.ff$datehour, format = "%Y-%m-%d %H:%M:%S"))
df.ff <- df.ff %>%
  group_by(shopid, datehour) %>%
  summarise_at(vars(countout), sum)

# -------------------------EXPLORATION EXAMPLES ---------------------------------

# list top 20 skus and plot
Top20SKU<- df.tnf %>% group_by(SKU, ProdGrp2) %>% summarise(Revenue=sum(Sales_Net_Base)) %>% ungroup() %>% arrange(desc(Revenue)) %>% top_n(20, wt=Revenue) %>% mutate(SKU = factor(SKU, SKU), Total_Revenue=sum(Revenue, na.rm=TRUE), Revenue_Perc = Revenue / Total_Revenue) 
print(Top20SK)

Top20SKU%>% ggplot(aes(x=SKU, y=Revenue)) + geom_bar(stat="identity") +theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ggtitle("Top 20 SKU's by revenue ")

# a gains chart (SKU style rank by revenue vs cumulative % revenue) to indicate the number of SKU styles covering a specific % of the revenue 

#### Top percentiles based on revenue
Top_Perc_SKU_TBL <- df.tbl %>% 
  group_by(Style, ProdGrp2) %>% 
  summarise(Revenue=sum(Sales_Net_Base)) %>% 
  ungroup() %>%
  arrange(desc(Revenue)) %>%
  mutate(Total_Revenue=sum(Revenue, na.rm=TRUE), 
         Revenue_Perc = Revenue / Total_Revenue, 
         Revenue_Perc_Cumul = cumsum(Revenue_Perc)) 


# Displaying table
Top_Perc_SKU_TBL$Rank<-as.integer(rownames(Top_Perc_SKU_TBL))
head(Top_Perc_SKU_TBL,30)

#plot
Top_Perc_SKU_TBL%>% ggplot(aes(x=Rank, y=Revenue_Perc_Cumul)) + geom_line(colour = "chartreuse3",size=1) + ggtitle("Timberland Gains chart: SKU style revenue rank vs cumulative revenue %")+ geom_hline(yintercept = 0.80, color="black") + geom_hline(yintercept = 0.40, color="dimgrey")+ geom_text(aes(0,0.80,label = "80%", vjust = -0.5)) + geom_text(aes(0,0.40,label = "40%", vjust = -0.5),colour="dimgrey")+ geom_text(aes(2000,0, label="17.4% SKU styles cover 80% revenue", vjust = -0.5), colour="black")

# taking all skus that cover 40% of the revenue
Top_40Perc_SKU_TBL<-Top_Perc_SKU_TBL[1:which.max(Top_Perc_SKU_TBL$Revenue_Perc_Cumul > 0.40), ]

# -------------------------CREATING REFERERENCE TABLES---------------------------

SKUREF <- df.pos_extended %>%
  group_by(SKU) %>%
  dplyr::summarize(SKU.n_lines         = n(),           # total lines in provided data time range 
                   SKU.min_date        = min(Trx_Date), # first date of sale
                   SKU.max_date        = max(Trx_Date), # last date of sale
                   SKU.date_range      = max(Trx_Date) - min(Trx_Date),
                   SKU.uniqDaysTotal   = n_distinct(Trx_Date),  # Number of days on which this SKU has been sold
                   
                   SKU.pricemax        = max((Sales_Net_Base + Disc_Net_Base) / Units), 
                   SKU.pricemin        = min((Sales_Net_Base + Disc_Net_Base) / Units),
                   SKU.pricemean       = mean((Sales_Net_Base + Disc_Net_Base) / Units), 
                   SKU.pricemedian     = median((Sales_Net_Base + Disc_Net_Base) / Units),
                   
                   SKU.freq_sold       = sum(SaleRetFlag=="S"),
                   SKU.freq_returns    = sum(SaleRetFlag=="R"),
                   SKU.ret_to_sold     = SKU.freq_returns / SKU.freq_sold,
                   SKU.freq_discounts  = sum(Disc_Net_Base > 0),
                   SKU.totalrevenue    = sum(Sales_Net_Base),
                   
                   SKU.freq_sold2014   = sum(SaleRetFlag=="S" & lubridate::year(Trx_Date)==2014),
                   SKU.freq_sold2015   = sum(SaleRetFlag=="S" & lubridate::year(Trx_Date)==2015),
                   SKU.freq_sold2016   = sum(SaleRetFlag=="S" & lubridate::year(Trx_Date)==2016),
                   SKU.tot_rev2014     = sum(Sales_Net_Base[lubridate::year(Trx_Date)==2014]),
                   SKU.tot_rev2015     = sum(Sales_Net_Base[lubridate::year(Trx_Date)==2015]),
                   SKU.tot_rev2016     = sum(Sales_Net_Base[lubridate::year(Trx_Date)==2016]))
                   
                   
                   # Computing for the percentage of SKU units sold /total number of sku's sold per store          
                   SKUREF2 <- df.pos_extended %>%   
                     group_by(SKU, shopid) %>%
                     dplyr::summarize(SKU.UnitsSold= sum(Units, na.rm=TRUE)) %>%
                     group_by(shopid) %>%
                     mutate(shopidprefix=paste("SKU.PercUnits", shopid, sep=""),SKU.PercUnits= 100*(SKU.UnitsSold/sum(SKU.UnitsSold)))%>%
                     spread(shopidprefix, SKU.PercUnits,fill=0) %>%
                     ungroup() %>%
                     dplyr::select(-shopid)%>%
                     group_by(SKU) %>%
                     dplyr::summarise_each(funs(sum))
                   
                   # Computing % revenue of sku per store/total revenue of that store
                   SKUREF3 <- df.pos_extended %>%
                     group_by(SKU, shopid) %>%  
                     dplyr::summarize(SKU.rev= sum(Sales_Net_Base)) %>%
                     group_by(shopid) %>%
                     mutate(shopidprefix=paste("SKU.PercRev", shopid, sep=""),SKU.PercRev= 100*(SKU.rev/sum(SKU.rev)))%>%
                     spread(shopidprefix, SKU.PercRev,fill=0) %>%
                     ungroup() %>%
                     dplyr::select(-shopid) %>%
                     group_by(SKU) %>%
                     dplyr::summarise_each(funs(sum))
                   
                   # Computing % units sku per brand/total units of brand sold
                   SKUREF4 <- df.pos_extended %>%  
                     group_by(SKU, shop_brand) %>%  
                     dplyr::summarize(SKU.UnitsSold= n()) %>%
                     group_by(shop_brand) %>%
                     mutate(brandprefix=paste("SKU.PercBrand", shop_brand, sep=""), SKU.PercUnits = 100*(SKU.UnitsSold/sum(SKU.UnitsSold)))%>%
                     spread(brandprefix, SKU.PercUnits,fill=0) %>%
                     ungroup() %>%
                     dplyr::select(-shop_brand) %>%
                     group_by(SKU) %>%
                     dplyr::summarise_each(funs(sum))
                   
                   # Computing the nr of unique days a SKU is sold over the entire history
                   SKUREF5 <- df.pos_extended %>%   
                     group_by(SKU, Year) %>%  
                     dplyr::summarize(SKU.uniqDaysTotal = n_distinct(Trx_Date)) %>%
                     group_by(Year) %>%
                     mutate(Yearprefix=paste("SKU.uniqDays", Year, sep="")) %>%
                     spread(Yearprefix, SKU.uniqDaysTotal,fill=0) %>%
                     ungroup() %>%
                     dplyr::select(-Year) %>%
                     group_by(SKU) %>%               
                     dplyr::summarise_each(funs(sum))
                   
                   
                   # Join all SKUREF tables by sku id
                   SKUREFERENCE <- left_join(SKUREF, SKUREF2, by="SKU", copy = FALSE) %>%
                     left_join(.,SKUREF3, by="SKU", type:"left") %>%
                     left_join(.,SKUREF4, by="SKU", type:"left") %>%
                     left_join(.,SKUREF5, by="SKU", type:"left")                   



# -------------DATA OBJECT SAVING IN CACHE----------------------------

# Saving cache for later usage as an .rda file
save(df.hrch, file=file.path(paths$cache, "101_prepped.rda"))


# ------------CONDITIONAL LOADING DEPENDING ON PRESENCE RDA CACHE------

if (!exists("df.pos")) {
  if (file.exists(file.path(paths$cache, "101_pos_prepped.rda"))) {
    load(file.path(paths$cache, "101_pos_prepped.rda"))
  } else {
    source(file.path(PROJECT_HOME, "src", "data", "101_Data_prep_posdata.R"))
  }
}
                   
                   
                   if(file.exists("rfModel.Rda")){
                     load("rfModel.Rda")} else {
                       modFit <- train(class~.,method="rf",data=train)
                     }
# ------------REMOVING RDA OBJECTS FROM ENVIRONMENT--------------------
rm(df.storemaster)
rm(df.pcward)


# ----------- CORRELATIONS ------------------------------------------

# Computing correlation matrix
# calculate correlation matrix CM for Timberland Footwear
CM.FW <- cor(Prod1byShop[, grep('Dem.|prd1perc.Timberland_Footwear', colnames(Prod1byShop))], use = "pairwise.complete.obs")
CM.FW[is.na(CM.FW)]<-0
TBL_FW<-CM.FW[nrow(CM.FW),]
TBL_FW_top10<-as.data.frame(head(TBL_FW[order(abs(TBL_FW), decreasing=TRUE)],11))
colnames(TBL_FW_top10)<-"Correlation"

DT::datatable(TBL_FWu_top10,caption = 'Top 10 correlations TBL ProdGrp1 Footwear % contribution to store volume with demographics', options = list(scrollX = TRUE),colnames = c('Demographics' = 1)) %>%formatRound(columns= colnames(TBL_FWu_top10[,sapply(TBL_FWu_top10,is.numeric)]), digits=2)
DT::datatable(TBL_TAu_top10,caption = 'Top 10 correlations TBL ProdGrp1 Apparel % contribution to store volume with demographics', options = list(scrollX = TRUE),colnames = c('Demographics' = 1)) %>%formatRound(columns= colnames(TBL_TAu_top10[,sapply(TBL_TAu_top10,is.numeric)]), digits=2)
DT::datatable(TBL_TACu_top10,caption = 'Top 10 correlations TBL ProdGrp1 Accessories % contribution to store volume with demographics', options = list(scrollX = TRUE),colnames = c('Demographics' = 1))%>%formatRound(columns= colnames(TBL_TACu_top10[,sapply(TBL_TACu_top10,is.numeric)]), digits=2)


ggplot(Prod1byShop, aes(x=Dem.Age_16_64, y=`prd1sqm.Timberland_Footwear`, colour=shopid))+geom_point(size=3) + xlab("Age_16_64")+ylab("revenue per sqm for TBL Footwear ") +ggtitle("TBL Footwear revenue/sqm vs Age_16_64")

# -------------------------ADDING PREFIXES OR SUFFIXES --------------------

# Extracting the revenue per sqm (shop revenue/sqm) for the years 2014,2015, 2016 separately
df3<-df2 %>% group_by(shopid, Year, ProdGrp2) %>% dplyr::summarize(year_sales = sum(Sales_Net_Base), sqm=mean(sqm_net)) %>% ungroup() %>% complete(shopid, ProdGrp2, Year) %>% dplyr::mutate(shop.pg2_yr_sqm = year_sales/sqm, ProdGrp2prefix=paste( "rev.sqm", Year, ProdGrp2, sep=".")) %>% spread(ProdGrp2prefix, shop.pg2_yr_sqm ,fill=0) %>%
  group_by(shopid)%>% dplyr::summarise_at(vars(starts_with("rev.sqm")), max)

n <- names(df3) 
n <- n[grep("rev.sqm.", n)]
n <- unique(gsub(".+\\.", "", n))

loopdieloop <- function(x) {
  years <- c(2014, 2015, 2016)
  vars <- paste0("rev.sqm.", years, ".", x)
  weighted_total <- 0.2 * df3[,vars[1]] + 0.3 * df3[,vars[2]] + 0.5 * df3[,vars[3]]
  return(weighted_total)
}

TW<-as.data.frame(sapply(n,loopdieloop))
TWnames <- gsub(".+\\.", "", colnames(TW))
colnames(TW) <- paste("TW", TWnames, sep = ".")
TW$TW.Undefined<-NULL
df3<-cbind(df3,TW) 

# -----------------------PRINCIPAL COMPONENT ANALYSIS -------------------

# Using scaling inside pr comp function 
Dem.Var<-df.dem[,c(3:38)]
colnames(Dem.Var) <- paste("Dem", colnames(Dem.Var), sep = ".")
dem.PCA<-prcomp(na.omit(Dem.Var), scale = TRUE, center = TRUE)
summary(dem.PCA)
PCAloadings<-as.data.frame(dem.PCA$rotation)
#PCAloadings[,1:4]

# Screeplot
screeplot(dem.PCA,  type = "lines")

# Extract first 4 components
Comp1<-PCAloadings[,"PC1", drop=FALSE]
Comp2<-PCAloadings[,"PC2", drop=FALSE]
Comp3<-PCAloadings[,"PC3", drop=FALSE]
Comp4<-PCAloadings[,"PC4", drop=FALSE]

# Order by 
Comp1<-Comp1[order(abs(Comp1$PC1), decreasing=TRUE), ,drop=FALSE]
Comp2<-Comp2[order(abs(Comp2$PC2), decreasing=TRUE), ,drop=FALSE]
Comp3<-Comp3[order(abs(Comp3$PC3), decreasing=TRUE), ,drop=FALSE]
Comp4<-Comp4[order(abs(Comp4$PC4), decreasing=TRUE), ,drop=FALSE]

print(Comp1)
print(Comp2)
print(Comp3)
print(Comp4)

# --------------------- GRAPHICAL OUTPUT ----------------------------------


# Plotting total sales shop revenue/sqm store per shop vs shop size for 2016
df.sqm_store_2016<-df1 %>% filter(Year == 2016) %>%
  dplyr::group_by(shopid) %>%
  dplyr::summarise(shop.base_total = sum(Sales_Net_Base), 
                   sqm=mean(sqm_net)) %>%
  ggplot(aes(x=sqm, y=shop.base_total/sqm, colour=shopid)) +
  geom_point(size=3) +
  xlab("Shop net size in square metres") +
  ylab("2016 total shop sales per square metre") +
  ggtitle("2016 Shop sales revenue per square metre vs Shop size")+ ylim(0,25000) + xlim(0,300)
print(df.sqm_store_2016)

# Plotting TBL SKUs highest correlations PC1 (Stability plots)
ii<-ggplot(ZZ1, aes(y=abs(mediancorr), x=stability))+geom_point(aes(size = Revenue_Perc)) + ylab("Absolute Median correlation")+xlab("Stability") +ggtitle("PC1 Median correlation vs Stability") +xlim(0,2) +ylim(0,1)+geom_rect(xmin=1.75, xmax=2, ymin=0.5, ymax=1, fill="green", alpha=0.008) + geom_rect(xmin=-Inf, xmax=1.75, ymin=-Inf, ymax=1, fill="red", alpha=0.008)+geom_rect(xmin=1.75, xmax=2, ymin=-Inf, ymax=0.5, fill="red", alpha=0.008)+ geom_label_repel(data = filter(ZZ1,abs(mediancorr)>=0.4 & stability>=1.5) ,aes(label=Row.names, x=stability, y=abs(mediancorr)))
jj<-ggplot(ZZ2, aes(y=abs(mediancorr), x=stability))+geom_point(aes(size = Revenue_Perc)) + ylab(" Absolute Median correlation")+xlab("Stability") +ggtitle("PC2 Median correlation vs Stability") +xlim(0,2) +ylim(0,1)+geom_rect(xmin=1.75, xmax=2, ymin=0.5, ymax=1, fill="green", alpha=0.008) + geom_rect(xmin=-Inf, xmax=1.75, ymin=-Inf, ymax=1, fill="red", alpha=0.008)+geom_rect(xmin=1.75, xmax=2, ymin=-Inf, ymax=0.5, fill="red", alpha=0.008)+geom_label_repel(data = filter(ZZ2,abs(mediancorr)>=0.4 & stability>=1.5) ,aes(label=Row.names, x=stability, y=abs(mediancorr)))
kk<-ggplot(ZZ3, aes(y=abs(mediancorr), x=stability))+geom_point(aes(size = Revenue_Perc)) + ylab(" Absolute Median correlation")+xlab("Stability") +ggtitle("PC3 Median correlation vs Stability")+xlim(0,2)+ylim(0,1)+geom_rect(xmin=1.75, xmax=2, ymin=0.5, ymax=1, fill="green", alpha=0.008) + geom_rect(xmin=-Inf, xmax=1.75, ymin=-Inf, ymax=1, fill="red", alpha=0.008)+geom_rect(xmin=1.75, xmax=2, ymin=-Inf, ymax=0.5, fill="red", alpha=0.008)+ geom_label_repel(data = filter(ZZ3,abs(mediancorr)>=0.4 & stability>=1.5) ,aes(label=Row.names, x=stability, y=abs(mediancorr)))
ll<-ggplot(ZZ4, aes(y=abs(mediancorr), x=stability))+geom_point(aes(size = Revenue_Perc)) + ylab(" Absolute Median correlation")+xlab("Stability") +ggtitle("PC4 Median correlation vs Stability")+xlim(0,2)+ylim(0,1)+geom_rect(xmin=1.75, xmax=2, ymin=0.5, ymax=1, fill="green", alpha=0.008) + geom_rect(xmin=-Inf, xmax=1.75, ymin=-Inf, ymax=1, fill="red", alpha=0.008)+geom_rect(xmin=1.75, xmax=2, ymin=-Inf, ymax=0.5, fill="red", alpha=0.008)+ geom_label_repel(data = filter(ZZ4,abs(mediancorr)>=0.4 & stability>=1.5) ,aes(label=Row.names, x=stability, y=abs(mediancorr)))

grid.arrange(ii, jj, kk, ll, nrow=2, ncol=2, top = "TBL Prodgrp2 rev/sqm with principal components correlation stability ")

# plottingweather
df.weather %>%
  filter(date(DateHrLwt)=="2015-01-02") %>%
  ggplot(aes(x=hour(DateHrLwt), y=weather.CloudCoveragePercent)) + 
  geom_line(color="#FF9999", size=0.8)

# Plotting in a loop 
CM.ST<- CM.ST[which(abs(CM.ST$Correlation) >= 0.40), ]
CM.ST<-CM.ST %>% top_n(ifelse(nrow(CM.ST)>24,24,nrow(CM.ST)),wt=abs(Correlation))

plots<-list()
for(i in 1:nrow(CM.ST)){
  plots[[i]]<- ggplot(df.dailynums, aes_string(y=paste(CM.ST[i,1]), x=paste(CM.ST[i,2]))) + 
    geom_point() + 
    geom_smooth() + 
    ggtitle(paste(CM.ST[i,1],"vs",CM.ST[i,2] , sep=' '))
}

do.call(grid.arrange, list(grobs=plots, top="Top TBL style correlations with weather variables"))

# -------------------------- INETRACTIVE PROMPT ---------------------------------------
fun <- function(){
  x <- readline("What is the value of x?")  
  y <- readline("What is the value of y?")
  t <- readline("What are the T values?")
  v <- readline("What are the V values?")
  
  x <- as.numeric(unlist(strsplit(x, ",")))
  y <- as.numeric(unlist(strsplit(y, ",")))
  t <- as.numeric(unlist(strsplit(t, ",")))
  v <- as.numeric(unlist(strsplit(v, ",")))
  
  out1 <- x + y
  out2 <- t + v
  
  return(list(out1, out2))
}

if(interactive()) fun()

# ------------------------- Recursive partitioning tree model ------------------------------------
df.splits_ss <- data.frame(stringsAsFactors = FALSE)

for (i in names(df.sales)){
  for (j in names(df.weathers)){
    r <- rpart(unlist(df.sales[,i]) ~ unlist(df.weathers[,j]), maxdepth=1)
    if (!is.null(nrow(r$splits))) {
      leaves <- r$frame[r$frame$var == "<leaf>",]
      df.splits_ss <- rbind(df.splits_ss, 
                            data.frame(target=as.character(i),
                                       predictor=j,
                                       n_split1=leaves$n[1],
                                       n_split2=leaves$n[2],
                                       y_split1=leaves$yval[1],
                                       y_split2=leaves$yval[2],
                                       improvement=r$splits[, "improve"], 
                                       split_point=r$splits[, "index"]))
    } else {
      df.splits_ss <- rbind(df.splits_ss, 
                            data.frame(target=as.character(i),
                                       predictor=j, 
                                       n_split1=NA,
                                       n_split2=NA,
                                       y_split1=NA,
                                       y_split2=NA,
                                       improvement=NA, 
                                       split_point=NA))
    }
  }
}

df.splits_ss <- df.splits_ss %>% 
  rowwise() %>% 
  mutate(split_ratio = max(n_split1, n_split2) / (n_split1+n_split2), 
         yval_ratio = max(y_split1, y_split2) / (min(y_split1, y_split2)), 
         yval_percincrease = (yval_ratio - 1) * 100)

df.splits_ss$target <- gsub("Style.", "", df.splits_ss$target)

DT::datatable(head(df.splits_ss, 20),options = list(scrollX = TRUE))%>%formatRound(columns= c("y_split1", "y_split2","improvement","split_point","split_ratio", "yval_ratio", "yval_percincrease"), digits=2)

# ----------------------- Dummy varible
# Create dummy variable for weekends (1=weekend, 0=weekday)
df.CIC$IsWeekend[df.CIC$Dag=="Sat"|df.CIC$Dag=="Sun"]<-1
df.CIC$IsWeekend<-ifelse(df.CIC$Dag=="Sat"|df.CIC$Dag=="Sun", 1, 0)
df.CIC[,Test3:=ifelse(df.CIC$Dag=="Sat",1,0)]

## -----------------------Remove weekends
# Remove weekends --> no we add dummy variables for the weekend and bankholidays
#df.CIC<-df.CIC[!grepl(paste(c("Sat","Sun"), collapse="|"), Dag)]

#Replace blank cells with NA
df.CIC[df.CIC==""]<-NA

# interactive plotting
ddd<-ggplot(df.master, aes(x=datum, y=Opdrachten_Totaal))+ geom_line() + ggtitle("GSA Opdrachten Totaal (wekelijks)")
ggplotly(ddd)

devtools::install_github('hadley/ggplot2')



