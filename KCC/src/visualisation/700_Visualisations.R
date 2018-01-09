if(!exists("df.pos_extended")){
  load(file.path(paths$cache, "140_PoS_Enriched.rda"))
}

visualisations <- list()

visualisations$V001 <- ggplot(df.pos_extended, aes(x=Trx_Date)) +
  geom_bar() +
  ggtitle("Number of PoS lines per day")



visualisations$V002 <- ggplot(df.pos_extended, aes(x=Trx_Date)) +
  geom_bar() +
  facet_wrap(~shopid) + 
  ggtitle("Number of PoS lines per day / store")


visualisations$V003 <- df.pos_extended %>%
  ggplot(aes(x=Sales_Net_Base)) +
  geom_histogram(bins=75) +
  facet_wrap(~shopid) +
  ggtitle("Distributions of unit prices sold per store") +
  scale_y_continuous(breaks=c(0, 50000)) +
  scale_x_continuous(breaks=c(-500, 0, 500))

visualisations$V004 <- df.pos_extended %>%
  ggplot(aes(x=SKU_ID, y=Sales_Net_Base)) +
  geom_bar(stat = "sum")


visualisations$V005 <- df.pos_extended %>%
  filter(SKU != "FSHOPBAGV") %>%
  group_by(shopid, Trx_Date, SKU) %>%
  dplyr::summarize(units_per_store_day_sku = sum(Units)) %>%
  group_by(shopid, SKU) %>%
  arrange(Trx_Date) %>%
  ggplot(aes(x=units_per_store_day_sku)) +
  geom_histogram(bins = 200) +
  facet_wrap(~shopid)


visualisations$V006 <- df.pos_extended %>%
  group_by(shopid) %>%
  dplyr::summarize(s=sum(Sales_Net_Base)) %>%
  dplyr::left_join(df.store, by="shopid") %>%
  ggplot(aes(x=shopid, y=s)) +
  geom_bar(stat="identity", aes(fill=shop_brand), color="black")



visualisations$V007 <- df.pos_extended %>%
  group_by(shopid, Trx_Date) %>%
  dplyr::summarize(s=sum(Sales_Net_Base)) %>%
  group_by(shopid) %>%
  dplyr::summarize(avg_s=mean(s),
                   median_s=median(s)) %>%
  dplyr::left_join(df.store, by="shopid") %>%
  ggplot(aes(x=shopid, y=avg_s)) +
    geom_bar(stat="identity", aes(fill=shop_brand), color="black") +
    ggtitle("Average sales per day for each store") +
    ylab("Average daily sales") +
    xlab("Store") +
    scale_fill_discrete(name="Brand")

visualisations$V008 <- df.pos_extended %>% 
  filter(Merchandising_Concept == "Kids Boots") %>% 
  ggplot(aes(x=(Sales_Net_Base + Disc_Net_Base) / Units)) + 
    geom_histogram(bins=100, fill="skyblue", color="black") 

visualisations$V009 <- df.pos_extended %>%  
  ggplot(aes(x=(Sales_Net_Base + Disc_Net_Base) / Units)) + 
    geom_histogram(bins=100, fill="skyblue", color="black") + facet_wrap("Merchandising_Concept", scales="free")

visualisations$V010 <- df.pos_extended %>%
  group_by(shopid, shop_brand, pos.ReceiptHash) %>%
  dplyr::summarize(basket_lines=n()) %>%
  ggplot(aes(x=shopid, y=basket_lines)) + 
    geom_violin(aes(color=shop_brand))