library(ggplot2)
eBayTrain4 <- eBayTrain2

eBayTrain4$startprice_levels <- cut(eBayTrain4$startprice,breaks=25*(0:40))

salesstat <- aggregate(sold~productline+startprice_levels,data=eBayTrain4,FUN="mean")
                                    
ggplot(salesstat,aes(x=productline,y=startprice_levels))+geom_tile(aes(fill=sold))+theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(title="Sales stats Train Set")
unknown <- subset(eBayTrain4,productline=="Unknown")

nobidTrain <- subset(eBayTrain4,biddable==0)
bidTrain <- subset(eBayTrain4,biddable==1)
nobidTrain_1 <- aggregate(sold~productline+startprice_levels,data=nobidTrain,FUN="mean")
bidTrain_1 <- aggregate(sold~productline+startprice_levels,data=bidTrain,FUN="mean")

ggplot(bidTrain_1,aes(x=productline,y=startprice_levels))+geom_tile(aes(fill=sold))+theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(title="biddable Train Set")
ggplot(nobidTrain_1,aes(x=productline,y=startprice_levels))+geom_tile(aes(fill=sold))+theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(title="non biddable Train Set")

bid_sold <- subset(bidTrain,sold==1)
price_table <- tapply(bid_sold$startprice,bid_sold$productline,mean)
price_vol_table <- tapply(bid_sold$startprice,bid_sold$productline,sd)

x <- (eBayTrain4$startprice- price_table[as.numeric(eBayTrain4$productline)])/price_vol_table[as.numeric(eBayTrain4$productline)]

nobidTest <- subset(eBayTest,biddable==0)
bidTest <- subset(eBayTest,biddable==1)
