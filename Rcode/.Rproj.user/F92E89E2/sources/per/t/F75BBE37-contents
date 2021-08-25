library(ggplot2)
library(reshape2)

theme_set(theme_bw(base_size = 18),legend.position="bottom")

d <- as.data.frame(F.mb)
d <- d %>% melt()
d$paesi <- paesi
tail(d)


names(d) <- c("Year","Cluster","Country")
head(d)

p <- d %>% arrange(Cluster) %>% ggplot(aes(x = Year, y =
                                           reorder(Country,Cluster), fill = Cluster))

pp <- p + geom_raster(interpolate = F) +
  scale_x_discrete(breaks=seq(1970, 2013, 4),expand = c(0, 1)) + scale_y_discrete(expand = c(0, 1)) +
  

  scale_fill_viridis_c(option = "C", direction = 1,alpha = 0.45) +
  guides(fill = guide_legend( title.position = "top", label.position = "bottom"))+


labs(x = "Year", y = "Country")+
  scale_x_discrete(breaks=seq(1970, 2013, 4))+
  theme(legend.position = "bottom",
        legend.title = element_text(size = 18)
  )+
  annotate(geom="text", x=c(12,14.5,17,21),
           y=c(31,25,19,9),size=7, label=c("Increasing Gap Cluster","Stagnating Gap Cluster", 
                                           "High-levels Gap Cluster", "Low-decreasing Gap Cluster"))
pp

ggsave("clust_1P.png",pp,width = 45, height = 35, units = "cm")


#########################
#                       #
#         MAP           #
#  (Needs improvement)  #
#                       #
#########################


dd <- d %>%  filter(Country%in%c("AUS","AUT","BEL","ITA","JPN","USA","CAN",
                                 
                                 "BGR","BLR","CHE","CZE",      
                                 "DEUTE","DEUTW","DNK","ESP","EST","FIN","FRACNP","HUN",     
                                 "IRL","ISL","ITA","JPN","LTU","LVA","NLD","NOR",      
                                 "POL","PRT","RUS","SVK","SWE","UKR","GBR_NP","USA"),Year==2000)

dd$Cluster[dd$Cluster==1] <- "Low-decreasing Gap"
dd$Cluster[dd$Cluster==2] <- "High-levels Gap"
dd$Cluster[dd$Cluster==3] <- "Stagnating Gap Gap"
dd$Cluster[dd$Cluster==4] <- "Increasing Gap Gap"

dd$Country[dd$Country%in%c("AUS","AUT","BEL","CAN","ITA","JPN","USA",
                           "BGR","BLR","CHE","CZE","DEUTE","DEUTW",
                           "DNK","ESP","EST","FIN","FRACNP","HUN",     
                           "IRL","ISL","LTU","LVA","NLD","NOR",
                           "POL","PRT","RUS","SVK","SWE","UKR",
                           "GBR_NP","USA")] <- 
  c("Australia","Austria","Belgium","Canada","Italy","Japan","USA",
    "Bulgaria","Belarus","Switzerland","Czech Republic","Germany","Germany",
    "Denmark","Spain","Estonia","Finland","France","Hungary",
    "Ireland","Iceland","Lithuania", "Latvia","Netherlands","Norway",
    "Poland","Portugal","Russia","Slovakia","Sweden","Ukraine", 
    "UK","USA" )

dd$Cluster <- as.factor(dd$Cluster)

WorldData <- map_data('world') %>% filter(region != ("Antarctica")) %>% 
  filter(region != ("South Africa"))%>% fortify


p <- ggplot() +
  geom_map(data = WorldData, map = WorldData,
           aes(x = long, y = lat, group = group, map_id=region),
           fill = "white", color="grey", size=0.7) + 
  
  geom_map(data = dd, map=WorldData,
           aes(fill=Cluster, map_id=Country),
           size=0.7)+
  coord_map("rectangular", lat0=0, xlim=c(-170,170), ylim=c(-60, 90)) 

p+ theme(legend.position="bottom")
