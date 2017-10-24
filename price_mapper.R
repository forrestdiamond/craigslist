# guide: http://zevross.com/blog/2014/07/16/mapping-in-r-using-the-ggplot2-package/
# data source: http://www.mass.gov/anf/research-and-tech/it-serv-and-support/application-serv/office-of-geographic-information-massgis/datalayers/counties.html

package_list <- c("tidyverse", "rgdal")
lapply(package_list, library, character.only = TRUE)

data_loc <- "YOUR_MASTER_LOC_HERE"

cl_posts <- data_loc %>%
  read_csv() %>%
  mutate(pid = as.character(pid),
         price_num = as.numeric(cost))%>% 
  filter((!is.na(lon)) & (!is.na(lat)) & (!is.na(price_num)))

coordinates(cl_posts) <- ~lon+lat
proj4string(cl_posts) <- CRS("+proj=longlat +datum=NAD83")

counties <- readOGR("counties/COUNTIES_POLY.shp")

cl_posts <- spTransform(cl_posts, CRS(proj4string(counties)))

# check
# identical(proj4string(cl_posts),proj4string(counties))

cl_posts <- cl_posts %>% data.frame()

names(cl_posts)[names(cl_posts)=="lon"]<-"x"
names(cl_posts)[names(cl_posts)=="lat"]<-"y"

ggplot() +
  geom_polygon(data = counties, aes(x = long, y = lat, group = group), fill="grey40", 
               colour="grey90", alpha=1) +
  geom_point(data = cl_posts, aes(x, y, color = price_num, alpha=1, size=1)) +
   labs(x="", y="", title="Sublet Posts in Boston") +
   theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), 
         axis.ticks.x = element_blank(),axis.text.x = element_blank(),
         plot.title = element_text(lineheight=.8, face="bold", vjust=1)) +
  scale_colour_gradientn("Price per sublet ($)",
                         colours=c( "#f9f3c2","#660000")) +
  coord_equal(ratio = 1, 
              xlim = c(225000, 245000),
              ylim = c(890000, 910000))
