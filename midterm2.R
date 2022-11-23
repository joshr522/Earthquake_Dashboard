library(tidyverse)
library(ggthemes)
library(maptools)
library(ggtext)
library(lubridate)
library(patchwork)
library(rgdal)
library(broom)


earthquakes <- read_csv("earthquake_record_1965_2016.csv") %>%
  mutate(Date = as.Date(Date, "%m/%d/%Y")) %>%
  filter(Type == "Earthquake",
         Date >= "2000-01-01")

earthquakes2000 <- read_csv("earthquake_record_1965_2016.csv") %>%
  mutate(Date = as.Date(Date, "%m/%d/%Y")) %>%
  filter(Type == "Earthquake",
         Date >= "2000-01-01",
         Magnitude >= 7)

worldMapping <- map_data("world")


plates <- readOGR( 
  dsn= paste0(getwd(),"/tectonicplates-master/") , 
  layer="PB2002_plates",
  verbose=FALSE
)


plates_fortified <- tidy(plates)  %>%
  filter(!group %in% c(1.1, 6.1))

ggplot(plates_fortified) + 
  geom_path(aes(x=long, y=lat, group=group, color=group)) + 
  theme(legend.position = "none") + 
  scale_x_continuous(limits=c(-179.5, 180), breaks=c(-180, -90, 0, 90, 180))



earthquakesMonthYear <- earthquakes %>%
  group_by(Month = lubridate::floor_date(Date, "month")) %>%
  summarize(totalEarthquakes = n())


earthquakesMonths <- earthquakes %>%
  mutate(Month = factor(format(Date, "%m"), 
                        levels = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), 
                        labels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))) %>%
  group_by(Month) %>%
  summarize(totalEarthquakes = n(), 
            avgDepth = mean(Depth))

plotMap <- ggplot(worldMapping) + 
  geom_path(data=plates_fortified, aes(x=long, y=lat, group = group), color="gray40") + 
  geom_polygon(aes(x=long, y=lat, group=group), fill="gray70", color="gray90") +
  coord_map(projection = "mercator", xlim=c(-180, 180)) + 
  theme_map() + 
  geom_point(data=earthquakes2000, aes(x=Longitude, y=Latitude, size=Magnitude), color = "#FF6666", shape=1) +
  scale_size_continuous(limits=c(7.0, 9.1), breaks = c(7, 7.5, 8, 8.5, 9), range=c(2,9)) + 
  scale_x_continuous(limits=c(-179.9, 180)) +
  labs(title="Map of major earthquakes and tectonic plates") + 
  theme(legend.position = "right",
        plot.title = element_text(family="serif", size=16),
        plot.title.position = "plot",
        plot.subtitle = element_text(family="serif", size=13)
        
  )

plotTime <- ggplot(earthquakesMonthYear) + 
  geom_line(aes(x=Month, y=totalEarthquakes), color="#BE8ED1") + 
  scale_y_continuous(limits=c(0,250), minor_breaks = NULL) + 
  scale_x_date(breaks = c("2 years"), limits = c(as.Date("2000-01-01"), as.Date("2016-12-01")), date_labels="%Y") + 
  labs(x=NULL, y="Earthquakes") + 
  theme_minimal() +
  annotate(geom="text", x=as.Date("2013-12-01"), y=200, label="9.1 Magnitude Earthquake\nnear Japan in March 2011", family="serif") + 
  annotate(geom="text", x=as.Date("2005-12-01"), y=150, label="9.1 Magnitude Earthquake\nnear Sri Lanka in December 2004", family="serif") + 
  annotate(geom="segment", x=as.Date("2013-12-01"), y=215, xend=as.Date("2011-03-01"), yend=228, arrow=arrow(length=unit(0.3, "cm"))) + 
  annotate(geom="segment", x=as.Date("2005-12-01"), y=135, xend=as.Date("2004-12-01"), yend=112, arrow=arrow(length=unit(0.3, "cm")))
    

plotMonths <- ggplot(earthquakesMonths, aes(x=totalEarthquakes, y=fct_rev(Month))) +
  geom_col(fill="#43C09D") +
  coord_cartesian(expand=FALSE) +
  scale_x_continuous(limits=c(0, 1000), 
                     breaks=c(0, 400, 800), 
                     minor_breaks = NULL, 
                     position="top") +
  geom_text(aes(x=10, label=Month), hjust=0, color="white", fontface="bold") +
  geom_text(aes(label=totalEarthquakes, x = totalEarthquakes+5), position = position_dodge(width=0.9), hjust=0, fontface="bold") +
  theme_minimal() + 
  theme(
    panel.grid.major.y = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_text(family="serif", size = 16)
    ) + 
  labs(title="Earthquakes by month")
  


plotDepth <- ggplot(earthquakesMonths) + 
  geom_col(aes(x=Month, y=avgDepth), fill="#D1A42E") +
  theme_minimal() + 
  theme(
    panel.grid.major.x = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(family="serif", size=16),
    plot.title.position = "plot"
  ) + 
  coord_cartesian(expand=FALSE) + 
  scale_y_continuous(limits=c(0, 90), breaks=c(0, 20, 40, 60, 80)) + 
  scale_x_discrete(labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  annotate(geom="text", x=6.2, y=80, label="Summer months tend to have\ndeeper earthquakes", family="serif", color="gray20", size=3) +
  annotate(geom="text", x=3.35, y=65, label="Spring earthquakes\ntend to be \nmore shallow", family="serif", color="gray20", size=3) + 
  labs(title="Average depth by month")

design <- "
  AAABBB
  AAABBB
  CCDDDD
  CCDDDD
"


dashboard <- plotMap + plotDepth + plotMonths + plotTime + 
  plot_layout(design=design) + 
  plot_annotation(
    title = "Earthquakes around the world from 2000 to 2016",
    caption = "Source: National Earthquake Information Center",
    theme = theme(plot.title = element_text(size = 24, family="serif"),
                  plot.caption = element_text(size=14, family="Courier"))
    ) 

ggsave(filename="roach-josh_dashboard.png", plot=dashboard, height=8, width=12, dpi=600)

