# Input load. Please do not change #
`dataset` = read.csv('C:/Users/cguio/REditorWrapper_27da1643-4d0c-4530-9a92-2e588098bdbb/input_df_d94056a6-0b49-4b46-86a0-1a364a11e953.csv', check.names = FALSE, encoding = "UTF-8", blank.lines.skip = FALSE);
# Original Script. Please update your script content here and once completed copy below section back to the original editing window #


library(grid)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(extrafont)

font_import()
loadfonts(device = "win", quiet = TRUE)

world_data <- ggplot2::map_data('world')%>%
  rename(country = region) %>%
  select(-subregion) %>%
  filter(country %in% `dataset`$country)

world_labels <- world_data %>%
  group_by(country) %>%
  summarise(long = 
               case_when(country == "Ethiopia" ~ mean(long),
                    country == "Senegal" ~ mean(long),
                    country == "Mali" ~ mean(long) - mean(long)/1.5,
                    country == "Niger" ~ mean(long) + mean(long)/1.5,
                    country == "Honduras" ~ mean(long) - mean(long)/45,
                    country == "Guatemala" ~ mean(long) + mean(long)/45,
                    country == "Colombia" ~ mean(long)), 
            lat = 
              case_when(country == "Ethiopia" ~ mean(lat),
                        country == "Senegal" ~ mean(lat) - mean(lat)/10,
                        country == "Mali" ~ mean(lat) + mean(lat)/5,
                        country == "Niger" ~ mean(lat) + mean(lat)/10,
                        country == "Honduras" ~ mean(lat) - mean(lat)/20,
                        country == "Guatemala" ~ mean(lat) + mean(lat)/20,
                        country == "Colombia" ~ mean(lat) + mean(lat)/2.5))


g <- ggplot(data = world_data,
       aes(x = long, 
           y = lat)) +
  geom_polygon(data = world_data,
               aes(group = group),
               color = "#faf9f5", 
               fill = "#ECEBE7", 
               size = 1) +
  geom_text(data = world_labels, 
            aes(label = country),
            color = "#B8B7B3",
            size = 10,
            face = "bold")+
  geom_point(data = `dataset`,
             aes(x = longitude_site, 
                 y = latitude_site, 
                 color = site,
                 size = n),
             shape = 19,
             alpha = 0.8)+
    scale_size_continuous(range = c(7,10))+
    scale_color_hue(l = 70, c = 40, h = c(60, 200))+
    scale_fill_hue(l = 70, c = 40, h = c(60, 200))+
    geom_label_repel(data = `dataset`,
                   aes(label = 
                         if(category == "Years"){
                           paste0("Years surveyed:\n", details)
                           } 
                       else if(category == "Communities"){
                         paste(n, "communities surveyed")
                         }
                       else if(category == "Practices"){
                         paste("Practices assessed:\n",details)
                         }
                       else if(category == "Households"){
                         paste(n,"households surveyed.\n",details)
                       }
                       else if(category == "Farmers"){
                         paste(n,"farmers surveyed.\n",details)
                       }
                       else {paste0(n, "% of households experienced shocks.\n From them:\n",details)},
                       x = longitude_site, 
                       y = latitude_site,
                       fill = site),
                   show_guide=F,
                   size = 5.5,
                   family = "Calibri Light",
                   box.padding = 2,
                   force = 2,
                   point.padding = 2,
                   #nudge_y = 1.5,
                   nudge_x = 0.35,
                   #color = "#5D5C58",
                   max.overlaps = Inf,
                   direction = "both",
                   xlim = c(-Inf, Inf),
                   alpha = 0.65)+
  guides(size = "none",
        color = guide_legend(override.aes = list(size = 8)))+
  labs(color = "Sites")+
  theme_minimal(base_family = "Calibri")+
  theme(panel.grid = element_blank(),
        plot.background = element_rect(fill = "#faf9f5", 
                                       color = "#faf9f5"),
        legend.text = element_text(size = 18, 
                                    color = "#5D5C58"),
        legend.title = element_text(size = 18,
                                    color = "#B8B7B3",
                                    face = "bold"),
        legend.key.size = unit(3, 'lines'),
        legend.justification = "top",
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.margin=unit(c(0,10,0,45), "mm"))+
    coord_quickmap(clip = "off")

gt <- ggplotGrob(g)
grid.newpage()
# Draw a rectangle with desired fill and color
grid.draw(rectGrob(gp = gpar(fill = "#faf9f5", col ="#faf9f5" )))
grid.draw(gt)
