# Input load. Please do not change #
`dataset` = read.csv('C:/Users/cguio/REditorWrapper_dd2630d9-76b3-4cc7-9495-e0989688dd9c/input_df_8524292d-3e19-47df-a863-25fb0cda4540.csv', check.names = FALSE, encoding = "UTF-8", blank.lines.skip = FALSE);
# Original Script. Please update your script content here and once completed copy below section back to the original editing window #

# Pegue o escriba aquí el código de script:
library(ggplot2)
library(ggrepel)
library(tidyverse)
library(extrafont)

font_import()
loadfonts(device = "win", quiet = TRUE)

sankey_dat <- `dataset` %>% 
  mutate(adopted_cases = factor(adopted_cases, levels = c("Implemented",
                                                 "Stopped implementing",
                                                 "Know about it",
                                                 "Heard about it",
                                                 "Never heard about it",
                                                 "Accessed and used it",
                                                 "Accessed but didn't use it")))%>%
  group_by(year, site, practice_type, subtype, gender, n_site) %>%
  filter(n != 0)%>%
  mutate(gender_n = paste(substr(gender,1,1), paste0("(",n_site,")"), sep = "\n"))%>%
  arrange(year, site, practice_type, subtype, gender, desc(adopted_cases)) %>%
  mutate(pos = cumsum(n) - (0.5 * n)) %>%
  ungroup() %>%
  group_by(site) %>%
  mutate(x = case_when(any(str_detect(gender, "Female"))& gender == "Female"~ 0.6,
                       any(str_detect(gender, "Female"))& gender == "Male"~ 1.6,
                       any(!str_detect(gender, "Female"))& gender == "Male"~ 0.6),
         xend = case_when(any(str_detect(gender, "Female"))& gender == "Female"~ 1.4,
                       any(str_detect(gender, "Female"))& gender == "Male"~ 2.4,
                       any(!str_detect(gender, "Female"))& gender == "Male"~ 1.4)) 


  ggplot(sankey_dat,
         aes(fill = adopted_cases, y = n, x = gender_n)) + 
  geom_bar(position="stack", stat="identity", alpha = 0.8)+
  geom_label_repel(data = sankey_dat,
                aes(label = paste(round(percentage,0),"%"), 
                x = gender_n, 
                y = pos,
                color = adopted_cases), 
            #color = "#5D5C58",
            show.legend=F,
            family = "Calibri",
            size = 5,
            alpha = 0.8,
            force = 0.1,
            box.padding = 0.1,
            direction = "x",
            max.overlaps = Inf,
            fill = "white")+
  geom_segment(data = sankey_dat, 
               aes(x = x, 
                   xend = xend, 
                   y = n_site, 
                   yend = n_site), 
               linetype=2, color = "#B8B7B3")+
  facet_grid(year ~ site, scales = "free_x", space = "free_x") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("#9ACD32", "#C67171", "#b5b0b0","#CDC9C9", "#EEE9E9"))+
  scale_color_manual(values = c("#9ACD32", "#C67171",  "#b5b0b0","#CDC9C9", "#EEE9E9"))+
  #scale_fill_hue(l = 70, c = 45, h = c(95,360))+
  #scale_color_hue(l = 70, c = 45, h = c(95,360))+  
  guides(fill = guide_legend(override.aes = list(size = 10),
                            title.position = "bottom",
                            title = ifelse(sankey_dat$option == "Practices", "* Greyscale: Not implemented", "")))+
  theme_minimal(base_family = "Calibri")+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "#faf9f5", color = "#ECEBE7", size = 1 ),
        plot.background = element_rect(fill = "#faf9f5", 
                                       color = "#faf9f5"),
        strip.background = element_rect(fill = "#ECEBE7", color = "#ECEBE7"),
        strip.text.x = element_text(margin = margin(4,0,4,0, "mm"), size = 18, color = "#5D5C58"),
        strip.text.y = element_text(margin = margin(0,2,0,2, "mm"), size = 18, color = "#5D5C58"),
        legend.title = element_text(size = 12),
        legend.key.size = unit(2.5, 'lines'),
        legend.justification = "top",
        legend.position = "left",
        legend.text = element_text(size = 16, color = "#5D5C58"),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 16, color = "#B8B7B3", vjust = -2),
        plot.margin=unit(c(5,10,10,5), "mm")) 
