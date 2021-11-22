# Input load. Please do not change #
`dataset` = read.csv('C:/Users/cguio/REditorWrapper_643874ff-25b4-420d-9503-4d96bd7d5692/input_df_09333d4c-f045-4d4d-8393-fd02dcf90917.csv', check.names = FALSE, encoding = "UTF-8", blank.lines.skip = FALSE);
# Original Script. Please update your script content here and once completed copy below section back to the original editing window #


# Pegue o escriba aquí el código de script:
library(tidyverse)
library(ggplot2)
library(extrafont)

font_import()
loadfonts(device = "win", quiet = TRUE)

tornado_dat <- `dataset` %>%
         filter(!is.na(practice_type)) %>%
         mutate(percentage_lab = case_when(percentage < 2 ~ "",
                                       TRUE ~ as.character(paste0(percentage, "%"))),
                gender_legend = paste0(farmer_gender," (n: ",n_site,")"))
        

ggplot(tornado_dat) +
  geom_col(aes(x = factor(practice_type, levels = rev(levels(factor(practice_type)))),
               y = ifelse(farmer_gender == "Male", percentage, -percentage), 
               fill = gender_legend)) +
  geom_text(aes(x = practice_type, 
                y = ifelse(farmer_gender == "Male", percentage + 5, -percentage -5), 
                label = percentage_lab),
            family = "Calibri",
            size = 5) +
  scale_alpha(range = c(0.5, 1))+
  scale_fill_manual(values = c("#FFC107", "#a9713d")) +
  scale_y_continuous(limits = c(-max(tornado_dat$percentage) -20, max(tornado_dat$percentage) +20))+
  facet_grid(indicator ~ site + year )+
  coord_flip() +
  guides(alpha = "none")+
  theme_minimal(base_family = "Calibri")+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "#faf9f5", color = "#ECEBE7" ),
        plot.background = element_rect(fill = "#faf9f5", 
                                       color = "#faf9f5"),
        strip.background = element_rect(fill = "#ECEBE7", color = "#ECEBE7"),
        strip.text.x = element_text(margin = margin(2,0,2,0, "mm"), size = 18, color = "#5D5C58"),
        strip.text.y = element_text(margin = margin(0,2,0,2, "mm"), size = 18, color = "#5D5C58"),
        legend.title = element_blank(),
        legend.justification = "left",
        legend.position = "bottom",
        legend.key.size = unit(2.5, 'lines'),
        legend.text = element_text(size = 18, color = "#5D5C58"),
        axis.title = element_blank(),
        axis.text.y = element_text(color = "#B8B7B3", size = 16),
        axis.text.x = element_blank(),
        plot.margin=unit(c(5,5,5,5), "mm"),
        panel.spacing = unit(1.5, "lines"))
