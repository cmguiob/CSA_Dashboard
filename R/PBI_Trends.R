# Input load. Please do not change #
`dataset` = read.csv('C:/Users/cguio/REditorWrapper_eb9fae2c-3dfc-4f41-9ffe-b780b49a8b31/input_df_5d069fb4-d5eb-4c9d-a8de-e4ac09dabe60.csv', check.names = FALSE, encoding = "UTF-8", blank.lines.skip = FALSE);
# Original Script. Please update your script content here and once completed copy below section back to the original editing window #


# Pegue o escriba aquí el código de script:
library(tidyverse)
library(ggsankey)
library(extrafont)

font_import()
loadfonts(device = "win", quiet = TRUE)

#Clarify in the title: gender is related to head of household

dat <- `dataset` %>%
   select(location_type, 
         location_name, 
         farmer_id, 
         gender, 
         adopted_cases,
         drivers,
         practice_type,
         exposure_weather)  %>%
  # Remove rows for practices for which farmers didn't answer
  filter(!is.na(adopted_cases),
         !is.na(exposure_weather)) %>%
  mutate(location_gender = paste(location_name, paste0("(",substr(gender,1,1),")"))) %>%
  mutate(adoption_gender = paste(adopted_cases, paste0("(",substr(gender,1,1),")"))) %>%
  distinct()%>%
  make_long(location_gender, exposure_weather, adoption_gender, drivers, practice_type) %>%
  mutate(node = fct_rev(node),
         next_node = fct_rev(next_node))

dat_n <- 
  dat %>% 
  filter(!is.na(node)) %>% 
  group_by(x, node)%>% 
  summarise(count = n()) %>%
  ungroup() 
  #%>%
  #group_by(x) %>%
  #mutate(percentage = round(count*100/sum(count),1))

dat %>% 
  left_join(dat_n) %>%
  ggplot(aes(x = x, 
          next_x = next_x, 
          node = node, 
          next_node = next_node,
          fill = node)) +
  geom_sankey(flow.alpha = 0.5,
              #node.color = "#5D5C58"
              )+
  #geom_sankey_text(aes(label = count), 
  #                 size = 3.5, 
  #                 vjust = -1.7,
  #                 family = "robotoc",
  #                 face = "bold",
  #                 color = "#5D5C58",
  #                 check_overlap = TRUE) +
    scale_x_discrete(labels = c("Farmers' replies\n by location (gender)",
                              "Exposure to\n weather shocks",
                              "Implementation / use\n cases (gender)",
                              "Drivers / Reasons\n",
                              "CSA option\n")
                   #position = "top"
                   )+
  geom_sankey_label(size = 5, 
                   color = "white",
                   family = "Calibri",
                   aes(label = node))+
  #scale_fill_hue(l = 70, c = 40)+
  scale_fill_hue(l = 70, c = 45, h = c(0, 360))+
  theme_minimal(base_family = "Calibri")+
  theme(panel.grid = element_blank(),
        plot.background = element_rect(fill = "#faf9f5", color = "#faf9f5"),
        legend.position = "none",
        axis.text.x = element_text(
            size = 18, 
            color = "#B8B7B3", #this is 0.2 darkened
            vjust = 5
            ),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        plot.margin=unit(c(-1,-10,1,-25), "mm"))

