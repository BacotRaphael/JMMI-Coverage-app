# JMMI Coverage app
# Side app to get coverage through time and space for all items

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Install/Load libraries
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(tidyverse, openxlsx, reshape2, sf, leaflet, shiny, shinyWidgets)
library(tidyverse)
library(openxlsx)
library(sf)
library(leaflet)
library(shiny)
library(shinyWidgets)
library(highcharter)

# Import full dataset
data_all <- read.csv("data/data_all.csv")

# Get number of observations per column at national, governorate and district level
coverage_nat <- data_all %>% dplyr::select(-aor, -X, -jmmi) %>% dplyr::select(-matches("government|district")) %>%
  group_by(date) %>% summarise_all(~sum(!is.na(.))) %>% ungroup %>% mutate_all(~ifelse(.==0,NA,.))
coverage_gov <- data_all %>% dplyr::select(-aor, -X, -jmmi) %>% dplyr::select(-matches("district")) %>%
  group_by(date, government_name, government_ID) %>% summarise_all(~sum(!is.na(.))) %>% ungroup %>% mutate_all(~ifelse(.==0,NA,.))
coverage_dis <- data_all %>% dplyr::select(-aor, -X, -jmmi) %>%
  group_by(date, government_name, government_ID, district_name, district_ID) %>% summarise_all(~sum(!is.na(.))) %>% ungroup %>% mutate_all(~ifelse(.==0,NA,.))

## For Food items
test_gov <- coverage_gov %>%
   dplyr::mutate(food=mean(c_across(wheat_flour:onion)),
                 fuels=mean(c_across(petrol:diesel)), .before="bleach") 
test_dis <- coverage_dis %>%
  rowwise() %>% dplyr::mutate(food=mean(c_across(wheat_flour:onion)),
                              fuels=mean(c_across(petrol:diesel)), .before="bleach") 
test_dis_out <- test_dis
test_dis_out %>%  write.csv("JMMI_district_coverage_ts_all_indicators.csv", row.names = F)

## List of governorates and district ever being covered:
dis.all.food <- test_dis %>% filter(food>0) %>%
  dplyr::select(matches("date|gover|district|food$"), wheat_flour:onion, -matches("authority")) %>%
  dplyr::select(-date) %>% group_by(government_name, government_ID, district_name, district_ID) %>%
  dplyr::mutate(number_time_covered_food=n(), .before = "food") %>% 
  summarise_all(~round(mean(.), 1)) %>% rename_at(vars(food:onion), ~paste0("average_n_obs_",.)) %>% ungroup
gov.all.food <- dis.all.food %>% dplyr::select(-matches("district|covered")) %>% group_by(government_name, government_ID) %>%
  summarise_all(~round(mean(.), 1))

dis.all.petrol <- test_dis %>% filter(fuels>0) %>%
  dplyr::select(matches("date|gover|district|fuels$"), petrol:food_gov_origin, -matches("authority")) %>%
  dplyr::select(-date) %>% group_by(government_name, government_ID, district_name, district_ID) %>%
  dplyr::mutate(number_time_covered_petrol=n(), .before = "fuels") %>% 
  summarise_all(~round(mean(.), 1)) %>% rename_at(vars(fuels:food_gov_origin), ~paste0("average_n_obs_",.)) %>% ungroup 
gov.all.petrol <- dis.all.petrol %>% dplyr::select(-matches("district|covered")) %>% group_by(government_name, government_ID) %>%
  summarise_all(~round(mean(.), 1))

dis.all <- dis.all.food %>% dplyr::select(-matches("fuels|government|district_name")) %>%
  full_join(dis.all.petrol %>% dplyr::select(-matches("food")), by = c("district_ID"))
dis.all %>% dplyr::select(matches("government|district"), everything()) %>% write.csv("JMMI_district_coverage.csv", row.names = F)
dis.all <- dis.all %>% dplyr::select(-matches("government|district_name"))

# mapping coverage
pacman::p_load(leaflet, sf)
adm1 <- st_read(dsn = "data/shapes/yem_adm_govyem_cso_ochayemen_20191002_GDB.gdb", layer="yem_admbnda_adm1_govyem_cso")
adm2_or <- st_read(dsn = "data/shapes/yem_adm_govyem_cso_ochayemen_20191002_GDB.gdb", layer="yem_admbnda_adm2_govyem_cso")
adm2 <- adm2_or %>% left_join(dis.all, by = c("admin2Pcode"="district_ID"))

# pal <- colorNumeric(palette=colorRamp(c("white","indianred", "red"), interpolate = "linear"), domain = adm2$number_time_covered_food, na.color = "#FBFBFB")
# map <- leaflet() %>% addTiles() %>%
#   addPolygons(data = adm1, weight= 0.5, stroke = T, color = "#58585A", fill=FALSE, fillOpacity = 0.1, opacity = 1) %>%
#   addPolygons(data = adm2, weight = 0.25, color = "#58585A", fillColor = ~pal(adm2$number_time_covered_food)) %>%
#   addLegend(pal=pal, values = sort(adm2$number_time_covered_food, decreasing = T), position = "bottomright", title = "Coverage<br>index food")
# map

cols      <- c("rgb(238,88,89)",   "rgb(88,88,90)",    "rgb(165,201,161)",        # define color palette for plot lines
               "rgb(86,179,205)",  "rgb(246,158,97)",  "rgb(255,246,122)",
               "rgb(210,203,184)", "rgb(247,172,172)", "rgb(172,172,173)",
               "rgb(210,228,208)", "rgb(171,217,230)", "rgb(251,207,176)",
               "rgb(255,251,189)", "rgb(233,229,220)")

dates <- test_dis_out$date %>% unique
dates_max <- dates[length(dates)]
vars <- colnames(test_dis_out)[!colnames(test_dis_out) %in% c("date", "government_ID", "government_name", "district_name", "district_ID")]
vars.name <- gsub("_", " ", vars)
item <- setNames(vars, vars.name)

ui <- fluidPage(
  titlePanel("JMMI Coverage app"),
  sidebarPanel(
      pickerInput("variable",
                  label = "Select a variable below",
                  choices = item,
                  options = list(title = "Select", `actions-box` = TRUE, `live-search` = TRUE),
                  selected = "food",
                  multiple = FALSE),
      sliderTextInput("date",                                        # set date slider
                      "Month:",
                      force_edges = TRUE,
                      choices = dates,
                      selected = dates_max,
                      animate = TRUE
      )
  ),
  mainPanel(
    leafletOutput("map"),
    br(),
    highchartOutput("graph")
  
  )
)

server <- function(input, output){
  output$map <- renderLeaflet({
    date_selected <- input$date
    var <- input$variable
    # var <- "food"
    # date_selected <- "2019-06-01"
    # date_selected <- "2021-03-01"
    # date_selected <- dates_max
    adm2 <- adm2_or %>% left_join(test_dis_out %>% dplyr::select(-matches("government|district_name")), by = c("admin2Pcode"="district_ID")) %>%
      filter(date==date_selected) %>% dplyr::select(all_of(var), "admin2Name_en")
    df_adm2 <- adm2 %>% st_drop_geometry
    df_na <- df_adm2 %>% filter(!is.na(!!sym(var))) 
        if (nrow(df_na)==0) {
          m <- leaflet() %>% addTiles() %>%
            addPolygons(data = adm1, weight= 0.5, stroke = T, color = "#58585A", fill=FALSE, fillOpacity = 0.1, opacity = 1)} else {
          
              pal <- colorNumeric(palette=colorRamp(c("#FAD5D6","#F5A6A6", "#EE5859", "#5F2324"), interpolate = "linear"),
                                  domain = df_adm2[, var], na.color = "#DCE5EF")
              m <- leaflet() %>% addTiles() %>%
                addPolygons(data = adm1, weight= 0.5, stroke = T, color = "#58585A", fill=FALSE, fillOpacity = 0.1, opacity = 1) %>%
                addPolygons(data = adm2, weight = 0.25, color = "#58585A", fillColor = ~pal(df_adm2[, var]), fillOpacity = 0.8,
                            label = paste0(adm2[[var]] %>% round(1), " observations for ", gsub("_", " ", gsub("n_obs", "", gsub("average_", "", var))), " in ", adm2[["admin2Name_en"]], " district")) %>%
                addLegend(pal=pal, values = sort(df_adm2[, var], decreasing = T), position = "bottomright", title = paste0(gsub("_", " ", var)), opacity = 0.8,
                          na.label = "NA")
      }
        
  })
  
  output$graph <- renderHighchart({
    var <- input$variable
    # var <- "bottled_water"
    data <- test_dis_out %>% pivot_longer(cols = c((which(colnames(.)=="district_ID")+1):ncol(.))) %>%
      group_by(date, name) %>%
      dplyr::summarise(monthly_total_n_obs = sum(value, na.rm=T) %>% round(1),
                       monthly_average_n_obs = mean(value, na.rm=T) %>% round(1)) %>%
      mutate_all(~ifelse(is.nan(.) | . == 0, NA, .)) %>% 
      filter(name==var) %>% dplyr::rename(item=name) %>%
      pivot_longer(cols=c(3:4)) %>% mutate(name=gsub("_", " ", name))
    
    graph <-  hchart(data, "line", hcaes(x = "date", y = "value", group = "name")) %>% hc_colors(cols) %>%
      hc_yAxis(min = 0, title = list(text = paste0("# observations per district"))) %>%
      hc_title(text = list(paste0("Number of observations for ", gsub("_", " ", var),"<br>(district average)")))
  })
}

shinyApp(ui, server)

# runApp()
