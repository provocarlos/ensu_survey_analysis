
dir.create("01_input") 
dir.create("02_scripts")
dir.create("03_grafs")
dir.create("04_temp")

pacman::p_load(tidyverse, scales, purrr, lubridate, janitor, cowplot, ggrepel, numform, leaflegend, htmltools, ggpol, doBy, viridis, foreign, reldist, cartography, 
               RPostgreSQL, yaml, sf, grid, gridExtra, leaflet, rpostgis, googlesheets4, sp, treemapify, ggbeeswarm, ggthemes, shapefiles, stargazer, rgeos, biscale,
               ggalluvial, htmltools, htmlwidgets, stringr, reshape2, gt, wordcloud, SnowballC, tm, cluster, rgdal, grid, survey, MetBrewer, srvyr, readxl)

Sys.setlocale(locale = "es_ES.UTF-8")

source("../../01. SCRIPTS/tema_euzen.R")

path_output <- "03_grafs/"
path_temp <- "04_temp/"

options(survey.adjust.domain.lonely = TRUE)
options(survey.lonely.psu = "adjust")



######################################################### PERCEPCION DE INSEGURIDAD #########################################################

# Percepcion Guadalajara vs Nacional----

#* Cargar bases----
files <- list.files(path = "../../00. DATOS/2.5 INEGI/ENSU/SERIE/ENSU-CB", pattern = "*.dbf", full.names = T, recursive = T)
files_temp <- files[(grep("18\\.|19\\.|20\\.|21\\.|22\\.", files, ignore.case = T))];

#* Limpiar bases----
lapply(files_temp, function(x) {
  
  # x = files[1]
  
  print(x)
  
  fecha_var <- gsub(".dbf", "", str_remove(x, "../../00. DATOS/2.5 INEGI/ENSU/SERIE/ENSU-CB/ENSU_CB_")) 
  
  if (grepl("15\\.", x, ignore.case = T)) {
    
    foreign::read.dbf(x) %>%
      clean_names() %>% 
      rename(cve_ent = ent,
             fac_sel = factor,
             est_dis = edis,
             bp1_1 = p1) -> temp
    
  } else if (grepl("0316|0616|0916", x, ignore.case = T)) {
    
    foreign::read.dbf(x) %>%
      clean_names() %>% 
      rename(cve_ent = ent, 
             cve_mun= mun) -> temp
    
  } else {
    
    foreign::read.dbf(x) %>%
      clean_names() -> temp
    
  }
  
  temp %>% 
    mutate(
      fac_sel = as.numeric(fac_sel),
      var_id = "Nacional",
      cd = as.character(cd),
      var_gdl = case_when(
        cd %in% c("65", "66", "67", "68", "69", "70") ~ "ZM Monterrey",
        cd == "64" ~ "Monterrey",
        cd %in% c("61", "62","60", "63") ~ "ZM Guadalajara",
        cd == "59" ~ "Guadalajara",
        cd == "03" ~ "Tijuana", 
        cd == "26" ~ "Toluca",
        cd == "36" ~ "Puebla",
                T ~ "otro")
    ) %>% 
    select(var_id, var_gdl, upm_dis, 
           est_dis, fac_sel, bp1_1) %>% 
    srvyr::as_survey_design(ids = upm_dis, strata = est_dis, weights = fac_sel) %>% 
    filter(!is.na(bp1_1)) -> temp2
  
  vars <- list(list("var_id", "bp1_1"), list("var_gdl", "bp1_1"))
  
  lapply(vars, function(v) {
    
    print(v[[1]])
    
    temp2 %>% 
      group_by_at(.vars = unlist(v)) %>%
      summarise(por = survey_mean(vartype = "cv"), 
                total = survey_total(vartype = "cv"),
                .groups = "drop") %>% 
      mutate(var = v[[1]])
    
  }) -> output
  
  output %>% 
    reduce(full_join) %>% 
    mutate(
      var_id = coalesce(var_id, var_gdl),
      bp1_1 = as.numeric(bp1_1),
      fecha = as.Date(paste("01", substr(fecha_var, start = 1, stop = 2), 
                            substr(fecha_var, start = 3, stop = 4), sep = "-"), 
                      format = "%d-%m-%y")
    ) %>% 
    select(-matches("cv|var_gdl")) %>% 
    filter(bp1_1 == "2") -> data
  
}) -> ensu_full

ensu_full %>% 
  reduce(full_join) %>% 
  filter(var_id!="otro") %>% 
  mutate(
    trim = as.yearmon(fecha),
    highlight = case_when(var_id %in% c("Guadalajara", "Monterrey") ~ "MCRELEVANT", T ~ "NOT")
  ) -> df_percp


df_percp %>% 
  group_by(var_id) %>% 
  filter(fecha=="2022-12-01") %>% 
  mutate(var_label=paste0(var_id, " (", percent(por, .1), ")"),
         por=case_when(var_id == "ZM Guadalajara" ~ por+.015,
                       var_id == "Guadalajara"  ~ por+.02,
                      T ~ por)
         ) %>%
  pull(por, var_label) -> last_labels


# Serie de tiempo----
ggplot(
  df_percp,
  aes(x = trim, y = por, col = var_id)) +
  geom_vline(xintercept = "1T-2020", linetype = "dotted",
             alpha = 0.7, color = "darkgray", size = 0.7) +
  geom_line(aes(group = var_id), size = 1.5, alpha = .7) +
  geom_point(alpha = 0.6, size = 4) +
  geom_text(aes(x = "1T-2020", y = .85 + 0.1, label = "Inicio de\nla pandemia"),
            size = 4, fontface = "bold", family = "Montserrat", col = "darkgray") +
  ggtitle("% de la población mayor de 18 años que se siente insegura",
          "Ciudades seleccionadas, 1T-2018 a 4T-2022") +
  labs(x = "Trimestre", 
       caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI
       Nota: 1) Se omite el valor del segundo trimestre de 2020, por la pandemia ocasionada por el COVID-19
       2) Las Zonas Metropolitanas (ZM) no incluyen sus respectivas capitales") +
  tema_euzen(size_var = 40, font_var = "Roboto") +
  scale_color_manual(values = met.brewer("Cross")) +
  scale_y_continuous("% de población", labels = scales::percent,
                     sec.axis = sec_axis(~ ., breaks = last_labels, labels = names(last_labels))) + 
  scale_x_yearmon(format = "%b %Y") +
  guides(color = guide_legend(nrow = 2)) +
  theme(
    axis.text.y = element_text(size = 30),
    legend.position = "bottom"
  ) -> graf

ggsave(paste0(path_output, "ensu_perc_comp.png"), graf,
       width = 16, height = 9)



#################################################### VICTIMAS #################################################### 

# Cargar bases----
files_temp <- files[(grep("1218|0619|1219|0920|1220|0621|1221|0622|1222", files, ignore.case = T))];
viv_files <- list.files(path = "../../00. DATOS/2.5 INEGI/ENSU/SERIE/ENSU-VIV", pattern="*.dbf", full.names = T, recursive = T)

# Numero de victimas----

#* Limpiar bases----
ensu_full <- lapply(files_temp, function(x) {
  
  print(x)
  
  fecha <- str_remove(x, "../../00. DATOS/2.5 INEGI/ENSU/SERIE/ENSU-CB/ENSU_CB_") 
  
  viv_temp <- viv_files[(grep(fecha, viv_files, ignore.case = T))];
  
  left_join(
    foreign::read.dbf(x) %>%
      clean_names(),
    foreign::read.dbf(viv_temp) %>% 
      clean_names() %>% 
      select(cve_ent, upm, viv_sel, fac_viv)
  ) %>% 
    mutate(
      var_id = "Nacional",
      var_guad = case_when(
        cd %in% c("65", "66", "67", "68", "69", "70") ~ "ZM Monterrey",
        cd == "64" ~ "Monterrey",
        cd %in% c("61", "62","60", "63") ~ "ZM Guadalajara",
        cd == "59" ~ "Guadalajara",
        cd == "03" ~ "Tijuana", 
        cd == "26" ~ "Toluca",
        cd == "36" ~ "Puebla",
        T ~ "Otro"),
      vict_var = ifelse(bp1_6_1 == "1" | bp1_6_2 == "1" | bp1_6_3 == "1" |
                          bp1_6_4 == "1" | bp1_6_5 == "1" | bp1_6_6 == "1", "1", "0"),
      fecha = as.Date(paste("01", substr(fecha, start = 1, stop = 2), 
                            substr(fecha, start = 3, stop = 4), sep = "-"), 
                      format = "%d-%m-%y")
    ) %>% 
    select(fecha, var_id, var_guad, cve_ent, 
           cve_mun, nom_mun, upm_dis, 
           est_dis, fac_viv, vict_var) %>% 
    srvyr::as_survey_design(ids = upm_dis, strata = est_dis, weights = fac_viv) -> temp 
  
  temp %>% 
    group_by(fecha, var_id, vict_var) %>% 
    summarise(por = survey_mean(vartype = "cv"), 
              total = survey_total(vartype = "cv")) -> nac
  
  temp %>% 
    filter(var_guad != "Otro") %>% 
    group_by(fecha, var_guad, vict_var) %>% 
    summarise(por = survey_mean(vartype = "cv"), 
              total = survey_total(vartype = "cv")) -> estat
  
  plyr::rbind.fill(nac, estat) %>% 
    mutate(
      var_id = coalesce(var_id, var_guad),
      vict_var = str_wrap(case_when(
        vict_var == "1" ~ "Hogares con víctima",
        vict_var == "0" ~ "Hogares sin víctima"
      ), 15)
    ) %>% 
    select(-c(var_guad)) -> data
  
})

ensu_full %>% 
  reduce(full_join) %>% 
  mutate(
    fecha = zoo::as.yearmon(fecha, format = "%b/%Y") 
  ) %>%
  filter(var_id !="otro",
         vict_var == "Hogares con\nvíctima")-> df_vict


df_vict %>% 
  group_by(var_id) %>% 
  filter(fecha=="2022-06-01") %>% 
  mutate(var_label=paste0(var_id, "(", percent(por, .1), ")"),
         por=case_when(var_id=="ZM Guadalajara" ~ por+.01, 
                       var_id=="Guadalajara" ~ por+.01, 
                                     
                       T ~ por)
         ) %>%
  pull(por, var_label) -> last_labels


# Serie de tiempo----
ggplot(
  df_vict,
  aes(x = fecha, y = por, col = var_id)
) +
  geom_line(aes(group = var_id), size = 1.5) +
  geom_point(alpha = 0.6, size = 3) +
  ggtitle("% de hogares víctima de al menos un delito",
          "Ciudades seleccionadas, 4T-2022") +
  labs(x = "Trimestre", 
       caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI
       Nota: 1) Incluye los delitos robo total o parcial de vehículo,robo o asalto en calle o transport epúblico,
       robo en casa habitación, robo en forma distinta a las anteriores yextorsión.") +
  tema_euzen(size_var = 40, font_var = "Roboto") +
  scale_color_manual("", values = met.brewer("Cross")) +
  scale_y_continuous("% de población", labels = scales::percent, limits = c(min(df_vict$por)-.1, max(df_vict$por)+.1),
                     sec.axis = sec_axis(~ ., breaks = last_labels, labels = names(last_labels))) + 
  guides(color = guide_legend(nrow = 2)) +
  theme(
    axis.text.y = element_text(size = 30),
    legend.position = "bottom"
  ) -> graf

ggsave(paste0(path_output, "ensu_vict_comp.png"), graf,
       width = 18, height = 8)





# Atestiguación----

#* Limpiar bases----

files_temp <- files[(grep("18\\.|19\\.|20\\.|21\\.|22\\.", files, ignore.case = T))];

ensu_full <- lapply(files_temp, function(x) {
  
  print(x)
  
  fecha <- str_remove(x, "../../00. DATOS/2.5 INEGI/ENSU/SERIE/ENSU-CB/ENSU_CB_") 
  
  if (grepl("0316|0616|0916", x, ignore.case = T)) {
    
    foreign::read.dbf(x) %>%
      clean_names() %>% 
      rename(cve_ent = ent,
             cve_mun = mun,
      ) %>%
      mutate(upm_dis= as.numeric(upm_dis),
             fac_sel= as.numeric(fac_sel),
             est_dis= as.numeric(est_dis)) -> temp
    
  } else {
    
    foreign::read.dbf(x) %>%
      clean_names() -> temp
    
  }
  
  temp %>%
    mutate(
      var_id = "Nacional",
      var_guad = case_when(
        cd %in% c("65", "66", "67", "68", "69", "70") ~ "ZM Monterrey",
        cd == "64" ~ "Monterrey",
        cd %in% c("61", "62","60", "63") ~ "ZM Guadalajara",
        cd == "59" ~ "Guadalajara",
        cd == "03" ~ "Tijuana", 
        cd == "26" ~ "Toluca",
        cd == "36" ~ "Puebla",
        T ~ "Otro")
    ) -> temp
  
  design <- svydesign(ids = ~upm_dis, weights = ~fac_sel, strata = ~est_dis, data = temp)
  
  vars <- c("var_id", "var_guad")
  
  funion <- function(z) {
    
    as_tibble(svyby(~bp1_4_1 + bp1_4_2 + bp1_4_3 +
                      bp1_4_4 + bp1_4_5 + bp1_4_6, 
                    by = as.formula(paste0("~", z)), design, svymean, vartype = "cvpct"))
    
  }
  
  output <- lapply(vars, funion)
  
  output %>% 
    reduce(full_join) %>% 
    select(-matches("cv")) %>% 
    pivot_longer(cols = matches("bp1"),
                 names_to = "var", 
                 values_to = "por") %>% 
    mutate(var_id = coalesce(var_guad, var_id),
           resp = str_sub(var, nchar(var)),
           var = substr(var, 1, nchar(var) - 1),
           fecha = as.Date(paste("01", substr(fecha, start = 1, stop = 2), 
                                 substr(fecha, start = 3, stop = 4), sep = "-"), 
                           format = "%d-%m-%y"),
           var = case_when(
             var == "bp1_4_1" ~ "Vandalismo",
             var == "bp1_4_2" ~ "Consumo de alcohol en las calles",
             var == "bp1_4_3" ~ "Robos o asaltos",
             var == "bp1_4_4" ~ "Pandillerismo",
             var == "bp1_4_5" ~ "Venta o consumo de drogas",
             var == "bp1_4_6" ~ "Disparos frecuentes con armas"
           )) %>% 
    filter(var_id != "Otro",
           resp == "1") %>% 
    select(-matches("guad"))  -> data
  
})

ensu_full %>% 
  reduce(full_join) %>% 
  group_by(var_id, fecha) %>%
  summarise(por_mean=mean(por)) -> df_atesti

df_atesti %>% 
  group_by(var_id) %>% 
  filter(fecha=="2022-06-01") %>% 
  mutate(var_label=paste0(var_id, "(", percent(por_mean, .1), ")"),
         por_mean=case_when(#var_id=="Guadalajara" ~ por+.022,
                       # var_id=="ZM Guadalajara" ~ por_mean + .015,
                       # var_id=="ZM Monterrey" ~ por_mean - .02,
                       # var_id=="Puebla" ~ por+.01,
                       # var_id=="Monterrey" ~ por-.01,
                       # var_id=="Tijuana" ~ por+.02,
                       # var_id=="Nacional" ~ por+.01,
                       T ~ por_mean)
  ) %>%
  pull(por_mean, var_label) -> last_labels

ggplot(
  df_atesti,
  aes(x = fecha, y = por_mean, col = var_id)
) +
  geom_line(size = 1.5) +
  geom_point(alpha = 0.6, size = 3) +
  # geom_vline(xintercept = as.Date("2020-12-01"), 
  #            linetype = "dotdash", size = 1, alpha = 0.75, col = "darkgray") +
  # geom_text_repel(
  #   aes(label = percent(por, accuracy = 0.1)),
  #   fontface = "bold", family = "Montserrat", size = 4.5, hjust = -1.1, vjust =  0.5, angle = 90, segment.size  = 0.2,
  #   show.legend = F) +
  ggtitle("Atestiguación de delitos y conductas antisociales (promedio)",
          "Guadalajara (1T-2016 al 2T-2022)") +
  labs(caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
  tema_euzen() +
  scale_color_manual(values = met.brewer("Cross")) +
  scale_x_date("", date_breaks = "3 month", labels = date_format("%b\n%y")) +
  scale_y_continuous("% de población", labels = scales::percent, limits = c(min(temp$por_mean)-.1, max(temp$por_mean)+.1),
                     sec.axis = sec_axis(~ ., breaks = last_labels, labels = names(last_labels)))+
  theme(
    axis.text.y = element_text(size = 11),
    legend.position = "none") -> graf

ggsave(paste0(path_output, "ensu_atesti_comp.png"), graf,
         width = 18, height = 9)
  
