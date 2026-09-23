
pacman::p_load(tidyverse, scales, lubridate, janitor, cowplot, ggrepel, numform, leaflegend, htmltools, ggpol, rio, viridis, tableHTML, openxlsx, cartography, 
               RPostgreSQL, yaml, sf, grid, gridExtra, leaflet, rpostgis, googlesheets4, sp, treemapify, ggbeeswarm, ggthemes, shapefiles, stargazer, rgeos, biscale,
               ggalluvial, htmltools, htmlwidgets, stringr, reshape2, gt, wordcloud, SnowballC, tm, cluster, rgdal, grid, survey, MetBrewer)

Sys.setlocale(locale = "es_ES.UTF-8")

source("../../01. SCRIPTS/tema_euzen.R")

path_output <- "03_grafs/"
path_temp <- "04_temp/"

options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")


ensu <- foreign::read.dbf("../../00. DATOS/2.5 INEGI/ENSU/SERIE/ENSU-CB/ENSU_CB_1222.dbf") %>% 
  clean_names() %>% 
  mutate(var_id = "nacional",
         var_guad = ifelse(cve_ent == "14" & 
                             cve_mun %in% c("039"),
                           "guadalajara", "otro")
  )
  

design <- svydesign(ids = ~upm_dis, weights = ~fac_sel, strata = ~est_dis, data = ensu)

vars <- c("var_id", "var_guad")

funion <- function(z) {
  
  as_tibble(svyby(~bp1_9_1+bp1_9_2+bp1_9_3+bp1_9_4+bp1_9_5, by = as.formula(paste0("~", z)), design, svytotal, na.rm=T, vartype = "cvpct"))
  
}

output <- lapply(vars, funion)

names(output[[2]])[1] <- "var_id"


 as.data.frame(do.call(rbind, output)) %>%
  filter(var_id %in% c("nacional", "guadalajara")) %>%
  select(-matches("cv")) %>% 
  pivot_longer(cols = matches("bp1"),
               names_to = "var", 
               values_to = "total")  %>% 
  mutate(
    resp = str_sub(var, nchar(var)),
    var = substr(var, 1, nchar(var) - 1)) %>% 
  group_by(var, var_id) %>% 
  mutate(
    tot = sum(total, na.rm = T),
    por = total / tot,
    var = case_when(
      var == "bp1_9_1" ~ "Policía Preventiva Municipal",
      var == "bp1_9_2" ~ "Policía Estatal",
      var == "bp1_9_3" ~ "Guardia Nacional",
      var == "bp1_9_4" ~ "Ejército",
      var == "bp1_9_5" ~ "Marina"),
    var = factor(var, levels = c("Policía Preventiva Municipal",
                                 "Policía Estatal",
                                 "Guardia Nacional",
                                 "Ejército",
                                 "Marina")),
    resp = factor(case_when(
      resp == "1" ~ "Confía",
      resp == "2" ~ "Confía",
      resp == "3" ~ "Desconfía",
      resp == "4" ~ "Desconfía",
      resp == "9" ~ "Ns/Nc"
    ), levels = c(
      "Desconfía", "Confía", "Ns/Nc"
    )),
    var_id=recode(var_id, "guadalajara"="Guadalajara",
                  "nacional"="Nacional")) %>%
  group_by(var_id, var, resp) %>%
  summarise(por=sum(por))-> df_graf

ggplot(df_graf,
       aes(x = por, y = var_id, fill = resp)) +
  geom_bar(stat = "identity", 
           size = 1.5, position=position_stack()) +
  geom_text(aes(label = ifelse(por>.1, percent(por, accuracy = 1), NA)),
            size = 15, family = "Montserrat", fontface = "bold",
            position=position_stack(vjust = .5), color="black") +
  facet_wrap(~var, ncol = 1)+
  ggtitle("Confianza en autoridades de Seguridad Pública",
          "% de personas mayores de 18 años que contestaron, 4T-2022") +
  labs(y = "", x = "", 
       caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
  tema_euzen(size_var = 40, font_var = "Roboto") +
  scale_fill_manual("",values = c(met.brewer(name = "OKeeffe1", n = 2), "grey20")) +
  scale_x_continuous(labels = percent) -> graf
graf
ggsave("03_grafs/ensu_confianza_sp.png", graf,
       width = 8, height = 9)


## Corrupción (solo en 2° y 4° Trimestre)
ensu <- foreign::read.dbf("../../00. DATOS/2.5 INEGI/ENSU/SERIE/ENSU-CB/ENSU_CB_1222.dbf") %>% 
  clean_names() %>% 
  mutate(var_id = "nacional",
         var_guad = ifelse(cve_ent == "14" & 
                             cve_mun %in% c("039"),
                           "guadalajara", "otro")
  )


design <- svydesign(ids = ~upm_dis, weights = ~fac_sel, strata = ~est_dis, data = ensu)

vars <- c("var_id", "var_guad")

funion <- function(z) {
  
  as_tibble(svyby(~bp3_6, by = as.formula(paste0("~", z)), design, svytotal, na.rm=T, vartype = "cvpct"))
  
}

output <- lapply(vars, funion)

names(output[[2]])[1] <- "var_id"


as.data.frame(do.call(rbind, output)) %>%
  filter(var_id %in% c("nacional", "guadalajara")) %>%
  select(-matches("cv")) %>% 
  pivot_longer(cols = matches("bp3_6"),
               names_to = "var", 
               values_to = "total")  %>% 
  mutate(
    resp = str_sub(var, nchar(var)),
    var = substr(var, 1, nchar(var) - 1)) %>% 
  group_by(var, var_id) %>% 
  mutate(
    tot = sum(total, na.rm = T),
    por = total / tot,
    resp = factor(case_when(
      resp == "1" ~ "Sí",
      resp == "2" ~ "No",
      T ~ "Ns/Nc"), levels = c(
        "Sí", "No", "Ns/Nc"
      )),
    var_id=recode(var_id, "guadalajara"="Guadalajara",
                  "nacional"="Nacional")) -> df_graf


ggplot(df_graf,
       aes(x = por, y = var_id, fill = resp)) +
  geom_bar(stat = "identity", 
           size = 1.5, position=position_stack()) +
  geom_text(aes(label = ifelse(por>.03, percent(por, accuracy = 1), NA)),
            size = 12, family = "Montserrat", fontface = "bold",
            position=position_stack(vjust = .5), color="white") +
  ggtitle("¿Ha estado involucrado en actos de corrupción que \ninvolucre autoridades de seguridad pública?",
          "% de personas mayores de 18 años que contestaron, 4T-2022") +
  labs(y = "", x = "% de personas que contestaron", caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
  tema_euzen(size_var = 20, font_var = "Roboto", lineheight_var = .7) +
  scale_fill_manual("",values = c(met.brewer(name = "Hiroshige", n = 6)[-2:-5], "grey20")) +
  scale_x_continuous(labels = percent) +
  theme(plot.title.position = "plot") -> graf
graf
ggsave("03_grafs/ensu_corrupcion.png", graf,
       width = 8, height = 8)



 # Efectividad del gobierno


#* Cargar bases----
files <- list.files(path = "../../00. DATOS/2.5 INEGI/ENSU/SERIE/ENSU-CB", pattern = "*.dbf", full.names = T, recursive = T)

files_temp <- files[(grep("18\\.|19\\.|20\\.|21\\.|22\\.", files, ignore.case = T))];

ensu_full <- lapply(files_temp, function(x) {
  
  print(x)
  
  fecha <- str_remove(x, "../../00. DATOS/2.5 INEGI/ENSU/SERIE/ENSU-CB/ENSU_CB_") 
  
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
    clean_names() %>% 
    mutate(
      fac_sel = as.numeric(fac_sel),
      var_id = "nacional",
      var_zm = ifelse(cve_ent == "14" & 
                        cve_mun %in% c("039"),
                      "guadalajara", "otro")
    ) -> temp
  
  print(nrow(temp[temp$var_zm=="guadalajara",]))
  
  design <- svydesign(ids = ~upm_dis, weights = ~fac_sel, strata = ~est_dis, data = temp)
  
  
  vars <- c("var_id", "var_zm")
  
  funion <- function(z) {
    
    as_tibble(svyby(~bp3_2, by = as.formula(paste0("~", z)), design, svytotal, na.rm = T, vartype = "cvpct"))
    
  }
  
  output <- lapply(vars, funion)
  
  output %>% 
    reduce(full_join) %>% 
    select(-matches("cv")) %>% 
    pivot_longer(cols = matches("bp3"),
                 names_to = "var", 
                 values_to = "total") %>% 
    mutate(var_id = coalesce(var_zm, var_id),
           resp = str_sub(var, nchar(var)),
           var = substr(var, 1, nchar(var) - 1)) %>% 
    filter(var_id != "otro") %>% 
    select(-matches("zm")) %>% 
    group_by(var_id) %>% 
    mutate(
      tot = sum(total, na.rm = T),
      por = total / tot,
      fecha = as.Date(paste("01", substr(fecha, start = 1, stop = 2), 
                            substr(fecha, start = 3, stop = 4), sep = "-"), 
                      format = "%d-%m-%y"),
      resp = factor(case_when(
        resp == "1" ~ "Muy o algo efectivo",
        resp == "2" ~ "Muy o algo efectivo",
        resp == "3" ~ "Poco o nada efectivo",
        resp == "4" ~ "Poco o nada efectivo", 
        resp == "9" ~ "Ns/Nc"
      ), levels = c("Muy o algo efectivo",
                    "Poco o nada efectivo",
                    "Ns/Nc")
      )
      
    ) %>% 
    ungroup()  -> data
})



ensu_full %>% 
  reduce(full_join) %>% 
  mutate(
    var_id = case_when(
      var_id == "guadalajara" ~ "Guadalajara",
      var_id == "nacional" ~ "Nacional"
    )) %>%
  group_by(var_id, resp, fecha) %>%
  summarise(por=sum(por)) %>%
  filter(resp!="Ns/Nc", 
         resp!="Poco o nada efectivo",
         fecha > as_date("2018-03-02"),
         )-> df_graf


ggplot(
  df_graf,
  aes(x=fecha,
      y=por, 
      color=var_id))+
  # geom_rect(aes(xmin = as.Date("2022-01-02"), xmax =  as.Date("2022-10-01"), ymin = 0, ymax = 1),
  #           alpha = .25, show.legend = F, color=NA)+
  geom_line(size=2, alpha = .5) +
  geom_point(alpha = 0.6, size = 4) +
  # geom_line(stat = "smooth",method = "lm", size = 1.5, linetype ="dashed", alpha = 0.5) +
  geom_text_repel(aes(label = percent(por, accuracy = 0.1)),
            fontface = "bold", family = "Montserrat", size = 6,
            show.legend = F, nudge_y = -.02) +
  
  ggtitle("Percepción de efecividad del gobierno para resolver los problemas de la ciudad",
          "% de población mayor de 18 años que contestaron, 1T-2018 a 4T-2022") +
  labs(caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
  tema_euzen(size_var = 20, font_var = "Roboto") +
  # facet_wrap(~var_id, ncol = 1) +
  scale_color_manual(values = c(rev(met.brewer(name = "OKeeffe1", n = 2)))) +
  scale_x_date("", date_breaks = "3 month", labels = date_format("%b\n%y")) +
  scale_y_continuous("% de población que contestaron", 
                     labels = scales::percent_format(accuracy = 1L)) +
  guides(size = "none") +
  theme(legend.position = "top",
        legend.title = element_blank()) -> graf

ggsave(paste0(path_output, "ensu_efectividad.png"), graf,
       width = 15, height = 6)



#* Serie de tiempo confianza en autoridades de SP
#* 

files_temp <- files[(grep("0919|0920|0921|1221|0322|0622|0922|1222", files, ignore.case = T))];


ensu_full <- lapply(files_temp, function(x) {
  
  print(x)
  
  fecha <- str_remove(x, "../../00. DATOS/2.5 INEGI/ENSU/SERIE/ENSU-CB/ENSU_CB_") 
  
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
      var_guad = ifelse(cve_ent == "14" & 
                          cve_mun %in% c("039"),
                        "guadalajara", "otro")
    ) -> temp
  
  design <- svydesign(ids = ~upm_dis, weights = ~fac_sel, strata = ~est_dis, data = temp) 
  
  as_tibble(svyby(~bp1_9_1 + bp1_9_2 + bp1_9_3 +
                    bp1_9_4 + bp1_9_5, 
                  by = ~var_guad, design, svymean, vartype = "cvpct", na.rm = T)) -> output
  
  output %>% 
    select(-matches("cv")) %>%
    pivot_longer(cols = matches("bp1"),
                 names_to = "var", 
                 values_to = "total")  %>% 
    mutate(
      resp = str_sub(var, nchar(var)),
      var = substr(var, 1, nchar(var) - 1)) %>% 
    group_by(var, var_guad) %>% 
    mutate(
      tot = sum(total, na.rm = T),
      por = total / tot,
      fecha = as.Date(paste("01", substr(fecha, start = 1, stop = 2), 
                            substr(fecha, start = 3, stop = 4), sep = "-"), 
                      format = "%d-%m-%y"),
      var = case_when(
        var == "bp1_9_1" ~ "Policía Preventiva Municipal",
        var == "bp1_9_2" ~ "Policía Estatal",
        var == "bp1_9_3" ~ "Guardia Nacional",
        var == "bp1_9_4" ~ "Ejército",
        var == "bp1_9_5" ~ "Marina"),
      var = factor(var, levels = c("Policía Preventiva Municipal",
                                   "Policía Estatal",
                                   "Guardia Nacional",
                                   "Ejército",
                                   "Marina")),
      resp = factor(case_when(
        resp == "1" ~ "Confía",
        resp == "2" ~ "Confía",
        resp == "3" ~ "Desconfía",
        resp == "4" ~ "Desconfía",
        resp == "9" ~ "Ns/Nc"
      ), levels = c(
        "Desconfía", "Confía", "Ns/Nc"
      ))) %>%
    group_by(var_guad, var, resp, fecha) %>%
    summarise(por=sum(por)) %>%
    filter(resp=="Confía", var_guad!="otro")-> data
  
})

ensu_full %>% 
  reduce(full_join) %>% 
  mutate(
    fecha = zoo::as.yearmon(fecha, format = "%b/%Y")
  ) -> df_graf

#* Barras: Confianza autoridades de SP
ggplot(df_graf,
       aes(x = reorder(var, -por), y = por, fill = as.factor(fecha), group = as.factor(fecha))) +
  geom_bar(stat = "identity", position = "dodge2") +
  geom_text(aes(label = percent(por, accuracy = 0.1), y = por - .2),
            fontface = "bold", family = "Montserrat", size = 8, hjust = -0.1, angle = 90, col = "white",
            position = position_dodge2(width = 0.9), 
  ) +
  ggtitle("Confianza en autoridades de Seguridad Pública",
          "") +
  labs(y = "", x = "% de personas que confían", 
       caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
  tema_euzen(size_var = 20, font_var = "Roboto") +
  scale_fill_manual(values = met.brewer(name = "Signac", n = 13)[-4:-8]) +
  scale_y_continuous("% de hogares", labels = scales::percent_format(accuracy = 1L), limits = c(0, max(df_graf$por) + 0.025)) +
  guides(fill = guide_legend(reverse = F, nrow = 1),
         color = "none") +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  ) -> graf

ggsave(paste0(path_output, "ensu_confianza_serie.png"), graf,
       width = 16, height = 9)




#* Serie de tiempo confianza en autoridades de SP
#* 
files <- list.files(path = "../../00. DATOS/2.5 INEGI/ENSU/SERIE/ENSU-CB", pattern = "*.dbf", full.names = T, recursive = T)

files_temp <- files[(grepl("22.dbf|21.dbf|20.dbf|19.dbf", files, ignore.case = T))];


ensu_full <- lapply(files_temp, function(x) {
  
  print(x)
  
  fecha <- str_remove(x, "../../00. DATOS/2.5 INEGI/ENSU/SERIE/ENSU-CB/ENSU_CB_") 
  
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
      var_guad = ifelse(cve_ent == "14" & 
                          cve_mun %in% c("039"),
                        "guadalajara", "otro")
    ) -> temp
  
  design <- svydesign(ids = ~upm_dis, weights = ~fac_sel, strata = ~est_dis, data = temp) 
  
  as_tibble(svyby(~bp1_9_1 + bp1_9_2 + bp1_9_3 +
                    bp1_9_4 + bp1_9_5, 
                  by = ~var_guad, design, svymean, vartype = "cvpct", na.rm = T)) -> output
  
  output %>% 
    select(-matches("cv")) %>%
    pivot_longer(cols = matches("bp1"),
                 names_to = "var", 
                 values_to = "total")  %>% 
    mutate(
      resp = str_sub(var, nchar(var)),
      var = substr(var, 1, nchar(var) - 1)) %>% 
    group_by(var, var_guad) %>% 
    mutate(
      tot = sum(total, na.rm = T),
      por = total / tot,
      fecha = as.Date(paste("01", substr(fecha, start = 1, stop = 2), 
                            substr(fecha, start = 3, stop = 4), sep = "-"), 
                      format = "%d-%m-%y"),
      var = case_when(
        var == "bp1_9_1" ~ "Policía Preventiva Municipal",
        var == "bp1_9_2" ~ "Policía Estatal",
        var == "bp1_9_3" ~ "Guardia Nacional",
        var == "bp1_9_4" ~ "Ejército",
        var == "bp1_9_5" ~ "Marina"),
      var = factor(var, levels = c("Policía Preventiva Municipal",
                                   "Policía Estatal",
                                   "Guardia Nacional",
                                   "Ejército",
                                   "Marina")),
      resp = factor(case_when(
        resp == "1" ~ "Confía",
        resp == "2" ~ "Confía",
        resp == "3" ~ "Desconfía",
        resp == "4" ~ "Desconfía",
        resp == "9" ~ "Ns/Nc"
      ), levels = c(
        "Desconfía", "Confía", "Ns/Nc"
      ))) %>%
    group_by(var_guad, var, resp, fecha) %>%
    summarise(por=sum(por)) %>%
    filter(resp=="Confía", var_guad!="otro")-> data
  
})


ensu_full %>% 
  reduce(full_join) %>% 
  filter(var=="Policía Preventiva Municipal") %>%
  mutate(
    trim = as.yearqtr(fecha)
  ) -> df_graf


# Serie de tiempo----
ggplot(
  df_graf,
  aes(x = trim, y = por, col = var)
) +
  geom_line(aes(group = var),
            alpha = 0.9, size = 1.5) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = percent(por, accuracy = 0.1)),
                  angle = 90, size = 15, fontface = "bold", family = "Montserrat", nudge_y = .05,
                  segment.size = 0.25, show.legend = F, force = 2) +
  ggtitle("Confianza en la Policía Preventiva Municipal",
          "% de la población mayor de 18 años que confía, 4T-2022") +
  labs(x = "Trimestre", caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
  tema_euzen(size_var = 40, font_var = "Roboto") +
  scale_color_manual(values = c(wesanderson::wes_palette(name = "FantasticFox1", n = 5)[1],
                                wesanderson::wes_palette(name = "FantasticFox1", n = 5)[3])
  ) +
  scale_x_yearqtr(format = "%qT-%y", n  = 10) +
  scale_y_continuous("% de población", labels = scales::percent_format(accuracy = 1L),
                     limits = c(min(df_graf$por) - 0.025,
                                max(df_graf$por) + 0.1)) +
  guides(col = guide_legend(title = "")) +
  theme(
    legend.position = "top"
  ) -> graf

ggsave(paste0(path_output, "ensu_conf_ppm.png"), graf,
       width = 14, height = 6.5)


#* Serie Corrupción
#* 
#* 


files_temp <- files[(grepl("\\_06|\\_12|0920", files, ignore.case = T))];
files_temp <- files_temp[!(grepl("0616|0617|0618|1216|1217|1218", files_temp, ignore.case = T))]

ensu_full <- lapply(files_temp, function(x) {
  
  # x <- files_temp[7]
  
  print(x)
  
  fecha_var <- str_remove(x, "../../00. DATOS/2.5 INEGI/ENSU/SERIE/ENSU-CB/ENSU_CB_") 
  
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
    mutate(var_id = "nacional",
           var_guad = ifelse(cve_ent == "14" & 
                               cve_mun %in% c("039"),
                             "guadalajara", "otro")
    ) -> temp
  
  print(length(temp$var_guad[temp$var_guad=="guadalajara"]))
  
  design <- svydesign(ids = ~upm_dis, weights = ~fac_sel, strata = ~est_dis, data = temp)
  
  vars <- c("var_id", "var_guad")
  
  funion <- function(z) {
    
    as_tibble(svyby(~bp3_6, by = as.formula(paste0("~", z)), design, svytotal, na.rm=T, vartype = "cvpct"))
    
  }
  
  output <- lapply(vars, funion)
  
  output %>%
    reduce(full_join) %>%
    mutate(var_id = coalesce(var_id, var_guad)) %>%
    filter(var_id %in% c("nacional", "guadalajara")) %>%
    select(-matches("cv"), -var_guad) %>% 
    pivot_longer(cols = matches("bp3_6"),
                 names_to = "var", 
                 values_to = "total")  %>% 
    mutate(
      resp = str_sub(var, nchar(var)),
      var = substr(var, 1, nchar(var) - 1)) %>% 
    group_by(var, var_id) %>% 
    mutate(
      tot = sum(total, na.rm = T),
      por = total / tot,
      resp = factor(case_when(
        resp == "1" ~ "Sí",
        resp == "2" ~ "No",
        T ~ "Ns/Nc"), levels = c(
          "Sí", "No", "Ns/Nc"
        )),
      fecha = as.Date(paste("01", substr(fecha_var, start = 1, stop = 2), 
                            substr(fecha_var, start = 3, stop = 4), sep = "-"), 
                      format = "%d-%m-%y"),
      var_id=recode(var_id, "guadalajara"="Guadalajara",
                    "nacional"="Nacional")) -> df_graf
  
})


ensu_full %>% 
  reduce(full_join) %>%
  filter(resp =="Sí") %>%
  mutate(trim = as.yearqtr(fecha)) -> df_graf

ggplot(
  df_graf,
  aes(x = trim, y = por, col = var_id)
) +
  geom_line(aes(group = var_id), size = 1.5) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = percent(por, accuracy = 0.1)),
                  size = 8, fontface = "bold", family = "Montserrat", nudge_y = +.02,
                  segment.size = 0.25, show.legend = F, point.padding = 2) +
  ggtitle("Corrupción por parte de autoridades de Seguridad Pública",
          "% población ha estado involucrado en corrupción en los últimos 6 meses, 4T-2022") +
  labs(x = "", caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
  tema_euzen(size_var = 20, font_var = "Roboto") +
  scale_color_manual(values = c(wesanderson::wes_palette(name = "FantasticFox1", n = 5)[1],
                                wesanderson::wes_palette(name = "FantasticFox1", n = 5)[3])
  ) +
  scale_x_yearqtr(format = "%qT-%y", n=7) + # limit=as.yearqtr(as.Date(c("2019-03-01", "2022-06-01")))
  scale_y_continuous("% de población", labels = scales::percent_format(accuracy = 1L)) +
  guides(col = guide_legend(title = "")) +
  theme(
    legend.position = "top"
  ) -> graf

ggsave(paste0(path_output, "ensu_corruphist.png"), graf,
       width = 14, height = 7)



### Barras de último trimestre
# 
# 
# ensu_full %>% 
#   reduce(full_join) %>% 
#   mutate(
#     var_id = case_when(
#       var_id == "guadalajara" ~ "Guadalajara",
#       var_id == "nacional" ~ "Nacional"
#     )) %>%
#   group_by(var_id, resp, fecha) %>%
#   summarise(por=sum(por)) %>%
#   filter(fecha == "2022-09-01") -> df_graf
# 
# 
# ggplot(df_graf,
#        aes(x = var_id,
#            y=por,
#            fill=fct_rev(resp)))+
#   geom_bar(stat = "identity", position = position_fill()) +
#   geom_text(aes(label=ifelse(por<.1, NA, percent(por, .1))), 
#             position = position_fill(vjust = .5),
#             fontface = "bold", family = "Montserrat", size = 4,
#             show.legend = F)+
#   labs(title="Percepción de efecividad del gobierno para resolver los problemas \nde la ciudad en el 4T-2021 (Nacional contra ZM de Guadalajara)",
#        subtitle = "% de población mayor de 18 años que contestaron",
#        x="") +
#   labs(caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
#   tema_euzen(size_var = 24, font_var = "Roboto") +
#   scale_fill_manual(values = c( "grey20", met.brewer(name = "OKeeffe1", n = 2))) +
#   scale_y_continuous("% de población que contestaron", 
#                      labels = scales::percent_format(accuracy = 1L)) +
#   theme(legend.position = "top",
#         legend.title = element_blank())-> graf
# 
# ggsave(paste0(path_output, "ensu_efectividad_ulttrim.png"), graf,
#        width = 8, height = 10)
# 


