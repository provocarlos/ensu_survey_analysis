
dir.create("01_input") 
dir.create("02_scripts")
dir.create("03_grafs")
dir.create("04_temp")

pacman::p_load(tidyverse, scales, purrr, lubridate, janitor, zoo, cowplot, ggrepel, numform, leaflegend, htmltools, ggpol, doBy, viridis, foreign, reldist, cartography, 
               RPostgreSQL, yaml, sf, grid, gridExtra, leaflet, rpostgis, googlesheets4, sp, treemapify, ggbeeswarm, ggthemes, shapefiles, stargazer, rgeos, biscale,
               ggalluvial, htmltools, htmlwidgets, stringr, reshape2, gt, wordcloud, SnowballC, tm, cluster, rgdal, grid, survey, MetBrewer, srvyr, readxl, sysfonts)

Sys.setlocale(locale = "es_ES.UTF-8")

source("../../01. SCRIPTS/tema_euzen.R")

path_output <- "03_grafs/"
path_temp <- "04_temp/"

font_add_google(name = "Poppins", family = "Poppins")
font_add_google(name = "Roboto", family = "Roboto")
showtext_auto()

options(survey.adjust.domain.lonely = TRUE)
options(survey.lonely.psu = "adjust")


mun_shp <- read_sf("01_input/Munis/muni_2018gw.shp") %>% 
  clean_names() %>% 
  as_tibble() %>%
  filter(cve_ent=="14")

#* Cargar bases----
files <- list.files(path = "../../00. DATOS/2.5 INEGI/ENSU/SERIE/ENSU-CB", pattern = "*.dbf", full.names = T, recursive = T)

######################################################### PERCEPCION DE INSEGURIDAD #########################################################

# Percepcion Guadalajara vs Nacional----


#* Limpiar bases----
lapply(files, function(x) {
  
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
      var_id = "nacional",
      var_gdl = case_when(cve_ent == "14" & cve_mun == "039" ~ "Guadalajara", 
                          grepl("16$|17$|0318", fecha_var, ignore.case = T) & cd == 24 ~ "Guadalajara",
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
      var_id = case_when(
        var_id == "Guadalajara" ~ "Guadalajara",
        var_id == "nacional" ~ str_to_title(var_id)
      ),
      bp1_1 = as.numeric(bp1_1),
      fecha = as.Date(paste("01", substr(fecha_var, start = 1, stop = 2), 
                            substr(fecha_var, start = 3, stop = 4), sep = "-"), 
                      format = "%d-%m-%y")
    ) %>% 
    drop_na(var_id) %>% 
    select(-matches("cv|var_gdl")) %>% 
    filter(bp1_1 == "2") -> data
  
}) -> ensu_full

ensu_full %>% 
  reduce(full_join) %>%
  mutate(trim=zoo::as.yearqtr(fecha))-> df_percp


# Serie de tiempo----
ggplot(
  df_percp,
  aes(x = trim, y = por, col = var_id)
) +
  geom_line(aes(group = var_id),
            alpha = 0.5, size = 1.5) +
  geom_point(alpha = 0.6, size = 3) +
  geom_text_repel(aes(label = percent(por, accuracy = 0.1)),
                  angle = 90, size = 15, fontface = "bold", family = "Montserrat", nudge_y = .05,
                  segment.size = 0.25, show.legend = F, force = 2) +
  ggtitle(" % de la población mayor de 18 años que se siente insegura",
          "1T-2016 a 4T-2022") +
  labs(x = "Trimestre", caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI\nNota: 1) Se omite el valor del segundo trimestre de 2020, por la pandemia ocasionada por el COVID-19\n2) Los datos previos al segundo trimestre de 2018 de Guadalajara son respecto a la Zona Metropolitana") +
  tema_euzen(size_var = 40, font_var = "Roboto", lineheight_var = .6) +
  scale_color_manual(values = c(wesanderson::wes_palette(name = "FantasticFox1", n = 5)[1],
                                wesanderson::wes_palette(name = "FantasticFox1", n = 5)[3])
  ) +
  scale_x_yearqtr(format = "%Y-T%q", n  = 8, limit = as.yearqtr(c("2016-01-01", "2022-12-01")) ) +
  scale_y_continuous("% de población", labels = scales::percent_format(accuracy = 1L),
                     limits = c(min(df_percp$por) - 0.025,
                                max(df_percp$por) + 0.1)) +
  guides(col = guide_legend(title = "")) +
  theme(
    legend.position = "top",
  ) -> graf
graf
ggsave(paste0(path_output, "ensu_perc.png"), graf,
       width = 16, height = 7)


# Percepcion en la ZMG --- 


foreign::read.dbf("../../00. DATOS/2.5 INEGI/ENSU/SERIE/ENSU-CB/ENSU_CB_1222.dbf") %>%
  clean_names() %>% 
  filter(
    cve_ent == "14",
    cve_mun %in% c("039", "097", 
                   "098", "101", 
                   "120")
  ) %>% 
  select(cve_ent, cve_mun, nom_mun, 
         upm_dis, est_dis, fac_sel, 
         bp1_1) %>% 
  mutate(nom_mun=recode(nom_mun, "TONALA"="TONALÁ",
                        "TLAJOMULCO DE ZUNIGA"="TLAJOMULCO DE ZÚÑIGA")) %>%
  srvyr::as_survey_design(ids = upm_dis, strata = est_dis, weights = fac_sel) %>% 
  filter(!is.na(bp1_1)) %>% 
  group_by(cve_ent, cve_mun, nom_mun, bp1_1) %>% 
  summarise(por = survey_mean(vartype = "cv"), 
            total = survey_total(vartype = "cv")) %>% 
  filter(bp1_1 == 2) %>% 
  left_join(mun_shp, by=c("cve_ent", "cve_mun")) %>% 
  st_as_sf() %>% 
  rename(nom_mun=nom_mun.x) %>%
  st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs") -> base_mapa

base_mapa$nom_mun <- str_wrap(base_mapa$nom_mun, 10)

#* Mapa----
ggplot() +
  geom_sf(data = base_mapa,
          aes(geometry = geometry,
              fill = por),
          size = 0.75, col = "darkgray",
          alpha = 0.7) +
  ggsflabel::geom_sf_text_repel(
    data = base_mapa,
    aes(label = paste0(str_to_title(nom_mun), "\n(", percent(por, accuracy = 0.1), ")")),
    fontface = "bold", family = "Montserrat", size = 15, lineheight = .4,
    # vjust = ifelse(grepl("san pedro", base_mapa$nom_mun, ignore.case = T), 1.125, 0.7),
    hjust = 0.5) +
  ggtitle("% de población mayor de 18 años que se siente insegura",
          "ZM de Guadalajara, 4T-2022") +
  labs(caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
  coord_sf() +
  tema_euzen(size_var = 50, font_var = "Roboto", is.map = T) +
  scale_color_gradient(low = "#ffeda0", high = "#f03b20",
                       aesthetics = "fill", labels = percent) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.title = element_blank(),
    axis.line = element_blank(), 
    axis.ticks = element_blank()) -> mapa

ggsave(paste0(path_output, "ensu_perc_zm_guad.png"), mapa,
       width = 9, height = 8)

## Lugar donde se siente inseguro  GDL vs NACIONAL

### Limpiar bases----
files_temp <- files[grep("1222", files, ignore.case = T)];

ensu_full <- lapply(files_temp, function(x) {
  
  print(x)
  
  fecha <- str_remove(x, "../../00. DATOS/2.5 INEGI/ENSU/SERIE/ENSU-CB/ENSU_CB_") 
  
  foreign::read.dbf(x) %>%
    clean_names() %>% 
    mutate(
      var_id = "nacional",
      var_guad = ifelse(cve_ent == "14" & 
                          cve_mun %in% c("039"),
                        "Guadalajara", "otro")
    ) -> temp
  
  design <- svydesign(ids = ~upm_dis, weights = ~fac_sel, strata = ~est_dis, data = temp)
  
  vars <- c("var_id", "var_guad")
  
  funion <- function(z) {
    
    as_tibble(svyby(~bp1_2_01 + bp1_2_02 + bp1_2_03 +
                      bp1_2_04 + bp1_2_05 + bp1_2_06 +
                      bp1_2_07 + bp1_2_08 + bp1_2_09 +
                      bp1_2_10 + bp1_2_11 + bp1_2_12, 
                    by = as.formula(paste0("~", z)), design, svytotal, vartype = "cvpct"))
    
  }
  
  output <- lapply(vars, funion)
  
  output %>% 
    reduce(full_join) %>% 
    select(-matches("cv")) %>% 
    pivot_longer(cols = matches("bp1"),
                 names_to = "var", 
                 values_to = "total") %>% 
    mutate(var_id = coalesce(var_guad, var_id),
           resp = str_sub(var, nchar(var)),
           var = substr(var, 1, nchar(var) - 1)) %>% 
    filter(var_id != "otro", 
           resp != "3") %>% 
    select(-matches("guad")) %>% 
    group_by(var_id, var) %>% 
    mutate(
      tot = sum(total, na.rm = T),
      por = total / tot,
      fecha = as.Date(paste("01", substr(fecha, start = 1, stop = 2), 
                            substr(fecha, start = 3, stop = 4), sep = "-"), 
                      format = "%d-%m-%y"),
      var = str_wrap(case_when(
        var == "bp1_2_01" ~ "Casa",
        var == "bp1_2_02" ~ "Trabajo",
        var == "bp1_2_03" ~ "Calles que habitualmente usa",
        var == "bp1_2_04" ~ "Escuela",
        var == "bp1_2_05" ~ "Mercado",
        var == "bp1_2_06" ~ "Centro comercial",
        var == "bp1_2_07" ~ "Banco",
        var == "bp1_2_08" ~ "Cajeros automáticos",
        var == "bp1_2_09" ~ "Transporte público",
        var == "bp1_2_10" ~ "Automóvil",
        var == "bp1_2_11" ~ "Carretera",
        var == "bp1_2_12" ~ "Parque o centro recreativo"
      ), 10)
    ) %>% 
    ungroup() %>% 
    filter(resp == "2") -> data
  
})

ensu_full %>% 
  reduce(full_join) %>% 
  mutate(
    var_id = str_to_title(var_id)
  ) -> df_graf

#* Barras de sensación de inseguridad por tipo----
df_graf %>% 
  filter(grepl("Guadalajara", var_id, ignore.case = T)) %>% 
  mutate(rank = rank(-por, ties.method = "first"),
         cut_var = case_when(
           rank >= min(rank) & rank < median(rank) ~ "primeros",
           rank >= median(rank) & rank <= max(rank) ~ "ultimos",
         )) %>% 
  select(var, cut_var) -> aux_orden

df_temp <- left_join(df_graf, aux_orden)

axis_lim <- max(df_temp$por + 0.05)

funico <- unique(df_temp$cut_var)

for (i in funico) {
  
  # i = funico[1]
  
  print(i)
  
  df_temp %>% 
    filter(cut_var == i) -> bar_temp
  
  bar_temp %>% 
    filter(grepl("Guadalajara", var_id, ignore.case = T)) %>% 
    arrange(-por) %>% 
    pull(var) %>% 
    unique() -> orden_var
  
  bar_temp$var <- factor(bar_temp$var, levels = orden_var)
  
  ggplot(
    bar_temp,
    aes(x = por, y = fct_rev(as.factor(var)), fill = fct_rev(var_id), group = fct_rev(var_id))
  ) +
    geom_bar(aes(),
             stat = "identity", position = "dodge2") +
    geom_text(aes(label = percent(por, accuracy = 0.1)),
              fontface = "bold", family = "Montserrat", size = 15, hjust = -0.5,
              position = position_dodge2(width = 0.9)) +
    ggtitle("% de población mayor de 18 años que se\nsiente insegura en espacio público",
            "Nacional vs. Guadalajara, 4T-2022") +
    labs(y = "", caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
    tema_euzen(size_var = 45, font_var = "Roboto") +
    scale_alpha_manual(values = c(0.6, 0.9)) +
    scale_fill_manual(values = c(rev(met.brewer(name = "OKeeffe1", n = 2)[-3:-4]), "grey20")) +
    scale_x_continuous("% de población", labels = scales::percent_format(accuracy = 1L), limits = c(0, axis_lim)) +
    guides(fill = guide_legend(reverse = T),
           alpha = guide_legend(reverse = T)) +
    theme(
      legend.title = element_blank(),
      legend.position = "bottom"
    ) -> graf
  
  ggsave(paste0(path_output, "ensu_sens_inseg_", i,".png"), graf,
         width = 11.5, height = 10.5)
  
}


## Lugar donde se siente inseguro  GDL trimestres 

### Limpiar bases----
files_temp <- files[grep("1222|1221|0922", files, ignore.case = T)];

ensu_full <- lapply(files_temp, function(x) {
  
  print(x)
  
  fecha <- str_remove(x, "../../00. DATOS/2.5 INEGI/ENSU/SERIE/ENSU-CB/ENSU_CB_") 
  
  foreign::read.dbf(x) %>%
    clean_names() %>% 
    mutate(
      var_id = ifelse(cve_ent == "14" & 
                          cve_mun %in% c("039"),
                        "Guadalajara", "otro")
    ) -> temp
  
  design <- svydesign(ids = ~upm_dis, weights = ~fac_sel, strata = ~est_dis, data = temp)
  
  vars <- c("var_id")
  
  funion <- function(z) {
    
    as_tibble(svyby(~bp1_2_01 + bp1_2_02 + bp1_2_03 +
                      bp1_2_04 + bp1_2_05 + bp1_2_06 +
                      bp1_2_07 + bp1_2_08 + bp1_2_09 +
                      bp1_2_10 + bp1_2_11 + bp1_2_12, 
                    by = as.formula(paste0("~", z)), design, svytotal, vartype = "cvpct"))
    
  }
  
  output <- lapply(vars, funion)
  
  output %>% 
    reduce(full_join) %>% 
    select(-matches("cv")) %>% 
    pivot_longer(cols = matches("bp1"),
                 names_to = "var", 
                 values_to = "total") %>% 
    mutate(resp = str_sub(var, nchar(var)),
           var = substr(var, 1, nchar(var) - 1)) %>% 
    filter(var_id != "otro", 
           resp != "3") %>% 
    group_by(var_id, var) %>% 
    mutate(
      tot = sum(total, na.rm = T),
      por = total / tot,
      fecha = as.Date(paste("01", substr(fecha, start = 1, stop = 2), 
                            substr(fecha, start = 3, stop = 4), sep = "-"), 
                      format = "%d-%m-%y"),
      var = str_wrap(case_when(
        var == "bp1_2_01" ~ "Casa",
        var == "bp1_2_02" ~ "Trabajo",
        var == "bp1_2_03" ~ "Calles que habitualmente usa",
        var == "bp1_2_04" ~ "Escuela",
        var == "bp1_2_05" ~ "Mercado",
        var == "bp1_2_06" ~ "Centro comercial",
        var == "bp1_2_07" ~ "Banco",
        var == "bp1_2_08" ~ "Cajeros automáticos",
        var == "bp1_2_09" ~ "Transporte público",
        var == "bp1_2_10" ~ "Automóvil",
        var == "bp1_2_11" ~ "Carretera",
        var == "bp1_2_12" ~ "Parque o centro recreativo"
      ), 10)
    ) %>% 
    ungroup() %>% 
    filter(resp == "2") -> data
  
})

ensu_full %>% 
  reduce(full_join) %>% 
  mutate(
    var_id = str_to_title(var_id),
    trim = as.yearmon(fecha)
  ) -> df_graf

#* Barras de sensación de inseguridad por tipo----
df_graf %>% 
  filter(fecha == "2022-09-01") %>% 
  mutate(rank = rank(-por, ties.method = "first"),
         cut_var = case_when(
           rank >= min(rank) & rank < median(rank) ~ "primeros",
           rank >= median(rank) & rank <= max(rank) ~ "ultimos",
         )) %>% 
  select(var, cut_var) -> aux_orden

df_temp <- left_join(df_graf, aux_orden)

axis_lim <- max(df_temp$por + 0.05)

funico <- unique(df_temp$cut_var)

for (i in funico) {
  
  # i = funico[1]
  
  print(i)
  
  df_temp %>% 
    filter(cut_var == i) -> bar_temp
  
  bar_temp %>% 
    filter(fecha=="2022-12-01") %>% 
    arrange(-por) %>% 
    pull(var) %>% 
    unique() -> orden_var
  
  bar_temp$var <- factor(bar_temp$var, levels = orden_var)
  
  ggplot(
    bar_temp,
    aes(x = por, y = fct_rev(as.factor(var)), fill = as.factor(trim))
  ) +
    geom_bar(aes(),
             stat = "identity", position = "dodge2") +
    geom_text(aes(label = percent(por, accuracy = 0.1)),
              fontface = "bold", family = "Montserrat", size = 15, hjust = -0.5,
              position = position_dodge2(width = 0.9)) +
    ggtitle("% de población mayor de 18 años que se\nsiente insegura en espacio público",
            "Guadalajara, 4T-2022") +
    labs(y = "", caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
    tema_euzen(size_var = 45, font_var = "Roboto", lineheight_var = .3) +
    scale_alpha_manual(values = c(0.6, 0.9)) +
    scale_fill_manual(values = rev(met.brewer(name = "Peru1", n = 4))) +
    scale_x_continuous("% de población", labels = scales::percent_format(accuracy = 1L), limits = c(0, axis_lim)) +
    guides(fill = guide_legend(reverse = T),
           alpha = guide_legend(reverse = T)) +
    theme(
      legend.title = element_blank(),
      legend.position = "bottom"
    ) -> graf
  
  ggsave(paste0(path_output, "ensu_sens_inseg_", i,".png"), graf,
         width = 8, height = 9)
  
}


# Situaciones alrededor de vivienda----

#* Limpiar bases----
#* 


ensu_full <- lapply(files, function(x) {
  
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
      var_id = "nacional",
      var_guad = case_when(
        cve_ent == "14" & cve_mun == "039" ~ "Guadalajara", 
        grepl("16$|17$|0318", fecha, ignore.case = T) & cd == 24 ~ "Guadalajara",
        T ~ "otro")
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
    filter(var_id != "otro",
           resp == "1") %>% 
    select(-matches("guad"))  -> data
  
})

ensu_full %>% 
  reduce(full_join) %>%
  mutate(
    var_id = str_to_title(var_id)) %>% 
  filter(grepl("Guadalajara", var_id, ignore.case = T)) -> df_atesti

#* Serie de tiempo Nacional vs Guadalajara, situaciones cercanas----
df_atesti %>% 
  filter(grepl("Guadalajara", var_id, ignore.case = T)) %>% 
  group_by(var) %>% 
  arrange(-por, .by_group = T) %>% 
  filter(fecha=="2022-09-01") %>%  #slice(1)
  ungroup() %>% 
  mutate(
    rank = rank(-por, ties.method = "first")) %>% 
  select(var, rank) %>%
  arrange(rank) -> aux_corte

df_temp <- left_join(df_atesti, aux_corte)

funico <- unique(df_temp$var)

for (i in funico) {
  
  # i = funico[1]
  
  print(i)
  
  df_temp %>% 
    filter(var == i) %>%
    mutate(trim = as.yearqtr(fecha))-> temp
  

  ggplot(
    temp,
    aes(x = trim, y = por, col = var_id, group = 1)
  ) +
    geom_line( size = 3, alpha = .7) +
    geom_point(size = 4) +
    geom_text_repel(
      aes(label = percent(por, accuracy = 0.1)),
      fontface = "bold", family = "Montserrat", size = 15,
      hjust = -1.1, vjust =  0.5, angle = 90, segment.size  = 0.2,
      show.legend = F) +
    facet_wrap(~reorder(var, -por), 
               ncol = 2) +
    ggtitle("Atestiguación de delitos y conductas antisociales",
            "Guadalajara, 4T-2022") +
    labs(caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
    tema_euzen(font_var = "Poppins", size_var = 46) +
    scale_size_manual(values = c(3.5, 1.5)) +
    scale_alpha_manual(values = c(0.8, 0.6)) +
    scale_color_manual(values = c(rev(met.brewer(name = "OKeeffe1", n = 1)[-3:-4]), "grey20")) +
    scale_x_yearqtr("Trimestre", format = "%Y-T%q", n  = 10) +
    scale_y_continuous("% de población", labels = scales::percent_format(accuracy = 1L), limits = c(.1, .9)) + #limits = c(min(temp$por), max(temp$por + 0.025))
    guides(size = "none",
           col = "none", 
           alpha = "none") +
    theme(legend.position = "bottom",
          legend.title = element_blank(), strip.text = 
    ) -> graf
  
  ggsave(paste0(path_output, "ensu_atesti_", aux_corte$rank[aux_corte$var==i],i, ".png"), graf,
         width = 18, height = 9)
  
}


## PERCEPCIÓN VS ATESTIGUACIÓN


df_perc_ates <- df_atesti %>%
  group_by(var_id, fecha) %>%
  summarise(mean_ates=mean(por)) %>%
  left_join(df_percp, by=c("var_id", "fecha")) %>%
  select(-var) %>%
  pivot_longer(cols = c(mean_ates, por), names_to = "var", values_to = "value") %>%
  mutate(var=recode(var, "mean_ates"="Atestiguación de delitos (promedio)", "por"="Percepción de inseguridad")) %>%
  select(-c(bp1_1, total))

# ggplot(
#   df_perc_ates,
#   aes(x = trim, y = value, col = var)
# ) +
#   geom_line(aes(group = var), show.legend = F, alpha=.8, size=2) +
#   geom_point(alpha=.8, size=2) +
#   # geom_vline(xintercept = as.Date("2020-12-01"), 
#   #            linetype = "dotdash", size = 1, alpha = 0.75, col = "darkgray") +
#   # geom_text(aes(x = as.Date("2021-03-01"), y = .85),
#   #           label = "2021", size = 8, fontface = "bold", family = "Montserrat", col = "black") +
#   geom_text_repel(
#     aes(label = percent(value, accuracy = 0.1)),
#     fontface = "bold", family = "Montserrat", size = 4.5, hjust = -1.1, vjust =  0.5, angle = 90, segment.size  = 0.2,
#     show.legend = F) +
#   geom_smooth(method = "lm", se = F, show.legend = F)+
#   ggtitle("Percepción de inseguridad contra atestiguación de delitos",
#           "Guadalajara, 1T-2016 a 3T-2022)") +
#   labs(caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
#   tema_euzen(size_var = 24, font_var = "Roboto") +
#   scale_alpha_manual(values = c(0.8, 0.6)) +
#   scale_color_manual(values = c(rev(met.brewer(name = "OKeeffe1", n = 1)[-3:-4]), "grey20")) +
#   scale_x_yearqtr(format = "%Y-T%q", n  = 10) +
#   scale_y_continuous("% de población", labels = scales::percent_format(accuracy = 1L), limits = c(.1, .9)) + #limits = c(min(temp$por), max(temp$por + 0.025))
#   guides(alpha = "none") +
#   theme(legend.position = "top",
#         legend.title = element_blank()) -> graf
# 
# ggsave(paste0(path_output, "ensu_atesti_perc.png"), graf,
       # width = 18, height = 9)

# Cambios de habitos GDL vs NACIONAL----

#* Limpiar bases----
files_temp <- files[grep("1222", files, ignore.case = T)];

ensu_full <- lapply(files_temp, function(x) {
  
  print(x)
  
  fecha <- str_remove(x, "../../00. DATOS/2.5 INEGI/ENSU/SERIE/ENSU-CB/ENSU_CB_") 
  
  foreign::read.dbf(x) %>%
    clean_names() %>% 
    mutate(
      var_id = "nacional",
      var_guad = ifelse(cve_ent == "14" & 
                          cve_mun %in% c("039"),
                        "Guadalajara", "otro")
    ) -> temp
  
  design <- svydesign(ids = ~upm_dis, weights = ~fac_sel, strata = ~est_dis, data = temp)
  
  vars <- c("var_id", "var_guad")
  
  funion <- function(z) {
    
    as_tibble(svyby(~bp1_5_1 + bp1_5_2 + bp1_5_3 + bp1_5_4 + bp1_5_5, 
                    by = as.formula(paste0("~", z)), design, svytotal, vartype = "cvpct"))
    
  }
  
  output <- lapply(vars, funion)
  
  output %>% 
    reduce(full_join) %>% 
    select(-matches("cv")) %>% 
    pivot_longer(cols = matches("bp1"),
                 names_to = "var", 
                 values_to = "total") %>% 
    mutate(var_id = coalesce(var_guad, var_id),
           resp = str_sub(var, nchar(var)),
           var = substr(var, 1, nchar(var) - 1)) %>% 
    filter(var_id != "otro", 
           resp != "3") %>% 
    select(-matches("guad")) %>% 
    group_by(var_id, var) %>% 
    mutate(
      tot = sum(total, na.rm = T),
      por = total / tot,
      fecha = as.Date(paste("01", substr(fecha, start = 1, stop = 2), 
                            substr(fecha, start = 3, stop = 4), sep = "-"), 
                      format = "%d-%m-%y"),
      var = str_wrap(case_when(
        var == "bp1_5_1" ~ "Llevar cosas de valor",
        var == "bp1_5_2" ~ "Caminar de noche en alrededores de su vivienda",
        var == "bp1_5_3" ~ "Visitar parientes o amigos",
        var == "bp1_5_4" ~ "Permitir que menores salgan de su vivienda",
        var == "bp1_5_5" ~ "Otro"
      ), 15)
    ) %>% 
    ungroup() %>% 
    filter(resp == "1") -> data
  
})

ensu_full %>% 
  reduce(full_join) %>% 
  mutate(
    var_id = str_to_title(var_id)
  )  -> df_temp

df_temp %>% 
  filter(grepl("guad", var_id, ignore.case = T)) %>% 
  arrange(-por) %>% 
  pull(var) %>% 
  unique() -> orden_var

df_temp$var <- factor(df_temp$var, levels = orden_var)

ggplot(
  df_temp,
  aes(x = as.factor(var), y = por, fill = var_id, group = var_id)
) +
  geom_bar(
    stat = "identity", position = "dodge2") +
  geom_text(aes(label = percent(por, accuracy = 0.1)),
            fontface = "bold", family = "Montserrat", size = 15, vjust = -0.5,
            position = position_dodge2(width = 0.9)) +
  ggtitle("Cambio de hábitos por temor a la delincuencia\nNacional vs. Guadalajara",
          subtitle="% de personas que dejaron de realizar dicha actividad, 4T-2022") +
  labs(x = "", caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
  tema_euzen(size_var = 40, font_var = "Roboto") +
  scale_alpha_manual(values = c(0.9, 0.6)) +
  scale_fill_manual(values = c(met.brewer(name = "OKeeffe1", n = 2)[-3:-4], "grey20")) +
  scale_y_continuous("", labels = scales::percent_format(accuracy = 1L)) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  ) -> graf

ggsave(paste0(path_output, "ensu_cam_ruti.png"), graf,
       width = 12, height = 10)

# Cambios de habitos----

#* Limpiar bases----
files_temp <- files[grep("1222|1221|0922", files, ignore.case = T)];

ensu_full <- lapply(files_temp, function(x) {
  
  print(x)
  
  fecha <- str_remove(x, "../../00. DATOS/2.5 INEGI/ENSU/SERIE/ENSU-CB/ENSU_CB_") 
  
  foreign::read.dbf(x) %>%
    clean_names() %>% 
    mutate(
      var_guad = ifelse(cve_ent == "14" & 
                          cve_mun %in% c("039"),
                        "Guadalajara", "otro")
    ) -> temp
  
  design <- svydesign(ids = ~upm_dis, weights = ~fac_sel, strata = ~est_dis, data = temp)
  
  vars <- c("var_guad")
  
  funion <- function(z) {
    
    as_tibble(svyby(~bp1_5_1 + bp1_5_2 + bp1_5_3 + bp1_5_4 + bp1_5_5, 
                    by = as.formula(paste0("~", z)), design, svytotal, vartype = "cvpct"))
    
  }
  
  output <- lapply(vars, funion)
  
  output %>% 
    reduce(full_join) %>% 
    select(-matches("cv")) %>% 
    pivot_longer(cols = matches("bp1"),
                 names_to = "var", 
                 values_to = "total") %>% 
    mutate(resp = str_sub(var, nchar(var)),
           var = substr(var, 1, nchar(var) - 1)) %>% 
    filter(var_guad != "otro", 
           resp != "3") %>% 
    group_by(var_guad, var) %>% 
    mutate(
      tot = sum(total, na.rm = T),
      por = total / tot,
      fecha = as.Date(paste("01", substr(fecha, start = 1, stop = 2), 
                            substr(fecha, start = 3, stop = 4), sep = "-"), 
                      format = "%d-%m-%y"),
      var = str_wrap(case_when(
        var == "bp1_5_1" ~ "Llevar cosas de valor",
        var == "bp1_5_2" ~ "Caminar de noche en alrededores de su vivienda",
        var == "bp1_5_3" ~ "Visitar parientes o amigos",
        var == "bp1_5_4" ~ "Permitir que menores salgan de su vivienda",
        var == "bp1_5_5" ~ "Otro"
      ), 15)
    ) %>% 
    ungroup() %>% 
    filter(resp == "1",
           var != "Otro") -> data
  
})

ensu_full %>% 
  reduce(full_join) %>% 
  mutate(
    trim = factor(as.yearmon(fecha))
  )  -> df_temp

df_temp %>% 
  filter(fecha=="2022-12-01") %>% 
  arrange(-por) %>% 
  pull(var) %>% 
  unique() -> orden_var

df_temp$var <- factor(df_temp$var, levels = orden_var)

ggplot(
  df_temp,
  aes(x = as.factor(var), y = por, fill = trim, group = trim)
) +
  geom_bar(
    stat = "identity", position = "dodge2") +
  geom_text(aes(label = percent(por, accuracy = 0.1)),
            fontface = "bold", family = "Montserrat", size = 12, vjust = -0.5,
            position = position_dodge2(width = 0.9)) +
  ggtitle("Cambio de hábitos por temor a la delincuencia\nGuadalajara",
          subtitle="% de personas que dejaron de realizar dicha actividad, 4T-2022") +
  labs(x = "", caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
  tema_euzen(size_var = 40, font_var = "Roboto", lineheight_var = .4) +
  scale_alpha_manual(values = c(0.9, 0.6)) +
  scale_fill_manual(values = rev(c(met.brewer(name = "OKeeffe1", n = 2)[-3:-4], "grey20"))) +
  scale_y_continuous("", labels = scales::percent_format(accuracy = 1L)) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  ) -> graf

ggsave(paste0(path_output, "ensu_cam_ruti.png"), graf,
       width = 8, height = 9)

# Conflictos sociales por ciudad----

#* Limpiar bases----
files_temp <- files[grep("1222", files, ignore.case = T)];

foreign::read.dbf(files_temp) %>% 
  clean_names() %>% 
  filter(cve_ent == "14" & 
           cve_mun %in% c("039", "097", "098",
                          "101", "120")) %>% 
  mutate(
    prob_veci = case_when(
      bp2_2_01 == "1" | bp2_2_03 == "1" | bp2_2_04 == "1" |
        bp2_2_06 == "1" ~ "1",
      T ~ "0"
    ),
    prob_ani = case_when(
      bp2_2_09 == "1" ~ "1",
      T ~ "0"
    ),
    prob_estac = case_when(
      bp2_2_05 == "1" ~ "1",
      T ~ "0"
    ),
    prob_auto = case_when(
      bp2_2_15 == "1" ~ "1",
      T ~ "0"
    ),
    prob_chism = case_when(
      bp2_2_08 == "1" ~ "1",
      T ~ "0"
    ),
    prob_trans = case_when(
      bp2_2_02 == "1" ~ "1",
      T ~ "0"
    ),
    prob_borra = case_when(
      bp2_2_10 == "1" ~ "1",
      T ~ "0"
    ),
    prob_graf = case_when(
      bp2_2_16 == "1" ~ "1",
      T ~ "0"
    )
  ) -> temp

design <- svydesign(ids = ~upm_dis, weights = ~fac_sel, strata = ~est_dis, data = temp)

as_tibble(svyby(~ prob_veci + prob_ani + prob_estac +
                  prob_auto + prob_chism + prob_trans + 
                  prob_borra + prob_graf, 
                by = ~nom_mun, design, svymean, vartype = "cvpct", na.rm = T)) -> output

output %>% 
  select(-matches("cv")) %>% 
  pivot_longer(cols = matches("prob"),
               names_to = "var", 
               values_to = "total") %>% 
  mutate(
    resp = str_sub(var, nchar(var)),
    var = substr(var, 1, nchar(var) - 1),
    var = str_wrap(case_when(
      var == "prob_veci" ~ "Problemas con vecinos",
      var == "prob_ani" ~ "Problemas relacionados con animales domésticos",
      var == "prob_estac" ~ "Problemas de estacionemiento",
      var == "prob_auto" ~ "Problemas con autoridades relacionadas con seguridad pública",
      var == "prob_chism" ~ "Chismes o malos entendidos",
      var == "prob_trans" ~ "Conflictos en transporte público o privado",
      var == "prob_borra" ~ "Molestias por borrachos, drogadictos o pandillas",
      var == "prob_graf" ~ "Grafiti o pintas a su casa"
    ), 20)
  ) %>% 
  filter(resp == "1") -> df_graf

levels(df_graf$nom_mun) <- str_wrap(levels(df_graf$nom_mun), 20)

#* Heatmap de conflictos----
ggplot(
  df_graf,
  aes(x = reorder(var, -total), y = reorder(nom_mun, total), fill = total)
) +
  geom_tile(color = "black") +
  geom_text(aes(label = scales::percent(total, accuracy = 0.1)),
            fontface = "bold", size = 15, family = "Montserrat") +
  ggtitle(" % de población que experimentó un conflicto, por motivo que generó conflicto",
          "Ciudades de ZM de Guadalajara, 4T 2022") +
  labs(x = "Motivos", y = "", caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
  tema_euzen(size_var = 40, font_var = "Roboto", lineheight_var = .4) +
  scale_fill_continuous(low = "#ffeda0", high = "#f03b20", name = "% de personas", 
                        labels = percent, breaks = c(0.1, 0.4, 0.7)) +
  guides(fill = "none") -> graf

ggsave(paste0(path_output, "ensu_conflict.png"), graf,
       width = 16, height = 9)

# Conflicto de GDL - Serie de Tiempo

files_temp <- files[(grep("18\\.|19\\.|20\\.|21\\.|22\\.", files, ignore.case = T))];

lapply(files_temp, function(x) {
  
  # x <- files_temp[19]
  
  print(x)
  
  fecha <- str_remove(x, "../../00. DATOS/2.5 INEGI/ENSU/SERIE/ENSU-CB/ENSU_CB_") 
  
  foreign::read.dbf(x) %>% 
    clean_names() %>% 
    filter(cve_ent == "14" & 
             cve_mun %in% c("039")) %>% 
    mutate(
      prob_veci = case_when(
        bp2_2_01 == "1" | bp2_2_03 == "1" | bp2_2_04 == "1" |
          bp2_2_06 == "1" ~ "1",
        T ~ "0"
      ),
      prob_ani = case_when(
        bp2_2_09 == "1" ~ "1",
        T ~ "0"
      ),
      prob_estac = case_when(
        bp2_2_05 == "1" ~ "1",
        T ~ "0"
      ),
      prob_auto = case_when(
        bp2_2_15 == "1" ~ "1",
        T ~ "0"
      ),
      prob_chism = case_when(
        bp2_2_08 == "1" ~ "1",
        T ~ "0"
      ),
      prob_trans = case_when(
        bp2_2_02 == "1" ~ "1",
        T ~ "0"
      ),
      prob_borra = case_when(
        bp2_2_10 == "1" ~ "1",
        T ~ "0"
      ),
      prob_graf = case_when(
        bp2_2_16 == "1" ~ "1",
        T ~ "0"
      )
    ) -> temp
  
  design <- svydesign(ids = ~upm_dis, weights = ~fac_sel, strata = ~est_dis, data = temp)
  
  as_tibble(svyby(~ prob_veci + prob_ani + prob_estac +
                    prob_auto + prob_chism + prob_trans + 
                    prob_borra + prob_graf, 
                  by = ~nom_mun, design, svymean, vartype = "cvpct", na.rm = T)) -> output
  
  output %>% 
    select(-matches("cv")) %>% 
    pivot_longer(cols = matches("prob"),
                 names_to = "var", 
                 values_to = "total") %>% 
    mutate(
      fecha = as.Date(paste("01", substr(fecha, start = 1, stop = 2), 
                            substr(fecha, start = 3, stop = 4), sep = "-"), 
                      format = "%d-%m-%y"),
      resp = str_sub(var, nchar(var)),
      var = substr(var, 1, nchar(var) - 1),
      var = str_wrap(case_when(
        var == "prob_veci" ~ "Problemas con vecinos",
        var == "prob_ani" ~ "Problemas relacionados con animales domésticos",
        var == "prob_estac" ~ "Problemas de estacionemiento",
        var == "prob_auto" ~ "Problemas con autoridades relacionadas con seguridad pública",
        var == "prob_chism" ~ "Chismes o malos entendidos",
        var == "prob_trans" ~ "Conflictos en transporte público o privado",
        var == "prob_borra" ~ "Molestias por borrachos, drogadictos o pandillas",
        var == "prob_graf" ~ "Grafiti o pintas a su casa"
      ), 20)
    ) %>% 
    filter(resp == "1") -> df_graf
}) -> ensu_full

ensu_full %>%
  reduce(full_join) %>%
  mutate(nom_mun = str_to_title(nom_mun),
         trim = as.yearmon(fecha),
         alphavar = case_when(var %in% c("Problemas con\nvecinos", "Problemas de\nestacionemiento", "Problemas\nrelacionados con\nanimales domésticos") ~ "Color",
                              T ~ "NoColor")) -> df_graf


#* Serie de timepo de conflictos----
ggplot(
  df_graf,
  aes(x = trim, y = total, color = reorder(var, -total), alpha = alphavar)
) +
  geom_line(size = 3) +
  geom_point(size = 5) +
  geom_text(data = df_graf %>% 
              filter(fecha == as.Date("2022-12-01")),
            aes(label = scales::percent(total, accuracy = 0.1)),
            fontface = "bold", size = 15, family = "Montserrat", nudge_x = .2) +
  ggtitle(" % de población que experimentó un conflicto, por motivo que generó conflicto",
          "Guadalajara, 4T 2022") +
  labs(x = "Trimestre", y = "", caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
  tema_euzen(size_var = 40, font_var = "Roboto", lineheight_var = .4) +
  scale_fill_continuous(low = "#ffeda0", high = "#f03b20", name = "% de personas", 
                        labels = percent, breaks = c(0.1, 0.4, 0.7)) +
  scale_color_manual(values = met.brewer("Paquin"), 8) +
  scale_alpha_manual(values = c(1, .5)) +
  scale_y_continuous(label = percent) +
  guides(alpha = "none") -> graf

ggsave(paste0(path_output, "ensu_conflicto_gdl.png"), graf,
       width = 16, height = 9)

# Principales problemas en su ciudad----

#* Limpiar bases----
files_temp <- files[grep("1222|1221|0922", files, ignore.case = T)];

ensu_full <- lapply(files_temp, function(x) {
  
  print(x)
  
  fecha <- str_remove(x, "../../00. DATOS/2.5 INEGI/ENSU/SERIE/ENSU-CB/ENSU_CB_") 
  
  foreign::read.dbf(x) %>%
    clean_names() %>% 
    mutate(
      var_guad = ifelse(cve_ent == "14" & 
                          cve_mun %in% c("039"),
                        "Guadalajara", "otro")
    ) -> temp
  
  design <- svydesign(ids = ~upm_dis, weights = ~fac_sel, strata = ~est_dis, data = temp)
  
  as_tibble(svyby(~bp3_1_01 + bp3_1_02 + bp3_1_03 + 
                    bp3_1_04 + bp3_1_05 + bp3_1_06 +
                    bp3_1_07 + bp3_1_08 + bp3_1_09 +
                    bp3_1_10 + bp3_1_11 + bp3_1_12 +
                    bp3_1_13 + bp3_1_14, 
                  by = ~var_guad, design, svymean, vartype = "cvpct", na.rm = T)) -> output
  
  output %>% 
    select(-matches("cv")) %>% 
    pivot_longer(cols = matches("bp3"),
                 names_to = "var", 
                 values_to = "total") %>% 
    mutate(
      resp = str_sub(var, nchar(var)),
      var = substr(var, 1, nchar(var) - 1),
      fecha = as.Date(paste("01", substr(fecha, start = 1, stop = 2), 
                            substr(fecha, start = 3, stop = 4), sep = "-"), 
                      format = "%d-%m-%y"),
      var = str_wrap(case_when(
        var == "bp3_1_01" ~ "Fallas y fugas en suministro de agua",
        var == "bp3_1_02" ~ "Deficiencias en red de drenaje",
        var == "bp3_1_03" ~ "Coladeras tapadas",
        var == "bp3_1_04" ~ "Falta de tratamiento de aguas residuales",
        var == "bp3_1_05" ~ "Alumbrado público insuficiente",
        var == "bp3_1_06" ~ "Recolección de basura ineficiente",
        var == "bp3_1_07" ~ "Mercados en mal estado",
        var == "bp3_1_08" ~ "Embotellamientos frecuentes",
        var == "bp3_1_09" ~ "Problemas de salud por mal manejo de rastros",
        var == "bp3_1_10" ~ "Baches en calles y avenidas",
        var == "bp3_1_11" ~ "Parques y jardínes descuidados",
        var == "bp3_1_12" ~ "Delincuencia",
        var == "bp3_1_13" ~ "Transporte ineficiente",
        var == "bp3_1_14" ~ "Hospitales saturados o ineficientes"
      ), 20)
    ) %>% 
    filter(var_guad != "otro", 
           resp == "1") -> data
  
})

ensu_full %>% 
  reduce(full_join) %>% 
  mutate(
    fecha = zoo::as.yearmon(fecha, format = "%b/%Y")
  ) -> df_graf

df_graf %>%
  spread(fecha, total) %>%
  mutate(dif=(`sep 2022`-`sep 2021`)*100) %>%
  arrange(-dif) -> difproblema

#* Barras de ZM de Guadalajara----
ggplot(
  df_graf,
  aes(x = total, y = reorder(var, total), fill = fct_rev(as.factor(fecha)), group = fct_rev(as.factor(fecha)))
) +
  geom_bar(
    aes(),
    stat = "identity", position = "dodge2") +
  geom_text(aes(label = percent(total, accuracy = 0.1)),
            fontface = "bold", family = "Montserrat", size = 12, hjust = -0.5,
            position = position_dodge2(width = 0.9)) +
  ggtitle("Principales problemas en su ciudad",
          "Guadalajara") +
  labs(y = "", caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
  tema_euzen(size_var = 40, font_var = "Roboto", lineheight_var = .3) +
  scale_fill_manual(values = met.brewer(name = "Peru1", n = 3)) +
  scale_x_continuous("% de población", labels = scales::percent_format(accuracy = 1L), limits = c(0, max(df_graf$total + 0.1))) +
  guides(fill = guide_legend(reverse = T), 
         alpha = guide_legend(reverse = T)) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    axis.title.y = element_text(size = 13)
  ) -> graf

ggsave(paste0(path_output, "ensu_prob.png"), graf,
       width = 8, height = 9.5)


# Principales problemas SERIE DE TIEMPO

files_temp <- files[(grep("1218|19\\.|20\\.|21\\.|22\\.", files, ignore.case = T))];

ensu_full <- lapply(files_temp, function(x) {
  
  print(x)
  
  fecha <- str_remove(x, "../../00. DATOS/2.5 INEGI/ENSU/SERIE/ENSU-CB/ENSU_CB_") 
  
  foreign::read.dbf(x) %>%
    clean_names() %>% 
    mutate(
      var_guad = ifelse(cve_ent == "14" & 
                          cve_mun %in% c("039"),
                        "Guadalajara", "otro")
    ) -> temp
  
  design <- svydesign(ids = ~upm_dis, weights = ~fac_sel, strata = ~est_dis, data = temp)
  
  as_tibble(svyby(~bp3_1_01 + bp3_1_02 + bp3_1_03 + 
                    bp3_1_04 + bp3_1_05 + bp3_1_06 +
                    bp3_1_07 + bp3_1_08 + bp3_1_09 +
                    bp3_1_10 + bp3_1_11 + bp3_1_12 +
                    bp3_1_13 + bp3_1_14, 
                  by = ~var_guad, design, svymean, vartype = "cvpct", na.rm = T)) -> output
  
  output %>% 
    select(-matches("cv")) %>% 
    pivot_longer(cols = matches("bp3"),
                 names_to = "var", 
                 values_to = "total") %>% 
    mutate(
      resp = str_sub(var, nchar(var)),
      var = substr(var, 1, nchar(var) - 1),
      fecha = as.Date(paste("01", substr(fecha, start = 1, stop = 2), 
                            substr(fecha, start = 3, stop = 4), sep = "-"), 
                      format = "%d-%m-%y"),
      var = str_wrap(case_when(
        var == "bp3_1_01" ~ "Fallas y fugas en suministro de agua",
        var == "bp3_1_02" ~ "Deficiencias en red de drenaje",
        var == "bp3_1_03" ~ "Coladeras tapadas",
        var == "bp3_1_04" ~ "Falta de tratamiento de aguas residuales",
        var == "bp3_1_05" ~ "Alumbrado público insuficiente",
        var == "bp3_1_06" ~ "Recolección de basura ineficiente",
        var == "bp3_1_07" ~ "Mercados en mal estado",
        var == "bp3_1_08" ~ "Embotellamientos frecuentes",
        var == "bp3_1_09" ~ "Problemas de salud por mal manejo de rastros",
        var == "bp3_1_10" ~ "Baches en calles y avenidas",
        var == "bp3_1_11" ~ "Parques y jardínes descuidados",
        var == "bp3_1_12" ~ "Delincuencia",
        var == "bp3_1_13" ~ "Transporte ineficiente",
        var == "bp3_1_14" ~ "Hospitales saturados o ineficientes"
      ), 20)
    ) %>% 
    filter(var_guad != "otro", 
           resp == "1") -> data
  
})

ensu_full %>% 
  reduce(full_join) %>% 
  mutate(
    fecha = zoo::as.yearqtr(fecha, format = "%b/%Y")
  ) -> df_graf


#* Serie de ZM de Guadalajara----
ggplot(
  df_graf,
  aes(x = fecha, y = reorder(var, total), fill = total)
) +
  geom_tile(color = "black") +
  geom_text(aes(label = scales::percent(total, accuracy = 1)),
            fontface = "bold", size = 6, family = "Montserrat") +
  ggtitle("% de la población que identificó como principal problema",
          "Guadalajara") +
  labs(x = "Trimestre", y = "", caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
  tema_euzen(font_var = "Roboto", size_var = 20, lineheight_var = .8) +
  scale_x_yearqtr(format = "T%q-%y", n = 6) +
  scale_fill_gradientn(colors = met.brewer("Morgenstern"))+
  guides(fill = "none") -> graf

ggsave(paste0(path_output, "ensu_problemas_serie.png"), graf,
       width = 18, height = 10)



# Expectativa de seguridad en los proximos 12 meses----

#* Limpiar bases----
files_temp <- files[(grep("18\\.|19\\.|20\\.|21\\.|22\\.", files, ignore.case = T))];

ensu_full <- lapply(files_temp, function(x) {
  
  print(x)
  
  fecha <- str_remove(x, "../../00. DATOS/2.5 INEGI/ENSU/SERIE/ENSU-CB/ENSU_CB_") 
  
  foreign::read.dbf(x) %>%
    clean_names() %>% 
    mutate(
      var_id = "nacional",
      var_guad = ifelse(cve_ent == "14" & 
                          cve_mun %in% c("039"),
                        "Guadalajara", "otro"),
      fecha = as.Date(paste("01", substr(fecha, start = 1, stop = 2), 
                            substr(fecha, start = 3, stop = 4), sep = "-"), 
                      format = "%d-%m-%y")
    ) %>% 
    select(fecha, var_id, var_guad, cve_ent, 
           cve_mun, nom_mun, upm_dis, 
           est_dis, fac_sel, bp1_3) %>% 
    srvyr::as_survey_design(ids = upm_dis, strata = est_dis, weights = fac_sel) %>% 
    filter(!is.na(bp1_3)) -> temp
  
  temp %>% 
    filter(var_guad == "Guadalajara") %>% 
    group_by(fecha, var_guad, bp1_3) %>% 
    summarise(por = survey_mean(vartype = "cv"), 
              total = survey_total(vartype = "cv")) -> data
  
})

ensu_full %>% 
  reduce(full_join) %>% 
  mutate(
    var_id = str_to_title(var_guad),
    bp1_3 = factor(case_when(
      bp1_3 == "1" ~ "Mejorará o seguirá igual de bien",
      bp1_3 == "2" ~ "Mejorará o seguirá igual de bien",
      bp1_3 == "3" ~ "Empeorará o seguirá igual de mal",
      bp1_3 == "4" ~ "Empeorará o seguirá igual de mal",
      bp1_3 == "9" ~ "Ns/Nc"
    ), levels = c("Mejorará o seguirá igual de bien",
                  "Empeorará o seguirá igual de mal",
                  "Ns/Nc")),
    fecha = zoo::as.yearmon(fecha, format = "%b/%Y"),
    var_colr = ifelse(grepl("Ns/Nc", bp1_3, ignore.case = T), "transparent", "color")
  ) %>%
  group_by(fecha, var_id, bp1_3, var_colr) %>%
  summarise(por=sum(por)) %>%
  filter(bp1_3 != "Ns/Nc")-> df_graf

levels(df_graf$bp1_3) <- str_wrap(levels(df_graf$bp1_3), 14)

#* Stacked bar Zm Guadalajara vs nacional----
ggplot(
  df_graf,
  aes(x = fecha, y = por, color = bp1_3)
) +
  geom_line(size = 2, alpha = .8) +
  geom_point(size = 5) +
  geom_text(
    # data = df_graf %>%
    #           filter(fecha == as.yearmon(as.Date("2022-12-01"))),
            aes(label = percent(por, accuracy = 1),
                alpha = var_colr), hjust = 0, nudge_y = -.04,
            family = "Montserrat", size = 8, show.legend = F) +
  ggtitle("Expectativas sobre la delincuencia en los próximos 12 meses",
          "Guadalajara, 4T-22") +
  labs(y = "Trimestres", caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
  tema_euzen(size_var = 25, font_var = "Roboto", lineheight_var = .7) +
  scale_alpha_manual(values = c(0.9, 0.6)) +
  scale_color_manual(values = rev(met.brewer(name = "Juarez", n = 2))) +
  # scale_color_manual(values = c("white", "transparent")) +
  scale_y_continuous("% de población", labels = scales::percent_format(accuracy = 1L)) +
  scale_x_yearmon("Trimestre",format = "%b - %y", n = 6) +
  guides(fill = guide_legend(reverse = T)) +
  expand_limits(x = as.yearqtr(as.Date(c("2018-03-01", "2023-06-01")))) +
  theme(
    legend.title = element_blank(),
    legend.position = "top"
  ) -> graf

ggsave(paste0(path_output, "ensu_expect_hist.png"), graf,
       width = 16, height = 9)


# Barras

files_temp <- files[grep("1222|1221|0922", files, ignore.case = T)];

ensu_full <- lapply(files_temp, function(x) {
  
  print(x)
  
  fecha <- str_remove(x, "../../00. DATOS/2.5 INEGI/ENSU/SERIE/ENSU-CB/ENSU_CB_") 
  
  foreign::read.dbf(x) %>%
    clean_names() %>% 
    mutate(
      var_id = "nacional",
      var_guad = ifelse(cve_ent == "14" & 
                          cve_mun %in% c("039"),
                        "Guadalajara", "otro"),
      fecha = as.Date(paste("01", substr(fecha, start = 1, stop = 2), 
                            substr(fecha, start = 3, stop = 4), sep = "-"), 
                      format = "%d-%m-%y")
    ) %>% 
    select(fecha, var_id, var_guad, cve_ent, 
           cve_mun, nom_mun, upm_dis, 
           est_dis, fac_sel, bp1_3) %>% 
    srvyr::as_survey_design(ids = upm_dis, strata = est_dis, weights = fac_sel) %>% 
    filter(!is.na(bp1_3)) -> temp
  
  temp %>% 
    group_by(fecha, var_id, bp1_3) %>% 
    summarise(por = survey_mean(vartype = "cv"), 
              total = survey_total(vartype = "cv")) -> nac
  
  temp %>% 
    filter(var_guad == "Guadalajara") %>% 
    group_by(fecha, var_guad, bp1_3) %>% 
    summarise(por = survey_mean(vartype = "cv"), 
              total = survey_total(vartype = "cv")) -> estat
  
  plyr::rbind.fill(nac, estat) %>%  
    mutate(
      var_id = coalesce(var_id, var_guad)
    ) %>% 
    select(-c(var_guad)) -> data
  
})

ensu_full %>% 
  reduce(full_join) %>% 
  mutate(
    var_id = str_to_title(var_id),
    bp1_3 = factor(case_when(
      bp1_3 == "1" ~ "Mejorará o seguirá igual de bien",
      bp1_3 == "2" ~ "Mejorará o seguirá igual de bien",
      bp1_3 == "3" ~ "Empeorará o seguirá igual de mal",
      bp1_3 == "4" ~ "Empeorará o seguirá igual de mal",
      bp1_3 == "9" ~ "Ns/Nc"
    ), levels = c("Mejorará o seguirá igual de bien",
                  "Empeorará o seguirá igual de mal",
                  "Ns/Nc")),
    fecha = zoo::as.yearmon(fecha, format = "%b/%Y"),
    var_colr = ifelse(grepl("Ns/Nc", bp1_3, ignore.case = T), "transparent", "color")
  ) %>%
  group_by(fecha, var_id, bp1_3, var_colr) %>%
  summarise(por=sum(por))-> df_graf

levels(df_graf$bp1_3) <- str_wrap(levels(df_graf$bp1_3), 14)

#* Stacked bar Zm Guadalajara vs nacional----
ggplot(
  df_graf,
  aes(x = por, y = fct_rev(as.factor(fecha)), fill = fct_rev(bp1_3), group = fct_rev(bp1_3))
) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = percent(por, accuracy = 0.1),
                col = var_colr),
            fontface = "bold", family = "Montserrat", size = 6, 
            position = position_stack(vjust = 0.5), show.legend = F) +
  facet_wrap(~var_id, nrow = 2) +
  ggtitle("Expectativas sobre la delincuencia \nen los próximos 12 meses",
          "Nacional vs. Guadalajara, 4T-22") +
  labs(y = "Trimestres", caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI") +
  tema_euzen(size_var = 20, font_var = "Roboto", lineheight_var = .77) +
  scale_alpha_manual(values = c(0.9, 0.6)) +
  scale_fill_manual(values = met.brewer(name = "Signac", n = 5)[c(-2, -4)]) +
  scale_color_manual(values = c("white", "transparent")) +
  scale_x_continuous("% de población", labels = scales::percent_format(accuracy = 1L)) +
  guides(fill = guide_legend(reverse = T)) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  ) -> graf

ggsave(paste0(path_output, "ensu_expect.png"), graf,
       width = 8, height = 9)
## Lugar donde se siente inseguro ESCUELAS

### Limpiar bases----

# files_temp <- files[grep("22.dbf|21.dbf|20.dbf|19.dbf|18.dbf", files, ignore.case = T)];
# 
# 
# ensu_full <- lapply(files_temp, function(x) {
#   
#   print(x)
#   
#   fecha <- str_remove(x, "../../00. DATOS/2.5 INEGI/ENSU/SERIE/ENSU-CB/ENSU_CB_") 
#   
# 
#   if (grepl("15\\.", x, ignore.case = T)) {
#     
#     foreign::read.dbf(x) %>%
#       clean_names() %>% 
#       rename(cve_ent = ent,
#              fac_sel = factor,
#              est_dis = edis,
#              bp1_1 = p1) -> temp
#     
#   } else if (grepl("0316|0616|0916", x, ignore.case = T)) {
#     
#     foreign::read.dbf(x) %>%
#       clean_names() %>% 
#       rename(cve_ent = ent, 
#              cve_mun= mun) -> temp
#     
#   } else {
#     
#     foreign::read.dbf(x) %>%
#       clean_names() -> temp
#     
#   }
#   
#   temp %>% 
#     mutate(
#       fac_sel = as.numeric(fac_sel),
#       var_id = "nacional",
#       var_gdl = ifelse(cve_ent == "14" & 
#                          cve_mun %in% c("039", "097", "098",
#                                         "101", "120"),
#                        "zmguadalajara", "otro")
#     ) %>% 
#     select(var_id, var_gdl, upm_dis, 
#            est_dis, fac_sel, bp1_2_04) %>% 
#     srvyr::as_survey_design(ids = upm_dis, strata = est_dis, weights = fac_sel) %>% 
#     filter(!is.na(bp1_2_04)) -> temp2
#   
#   design <- svydesign(ids = ~upm_dis, weights = ~fac_sel, strata = ~est_dis, data = temp2)
#   
#   vars <- c("var_id", "var_gdl")
#   
#   funion <- function(z) {
#     
#     as_tibble(svyby(~bp1_2_04, by = as.formula(paste0("~", z)), design, svytotal, vartype = "cvpct"))
#     
#   }
#   
#   output <- lapply(vars, funion)
#   
#   output %>% 
#     reduce(full_join) %>% 
#     mutate(`bp1_2_042`=ifelse(`cv%.bp1_2_042`>40, NA, `bp1_2_042`)) %>%
#     select(-matches("cv")) %>% 
#     pivot_longer(cols = matches("bp1"),
#                  names_to = "var", 
#                  values_to = "total") %>% 
#     mutate(var_id = coalesce(var_gdl, var_id),
#            resp = str_sub(var, nchar(var)),
#            var = substr(var, 1, nchar(var) - 1)) %>% 
#     filter(var_id != "otro", 
#            resp != "3") %>% 
#     select(-matches("guad")) %>% 
#     group_by(var_id, var) %>% 
#     mutate(
#       tot = sum(total, na.rm = T),
#       por = total / tot,
#       fecha = as.Date(paste("01", substr(fecha, start = 1, stop = 2), 
#                             substr(fecha, start = 3, stop = 4), sep = "-"), 
#                       format = "%d-%m-%y"),
#       var = str_wrap(case_when(
#         var == "bp1_2_01" ~ "Casa",
#         var == "bp1_2_02" ~ "Trabajo",
#         var == "bp1_2_03" ~ "Calles que habitualmente usa",
#         var == "bp1_2_04" ~ "Escuela",
#         var == "bp1_2_05" ~ "Mercado",
#         var == "bp1_2_06" ~ "Centro comercial",
#         var == "bp1_2_07" ~ "Banco",
#         var == "bp1_2_08" ~ "Cajeros automáticos",
#         var == "bp1_2_09" ~ "Transporte público",
#         var == "bp1_2_10" ~ "Automóvil",
#         var == "bp1_2_11" ~ "Carretera",
#         var == "bp1_2_12" ~ "Parque o centro recreativo"
#       ), 10)
#     ) %>% 
#     ungroup() %>% 
#     filter(resp == "2") -> data
#   
# })
# 
# ensu_full %>% 
#   reduce(full_join) %>% 
#   mutate(
#     var_id = str_to_title(var_id),
#     var_id = recode(var_id, "Zmguadalajara"="ZM Guadalajara"),
#     trim = case_when(
#       grepl("-03-01", fecha, ignore.case = T) ~ gsub("mar", "1T", format(fecha, "%b-%Y"), ignore.case = T),
#       grepl("-06-01", fecha, ignore.case = T) ~ gsub("jun", "2T", format(fecha, "%b-%Y"), ignore.case = T),
#       grepl("-09-01", fecha, ignore.case = T) ~ gsub("sep", "3T", format(fecha, "%b-%Y"), ignore.case = T),
#       grepl("-12-01", fecha, ignore.case = T) ~ gsub("dic", "4T", format(fecha, "%b-%Y"), ignore.case = T)
#     )) %>%
#       select(-var_gdl)-> df_graf
# 
# # Serie de tiempo----
# ggplot(
#   df_graf,
#   aes(x = reorder(trim,fecha ), y = por, col = var_id)
# ) +
#   geom_vline(xintercept = "1T-2021", linetype = "dotted", alpha = 0.9, color = "darkgray", size = 0.7) +
#   geom_line(aes(group = var_id),
#             alpha = 0.5, size = 1.5) +
#   geom_point(alpha = 0.6, size = 3) +
#   geom_text(aes(x = "1T-2020", y = .87 + 0.1, label = "Inicio de\nla pandemia"),
#             size = 4, fontface = "bold", family = "Montserrat", col = "black") +
#   geom_text_repel(aes(label = percent(por, accuracy = 0.1)),
#                   angle = 90, size = 5, fontface = "bold", family = "Montserrat", nudge_y = .05,
#                   # hjust = case_when(
#                   #   df_graf$var_id == "Tamaulipas" &
#                   #     df_graf$trim %in% c("1T\n2016", "2T\n2016", "3T\n2016",
#                   #                         "4T\n2020", "1T\n2021", "2T\n2021",
#                   #                         "4T\n2021") ~ -1.25,
#                   #   df_graf$var_id == "Nacional" &
#                   #     df_graf$trim %in% c("1T\n2016", "2T\n2016", "3T\n2016") ~ 2.25,
#                   #   df_graf$var_id == "Tamaulipas" ~ 2.25,
#                   #   df_graf$var_id == "Nacional" ~ -1.25
#                   # ),
#                   segment.size = 0.25, show.legend = F, force = 2) +
#   ggtitle("Sensación de inseguridad en las escuelas: Nacional y ZMG",
#           "% de la población mayor de 18 años que se siente insegura") +
#   labs(x = "Trimestre", caption = "Fuente: Encuesta Nacional de Seguridad Pública Urbana - INEGI\nNota: 1) Se omite el valor del segundo trimestre de 2020, por la pandemia ocasionada por el COVID-19") +
#   tema_euzen() +
#   scale_color_manual(values = c(wesanderson::wes_palette(name = "FantasticFox1", n = 5)[1],
#                                 wesanderson::wes_palette(name = "FantasticFox1", n = 5)[3])
#   ) +
#   scale_x_discrete(breaks = function(x) x[seq_along(x) %% 2 == 0]) +
#   scale_y_continuous("% de población", labels = scales::percent_format(accuracy = 1L),
#                      limits = c(min(df_graf$por) - 0.025,
#                                 max(df_graf$por) + 0.1)) +
#   guides(col = guide_legend(title = "")) +
#   theme(
#     legend.position = "top"
#   ) -> graf
# 
# ggsave(paste0(path_output, "ensu_perc_escuela.png"), graf,
#        width = 14, height = 6.5)



