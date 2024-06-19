# libraries ####
library(tidyverse)
library(lubridate)
library(geosphere)
library(sf)
library(jsonlite)
library(units)
library(janitor)
library(brms)
library(cmdstanr)
library(tidybayes)
library(bayesplot)
library(ggeffects)
library(MetBrewer)
library(tidyverse)
library(readxl)
library(tmap)
library(tmaptools)
library(mosquitoR)
library(parallel)
library(latticeExtra)
library(BayesPostEst)
library(ggspatial)

# helper functions ####

# distance decay function
ddf = function(d, a=.01, b=1) exp(-a*(d^b))


pretty_ce_plot = function(model, variable_name, xlab, ylab, title, color = "#E74C3C", unstandardize = FALSE, var_sd = NA, var_mean = NA){
  
  c_eff <- conditional_effects(model, effects = variable_name)
  
  df <- as.data.frame(c_eff[[1]])
  
  if(unstandardize){
    df[ ,variable_name] = df[ ,variable_name]*var_sd + var_mean
  }
  
  variable_name_sym = sym(variable_name)
  
  this_plot <- ggplot(df, aes(x = !!variable_name_sym, y = estimate__)) +
    geom_line(linewidth = 1.5, color = color) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = color, alpha = 0.2) +
    labs(title = title,
         x = xlab,
         y = ylab) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, hjust = 0, face = "bold"),
      axis.title = element_text(size = 12, face = "bold"),
      axis.title.x = element_text(margin = margin(t = 20)),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.text = element_text(size = 10),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      plot.margin = margin(1, 1.3, 1.3, 1.3, "cm"),
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(color = "#D5DBE1"), 
      panel.grid.minor = element_blank() 
    )
  options(
    repr.plot.width = 4, 
    repr.plot.height = 4
  )
  return(this_plot)
}

message_parallel <- function(...){
  system(sprintf('echo "%s"', paste0(..., collapse="")))
}

chunkified_posterior_predict = function(chunksize = 1000, cores = 1, model, newdata, draws = 1000, allow_new_levels = TRUE){
  chunks = seq(1, nrow(newdata), chunksize)
  n_chunks = length(chunks)
  
  if(cores > 1){
  
  return(unlist(mclapply(chunks, function(i){
    message_parallel(paste0(i, " of ", nrow(newdata)))
    apply(posterior_predict(model, newdata = newdata[i:min(nrow(newdata), (i+(chunksize-1))), ], draws = draws, allow_new_levels = allow_new_levels), 2, function(x) mean(x))
  }, mc.cores = cores)))
  } else{
    
    return(unlist(lapply(chunks, function(i){
      print(paste0(i, " of ", nrow(newdata)))
      apply(posterior_predict(model, newdata = newdata[i:min(nrow(newdata), (i+(chunksize-1))), ], draws = draws, allow_new_levels = allow_new_levels), 2, function(x) mean(x))
    })))
  }
    
}


buffered_sample = function(data, sample_size, buffer_radius_m, group = NULL){
  
  min_distance = set_units(buffer_radius_m, m) # turn the integer value of the minimum distance into a units object in meters.
  
  D = data # create a new sf object that will be manipulated as samples are selected.
  
  this_sample  = NULL # create a null object that will be used to store the final sample. This will be turned into an sf object as data is added to it.
  
  for(i in 1:sample_size){
    this_draw = D %>% sample_n(1) # select one drain at random from the initial pool
    if(!is.null(group)){
      D = D[ (st_distance(D, this_draw) > min_distance) | (this_draw %>% st_drop_geometry() %>% pull(group) != D %>% st_drop_geometry() %>% pull(group)),] # Remove this drain from the pool and also remove any other drains less than or equal to the minimal distance
      
    } else{
      D = D[st_distance(D, this_draw) > min_distance,] # Remove this drain from the pool and also remove any other drains less than or equal to the minimal distance
    }
    this_sample = bind_rows(this_sample, this_draw) # add the selected drain to the sf object in which the final sample is being stored
    if(nrow(D)==0) break # stop here if we no longer have any drains left in the pool
  }
  return(this_sample) # return the final result
}



# load Barcelona census tract shapefile ####
get_bcn_census_tract_shapes = function(file){
  st_read("data/external/seccionado_2017/SECC_CE_20170101.shp") %>% subset(NMUN == "Barcelona") 
}

# load Barcelona land cover data from Urban Atlas
get_urban_atlas_data = function(urban_atlas_file){
  st_read(urban_atlas_file, layer = "ES002L2_BARCELONA_UA2018")
}

# generate perimeter of set of polygons ####
generate_perimeter = function(polygons){
  polygons %>% summarise()
}

# get all malert reports ####
get_malert_reports_all = function(){
  read_rds("https://zenodo.org/records/10699488/files/mosquito_alert_adult_bite_reports_Barcelona_2014_2023.Rds?download=1")
}


# get validated albopictus malert reports ####
get_malert_reports_albopictus_validated = function(){
  read_rds("https://zenodo.org/records/10699488/files/mosquito_alert_validated_albopictus_reports_Barcelona_2014_23.Rds?download=1")
}

# get malert sampling effort data ####
get_malert_sampling_effort_data = function(){
  read_csv("https://github.com/Mosquito-Alert/sampling_effort_data/raw/main/sampling_effort_daily_cellres_025.csv.gz") 
}

# get drains data
get_drain_data = function(file){
  read_xlsx(file, sheet = "2_items_revisats_per_any", col_types = c("numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric"))
}

# get NDVI (private green areas) data ####
get_ndvi = function(file){
  st_read(file)
}

# get BCN BG trap data ####
get_bcn_bg_traps = function(file){
  read_rds(file)
}

# make bcn sociodemographic data ####
make_bcn_characteristics = function(socio_dem_file, income_file, socio_ec_file, housing_age_file){
  
bcn_socdem <- fromJSON(socio_dem_file, simplifyDataFrame = T) %>% 
  filter(str_detect(Nombre, "Barcelona sección"))

# Filter the nested data frames to keep only rows with "Anyo" equal to "2020"
filtered_df <- bcn_socdem %>%
  mutate(Data = purrr::map(Data, ~ filter(.x, Anyo == "2020"))) # Removing the "Anyo" column since all rows have the same value


# Unnest the filtered data frames
unnested_df <- filtered_df %>%
  unnest(cols = Data) %>%
  mutate(
    census_section = str_extract(Nombre, "\\d{5}"),
    var = str_match(Nombre, "\\d{5}\\.\\s(.*?)\\.\\sBase data\\.")[, 2]
  )

reshaped_socdem <- pivot_wider(
  unnested_df,
  id_cols = census_section,
  names_from = var,
  values_from = Valor
)


# Renta ####
bcn_inc = fromJSON(income_file, simplifyDataFrame = T) %>%
  filter(str_detect(Nombre, "Barcelona sección"))

filtered_df <- bcn_inc %>%
  mutate(Data = purrr::map(Data, ~ filter(.x, Anyo == "2020"))) # Removing the "Anyo" column since all rows have the same value

# Unnest the filtered data frames
unnested_df <- filtered_df %>%
  unnest(cols = Data) %>%
  mutate(
    census_section = str_extract(Nombre, "\\d{5}"),
    var = sub(".*\\. (.*)\\.", "\\1", Nombre)
  )

reshaped_inc <- pivot_wider(
  unnested_df,
  id_cols = census_section,
  names_from = var,
  values_from = Valor
)

bcn_chars <- reshaped_socdem %>% 
  left_join(reshaped_inc, by = "census_section") %>% 
  mutate(mean_age = `Average age of the population`,
         p_under18 = `Percentage of the population under the age of 18`,
         p_over65 = `Percentage of the population over the age of 65`,
         mean_hhsize = `Tamaño medio del hogar`,
         p_singlehh = `Porcentaje de hogares unipersonales`,
         population = Population,
         p_spanish = `Percentage of Spanish population`,
         mean_netinc_person = `Renta neta media por persona `,
         mean_netinc_hh = `Renta neta media por hogar `,
         mean_rent_consumption_unit = `Media de la renta por unidad de consumo `,
         median_rent_consumption_unit = `Mediana de la renta por unidad de consumo `,
         mean_grossinc_person = `Renta bruta media por persona `,
         mean_grossinc_hh = `Renta bruta media por hogar ` ) %>% 
  select(census_section, mean_age, p_under18, p_over65, mean_hhsize, p_singlehh, population, p_spanish, mean_netinc_hh, mean_netinc_person, mean_rent_consumption_unit, median_rent_consumption_unit,  mean_grossinc_person, mean_grossinc_hh)

socio_ec_vars = read_csv(socio_ec_file) %>% 
  mutate(CUSEC = paste0("08019", str_pad(Codi_Districte, side = "left", pad = "0", width = 2), str_pad(Seccio_Censal, side = "left", pad = "0", width = 3))) %>% 
  left_join(read_csv(housing_age_file) %>% 
              rename(mean_housing_age = Edat_mitjana) %>%
              mutate(CUSEC = paste0("08019", str_pad(Codi_districte, side = "left", pad = "0", width = 2), str_pad(Seccio_censal, side = "left", pad = "0", width = 3))) ) 

bcn_chars = bcn_chars %>% mutate(CUSEC = paste0("08019", census_section)) %>% left_join(socio_ec_vars)

return(bcn_chars)
}

# merging census tract data ####
merge_census_tract_data = function(census_tracts, bcn_chars){
  census_tracts %>% left_join(bcn_chars) %>% mutate(popd = population/(Shape_area/10000) )
}

# make pres abs ####
make_presence_absence_data = 
function(bcn_perimeter, malert_reports_all){

  
  # Converting the MA data to SF format and limiting it to BCN
  malert_reports_all = malert_reports_all  %>% 
    filter(!is.na(lon)) %>% 
    st_as_sf(coords=c("lon", "lat"), crs=4326, remove = FALSE) %>% 
    st_filter(bcn_perimeter %>% st_transform(4326))
  
  #Creating our pseudo-absences
  pa = st_sample(bcn_perimeter, nrow(malert_reports_all)) %>% st_as_sf() %>% st_transform(4326)
  
  date_vector = seq.Date(from = min(malert_reports_all$date), to = max(malert_reports_all$date), by = "day")
  
  type_vector = malert_reports_all$type
  
  coords = st_coordinates(pa)
  pa$lon = coords[,1]
  pa$lat = coords[,2]
  pa$presence = FALSE
  pa = pa %>% st_drop_geometry() %>% mutate(date = sample(date_vector, nrow(pa), replace = TRUE), type = sample(type_vector, nrow(pa), replace = TRUE))
  
  result = malert_reports_all %>% st_drop_geometry()
  
  result <- result %>% 
    select(lon, lat, date, type) %>% 
    mutate(presence = TRUE) %>% bind_rows(pa) %>% 
    st_as_sf(coords=c("lon", "lat"), crs=4326)
  return(result)
  
}


# merging datasets ####
merge_datasets = function(pres_abs_data, landcover_data, ages_file, incomes_file, census_tracts_merged){

  #Merging the MA, land cover and pseudo-absences.

  ua = landcover_data
  
  result <- pres_abs_data %>% 
    st_transform(st_crs(ua)) %>% 
    st_join(ua) %>% 
    filter(!is.na(country)) #removes two pseudo-absences that gave a missing value when merging them with UA data presumably because polygons don't fully overlap. 


# Obtaining ages and rent per census section of BCN
  ages2017 = read_csv(ages_file)
names(ages2017)[2:23] = paste0("Y2017_", names(ages2017)[2:23])
ages2017 = ages2017 %>% 
    clean_names(case = "all_caps")

incomes = read_csv(incomes_file)
incomes$CUSEC = unlist(lapply(incomes$Name, function(x) strsplit(x, " ")[[1]][1]))

  # Merging ages and rent with the census section file
  sscc = census_tracts_merged %>% 
  merge(incomes, by=c("CUSEC"="CUSEC")) %>%
  merge(ages2017, by=c("CUSEC"="CUSEC"))

  # merging with data from previous step
   result = result  %>% 
  st_transform(st_crs(sscc)) %>% 
  st_join(sscc) %>% 
     mutate(inc = Renta_media_por_hogar_2017 / 1000) %>%
     mutate(inc2 = inc^2) %>%
     mutate(popt = (Y2017_20_24 + Y2017_25_29 + Y2017_30_34 + Y2017_35_39 + Y2017_40_44 + Y2017_45_49 + Y2017_50_54 + Y2017_55_59 + Y2017_60_64 + Y2017_65_69)) %>% 
     mutate(pop2069 = ((Y2017_20_24 + Y2017_25_29 + Y2017_30_34 + Y2017_35_39 + Y2017_40_44 + Y2017_45_49 + Y2017_50_54 + Y2017_55_59 + Y2017_60_64 + Y2017_65_69) / 100)) %>%
     mutate(lnpop2069 = log(popt)) %>%
     mutate(popd2069 = ((Y2017_20_24 + Y2017_25_29 + Y2017_30_34 + Y2017_35_39 + Y2017_40_44 + Y2017_45_49 + Y2017_50_54 + Y2017_55_59 + Y2017_60_64 + Y2017_65_69) / (Shape_area/1000000))) %>%
     mutate(lnpopd2069 = log(popd2069)) %>%
     mutate(hectare = Shape_area / 10000) %>%
     mutate(lnhectare = log(hectare)) %>% 
     mutate(day = wday(date)) %>% 
     mutate(weekend = factor(case_when(
       day > 5 ~  "weekend",
       day <= 5  ~ "weekday"))) %>%
     mutate(month = as.factor(month(date))) %>% 
     mutate(year = as.factor(year(date)))
   
   
   result$district <- factor(result$CDIS)
   result$tract <- factor(result$CUSEC)
   result$landcover <- factor(result$code_2018)
   result$landcover <- recode(result$landcover, "11100" = 1, "11210" = 1, "11220" = 1, "11230" = 1, "11240" = 1, "11300" = 1, "14100" = 2, "14200" = 2, .default = 3)
   result$landcover <- factor(result$landcover, levels = c(1,2,3),labels = c("Urban fabric", "green and leisure", "other"))
   result$inc = result$Renta_media_por_hogar_2017
   result$popd = (result$Y2017_TOTAL/(result$Shape_area/10000))
   result$type = factor(result$type)
   
   
   result <- result #%>% select(date, presence, type, weekend, day, month, year, geometry, tract, district, landcover, code_2018, inc, popd, CUSEC) 
   
   return(result)
}

# Multicolinearity check ####

check_colinearity_gpm = function(data_clean){
  inc <- lm(mean_rent_consumption_unit~p_singlehh+popd+mean_age, data = data_clean)
  singlehh <- lm(p_singlehh~mean_rent_consumption_unit+popd+mean_age, data = data_clean)
  age <- lm(mean_age~mean_rent_consumption_unit+popd+p_singlehh, data = data_clean)
  return(c("inc" = summary(inc)$r.squared, "singlehh" = summary(singlehh)$r.squared, "age" = summary(age)$r.squared)) 
}

check_colinearity_asdm = function(asdm_data_clean){
  inc <- lm(mean_rent_consumption_unit~p_singlehh+popd+SE_expected+mean_age, data = asdm_data_clean$drains_activity_yearly_buff200)
  singlehh <- lm(p_singlehh~mean_rent_consumption_unit+popd+SE_expected+mean_age, data = asdm_data_clean$drains_activity_yearly_buff200)
  se <- lm(SE_expected~mean_rent_consumption_unit+popd+p_singlehh+mean_age, data = asdm_data_clean$drains_activity_yearly_buff200)
  age <- lm(mean_age~mean_rent_consumption_unit+popd+p_singlehh+SE_expected, data = asdm_data_clean$drains_activity_yearly_buff200)
  return( c("inc" = summary(inc)$r.squared, "singlehh" = summary(singlehh)$r.squared, "age" = summary(age)$r.squared, "se" = summary(se)$r.squared) )
}

# Main General Participation Model ####
fit_gpm_main = function(bcn_census_tract_polygons, data_clean){

D <- data_clean

n_cores = parallel::detectCores()
n_chains = 4
n_threads = n_cores/n_chains

st_queen <- function(a, b = a) st_relate(a, b, pattern = "F***T****", sparse=FALSE)
queens <- 1*st_queen(bcn_census_tract_polygons)
rownames(queens) = bcn_census_tract_polygons$CUSEC
colnames(queens) = bcn_census_tract_polygons$CUSEC

M = brm(presence ~ poly(mean_rent_consumption_unit, 2) + log(popd) + mean_age + p_singlehh + car(queens, gr=CUSEC, type="icar"), data = D, data2 = list(queens = queens), family = bernoulli(link = "logit"), prior = set_prior("normal(0,2)", class="b"), chains=n_chains, cores=n_chains, backend = "cmdstanr", threads = n_threads, iter = 2000, silent=0)

return(M)

}

# Main GPM Conditional Effects Plot ####

plot_gpm_main_ces = function(gpm_main){

these_variables = tibble(variable_name = c("mean_age", "p_singlehh", "popd", "mean_rent_consumption_unit"), label = c("Mean age", "Percent single person households", "Population density (per hectare)", "Mean income (euros per consumption unit)"))

filenames = NULL
  
for(i in 1:nrow(these_variables)){
  p = pretty_ce_plot(model = gpm_main, variable_name = these_variables$variable_name[i], xlab= these_variables$label[i], ylab = 'Sampling probability', title = "", color = "#7b3294") 
  this_filename = paste0("figures/pa_model_car1_final_CEs_", these_variables$variable_name[i], ".png")
  
  ggsave(plot = p, filename = this_filename, width=8, height = 8)
  
  filenames = c(filenames, this_filename)
  
}

return(filenames)
}



# ASDM data prep ####

asdm_data_prep = function(bcn_chars, census_tracts_merged, malert_sampling_effort, drain_data, data_clean){

# n_cores = parallel::detectCores()

bcn_ssccs = census_tracts_merged

# MALERT sampling effort 
malert_se = malert_sampling_effort %>% mutate(year = year(date)) %>% group_by(TigacellID, year) %>% summarise(SE_expected = sum(SE_expected))

# drains
drains_table_yearly = drain_data %>% dplyr::select(year =yyear, id_item = codi_svipla, geom = geometria, tipus_entitat, tipologia, n_activity_detections = num_rev_activitat, n_water_detections = num_rev_aigua) %>% mutate(activity = n_activity_detections>0, water = n_water_detections>0) %>% filter(!is.na(geom))

drain_locations_wkb_character = drains_table_yearly %>% dplyr::select(id_item, geom) %>% distinct()

drain_locations = bind_rows(lapply(1:nrow(drain_locations_wkb_character), function(i) st_sf(id_item = drain_locations_wkb_character$id_item[i], geometry = st_as_sfc(structure(list(drain_locations_wkb_character$geom[i]), class = "WKB"), EWKB = TRUE))))

drains_lon_lat = as_tibble(st_coordinates(drain_locations %>% st_transform(4326))) %>% rename(lon = X, lat = Y)

drains_tigacellID = make_samplingcell_ids(lon = drains_lon_lat$lon, lat = drains_lon_lat$lat, mask = .025)

drain_locations$TigacellID = drains_tigacellID

drains_yearly_buff200 = drain_locations %>% right_join(drains_table_yearly, multiple = "all") %>% st_transform(st_crs(bcn_ssccs)) %>% st_join(bcn_ssccs) %>% left_join(malert_se) %>% st_buffer(dist = 200) 

# reports

adult_reports = data_clean %>% mutate(year = year(date)) %>% filter(presence, type == "adult") %>% st_transform(st_crs(drains_yearly_buff200)) %>% dplyr::select(date, year, weekend, month, CUSEC)

adult_and_bite_reports = data_clean %>% mutate(year = year(date)) %>% filter(presence, type %in% c("adult", "bite")) %>% st_transform(st_crs(drains_yearly_buff200)) %>% dplyr::select(date, year, weekend, month, CUSEC)


counts = data_clean %>% mutate(year = year(date)) %>% filter(presence, type %in% c("adult", "bite")) %>% st_drop_geometry()

# counts %>% nrow()
# table(counts$type)
# unique(counts$CUSEC) %>% length()

# merging adults and bites

these_grouping_variables = c("id_item", "year", "water", "n_activity_detections", "activity", "CUSEC", "Nom_Barri", "mean_age", "p_under18", "p_over65", "mean_hhsize", "p_singlehh", "population", "p_spanish", "mean_netinc_hh", "mean_netinc_person", "mean_rent_consumption_unit", "median_rent_consumption_unit", "mean_grossinc_person", "mean_grossinc_hh", "popd", "mean_housing_age", "SE_expected")


drains_activity_yearly_buff200 = drains_yearly_buff200 %>% filter(activity == TRUE) %>% st_join(adult_and_bite_reports %>% dplyr::select(year)) %>% mutate(report = if_else(year.x == year.y, TRUE,  FALSE)) %>% st_drop_geometry() %>% rename(year = year.x) %>% group_by_at(these_grouping_variables) %>% summarise(n_reports = sum(report), .groups = "drop") %>% replace_na(list(n_reports = 0)) %>% mutate(any_reports = n_reports > 0)
  
  return(list("drains_activity_yearly_buff200"= drains_activity_yearly_buff200, "drain_locations" = drain_locations, "drains_yearly_buff200" = drains_yearly_buff200))

}

# Drain map ####
make_drain_map = function(asdm_data_clean,
                          bcn_perimeter_polygon,
                          bcn_census_tract_polygons) {
  
  drains_yearly_buff200 = asdm_data_clean$drains_yearly_buff200

  drains_activity_yearly_buff200 = asdm_data_clean$drains_activity_yearly_buff200
  
  map_data = drains_yearly_buff200 %>% filter(activity) %>% dplyr::select(id_item, year) %>% left_join(drains_activity_yearly_buff200)
  
  this_pal = met.brewer(name = "Hiroshige", n = 10)
  
  bcn_perimeter = bcn_perimeter_polygon
  
  this_p = tm_shape(bcn_census_tract_polygons) +
    tm_polygons(col = "#ffffbb", border.col = "#aaaaaa") + tm_shape(map_data) +
    tm_dots(col = "any_reports", size = .1, pal = this_pal[c(10, 1)]) +
    tm_layout(frame = FALSE, legend.show = FALSE) + tm_scale_bar(position=c("left", "bottom"))
  
  this_filename = "figures/map_bcn_active_drains.png"
  tmap_save(this_p, this_filename, dpi = 600)
  
  return(this_filename)
  
}

# Main ASDM####
fit_asdm_main = function(asdm_data_clean){
  
  drains_activity_yearly_buff200 = asdm_data_clean$drains_activity_yearly_buff200
  
n_cores = parallel::detectCores()
n_chains = 4
n_threads = n_cores/n_chains
n_iters = 2000

brm(any_reports ~  SE_expected + poly(mean_rent_consumption_unit, 2) + log(popd) + p_singlehh + mean_age + (1 | id_item), data = drains_activity_yearly_buff200, family = bernoulli(link = "logit"), chains=n_chains, cores=n_chains, backend = "cmdstanr", threads = n_threads, iter = 2000, silent = 0, control = list(adapt_delta = 0.97))

}

# plot ASDM CESs ####
plot_asdm_ces = function(model, name){

  these_variables = tibble(variable_name = c("p_singlehh", "popd", "mean_rent_consumption_unit", "SE_expected", "mean_age"), label = c("Percent single person households", "Population density (per hectare)", "Mean income (euros per consumption unit)", "Sampling Cell Effort", "Mean Age"))

  filenames = NULL
  
for(i in 1:nrow(these_variables)){
  
  this_filename = paste0("figures/", name, "_", these_variables$variable_name[i], ".png")
  
  p = pretty_ce_plot(model = model, variable_name = these_variables$variable_name[i], xlab= these_variables$label[i], ylab = 'Sampling probability', title = "", color = "#2b8cbe")

    ggsave(plot = p, filename = this_filename, width=8, height = 8)

    filenames = c(filenames, this_filename)
    
}
  
  return(filenames)
}


# robustness check ####
fit_asdm_robust_check = function(asdm_data_clean){

  n_cores = parallel::detectCores()
  n_chains = 4
  n_threads = n_cores/n_chains
  n_iters = 2000
  
  drains_activity_yearly_buff200 = asdm_data_clean$drains_activity_yearly_buff200

  drain_locations = asdm_data_clean$drain_locations 
  
  drains_activity_yearly_buff200_sf = drain_locations %>% right_join(drains_activity_yearly_buff200, multiple = "all") 
  
  
  drains_activity_yearly_buff200_spaced200m = buffered_sample(data = drains_activity_yearly_buff200_sf, buffer_radius_m = 200, sample_size = nrow(drains_activity_yearly_buff200_sf), group = "year")

  brm(any_reports ~  SE_expected + poly(mean_rent_consumption_unit,2) + log(popd) + p_singlehh + mean_age, data = drains_activity_yearly_buff200_spaced200m, family = bernoulli(link = "logit"), chains=n_chains, cores=n_chains, backend = "cmdstanr", threads = n_threads, iter = 2000, silent = 0, control = list(adapt_delta = 0.97))

}

# ASDM Prediction points ####
make_asdm_prediction_points = function(asdm_main, malert_sampling_effort, ndvi, landcover_data, bcn_perimeter_polygon, census_tracts_merged, bcn_chars){
  
  n_cores = 1

  # loading storm drain model
  storm_drain_model = asdm_main
  
  # MALERT sampling effort
  malert_se_full = malert_sampling_effort
  
  malert_se = malert_se_full %>% mutate(year = year(date)) %>% group_by(TigacellID, year) %>% summarise(SE_expected = sum(SE_expected))
  
  private_green = ndvi
  
  ua = landcover_data
  
  bcn_ssccs = census_tracts_merged
  
  sscc_private_green_areas = bcn_ssccs %>% st_intersection(private_green %>% st_transform(st_crs(bcn_ssccs))) %>% mutate(area = st_area(.)) %>% st_drop_geometry() %>% group_by(CUSEC) %>% summarise(private_green_area = sum(as.numeric(area)))
  
  bcn_ssccs = bcn_ssccs %>% left_join(sscc_private_green_areas) %>% replace_na(list(private_green_area = 0))
  
  # predicting drain model into bcn_ssccs here so that we can then sample proportionally to those predictions
  
  bcn_sscc_pred_points = st_make_grid(bcn_perimeter_polygon, cellsize = c(20,20), what = "centers", square = TRUE) %>% st_sf %>% st_join(bcn_ssccs, join = st_intersects, left=FALSE) 
  
  these_coords = st_coordinates(bcn_sscc_pred_points)
  bcn_sscc_pred_points$lon = these_coords[,1]
  bcn_sscc_pred_points$lat = these_coords[,2]
  bcn_sscc_pred_points = bcn_sscc_pred_points %>% mutate(TigacellID = make_samplingcell_ids(lon = lon, lat = lat, mask = .025)) 
  
  bcn_sscc_pred_points_all = bind_rows(lapply(2014:2023, function(this_year){
    bcn_sscc_pred_points %>% mutate(year = this_year)
  }))
  
  bcn_sscc_pred_points_all = bcn_sscc_pred_points_all %>% left_join(malert_se) %>% replace_na(list(SE_expected = 0))
  
  bcn_sscc_pred_points_all$id_item = "new_drain"
  
  this_chunksize = 1000
  
  bcn_sscc_pred_points_all$preds = chunkified_posterior_predict(chunksize = this_chunksize, cores = n_cores, model=storm_drain_model, newdata = bcn_sscc_pred_points_all, draws = 1000, allow_new_levels = TRUE)
  
  nearest_private_green_indexes = bcn_sscc_pred_points_all %>% st_nearest_feature(private_green %>% st_transform(st_crs(bcn_ssccs)))
  
  dist_nearest_private_green = bcn_sscc_pred_points_all %>% st_distance(private_green[nearest_private_green_indexes,]%>% st_transform(st_crs(bcn_ssccs)), by_element = TRUE)
  
  
  bcn_sscc_pred_points_all$x = st_coordinates(bcn_sscc_pred_points_all)[, "X"]
  
  bcn_sscc_pred_points_all$y = st_coordinates(bcn_sscc_pred_points_all)[, "Y"]
  
  bcn_sscc_pred_points_all = bcn_sscc_pred_points_all %>% mutate(dist_nearest_private_green = as.numeric(dist_nearest_private_green), dist_nearest_private_green_neg_exp = -exp(dist_nearest_private_green), ddf_proximity = ddf(dist_nearest_private_green)) 
  
  return(bcn_sscc_pred_points_all)
}
 
# MAVM data prep ####
prepare_mavm_data = function(asdm_main, asdm_prediction_points, malert_sampling_effort, ndvi, landcover_data, census_tracts_merged, bcn_chars, malert_reports_validated){
  
  n_cores = parallel::detectCores()

  # loading storm drain model
  storm_drain_model = asdm_main
  
  private_green = ndvi
  
  # MALERT sampling effort
  malert_se_full = malert_sampling_effort
  
  malert_se = malert_se_full %>% mutate(year = year(date)) %>% group_by(TigacellID, year) %>% summarise(SE_expected = sum(SE_expected))
  
ua = landcover_data

bcn_ssccs = census_tracts_merged

n_absences = 20000

pseudo_absences = sample_n(asdm_prediction_points, size = n_absences, weight = asdm_prediction_points$preds, replace = FALSE) %>% dplyr::select(year)

# now doing a version witout accounting for sampling effort, same size as the other but simple random sample across the city
pseudo_absences_no_se = sample_n(asdm_prediction_points, size = n_absences, replace = FALSE) %>% dplyr::select(year)

# ggplot(pseudo_absences) + geom_sf(alpha = .5, size=.5)
# ggplot(pseudo_absences_no_se) + geom_sf(alpha = .5, size=.5)

vrs = malert_reports_validated %>% filter(!is.na(lon)) %>% st_as_sf(coords = c("lon", "lat"), crs=4326, remove = FALSE) %>% st_transform(st_crs(bcn_ssccs)) %>% st_filter(bcn_ssccs) %>% mutate(presence = TRUE) %>% dplyr::select(presence, year, lon, lat) %>% mutate(TigacellID = make_samplingcell_ids(lon = lon, lat = lat, mask = .025))

vrs_absence = pseudo_absences %>% mutate(presence = FALSE)

vrs_absence_no_se = pseudo_absences_no_se  %>% mutate(presence = FALSE)

# checking missing CUSECS (since this will mess things up for later predictions with the spatial model)
covered_CUSECs = vrs_absence %>% st_transform(st_crs(bcn_ssccs)) %>% st_join(bcn_ssccs) %>% pull(CUSEC) %>% unique()
missing_ssccs = bcn_ssccs %>% filter(!CUSEC %in% covered_CUSECs)
(nmissing = nrow(missing_ssccs))
# now taking one sample for each sscc that is not yet represented so that we end up with all of them covered
if(nmissing>0){
  vrs_absence_one_per_sscc = bind_rows(lapply(1:nmissing, function(i) {
    print(i)
    st_sample(missing_ssccs[i,], size = 1) %>% st_sf()
  })) %>% mutate(presence = FALSE)
  
  vrs_absence = bind_rows(vrs_absence, vrs_absence_one_per_sscc)
}

# again with the no_se version
covered_CUSECs = vrs_absence_no_se %>% st_transform(st_crs(bcn_ssccs)) %>% st_join(bcn_ssccs) %>% pull(CUSEC) %>% unique()
missing_ssccs = bcn_ssccs %>% filter(!CUSEC %in% covered_CUSECs)
(nmissing = nrow(missing_ssccs))
# now taking one sample for each sscc that is not yet represented so that we end up with all of them covered
if(nmissing>0){
  vrs_absence_one_per_sscc = bind_rows(lapply(1:nmissing, function(i) {
    print(i)
    st_sample(missing_ssccs[i,], size = 1) %>% st_sf()
  })) %>% mutate(presence = FALSE)
  
  vrs_absence_no_se = bind_rows(vrs_absence_no_se, vrs_absence_one_per_sscc)
}
vrs_absence_lon_lat = vrs_absence %>% st_coordinates() %>% as_tibble() %>% rename(lon = X, lat = Y)

vrs_absence_lon_lat_no_se = vrs_absence_no_se %>% st_coordinates() %>% as_tibble() %>% rename(lon = X, lat = Y)

vrs_absence$TigacellID = make_samplingcell_ids(lon = vrs_absence_lon_lat$lon, lat = vrs_absence_lon_lat$lat, mask = .025)

vrs_absence_no_se$TigacellID = make_samplingcell_ids(lon = vrs_absence_lon_lat_no_se$lon, lat = vrs_absence_lon_lat_no_se$lat, mask = .025)

D = bind_rows(vrs, vrs_absence %>% st_transform(st_crs(vrs))) %>% st_join(bcn_ssccs) %>% st_join(ua %>% st_transform(st_crs(bcn_ssccs))) %>% dplyr::select(-c(OBS, comment)) %>% left_join(malert_se) %>% replace_na(list(SE_expected=0))

D_no_se = bind_rows(vrs, vrs_absence_no_se %>% st_transform(st_crs(vrs))) %>% st_join(bcn_ssccs) %>% st_join(ua %>% st_transform(st_crs(bcn_ssccs))) %>% dplyr::select(-c(OBS, comment)) %>% left_join(malert_se) %>% replace_na(list(SE_expected=0))


D = D %>% st_join(private_green %>% dplyr::select(id_ndvi) %>% st_transform(st_crs(bcn_ssccs))) %>% mutate(private_green = !is.na(id_ndvi)) %>% dplyr::select(-id_ndvi)

D_no_se = D_no_se %>% st_join(private_green %>% dplyr::select(id_ndvi) %>% st_transform(st_crs(bcn_ssccs))) %>% mutate(private_green = !is.na(id_ndvi)) %>% dplyr::select(-id_ndvi)

nearest_private_green_indexes = D %>% st_nearest_feature(private_green %>% st_transform(st_crs(bcn_ssccs)))

nearest_private_green_indexes_no_se = D_no_se %>% st_nearest_feature(private_green %>% st_transform(st_crs(bcn_ssccs)))

dist_nearest_private_green = D %>% st_distance(private_green[nearest_private_green_indexes,]%>% st_transform(st_crs(bcn_ssccs)), by_element = TRUE)

dist_nearest_private_green_no_se = D_no_se %>% st_distance(private_green[nearest_private_green_indexes_no_se,]%>% st_transform(st_crs(bcn_ssccs)), by_element = TRUE)

D = D %>% mutate(dist_nearest_private_green = as.numeric(dist_nearest_private_green), dist_nearest_private_green_neg_exp = -exp(dist_nearest_private_green), ddf_proximity = ddf(dist_nearest_private_green))

D_no_se = D_no_se %>% mutate(dist_nearest_private_green = as.numeric(dist_nearest_private_green_no_se), dist_nearest_private_green_neg_exp = -exp(dist_nearest_private_green), ddf_proximity = ddf(dist_nearest_private_green))

D %>% ggplot(aes(x=mean_rent_consumption_unit, fill=presence)) + geom_histogram()

# max(D$mean_rent_consumption_unit)
# max(D_no_se$mean_rent_consumption_unit)


# spatial weights matrix
bcn_in_data <- bcn_ssccs %>% filter(CUSEC %in% unique(D$CUSEC))

st_queen <- function(a, b = a) st_relate(a, b, pattern = "F***T****", sparse=FALSE)
W <- 1*st_queen(bcn_in_data)
rownames(W) = bcn_in_data$CUSEC
colnames(W) = bcn_in_data$CUSEC
diag(W) = 1

# predicted SE

D$id_item = "new_drain"

D$pred = apply(posterior_epred(storm_drain_model, newdata = D, allow_new_levels = TRUE), 2, function(x) mean(x))

D_no_se$id_item = "new_drain"

D_no_se$pred = apply(posterior_epred(storm_drain_model, newdata = D_no_se, allow_new_levels = TRUE), 2, function(x) mean(x))

if(nrow(D) != nrow(D %>% dplyr::select(presence, pred, mean_housing_age, mean_rent_consumption_unit, ddf_proximity) %>% drop_na())) stop("There are missing values in the data.")

if(nrow(D_no_se) != nrow(D_no_se %>% dplyr::select(presence, pred, mean_housing_age, mean_rent_consumption_unit, ddf_proximity) %>% drop_na())) stop("There are missing values in the data.")

return(list("W" = W, "data_se" = D, "data_no_se" = D_no_se))

}

# Main MAVM ####
fit_mavm_main = function(mavm_data_clean){
  
  n_cores = parallel::detectCores()
  n_chains = 4
  threads_per_chain = n_cores/n_chains
  
  brm(presence ~ offset(log(pred)) + ddf_proximity + poly(mean_rent_consumption_unit, 2) + car(W, gr=CUSEC, type="icar"), data = mavm_data_clean$data_se, data2 = list(W = mavm_data_clean$W), family=bernoulli(link="logit"), iter = 2000, chains=n_chains, cores=n_chains, backend = "cmdstanr", threads = threading(threads_per_chain), control = list(adapt_delta = 0.97), save_pars = save_pars(latent = TRUE), silent=0)
  
}


# plot MAVM CEs ####
plot_mavm_ces = function(model) {
  these_variables = tibble(
    variable_name = c("ddf_proximity", "mean_rent_consumption_unit"),
    label = c("Proximity", "Mean income (euros per consumption unit)")
  )
  
  filenames = NULL
  
  for (i in 1:nrow(these_variables)) {
    p = pretty_ce_plot(
      model = model,
      variable_name = these_variables$variable_name[i],
      xlab = these_variables$label[i],
      ylab = 'Probability of presence',
      title = ""
    )
    
    this_filename = paste0(
      "figures/albo_model_points_M_car_final_CEs_",
      these_variables$variable_name[i],
      ".png"
    )
    
    ggsave(
      plot = p,
      filename = this_filename,
      width = 8,
      height = 8
    )
    
    filenames = c(filenames, this_filename)
    
  }
  return(filenames)
}

# Main MAVM no SE ####
fit_mavm_main_no_se = function(mavm_data_clean){
  
    n_cores = parallel::detectCores()
    n_chains = 4
    threads_per_chain = n_cores/n_chains
    
  brm(presence ~ ddf_proximity + poly(mean_rent_consumption_unit, 2) + car(W, gr=CUSEC, type="icar"), data = mavm_data_clean$data_no_se, data2 = list(W = mavm_data_clean$W), family=bernoulli(link="logit"), iter = 2000, chains=n_chains, cores=n_chains, backend = "cmdstanr", threads = threading(threads_per_chain), control = list(adapt_delta = 0.97), save_pars = save_pars(latent = TRUE), silent=0)

}

# MAVM Main Predictions High Res ####

make_mavm_prediction_points = function(mavm_main, asdm_prediction_points, mavm_main_no_se, bcn_perimeter_polygon, ndvi, census_tracts_merged, landcover_data){
    
  n_cores = 1

  bcn_ssccs = census_tracts_merged
  
  ua = landcover_data
  D = mavm_main$data
  
  private_green = ndvi
  
  bcn_perimeter = bcn_perimeter_polygon
  # bcn_perimeter %>% ggplot() + geom_sf()
  
  these_points = asdm_prediction_points %>% filter(year == 2022) %>% st_transform(st_crs(bcn_ssccs)) %>% st_join(ua %>% st_transform(st_crs(bcn_ssccs)))
  
  
  these_points$pred = median(D$pred)
  
  these_points$id_item = "new_drain"
  
  
  this_chunksize = 1000

  these_points$preds = chunkified_posterior_predict(chunksize = this_chunksize, cores = n_cores, model = mavm_main, newdata = these_points, draws = 1000, allow_new_levels = TRUE)

  these_points$preds_no_se = chunkified_posterior_predict(chunksize = this_chunksize, cores = n_cores, model = mavm_main_no_se, newdata = these_points, draws = 1000, allow_new_levels = TRUE)
  
  return(these_points)
  
}

# MAVM prediction figures ####
make_mavm_prediction_figures = function(prediction_points, bcn_perimeter){
  
  these_filenames = NULL
  
  these_points = prediction_points
  
  r1 = raster::rasterFromXYZ(these_points %>% st_drop_geometry() %>% select(x, y, preds), crs=st_crs(these_points)) 
  
  # rasterVis::levelplot(r1)
  
  r1_no_se = raster::rasterFromXYZ(these_points %>% st_drop_geometry() %>% select(x, y, preds_no_se), crs=st_crs(these_points)) 
  

  these_points = these_points %>% mutate(pred_diff = preds - preds_no_se)
  
  r1_diffs = raster::rasterFromXYZ(these_points %>% st_drop_geometry() %>% select(x, y, pred_diff), crs=st_crs(these_points)) 
  
    range(these_points$pred_diff)
  
  # hist(these_points$pred_diff)
  # breaks = c(-.3, -.2, -.1, .1, .2, .3)
  
  breaks = c(-.5, -.2,-.1, -.05, .05, .1, .2, .5)
  
  cols = colorRampPalette(RColorBrewer::brewer.pal(length(breaks), "Spectral")[length(breaks):1])(length(breaks)-1)
  
  cols[4] = "lightgrey"
  
  these_points$pred_diff_cuts = cut(these_points$pred_diff, breaks = breaks)
  
  ggplot(bcn_perimeter) + geom_tile(data=these_points, aes(x=x, y=y, fill=pred_diff_cuts)) + scale_fill_manual("Difference", values = cols) + theme_void() + xlab("") + ylab("") + geom_sf(fill=NA) + annotation_scale()
  
  this_filename = "figures/pred_map_diffs.png"
  these_filenames = c(these_filenames, this_filename)

    ggsave(this_filename, width=6, height =6, device = png, type = "cairo", dpi = 600)
  
  ggplot(bcn_perimeter) + geom_tile(data=these_points, aes(x=x, y=y, fill=preds)) + theme_void() + xlab("") + ylab("") + geom_sf(fill=NA) + scale_fill_distiller("Probability", palette = "Spectral", limits = c(0,1)) + annotation_scale()
  
  this_filename = "figures/pred_map.png"
  these_filenames = c(these_filenames, this_filename)
  
  ggsave(this_filename, width=6, height =6, device = png, type = "cairo", dpi = 600)
  
  ggplot(bcn_perimeter) + geom_sf(fill="white") + geom_tile(data=these_points, aes(x=x, y=y, fill=preds_no_se)) + theme_void() + xlab("") + ylab("") + geom_sf(fill=NA)  + scale_fill_distiller("Probability", palette = "Spectral", limits = c(0,1)) + annotation_scale()
  
  this_filename = "figures/pred_map_no_se.png"
  these_filenames = c(these_filenames, this_filename)
  
  ggsave(this_filename, width=6, height =6, device = png, type = "cairo", dpi = 600)
  
  this_filename = "figures/districts_npoints_pred_diff_lt_neg.3.tex"
  these_filenames = c(these_filenames, this_filename)
  
  these_points = these_points %>% st_drop_geometry()
  
  these_points %>% filter(pred_diff < -.3) %>% group_by(Nom_Districte) %>% summarise(n = n()) %>% xtable::xtable(type = "latex") %>% print(file = this_filename)
  
  this_filename = "figures/districts_over_under_10pp.tex"
  these_filenames = c(these_filenames, this_filename)
  
  these_points %>% filter(pred_diff < -.1)  %>% group_by(District = Nom_Districte) %>% summarise(Overpredicted = n()) %>% full_join(these_points %>% filter(pred_diff > .1)  %>% group_by(District = Nom_Districte) %>% summarise(Underpredicted = n())) %>% replace_na(list(Overpredicted=0, Underpredicted=0)) %>% xtable::xtable(type = "latex", caption = "Number of precition points at which \\textit{Ae. albopictus} probability is over-predicted and under-predcited by more then 10 percentage points when sampling effort is not accounted for (compared to the model in which it is).", label = "tab:over-under") %>% print(file = this_filename)
  
  # these_points %>% ggplot(aes(x=mean_rent_consumption_unit, y=pred_diff)) + geom_point()
  
  this_pal1 = met.brewer("Navajo", 5)[c(2,4)]
  this_pal2 = met.brewer("Navajo", 5)[c(2,1)]
  
  these_points %>% dplyr::select(INC = mean_rent_consumption_unit, AGE = mean_age, PSH = p_singlehh, POP = popd, pred_diff) %>% filter(pred_diff>.1) %>% pivot_longer(-pred_diff, names_to = "variable", values_to = "value") %>% ggplot(aes(x=value)) + geom_density() + facet_wrap(.~variable, scales = "free", nrow = 2) 
  
  this_filename = "figures/albo_pred_diffs_underpred_10pp_density.png"
  these_filenames = c(these_filenames, this_filename)
  
  ggsave(this_filename, width = 6, height = 6)
  
  
  these_points %>% dplyr::select(INC = mean_rent_consumption_unit, AGE = mean_age, PSH = p_singlehh, POP = popd, pred_diff)  %>% filter(pred_diff < -0.1) %>% pivot_longer(-pred_diff, names_to = "variable", values_to = "value") %>% ggplot(aes(x=value)) + geom_density() + facet_wrap(.~variable, scales = "free", nrow = 2)
  
  this_filename = "figures/albo_pred_diffs_overpred_10pp_density.png"
  these_filenames = c(these_filenames, this_filename)
  
  ggsave(this_filename, width = 6, height = 6)
  
  return(these_filenames)
}

# MTVM trap map ####
make_trap_map = function(bcn_bg_traps, bcn_census_tract_polygons){
  
  this_p = tm_shape(bcn_census_tract_polygons) + 
    tm_polygons(col = "#ffffbb", border.col = "#aaaaaa") + tm_shape(bcn_bg_traps %>% dplyr::select(trap_name_lon_lat) %>% distinct()) + 
    tm_dots(col = "#006d2c", size = .1) +
    tm_layout(frame = FALSE, legend.show = FALSE) + tm_scale_bar(position=c("left", "bottom"))
  
  this_figure = "figures/map_bcn_bg_traps.png"
  tmap_save(this_p, this_figure, dpi=600)
  
  return(this_figure)
  
}


# Main MTVM ####
fit_mtvm_main = function(bcn_bg_traps, bcn_chars, census_tracts_merged, ndvi){
  
  n_cores = parallel::detectCores()
  n_chains = 4
  threads_per_chain = n_cores/n_chains

  private_green = ndvi

  bcn_ssccs = census_tracts_merged
  
  bg = bcn_bg_traps %>% left_join(bcn_chars %>% select(mean_rent_consumption_unit, mean_housing_age, census_section))
  
  nearest_private_green_indexes = bg %>% st_nearest_feature(private_green %>% st_transform(st_crs(bg)))
  
  dist_nearest_private_green = bg %>% st_distance(private_green[nearest_private_green_indexes,]%>% st_transform(st_crs(bg)), by_element = TRUE)
  
  bg = bg %>% mutate(dist_nearest_private_green = as.numeric(dist_nearest_private_green), dist_nearest_private_green_neg_exp = -exp(dist_nearest_private_green), ddf_proximity = ddf(dist_nearest_private_green))
  
  brm(bf(females ~ poly(meanTM30,2) + poly(mean_rent_consumption_unit,2) + (1 | trap_name), zi ~ poly(meanTM30,2) ), data=bg, family = zero_inflated_poisson(), prior = set_prior("normal(0,1)", class="b"), iter = 2000, chains=n_chains, cores=n_chains, backend = "cmdstanr", threads = threading(threads_per_chain), control = list(adapt_delta = 0.97), silent = 0, save_pars = save_pars(all = TRUE))
  
}

# plot MTVM CEs and comparisons ####
plot_mtvm_ces_plus_comparison = function(mtvm_main, mavm_main){
  
  these_filenames = NULL
  
  these_variables = tibble(variable_name = c( "mean_rent_consumption_unit", "meanTM30"), label = c("Mean income (euros per consumption unit)", "Mean temperature"), unstandardize = FALSE)
  
  
  for(i in 1:nrow(these_variables)){
    p = pretty_ce_plot(model = mtvm_main, variable_name = these_variables$variable_name[i], unstandardize = these_variables$unstandardize[i], var_mean = these_variables$means[i], var_sd = these_variables$sds[i], xlab= these_variables$label[i], ylab = 'Predicted count', title = "", color = "#4daf4a")
    
    this_filename = paste0("figures/bg_model_M2_final_CEs_", these_variables$variable_name[i], ".png")
    these_filenames = c(these_filenames, this_filename)
    
    ggsave(plot = p, filename = this_filename, width=8, height = 8)
  }
  
  
  # making final figure comparing income effects
  
  xlab = "Mean income (euros per consumption unit)"
  ylab = 'Predicted count'
  title = ""
  color = "#4daf4a"
  c_eff <- conditional_effects(mtvm_main, effects = "mean_rent_consumption_unit")
  
  df <- as.data.frame(c_eff[[1]])
  
  max_bg_income = max(df$mean_rent_consumption_unit)
  
  this_plot1 <- ggplot(df, aes(x = mean_rent_consumption_unit, y = estimate__)) +
    geom_line(linewidth = 1.5, color = color) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = color, alpha = 0.2) + xlim(10000, 52000) +
    vline_at(max_bg_income, lty = 2) +
    labs(title = title,
         x = xlab,
         y = ylab) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, hjust = 0, face = "bold"),
      axis.title = element_text(size = 12, face = "bold"),
      axis.title.x = element_text(margin = margin(t = 20)),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.text = element_text(size = 10),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      plot.margin = unit(c(0,1,1,1), "cm"),
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(color = "#D5DBE1"), 
      panel.grid.minor = element_blank() 
    )
  
  
  xlab = ""
  ylab = 'Probability of presence'
  title = ""
  color = "#E74C3C"
  c_eff <- conditional_effects(mavm_main, effects = "mean_rent_consumption_unit")
  
  df <- as.data.frame(c_eff[[1]])
  
  this_plot2 <- ggplot(df, aes(x = mean_rent_consumption_unit, y = estimate__)) +
    geom_line(linewidth = 1.5, color = color) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = color, alpha = 0.2) + xlim(10000, 52000) +
    vline_at(max_bg_income, lty = 2) +
    labs(title = title,
         x = xlab,
         y = ylab) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, hjust = 0, face = "bold"),
      axis.title = element_text(size = 12, face = "bold"),
      axis.title.x = element_text(margin = margin(t = 20)),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.text = element_text(size = 10),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      plot.margin = unit(c(0,1,0,1), "cm"),
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(color = "#D5DBE1"), 
      panel.grid.minor = element_blank() 
    )
  
  cowplot::plot_grid(this_plot2, NULL, this_plot1, ncol = 1, align = "v", rel_heights = c(1,-.1, 1))
  
  this_filename = "figures/income_coef_comparison_bg_albo.png"
  these_filenames = c(these_filenames, this_filename)
  
  ggsave(this_filename, width = 6, height = 8)
  
  return(these_filenames)
  
  
}

# MTVM comparisons ####

fit_mtvm_comparisons = function(mtvm_main){
  
  n_cores = parallel::detectCores()
  n_chains = 4
  threads_per_chain = n_cores/n_chains
  
  
  M4 = mtvm_main
  
  D = M4$data
  
  M3 = brm(bf(females ~ poly(meanTM30,2) + poly(mean_rent_consumption_unit,2), zi ~ poly(meanTM30,2) ), data=D, family = zero_inflated_poisson(), prior = set_prior("normal(0,1)", class="b"), iter = 2000, chains=n_chains, cores=n_chains, backend = "cmdstanr", threads = threading(threads_per_chain), control = list(adapt_delta = 0.97), silent = 0, save_pars = save_pars(all = TRUE))
  
  M2 = brm(bf(females ~ poly(meanTM30,2) + (1 | trap_name), zi ~ poly(meanTM30,2) ), data=D, family = zero_inflated_poisson(), prior = set_prior("normal(0,1)", class="b"), iter = 2000, chains=n_chains, cores=n_chains, backend = "cmdstanr", threads = threading(threads_per_chain), control = list(adapt_delta = 0.97), silent = 0, save_pars = save_pars(all = TRUE))

  M5 = brm(females ~ poly(mean_rent_consumption_unit,2) + (1 | trap_name), data=D, family = zero_inflated_poisson(), prior = set_prior("normal(0,1)", class="b"), iter = 2000, chains=n_chains, cores=n_chains, backend = "cmdstanr", threads = threading(threads_per_chain), control = list(adapt_delta = 0.97), silent = 0, save_pars = save_pars(all = TRUE))
  
    
  M1 = brm(females ~ poly(meanTM30,2) + poly(mean_rent_consumption_unit,2) + (1 | trap_name), data=D, family = poisson(), prior = set_prior("normal(0,1)", class="b"), iter = 2000, chains=n_chains, cores=n_chains, backend = "cmdstanr", threads = threading(threads_per_chain), control = list(adapt_delta = 0.97), silent = 0, save_pars = save_pars(all = TRUE))
  
  models = list("MTVM1" = M1, "MTVM2" = M2, "MTVM3" = M3, "MTVM4" = M4, "MTVM5" = M5)
  
return(models)
}

# MTVM comparison loos ####
make_mtvm_comparison_loos = function(mtvm_comparisons){
  
  n_cores = min(length(mtvm_comparisons), parallel::detectCores())
  
  loos = lapply(mtvm_comparisons, function(M){
    this_loo = loo(M, moment_match=TRUE, reloo = TRUE)
    return(this_loo)
  })
 
  names(loos) = names(mtvm_comparisons)
  
  return(loos) 
}
  
# MTVM comparison Bayes R sq ####
make_mtvm_comparison_br2s = function(mtvm_comparisons){
  
  n_cores = min(length(mtvm_comparisons), parallel::detectCores())
  
  bind_rows(mclapply(1:length(mtvm_comparisons), function(i){
    as_tibble(bayes_R2(mtvm_comparisons[[i]])) %>% mutate(model=names(mtvm_comparisons)[i])
  }, mc.cores = n_cores))
  
}

# MTVM comparison table ####
make_mtvm_comparison_table = function(mtvm_comparisons, mtvm_comparison_loos, mtvm_comparison_br2s){
  
  models = mtvm_comparisons
  
  model_names = names(models)
  
  names(mtvm_comparison_loos) = names(models)
  loo_comparison = loo_compare(mtvm_comparison_loos)
  
  lcb_beta = as_tibble(print(loo_comparison, simplify = FALSE))
  lcb_beta$model = row.names(loo_comparison)
  
  N = nrow(models[[1]]$data)
  
  gof_tab = mtvm_comparison_br2s %>% select(Estimate, Est.Error, model) %>% rename(r2_est = Estimate, r2_se = Est.Error) %>% left_join(lcb_beta %>% select(elpd_loo, se_elpd_loo, model)) %>% arrange(model)
  
  this_gof = lapply(model_names, function(this_model) {
    return(gof_tab %>% filter(model == this_model) %>% select(-model) %>% as.numeric())
  })
  
  these_gof_names = lapply(1:length(models), function(x){ return(c("Bayes R-sq.", "SE Bayes R-sq.", "ELPD", "SE ELPD"))})
  
  these_coef_names = list(c("Int.",  "TMP", "TMP sq.", "INC", "INC sq." ),  c("Int.",  "TMP", "TMP sq.", "ZI Int.", "ZI TMP", "ZI TMP sq."), c("Int.",  "TMP", "TMP sq.", "INC", "INC sq.", "ZI Int.", "ZI TMP", "ZI TMP sq."),  c("Int.",  "TMP", "TMP sq.", "INC", "INC sq.", "ZI Int.", "ZI TMP", "ZI TMP sq."),  c("Int.", "INC", "INC sq."))
  
  cust_rows = list("Random Intercepts" = c("trap", "trap", "none", "trap", "trap"), "Distribution" = c("Poisson", "ZI Poisson", "ZI Poisson", "ZI Poisson", "ZI Poisson"), Observations = rep(N, length(models)))
  
  this_filename = "figures/mosquito_trap_vector_model_table.tex"
  
  mcmcReg(models, pars = "b_", regex = TRUE, format = 'latex', gof = this_gof, gofnames = these_gof_names, custom.gof.rows = cust_rows, coefnames = these_coef_names, pointest="median", ci=.90, single.row=FALSE, sd=FALSE, custom.model.names = model_names, digits = 2, dcolumn = TRUE, use.packages = FALSE, center = FALSE, label = "mtvm", caption = "Parameter estimates for Mosquito Trap Vector Models.", file = this_filename)
  
return(this_filename)
  
}


# MAVM comparisons ####

fit_mavm_comparisons = function(mavm_main){
  
  n_cores = parallel::detectCores()
  n_chains = 4
  threads_per_chain = n_cores/n_chains

  M4 = mavm_main
  
  D = M4$data
  D2 = M4$data2
  
  M1 = brm(presence ~ offset(log(pred)) + poly(mean_rent_consumption_unit,2) + car(W, gr=CUSEC, type="icar"), data = D, data2 = D2, family=bernoulli(link="logit"), iter = 2000, chains=n_chains, cores=n_chains, backend = "cmdstanr", threads = threading(threads_per_chain), control = list(adapt_delta = 0.97), save_pars = save_pars(latent = TRUE), silent=0)
  
  M2 = brm(presence ~ offset(log(pred)) + ddf_proximity + car(W, gr=CUSEC, type="icar"), data = D, data2 = D2, family=bernoulli(link="logit"), iter = 2000, chains=n_chains, cores=n_chains, backend = "cmdstanr", threads = threading(threads_per_chain), control = list(adapt_delta = 0.97), save_pars = save_pars(latent = TRUE), silent=0)
  
  
  M3 = brm(presence ~ offset(log(pred)) + ddf_proximity + poly(mean_rent_consumption_unit,2), data = D, data2 = D2, family=bernoulli(link="logit"), iter = 2000, chains=n_chains, cores=n_chains, backend = "cmdstanr", threads = threading(threads_per_chain), control = list(adapt_delta = 0.97), save_pars = save_pars(latent = TRUE), silent=0)
  
  models = list("MAVM1" = M1, "MAVM2" = M2, "MAVM3" = M3, "MAVM4" = M4)
  
  return(models)
}
  
# MAVM compasison loos ####
make_mavm_comparison_loos = function(mavm_comparisons){
  
  n_cores = min(length(mavm_comparisons), parallel::detectCores())
  
  loos = mclapply(mavm_comparisons, function(M){
    loo(M)
  }, mc.cores = n_cores)
  
  names(loos) = names(mavm_comparisons)
  
  return(loos) 
  
}

# MAVM comparison Bayes R sq ####
make_mavm_comparison_br2s = function(mavm_comparisons){
  
  n_cores = min(length(mavm_comparisons), parallel::detectCores())
  
  bind_rows(mclapply(1:length(mavm_comparisons), function(i){
    as_tibble(bayes_R2(mavm_comparisons[[i]])) %>% mutate(model=names(mavm_comparisons)[i])
  }, mc.cores = n_cores))
  
}


# MAVM comparison table ####
make_mavm_comparison_table = function(mavm_comparisons, mavm_comparison_loos, mavm_comparison_br2s){
  
  models = mavm_comparisons
  
  model_names = names(models)
  
  loo_comparison = loo_compare(mavm_comparison_loos)
  
  lcb_beta = as_tibble(print(loo_comparison, simplify = FALSE))
  lcb_beta$model =row.names(loo_comparison)
  
  N = nrow(mavm_comparisons[[1]]$data)
  
  gof_tab = mavm_comparison_br2s %>% select(Estimate, Est.Error, model) %>% rename(r2_est = Estimate, r2_se = Est.Error) %>% left_join(lcb_beta %>% select(elpd_loo, se_elpd_loo, model)) %>% arrange(model)
  
  this_gof = lapply(model_names, function(this_model) {
    return(gof_tab %>% filter(model == this_model) %>% select(-model) %>% as.numeric())
  })
  
  these_gof_names = lapply(1:length(models), function(x){ return(c("Bayes R-sq.", "SE Bayes R-sq.", "ELPD", "SE ELPD"))})
  
  these_coef_names = list(c("Int.",  "INC", "INC sq." ),  c("Int.",  "GPI" ), c("Int.",  "GPI", "INC", "INC sq."),  c("Int.", "GPI", "INC", "INC sq."))
  
  this_filename = "figures/mosquito_alert_vector_model_table.tex"
  
  mcmcReg(models, pars = "b_", regex = TRUE, format = 'latex', gof = this_gof, gofnames = these_gof_names, custom.gof.rows = list("Spatial Autocorr." = c("icar", "icar", "none", "icar"), Observations = rep(N, length(models))), coefnames = these_coef_names, pointest="median", ci=.90, single.row=FALSE, sd=FALSE, custom.model.names = model_names, digits = 2, dcolumn = TRUE, use.packages = FALSE, center = FALSE, label = "tab:mavm", caption = "Parameter estimates for Mosquito Alert Vector Models.", file = this_filename)
  
  return(this_filename)
}



# ASDM comparisons ####
fit_asdm_comparisons = function(asdm_main){
 
  n_cores = parallel::detectCores()
  n_chains = 4
  threads_per_chain = n_cores/n_chains
  
  M6 = asdm_main
  
  D = M6$data
  
  M5 = brm(any_reports ~  SE_expected + poly(mean_rent_consumption_unit, 2) + log(popd) + p_singlehh + mean_age, data = D, family = bernoulli(link = "logit"), chains=n_chains, cores=n_chains, backend = "cmdstanr", threads = threads_per_chain, iter = 2000, silent = 0, control = list(adapt_delta = 0.97), save_pars = save_pars(all = TRUE))
  
  M4 = brm(any_reports ~  SE_expected + poly(mean_rent_consumption_unit, 2) + log(popd) + p_singlehh + (1 | id_item), data = D, family = bernoulli(link = "logit"), chains=n_chains, cores=n_chains, backend = "cmdstanr", threads = threads_per_chain, iter = 2000, silent = 0, control = list(adapt_delta = 0.97), save_pars = save_pars(all = TRUE))
  
  M3 = brm(any_reports ~  SE_expected + poly(mean_rent_consumption_unit, 2) + log(popd) + (1 | id_item), data = D, family = bernoulli(link = "logit"), chains=n_chains, cores=n_chains, backend = "cmdstanr", threads = threads_per_chain, iter = 2000, silent = 0, control = list(adapt_delta = 0.97), save_pars = save_pars(all = TRUE))
  
  M2 = brm(any_reports ~  SE_expected + poly(mean_rent_consumption_unit, 2) + (1 | id_item), data = D, family = bernoulli(link = "logit"), chains=n_chains, cores=n_chains, backend = "cmdstanr", threads = threads_per_chain, iter = 2000, silent = 0, control = list(adapt_delta = 0.97), save_pars = save_pars(all = TRUE))
  
  M1 = brm(any_reports ~ poly(mean_rent_consumption_unit, 2) + (1 | id_item), data = D, family = bernoulli(link = "logit"), chains=n_chains, cores=n_chains, backend = "cmdstanr", threads = threads_per_chain, iter = 2000, silent = 0, control = list(adapt_delta = 0.97), save_pars = save_pars(all = TRUE))
  
  models = list("ASDPM1" = M1, "ASDPM2" = M2, "ASDPM3" = M3, "ASDPM4" = M4, "ASDPM5" = M5, "ASDPM6" = M6)
  
  return(models)
  
}

# ASDM comparison loos ####
make_asdm_comparison_loos = function(asdm_comparisons){
  
  n_cores = min(length(asdm_comparisons), parallel::detectCores())
  
  loos = mclapply(asdm_comparisons, function(M){
    loo(M)
  }, mc.cores = n_cores)
  
  names(loos) = names(asdm_comparisons)
  
  return(loos) 

}

# ASDM comparison Bayes R sq ####
make_asdm_comparison_br2s = function(asdm_comparisons){
  
  n_cores = min(length(asdm_comparisons), parallel::detectCores())
  
  bind_rows(mclapply(1:length(asdm_comparisons), function(i){
    as_tibble(bayes_R2(asdm_comparisons[[i]])) %>% mutate(model=names(asdm_comparisons)[i])
  }, mc.cores = n_cores))
  
  
}

# ASDM comparison table ####
make_asdm_comparison_table = function(asdm_comparisons, asdm_comparison_loos, asdm_comparison_br2s){
  
  models = asdm_comparisons
  
model_names = names(models)

loo_comparison = loo_compare(asdm_comparison_loos)

lcb_beta = as_tibble(print(loo_comparison, simplify = FALSE))
lcb_beta$model = row.names(loo_comparison)

N = nrow(asdm_comparisons[[1]]$data)

gof_tab = asdm_comparison_br2s %>% select(Estimate, Est.Error, model) %>% rename(r2_est = Estimate, r2_se = Est.Error) %>% left_join(lcb_beta %>% select(elpd_loo, se_elpd_loo, model)) %>% arrange(model)

this_gof = lapply(model_names, function(this_model) {
  return(gof_tab %>% filter(model == this_model) %>% select(-model) %>% as.numeric())
})

these_gof_names = lapply(1:length(models), function(x){ return(c("Bayes R-sq.", "SE Bayes R-sq.", "ELPD", "SE ELPD"))})

these_coef_names = list(c("Int.", "INC", "INC sq." ),  c("Int.", "INC", "INC sq.", "SE"), c("Int.", "INC", "INC sq.", "SE", "POP"), c("Int.", "INC", "INC sq.", "SE", "POP", "PSH"), c("Int.", "INC", "INC sq.", "SE", "POP", "PSH", "AGE"), c("Int.", "INC", "INC sq.", "SE", "POP", "PSH", "AGE"))

cust_rows = list("Random Intercepts" = c("drain", "drain", "drain", "drain", "none", "drain"), Observations = rep(N, length(models)))

this_filename = "figures/drain_model_table.tex"

mcmcReg(models, pars = "b_", regex = TRUE, format = 'latex', gof = this_gof, gofnames = these_gof_names, custom.gof.rows = cust_rows, coefnames = these_coef_names, pointest="median", ci=.90, single.row=FALSE, sd=FALSE, custom.model.names = model_names, digits = 2, dcolumn = TRUE, use.packages = FALSE, center = FALSE, sideways = TRUE, label = "asdpm", caption = "Parameter estimates for Active Sewer Drain Participation Models.", file = this_filename)

return(this_filename)

}


# GPM comparisons ####
fit_gpm_comparisons = function(gpm_main){
  
  
  n_cores = parallel::detectCores()
  n_chains = 4
  threads_per_chain = n_cores/n_chains
  
  M5 = gpm_main
  
  D = M5$data
  D2 = M5$data2
  
  M4 = brm(presence ~ poly(mean_rent_consumption_unit, 2) + log(popd) + mean_age + p_singlehh + car(queens, gr=CUSEC, type="icar"), data = D, data2 = D2, family = bernoulli(link = "logit"), prior = set_prior("normal(0,2)", class="b"), chains=n_chains, cores=n_chains, backend = "cmdstanr", threads = threads_per_chain, iter = 2000, silent=0, save_pars = save_pars(all = TRUE))
  
  M3 = brm(presence ~ poly(mean_rent_consumption_unit, 2) + log(popd) + mean_age + car(queens, gr=CUSEC, type="icar"), data = D, data2 = D2, family = bernoulli(link = "logit"), prior = set_prior("normal(0,2)", class="b"), chains=n_chains, cores=n_chains, backend = "cmdstanr", threads = threads_per_chain, iter = 2000, silent=0, save_pars = save_pars(all = TRUE))
  
  M2 = brm(presence ~ poly(mean_rent_consumption_unit, 2) + log(popd) + car(queens, gr=CUSEC, type="icar"), data = D, data2 = D2, family = bernoulli(link = "logit"), prior = set_prior("normal(0,2)", class="b"), chains=n_chains, cores=n_chains, backend = "cmdstanr", threads = threads_per_chain, iter = 2000, silent=0, save_pars = save_pars(all = TRUE))
  
  M1 = brm(presence ~ poly(mean_rent_consumption_unit, 2) + car(queens, gr=CUSEC, type="icar"), data = D, data2 = D2, family = bernoulli(link = "logit"), prior = set_prior("normal(0,2)", class="b"), chains=n_chains, cores=n_chains, backend = "cmdstanr", threads = threads_per_chain, iter = 2000, silent=0, save_pars = save_pars(all = TRUE))

  models = list("GPM1" = M1, "GPM2" = M2, "GPM3" = M3, "GPM4" = M4, "GPM5" = M5)
  
  return(models)
}

# GPM comparison loos ####
make_gpm_comparison_loos = function(gpm_comparisons){
  
  n_cores = min(length(gpm_comparisons), parallel::detectCores())
  
  loos = mclapply(gpm_comparisons, function(M){
    loo(M)
  }, mc.cores = n_cores)
  
  names(loos) = names(gpm_comparisons)
  
  return(loos) 
  
}

# GPM comparison BR2s ####
make_gpm_comparison_br2s = function(gpm_comparisons){
  
  n_cores = min(length(gpm_comparisons), parallel::detectCores())
  
  bind_rows(mclapply(1:length(gpm_comparisons), function(i){
    as_tibble(bayes_R2(gpm_comparisons[[i]])) %>% mutate(model=names(gpm_comparisons)[i])
  }, mc.cores = n_cores))
  
}


# GPM comparison table ####
make_gpm_comparison_table = function(gpm_comparisons, gpm_comparison_loos, gpm_comparison_br2s){
  
  models = gpm_comparisons
  
  model_names = names(models)
  
  loo_comparison = loo_compare(gpm_comparison_loos)
  
  lcb_beta = as_tibble(print(loo_comparison, simplify = FALSE))
  lcb_beta$model = row.names(loo_comparison)
  
  N = nrow(gpm_comparisons[[1]]$data)
  
  gof_tab = gpm_comparison_br2s %>% select(Estimate, Est.Error, model) %>% rename(r2_est = Estimate, r2_se = Est.Error) %>% left_join(lcb_beta %>% select(elpd_loo, se_elpd_loo, model)) %>% arrange(model)
  
  this_gof = lapply(model_names, function(this_model) {
    return(gof_tab %>% filter(model == this_model) %>% select(-model) %>% as.numeric())
  })
  
  these_gof_names = lapply(1:length(models), function(x){ return(c("Bayes R-sq.", "SE Bayes R-sq.", "ELPD", "SE ELPD"))})
  
  mcmcReg(models, pars = "b_", regex = TRUE, format = 'latex')
  
  these_coef_names = list(c("Int.", "INC", "INC sq." ), c("Int.", "INC", "INC sq.", "POP"), c("Int.", "INC", "INC sq.", "POP", "AGE"), c("Int.", "INC", "INC sq.", "POP", "AGE", "PSH"), c("Int.", "INC", "INC sq.", "POP", "AGE", "PSH"))
  
  cust_rows = list("Spatial autocorr." = c("icar", "icar", "icar", "none", "icar"), Observations = rep(N, length(models)))
  
  this_filename = "figures/general_participation_model_table.tex"
  
  mcmcReg(models, pars = "b_", regex = TRUE, format = 'latex', gof = this_gof, gofnames = these_gof_names, custom.gof.rows = cust_rows, coefnames = these_coef_names, pointest="median", ci=.90, single.row=FALSE, sd=FALSE, custom.model.names = model_names, digits = 2, dcolumn = TRUE, use.packages = FALSE, center = FALSE, sideways = TRUE, label = "gpm", caption = "Parameter estimates for General Participation Models.", file = this_filename)
  
  return(this_filename)
  
}

# Descriptive Figures ####

make_descriptive_figures = function(data_clean, bcn_chars, bcn_perimeter_polygon, bcn_census_tract_polygons){
  
  these_figures = NULL
  
  bcn_pop = bcn_census_tract_polygons %>% left_join(bcn_chars) %>% mutate(popd = population/(Shape_area/10000)) 
  
  bcn_pop <- bcn_pop %>% 
    mutate(inctile = ntile(mean_rent_consumption_unit, 5),
           singletile = ntile(p_singlehh, 5),
           popdtile = ntile(popd, 5),
           mean_housing_age_tile = ntile(mean_housing_age, 5),
           p_spanish_tile = ntile(p_spanish, 5),
           constant = 1)
  

  bcn_perimeter = bcn_perimeter_polygon
  
  D <- data_clean %>% 
    filter(presence == T & type != "site") %>% 
    mutate(presence = as.numeric(presence)) %>% 
    st_transform(st_crs(bcn_perimeter)) %>%
    st_filter(bcn_perimeter, .predicate = st_intersects)
  
  
  # sampling cell grid
  sampling_cells_small = st_make_grid(bcn_perimeter %>% st_transform(4326), cellsize = c(.025, .025), what = "polygons") %>% st_transform(st_crs(bcn_perimeter)) %>% st_intersection(bcn_perimeter)
  
  
  this_figure = "figures/bcn_sampling_cells.png"
  these_figures = c(these_figures, this_figure)
  
  ggplot(sampling_cells_small) + geom_sf() + annotation_scale()
  ggsave(this_figure, width=6, height=6)
  
  
  fig1A <- tm_shape(bcn_perimeter)+
    tm_polygons(col = "#ffffbb", border.col = "#aaaaaa", lwd = .3)+
    tm_shape(D)+
    tm_dots("presence", col = "#003366", size = .05, border.lwd = 0, alpha = .3 )+
    tm_layout(frame = FALSE, legend.show = FALSE) + tm_scale_bar(position=c("left", "bottom"))
  
  this_figure = "figures/fig1A.png"
  these_figures = c(these_figures, this_figure)
  
  tmap_save(fig1A, filename = this_figure, dpi = 600)
  
  
  fig1B <- tm_shape(bcn_pop)+
    tm_polygons(col = "inctile", palette = viridisLite::viridis(5), lwd = .3, title = "Income", labels = c("10-19k", "19-22k", "22-25k", "25-29k", "29-51k"))+
    tm_layout(legend.title.size = 1,
              legend.text.size = .7) + tm_scale_bar(position=c("left", "top"))
  
  this_figure = "figures/fig1B.png"
  these_figures = c(these_figures, this_figure)
  
  tmap_save(fig1B, filename = this_figure, dpi = 600)
  
  fig1C <- tm_shape(bcn_pop)+
    tm_polygons(col = "singletile", palette = viridisLite::viridis(5), lwd = .3, title = "% One-person households", labels = c("16-28", "28-31", "31-34", "34-37", "37-48"))+
    tm_layout(legend.title.size = .89,
              legend.text.size = .7) + tm_scale_bar(position=c("left", "bottom"))
  
  this_figure = "figures/fig1C.png"
  these_figures = c(these_figures, this_figure)
  
  tmap_save(fig1C, filename = this_figure, dpi = 600)
  
  fig1D <- tm_shape(bcn_pop)+
    tm_polygons(col = "popdtile", palette = viridisLite::viridis(5), lwd = .3, title = "Population density", labels = c("1-222", "222-358", "358-460", "460-598", "598-1553"))+
    tm_layout(legend.title.size = 1,
              legend.text.size = .7) + tm_scale_bar(position=c("left", "bottom"))
  
  this_figure = "figures/fig1D.png"
  these_figures = c(these_figures, this_figure)
  
  tmap_save(fig1D, filename = this_figure, dpi = 600)
  
  fig1E <- tm_shape(bcn_pop)+
    tm_polygons(col = "p_spanish_tile", palette = viridisLite::viridis(5), lwd = .3, title = "% Spanish citizens", labels = c("19-75", "75-80", "80-84", "84-88", "88-98"))+
    tm_layout(legend.title.size = 1,
              legend.text.size = .7)
  
  this_figure = "figures/fig1E.png"
  these_figures = c(these_figures, this_figure)
  
  tmap_save(fig1E, filename = this_figure, dpi = 600)
  
}

