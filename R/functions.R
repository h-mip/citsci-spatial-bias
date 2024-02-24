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

# helper functions ####
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
  return(unlist(mclapply(chunks, function(i){
    message_parallel(paste0(i, " of ", nrow(newdata)))
    apply(posterior_predict(model, newdata = newdata[i:min(nrow(newdata), (i+(chunksize-1))), ], draws = draws, allow_new_levels = allow_new_levels), 2, function(x) mean(x))
  }, mc.cores = cores)))
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
  mutate(census_section = str_extract(Nombre, "\\d{5}"),
         var = str_match(Nombre, "\\d{5}\\.\\s(.*?)\\.\\sBase data\\.")[,2])

reshaped_socdem <- pivot_wider(unnested_df, 
                               id_cols = census_section,
                               names_from = var, 
                               values_from = Valor)


# Renta ####
bcn_inc = fromJSON(income_file, simplifyDataFrame = T) %>% 
  filter(str_detect(Nombre, "Barcelona sección"))

filtered_df <- bcn_inc %>%
  mutate(Data = purrr::map(Data, ~ filter(.x, Anyo == "2020"))) # Removing the "Anyo" column since all rows have the same value

# Unnest the filtered data frames
unnested_df <- filtered_df %>%
  unnest(cols = Data) %>% 
  mutate(census_section = str_extract(Nombre, "\\d{5}"),
         var = sub(".*\\. (.*)\\.", "\\1", Nombre))

reshaped_inc <- pivot_wider(unnested_df, 
                            id_cols = census_section,
                            names_from = var, 
                            values_from = Valor)

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
