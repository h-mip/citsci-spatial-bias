# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("tibble", "jsonlite") # Packages that your targets need for their tasks.
  # format = "qs", # Optionally set the default storage format. qs is fast.
  #
  # Pipelines that take a long time to run may benefit from
  # optional distributed computing. To use this capability
  # in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller that scales up to a maximum of two workers
  # which run as local R processes. Each worker launches when there is work
  # to do and exits if 60 seconds pass with no tasks to run.
  #
  #   controller = crew::crew_controller_local(workers = 2, seconds_idle = 60)
  #
  # Alternatively, if you want workers to run on a high-performance computing
  # cluster, select a controller from the {crew.cluster} package.
  # For the cloud, see plugin packages like {crew.aws.batch}.
  # The following example is a controller for Sun Grid Engine (SGE).
  # 
  #   controller = crew.cluster::crew_controller_sge(
  #     # Number of workers that the pipeline can scale up to:
  #     workers = 10,
  #     # It is recommended to set an idle time so workers can shut themselves
  #     # down if they are not running tasks.
  #     seconds_idle = 120,
  #     # Many clusters install R as an environment module, and you can load it
  #     # with the script_lines argument. To select a specific verison of R,
  #     # you may need to include a version string, e.g. "module load R/4.3.2".
  #     # Check with your system administrator if you are unsure.
  #     script_lines = "module load R"
  #   )
  #
  # Set other options as needed.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# tar_source("other_functions.R") # Source other scripts as needed.

# Seed ####
# setting seed to make it reproducible 
tar_option_set(seed = 321)

# Targets list ####

# Replace the target list below with your own:
list(
  ## file names ####
  tar_target(
    bcn_census_tract_file,
    "data/external/seccionado_2017/SECC_CE_20170101.shp",
    format = "file"
  ),
  tar_target(bcn_sociodem_file, "data/external/socecon.json", format = "file"),
  tar_target(bcn_income_file, "data/external/30896.json", format = "file"),
  tar_target(
    bcn_socio_ec_file,
    "data/external/2020_atles_renda_index_gini.csv",
    format = "file"
  ),
  tar_target(
    bcn_housing_age_file,
    "data/external/2020_loc_hab_edat_mitjana.csv",
    format = "file"
  ),
  tar_target(
    urban_atlas_file,
    "data/external/urban_atlas_bcn/ES002L2_BARCELONA_UA2018_v012/Data/ES002L2_BARCELONA_UA2018_v012.gpkg",
    format = "file"
  ),
  tar_target(
    bcn_ages_file,
    "data/external/age-censusdis-1.1.17.csv",
    format = "file"
  ),
  tar_target(
    bcn_cens_incomes_file,
    "data/external/Medium.income_censusdis_BCN_2015-17.csv",
    format = "file"
  ),
  tar_target(
    drain_data_file,
    "data/private/dades_items_revisats_1-1-2019_31-12-2023.xlsx",
    format = "file"
  ),
  tar_target(ndvi_data_file, "data/private/NDVI_privat.shp", format = "file"),
  tar_target(
    bcn_bg_traps_file,
    "data/private/bcn_bg_traps.Rds",
    format = "file"
  ),
  ## loading data ####
  tar_target(bcn_bg_traps, get_bcn_bg_traps(bcn_bg_traps_file)),
  tar_target(landcover_data, get_urban_atlas_data(urban_atlas_file)),
  tar_target(
    bcn_census_tract_polygons,
    get_bcn_census_tract_shapes(bcn_census_tract_file)
  ),
  tar_target(ndvi, get_ndvi(ndvi_data_file)),
  tar_target(
    bcn_perimeter_polygon,
    generate_perimeter(bcn_census_tract_polygons)
  ),
  tar_target(malert_reports_all, get_malert_reports_all()),
  tar_target(
    malert_reports_validated,
    get_malert_reports_albopictus_validated()
  ),
  tar_target(malert_sampling_effort, get_malert_sampling_effort_data()),
  tar_target(drain_data, get_drain_data(drain_data_file)),
  tar_target(
    bcn_chars,
    make_bcn_characteristics(
      bcn_sociodem_file,
      bcn_income_file,
      bcn_socio_ec_file,
      bcn_housing_age_file
    )
  ),
  ## wrangling data ####
  tar_target(
    census_tracts_merged,
    merge_census_tract_data(bcn_census_tract_polygons, bcn_chars)
  ),
  tar_target(
    pres_abs_data,
    make_presence_absence_data(bcn_perimeter_polygon, malert_reports_all)
  ),
  tar_target(
    data_clean,
    merge_datasets(
      pres_abs_data,
      landcover_data,
      bcn_ages_file,
      bcn_cens_incomes_file,
      census_tracts_merged
    )
  ),
  ## GPM Main ####
  tar_target(
    gpm_main,
    fit_gpm_main(bcn_census_tract_polygons, data_clean)
  ),
  tar_target(
    gpm_main_ces_plots, 
    plot_gpm_main_ces(gpm_main)
    ),
  ## ASDM Main ####
  tar_target(
    asdm_data_clean,
    asdm_data_prep(
      bcn_chars,
      census_tracts_merged,
      malert_sampling_effort,
      drain_data,
      data_clean
    )
  ),
  tar_target(
    drain_map, 
    make_drain_map(
      asdm_data_clean, 
      bcn_perimeter_polygon, 
      bcn_census_tract_polygons
      )
    ),
  tar_target(asdm_main, fit_asdm_main(asdm_data_clean)),
  tar_target(
    asdm_main_ces_plots,
    plot_asdm_ces(asdm_main, "drain_model_M_final_CEs")
  ),
  tar_target(asdm_robust, fit_asdm_robust_check(asdm_data_clean)),
  tar_target(
    asdm_robust_ces_plots,
    plot_asdm_ces(asdm_robust, "drain_model_M_final_spaced200m")
  ),
  tar_target(asdm_prediction_points, make_asdm_prediction_points(asdm_main, malert_sampling_effort, ndvi, landcover_data, bcn_perimeter_polygon, census_tracts_merged, bcn_chars)),
  ## MAVM Main ####
  tar_target(
    mavm_data_clean,
    prepare_mavm_data(
      asdm_main, asdm_prediction_points,
      malert_sampling_effort,
      ndvi,
      landcover_data,
      census_tracts_merged,
      bcn_chars,
      malert_reports_validated
    )
  ),
  tar_target(mavm_main, fit_mavm_main(mavm_data_clean)),
  tar_target(mavm_main_ces_plots, plot_mavm_ces(mavm_main)),
  tar_target(mavm_main_no_se, fit_mavm_main_no_se(mavm_data_clean)),
  tar_target(
    mavm_prediction_points,
    make_mavm_prediction_points(
      mavm_main, asdm_prediction_points,
      mavm_main_no_se,
      bcn_perimeter_polygon,
      ndvi,
      census_tracts_merged,
      landcover_data
    )
  ),
  tar_target(
    mavm_prediction_figures, 
    make_mavm_prediction_figures(
      mavm_prediction_points, 
      bcn_perimeter_polygon
      )
    ),
  ## MTVM Main ####
  tar_target(
    mtvm_main,
    fit_mtvm_main(bcn_bg_traps, bcn_chars, census_tracts_merged, ndvi)
  ),
  tar_target(
    trap_map,
    make_trap_map(
      bcn_bg_traps, 
      bcn_census_tract_polygons)
  ),
  tar_target(
    mtvm_plots,
    plot_mtvm_ces_plus_comparison(mtvm_main, mavm_main)
  ),
  ## Model Comparisons ####
  tar_target(
    mtvm_comparisons, 
    fit_mtvm_comparisons(mtvm_main)
  ),
  tar_target(
    mtvm_comparison_loos,
    make_mtvm_comparison_loos(mtvm_comparisons)
  ),
  tar_target(
    mtvm_comparison_br2s,
    make_mtvm_comparison_br2s(mtvm_comparisons)
  ),
  tar_target(
    mtvm_comparison_table,
    make_mtvm_comparison_table(mtvm_comparisons, mtvm_comparison_loos, mtvm_comparison_br2s)
  ),
  tar_target(
    mavm_comparisons, 
    fit_mavm_comparisons(mavm_main)
  ),
  tar_target(
    mavm_comparison_loos,
    make_mavm_comparison_loos(mavm_comparisons)
  ),
  tar_target(
    mavm_comparison_br2s,
    make_mavm_comparison_br2s(mavm_comparisons)
  ),
  tar_target(
    mavm_comparison_table,
    make_mavm_comparison_table(mavm_comparisons, mavm_comparison_loos, mavm_comparison_br2s)
  ),
  tar_target(
    asdm_comparisons, 
    fit_asdm_comparisons(asdm_main)
  ),
  tar_target(
    asdm_comparison_loos,
    make_asdm_comparison_loos(asdm_comparisons)
  ),
  tar_target(
    asdm_comparison_br2s,
    make_asdm_comparison_br2s(asdm_comparisons)
  ),
  tar_target(
    asdm_comparison_table,
    make_asdm_comparison_table(asdm_comparisons, asdm_comparison_loos, asdm_comparison_br2s)
  ),
  tar_target(
    gpm_comparisons, 
    fit_gpm_comparisons(gpm_main)
  ),
  tar_target(
    gpm_comparison_loos,
    make_gpm_comparison_loos(gpm_comparisons)
  ),
  tar_target(
    gpm_comparison_br2s,
    make_gpm_comparison_br2s(gpm_comparisons)
  ),
  tar_target(
    gpm_comparison_table,
    make_gpm_comparison_table(gpm_comparisons, gpm_comparison_loos, gpm_comparison_br2s)
  ),
  ## Descriptive Figures ####
  tar_target(
    descriptive_figures, 
    make_descriptive_figures(
      data_clean, bcn_chars, 
      bcn_perimeter_polygon, 
      bcn_census_tract_polygons)
  ),
  tar_target(
    colinearity_gpm,
    check_colinearity_gpm(data_clean)
  ),
  tar_target(
    colinearity_asdm,
    check_colinearity_asdm(asdm_data_clean)
  )
  
)

# visNetwork::visSave(tar_visnetwork(), file = "visnetwork.html")
