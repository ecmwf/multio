set(MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Enter in the module
message("${OFFSET} + grib2 section4_level sources: ${MULTIOM_ENCODERS_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})


# Collect source files in module2
set( MULTIOM_ENCODER_GRIB2_SECTION4_LEVEL_MAIN_SOURCES
  ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_surface_mod.F90
  ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_snowlayer_mod.F90
  ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_soillayer_mod.F90
  ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_seaicelayer_mod.F90
  ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_hybrid_mod.F90
  ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_isobaricinhpa_mod.F90
  ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_isobaricinpa_mod.F90
  ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_potentialvorticity_mod.F90
  ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_theta_mod.F90
  ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_heightaboveground_mod.F90
  ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_custom_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_adiabaticcondensation_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_atmosphere_mod.F90
  ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_cloudbase_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_cloudtop_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_convectivecondensation_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_cumulonimbusbase_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_cumulonimbustop_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_depthbelowlandlayer_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_depthbelowland_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_depthbelowsealayer_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_depthbelowsea_mod.F90
  ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_entireatmosphere_mod.F90
  ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_entirelake_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_entiremeltpond_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_entireocean_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_eta_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_freeconvection_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_generalverticallayer_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_generalvertical_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_heightabovegroundlayer_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_heightabovesealayer_mod.F90
  ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_heightabovesea_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_highcloudlayer_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_hybridheight_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_hybridlayer_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_hybridpressure_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_icebottomonwater_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_icelayerabovewatersurface_mod.F90
  ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_icelayeronwater_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_icelayerundersnowonwater_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_icetoponwater_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_icetopundersnowonwater_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_indefinitesoildepth_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_isobariclayer_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_isothermal_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_isothermzero_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_lakebottom_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_lowcloudlayer_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_maxwind_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_meansea_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_mediumcloudlayer_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_meltpondbottom_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_meltpondtop_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_mixedlayerdepthbydensity_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_mixedlayerdepthbydiffusivity_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_mixedlayerdepthbytemperature_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_mixedlayerdepthgeneric_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_mixedlayerparcel_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_mixinglayer_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_mostunstableparcel_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_neutralbuoyancy_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_nominaltop_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_oceanmodellayer_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_oceanmodel_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_oceansurface_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_oceansurfacetobottom_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_pressurefromgroundlayer_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_pressurefromground_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_roadlayer_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_road_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_rooflayer_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_roof_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_rootzone_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_seabottom_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_seaice_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_sigmalayer_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_sigma_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_snowlayerovericeonwater_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_snow_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_snowtopovericeonwater_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_soil_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_thetalayer_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_totalsoillayer_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_tropopause_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_urbancanyon_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_walllayer_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_wall_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_watersurfacetoisothermaloceanlayer_mod.F90
  ${MULTIOM_ENCODERS_GRIB2_SECTION4_LEVEL_DIR}/grib2_section4_level_factory_mod.F90
)

# Collect source files in module2
set( MULTIOM_ENCODER_GRIB2_SECTION4_LEVEL_SOURCES
  ${MULTIOM_ENCODER_GRIB2_SECTION4_LEVEL_MAIN_SOURCES}
  CACHE INTERNAL "List of all sources in encoders directory"
)

# Optionally, message what has been appended for debugging
foreach(source_file IN LISTS MULTIOM_ENCODER_GRIB2_SECTION4_LEVEL_MAIN_SOURCES)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()