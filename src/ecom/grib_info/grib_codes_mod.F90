MODULE GRIB_CODES_MOD

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

IMPLICIT NONE



!     ------------------------------------------------------------------
!*    GRIB CODING DESCRIPTORS


! NGRB...  - Fixed GRIB codes according to ECMWF local Code Table 2
! The characters after NGRB should be the same as the MARS short name !!!!!!
! NGRBd.. where d.. are digits means not assigned (yet) in MARS
! Some of the descriptions below might not agree with MARS manual (they should)

! NGRBSTRF  -  1 Stream function
! NGRBVP    -  2 Velocity potential
! NGRBPT    -  3 Potential Temperature
! NGRBSRO   -  8 Surface Runoff
! NGRBSSRO  -  9 Sub-Surface Runoff 
! NGRBALUVP - 15 MODIS albedo UV-vis parallel radiation
! NGRBALUVD - 16 MODIS albedo UV-vis diffuse radiation
! NGRBALNIP - 17 MODIS albedo Near-IR parallel radiation
! NGRBALNID - 18 MODIS albedo Near-IR diffuse radiation

! NGRBALUVI - 210186 MODIS albedo UV-vis parallel radiation (isom.)
! NGRBALNII - 210189 MODIS albedo Near-IR parallel radiation (isom.)
! NGRBALUVV - 210187 MODIS albedo UV-vis parallel radiation (volu.)
! NGRBALNIV - 210190 MODIS albedo Near-IR parallel radiation (volu.)
! NGRBALUVG - 210188 MODIS albedo UV-vis parallel radiation (geom.)
! NGRBALNIG - 210191 MODIS albedo Near-IR parallel radiation (geom.)

! NGRBPARCS - 20 surface clear-sky PARadiation
! NGRBCL   -  26 Lake cover
! NGRBCVL  -  27 Low vegetation cover
! NGRBCVH  -  28 High vegetation cover
! NGRBFWET -  200026 Wetland Fraction (experimental)
! NGRBCUR  -  200199 Urban Cover (experimental)
! NGRBTVL  -  29 Low vegetation type
! NGRBTVH  -  30 High vegetation type
! NGRBCI   -  31 Sea ice cover
! NGRBASN  -  32 Snow albedo
! NGRBRSN  -  33 Snow density
! NGRBSSTK  - 34 Sea surface temperature
! NGRBISTL1 - 35 Ice surface temperature layer 1
! NGRBISTL2 - 36 Ice surface temperature layer 2
! NGRBISTL3 - 37 Ice surface temperature layer 3
! NGRBISTL4 - 38 Ice surface temperature layer 4
! NGRBSWVL1 - 39 Volumetric soil water content layer 1
! NGRBSWVL2 - 40 Volumetric soil water content layer 2
! NGRBSWVL3 - 41 Volumetric soil water content layer 3      
! NGRBSWVL4 - 42 Volumetric soil water content layer 4 
! NGRBSLT   - 43 Soil type
! NGRBES   -  44 Evaporation of snow
! NGRBSMLT -  45 Snow melt
! NGRBDSRP -  47 Direct solar radiation 
!                Incident on a plane perpendicular to the Sun's direction
! NGRB10FG -  49 gust at 10 m level
! NGRBLSPF -  50 large scale precipitation fraction 

! NGRBMONT  - 53 Montgomery Geopotential
! NGRBPRES  - 54 Pressure on Theta and PV surfaces

! NGRBUVB  -  57 surface UV-B radiation
! NGRBPAR  -  58 surface PARadiation
! NGRBCAPE -  59 convect.avail.potential energy
! NGRBMLCAPE50  - 228231 Convective Inhibition for near surface 50 hPa mixed layer parcel
! NGRBMLCAPE100 - 228233 Convective Inhibition for near surface 100 hPa mixed layer parcel
! NGRBMUCAPE - 228235 Most Unstable CAPE (using Tv)
! NGRBMUDEPL  - 228237 Departure level (Pa) of Most Unstable CAPE
! NGRBCAPES-  228044 wind shear*sqrt(cape)
! NGRBMXCAP6- 228035 maximum CAPE in the last 6 hours
! NGRBMXCAPS6-228036 maximum CAPES in the last 6 hours
 
! NGRBPV   -  60 Potential Vorticity
! NGRBLAIL -  66 Leaf Area Index Low vegitation
! NGRBLAIH -  67 Leaf Area Index High vegitation

! NGRBSDFOR - 74 standard deviation of a filtered orography

! NGRBCRWC -  75 Precipitating rain water content
! NGRBCSWC -  76 Precipitating snow water content

! NGRBTCLW -  78 Total column liquid water
! NGRBTCIW -  79 Total column ice water
! NGRBTCRW -  228089 Total column rain water
! NGRBTCSW -  228090 Total column snow water
! NGRBTCSLW - 228088 Total column supercooled liquid water
! NGRBSPD  -  80 !! 80 and 81 extra grib code introduced to 
! NGRBSVD  -  81 !! introduce extra fields for NH Not MARS codes!!!!!!

! NGRB082 to NGRB117 reserved for extra fields. Do not use for permanent post-processed fields

! Codes for Extended control variable
! NGRBALPHA  - 90 Alpha control variable 
! NGRBSKTECV - 91 Skin temperatures
! NGRBSSHECV - 92 Sea surface height
! NGRBTSLECV - 93 Surface temperatures
! NGRBSDFORE - 100 Standard deviation of a filtered orography

! Codes for the lake model
! NGRBDL    - 228007 Lake depth
! NGRBLMLT  - 228008 Lake mix-layer temperature
! NGRBLMLD  - 228009 Lake mix-layer depth
! NGRBLBLT  - 228010 Lake bottom layer temperature
! NGRBLTLT  - 228011 Lake total layer temperature
! NGRBLSHF  - 228012 Lake shape factor
! NGRBLICT  - 228013 Lake ice temperature
! NGRBLICD  - 228014 Lake ice depth

! Codes for 2D and 3D extra fields
! NGRBMINXTRA  to NGRBMAXXTRA

! NGRBMX2T3 - 228026 Maximum temperature at 2 m since last 3 hours 
! NGRBMN2T3 - 228027 Minimum temperature at 2 m since last 3 hours 
! NGRB10FG3 - 228028 Wind gust at 10 metres since last 3 hours
! NGRBI10FG - 228029 Wind gust at 10 metres ("instantaneous")
! NGRBMX2T6 - 121 Maximum temperature at 2 m since last 6 hours 
! NGRBMN2T6 - 122 Minimum temperature at 2 m since last 6 hours 
! NGRB10FG6 - 123 Wind gust at 10 metres since last 6 hours 
! NGRBEMIS  - 124 Surface Longwave emissivity # has replaced NGRB212

! NGRBETADOT - 077 Etadotdpdeta

! NGRBAT   - 127 Atmospheric tide
! NGRBBV   - 128 Budget values
! NGRBZ    - 129 Geopotential (at the surface orography)
! NGRBT    - 130 Temperature
! NGRBU    - 131 U-velocity
! NGRBV    - 132 V-velocity
! NGRBUCUR - 262140 Eastward surface sea water velocity
! NGRBVCUR - 262139 Northward surface sea water velocity
! NGRBSSS  - 151130 sea surface salinity, (ocean table 151)

! NGRBQ    - 133 Specific humidity
! NGRBSP   - 134 Surface pressure
! NGRBW    - 135 Vertical velocity
! NGRBTCW  - 136 Total column water
! NGRBTCWV - 137 Total column water vapour
! NGRBVO   - 138 Vorticity (relative)
! NGRBSTL1 - 139 Surface temperature level 1
! NGRBSDSL - 141 Snow depth (total)
! NGRBSD   - 228141 Snow depth (multi-layer)
! NGRBWSN  - 228038 Snow liquid water (multi-layer)
! NGRBLSP  - 142 Large scale precipitation
! NGRBCP   - 143 Convective precipitation
! NGRBSF   - 144 Snow fall
! NGRBFZRA - 228216 Freezing rain accumulation
! NGRBBLD  - 145 Boundary layer dissipation
! NGRBSSHF - 146 Surface sensible heat flux
! NGRBSLHF - 147 Surface latent heat flux
! NGRBCHNK - 148 Charnock parameter 
! NGRBSNR  - 149 Surface net radiation
! NGRBTNR  - 150 Top net radiation
! NGRBMSL  - 151 Mean sea level pressure
! NGRBLNSP - 152 Log surface pressure
! NGRBSWHR - 153 Not used?
! NGRBLWHR - 154 Not used?
! NGRBD    - 155 Divergence
! NGRBGH   - 156 Height (geopotential)
! NGRBR    - 157 Relative humidity
! NGRBTSP  - 158 Tendency of surface pressure
! NGRBBLH  - 159 Boundary layer height
! NGRBSDOR - 160 Standard deviation of orography
! NGRBISOR - 161 Anisotropy of subgrid scale orography
! NGRBANOR - 162 Angle of subgrid scale orography
! NGRBSLOR - 163 Slope of subgrid scale orography
! NGRBTCC  - 164 Total cloud cover
! NGRB10U  - 165 10 metre u wind
! NGRB10V  - 166 10 metre v wind
! NGRBZUST - 228003 Friction velocity
! NGRBFDIR - 228021 Surface total sky direct SW
! NGRBCDIR - 228022 Surface clear sky direct SW
! NGRBCBASE- 228023 Cloud base level
! NGRB0DEGL- 228024 Zero deg. level
! NGRBM10DEGL- 228020 -10 deg. level
! NGRBVISIH- 3020 Visibility ! Changed from 228025 to be WMO compliant
! NGRBMUCIN  - 228236 Convective Inhibition = MUCIN
! NGRBMLCIN50  - 228232 Convective Inhibition for near surface 50 hPa mixed layer parcel
! NGRBMLCIN100 - 228234 Convective Inhibition for near surface 100 hPa mixed layer parcel
! NGRBKINDEX - 260121 Convective K-Index
! NGRBTTINDEX- 260123 Convective TT-Index
! NGRBCBASEA-260109 Ceiling = Cloud base level aviation
! NGRBCTOPC- 228046 Cloud top level convective
! NGRBZTWETB0-228047 Zero Deg Wet Bulb temperature
! NGRBZTWETB1-228048 One  Deg Wet Bulb temperature
! NGRBTROPOTP-228045 Pressure at thermal tropopause
! NGRB10NU - 228131 10 metre u neutral wind
! NGRB10NV - 228132 10 metre v neutral wind
! NGRB2T   - 167 2 metre temperature
! NGRB2D   - 168 2 metre dewpoint temperature
! NGRB2R   - 260242 2 metre relative humidity
! NGRB2SH  - 174096 2 metre specific humidity
! NGRBSSRD - 169 Surface solar radiation downwards
! NGRBSTL2 - 170 Soil temperature level 2
! NGRBLSM  - 172 Land/sea mask
! NGRBSR   - 173 Surface roughness
! NGRBAL   - 174 Albedo
! NGRBSTRD - 175 Surface thermal radiation downwards
! NGRBSSR  - 176 Surface solar radiation
! NGRBSTR  - 177 Surface thermal radiation
! NGRBTSR  - 178 Top solar radiation
! NGRBTTR  - 179 Top thermal radiation
! NGRBEWSS - 180 U-stress
! NGRBNSSS - 181 V-stress
! NGRBE    - 182 Evaporation
! NGRBPEV  - 228251 Potential evaporation
! NGRBSTL3 - 183 Soil temperature level 3
! NGRBCCC  - 185 Convective cloud cocer
! NGRBLCC  - 186 Low cloud cover
! NGRBMCC  - 187 Medium cloud cover
! NGRBHCC  - 188 High cloud cover
! NGRBSUND - 189 Sunshine duration
! NGRBEWOV - 190 EW component of sub-grid scale orographic variance
! NGRBNSOV - 191 NS component of sub-grid scale orographic variance
! NGRBNWOV - 192 NWSE component of sub-grid scale orographic variance
! NGRBNEOV - 193 NESW component of sub-grid scale orographic variance
! NGRBBTMP - 194 Brightness temperature (K)
! NGRBCLBT - 260510 Cloudy brightness temperature
! NGRBCSBT - 260511 Clear-sky brightness temperature
! NGRBCDRFL - 260512 Cloudy reflectance
! NGRBCRRFL - 260513 Clear-sky reflectance
! NGRBLGWS - 195 Latitudinal component of gravity wave stress
! NGRBMGWS - 196 Meridional component of gravity wave stress
! NGRBGWD  - 197 Gravity wave dissipation
! NGRBSRC  - 198 Skin reservoir content
! NGRBVEG  - 199 Percentage of vegetation
! NGRBVSO  - 200 variance of sub-grid scale orogrophy
! NGRBMX2T - 201 Maximum temperature at 2m since last post-processing
! NGRBMN2T - 202 Minimum temperature at 2m since last post-processing
! NGRBO3   - 203 Ozone mixing ratio (EC prognostic ozone)
! NGRBPAW  - 204 Precipitation analysis weights
! NGRBRO   - 205 Runoff
! NGRBTCO3 - 206 Total column ozone 
! NGRB10SI - 207 10m wind speed
! NGRBTSRC - 208 Top solar radiation clear sky 
! NGRBTTRC - 209 Top thermal radiation clear sky
! NGRBSSRC - 210 Surface solar radiation clear sky
! NGRBSTRC - 211 Surface thermal radiation clear sky

! NGRBTISR - 212 TOA incident solar radiation


!-- bunch of codes, confusing in their use ...
! NGRBDHR  - 214 Diabatic heating by radiation ?
! NGRBDHVD - 215 Diabatic heating by vertical diffusion?
! NGRBDHCC - 216 Diabatic heating by cumulus convection?
! NGRBDHLC - 217 Diabatic heating by large-scale condensation?
! NGRBVDZW - 218 INTSURFTEMPERATU (vertical diffusion of zonal wind?)
! NGRBVDMW - 219 PROFTEMPERATURE (vertical diffusion of meridional wind?) 

! NGRB221  - 221 Not used
! NGRBCTZW - 222 Convective tendency of zonal wind
! NGRBCTMW - 223 Convective tendency of meridional wind
! NGRBVDH  - 224 Not used (Vertical diffusion of humidity)
! NGRBHTCC - 225 Not used (Humidity tendency by cumulus convection)
! NGRBHTLC - 226 Not used (Humidity tendency by large-scale condensation)
! NGRBCRNH - 227 Not used (Change from removal of negative humidity)
! NGRBTP   - 228 Total precipitation
! NGRBIEWS - 229 Istantaneous X-surface stress
! NGRBINSS - 230 Istantaneous Y-surface stress
! NGRBISHF - 231 Istantaneous surface heat flux
! NGRBIE   - 232 Istantaneous moisture flux (evaporation)
! NGRBLSRH - 234 Logarithm of surface roughness length for heat
! NGRBSKT  - 235 Skin temperature
! NGRBSTL4 - 236 Soil temperature level 4
! NGRBTSN  - 238 Temperature of snow layer
! NGRBCSF  - 239 Convective snow-fall
! NGRBLSF  - 240 Large scale snow-fall
! NGRBTPR    - 260048 Total precipitation rate
! NGRBILSPF  - 228217 Large-scale precipitation fraction
! NGRBCRR    - 228218 Convective rain rate
! NGRBLSRR   - 228219 Large scale rain rate
! NGRBCSFR   - 228220 Convective snowfall rate water equivalent
! NGRBLSSFR  - 228221 Large scale snowfall rate water equivalent
! NGRBMXTPR3 - 228222 Max precip rate in last 3 hours
! NGRBMNTPR3 - 228223 Min precip rate in last 3 hours
! NGRBMXTPR6 - 228224 Max precip rate in last 6 hours
! NGRBMNTPR6 - 228225 Min precip rate in last 6 hours
! NGRBMXTPR  - 228226 Max precip rate since last post-processing
! NGRBMNTPR  - 228227 Min precip rate since last post-processing
! NGRBPTYPE  - 260015 Precipitation type

! NGRBACF  - 241 Not used
! NGRBALW  - 242 Not used
! NGRBFAL  - 243 Forecast albedo
! NGRBFSR  - 244 Forecast surface roughness
! NGRBFLSR - 245 Forecast logarithm of surface roughness for heat
! NGRBCLWC - 246 Cloud liquid water content
! NGRBCIWC - 247 Cloud ice water content
! NGRBCC   - 248 Cloud cover
! NGRB100U - 228246 100m u wind
! NGRB100V - 228247 100m v wind
! NGRB100SI - 228249 100m wind speed
! NGRB200U - 228239 200m u wind
! NGRB200V - 228240 200m v wind
! NGRB200SI - 228241 200m wind speed
! NGRBDNDZN - 228015 Minimum refractivity gradient inside trapping layer
! NGRBDNDZA - 228016 Mean refractivity gradient inside trapping layer
! NGRBDCTB  - 228017 Duct base height
! NGRBTPLB  - 228018 Trapping layer base height
! NGRBTPLT  - 228019 Trapping layer top height
! NGRBSSRDC - 228129 Surface clear-sky downward solar radiation 
! NGRBSTRDC - 228130 Surface clear-sky downward thermal radiation

!-- land carbon dioxide fields in Table 228 ---------------------
! NGRBFASGPPCOEF - 228078 Gross Primary Production flux adjustment factor
! NGRBFASRECCOEF - 228079 Ecosystem respiration flux adjustment factor
! NGRBNEE  - 228080 Net ecosysten exchange for CO2
! NGRBGPP  - 228081 Gross primary production for CO2
! NGRBREC  - 228082 Ecosystem respiration for CO2
! NGRBINEE - 228083 Istantaneous net ecosysten exchange for CO2
! NGRBIGPP - 228084 Istantaneous gross primary production for CO2
! NGRBIREC - 228085 Istantaneous ecosysten respiration for CO2

! NGRBCO2TYP- 129172 CO2 photosynthesis type (C3/C4) 

!-- ocean fields in Table 151 ---------------------
! NGRBOCT   - 128 *In situ* ocean temperature
! NGRBOCS   - 130 salinity
! NGRBOCU   - 131 zonal velocity U
! NGRBOCV   - 132 meridional velocity V
! NGRBOCVVS - 135 viscosity
! NGRBOCVDF - 136 diffusibity
! NGRBOCDEP - 137 bathymetry (bottom layer depth)
! NGRBOCLDP - 176 layer thickness (scalar & vector)
! NGRBOCLZ  - 213 layer depth
! NGRBADVT  - 214 correction term for temperature 
! NGRBADVS  - 215 correction term for salinity

!-- aerosols in Table 210 -------------------------
! NGRBAERMR01 - 001 aerosol mixing ratio 1
! NGRBAERMR02 - 002 aerosol mixing ratio 2
! NGRBAERMR03 - 003 aerosol mixing ratio 3
! NGRBAERMR04 - 004 aerosol mixing ratio 4
! NGRBAERMR05 - 005 aerosol mixing ratio 5
! NGRBAERMR06 - 006 aerosol mixing ratio 6
! NGRBAERMR07 - 007 aerosol mixing ratio 7
! NGRBAERMR08 - 008 aerosol mixing ratio 8
! NGRBAERMR09 - 009 aerosol mixing ratio 9
! NGRBAERMR10 - 010 aerosol mixing ratio 10
! NGRBAERMR11 - 011 aerosol mixing ratio 11
! NGRBAERMR12 - 012 aerosol mixing ratio 12
! NGRBAERMR13 - 013 aerosol mixing ratio 13
! NGRBAERMR14 - 014 aerosol mixing ratio 14
! NGRBAERMR15 - 015 aerosol mixing ratio 15
! NGRBAERGN01 - 016 aerosol gain acc. 1 2D
! NGRBAERGN02 - 017 aerosol gain acc. 2 2D
! NGRBAERGN03 - 018 aerosol gain acc. 3 2D
! NGRBAERGN04 - 019 aerosol gain acc. 4 2D
! NGRBAERGN05 - 020 aerosol gain acc. 5 2D
! NGRBAERGN06 - 021 aerosol gain acc. 6 2D
! NGRBAERGN07 - 022 aerosol gain acc. 7 2D
! NGRBAERGN08 - 023 aerosol gain acc. 8 2D
! NGRBAERGN09 - 024 aerosol gain acc. 9 2D
! NGRBAERGN10 - 025 aerosol gain acc. 10 2D
! NGRBAERGN11 - 026 aerosol gain acc. 11 2D
! NGRBAERGN12 - 027 aerosol gain acc. 12 2D
! NGRBAERGN13 - 028 aerosol gain acc. 13 2D
! NGRBAERGN14 - 029 aerosol gain acc. 14 2D
! NGRBAERGN15 - 030 aerosol gain acc. 15 2D

! NGRBAERLS01 - 031 black carbon biog.      clim2D 
! NGRBAERLS02 - 032 black carbon fossil     clim2D
! NGRBAERLS03 - 033 organic matter biog.    clim2D
! NGRBAERLS04 - 034 organic amtter fossil   clim2D
! NGRBAERLS05 - 035 sulphate low-level      clim2D
! NGRBAERLS06 - 036 sulphate high-level     clim2D
! NGRBAERLS07 - 037 volcanic continuous     clim2D
! NGRBAERLS08 - 038 volcanic explosive      clim2D
! NGRBAERLS09 - 039 secondary organic       clim2D
! NGRBAERLS10 - 040 black carbon GFED           2D
! NGRBAERLS11 - 041 organic matter GFED         2D
! NGRBAERLS12 - 042 sulphate GFED               2D
! NGRBAERLS13 - 043 oceanic DMS             clim2D

! NGRBAERLS14 - 044 aerosol loss acc. 14        2D
! NGRBAERLS15 - 045 aerosol loss acc. 15        2D
! NGRBAERPR   - 046 aerosol precursor mixing ratio 
! NGRBAERSM   - 047 small aerosols mixing ratio  
! NGRBAERLG   - 048 large aerosols mixing ratio ! This is used for the total mixing 
!                                                 ratio if YDML_GCONFIG%YGFL%NAEROCV=1
! NGRBAODPR   - 049 aerosol precursor opt.depth 2D
! NGRBAODSM   - 050 small aerosols opt. depth   2D
! NGRBAODLG   - 051 large aerosols opt. depth   2D
! NGRBAERDEP  - 052 dust emission potential clim2D 
! NGRBAERLTS  - 053 lifting threshold speed clim2D
! NGRBAERSCC  - 054 soli clay content       clim2D

! NGRBAEPM1   - 072 PM1 particulate matter <= 1um   2D
! NGRBAEPM25  - 073 PM2.5 particulate matter <= 2.5um 2D
! NGRBAEPM10  - 074 PM10 particulate matter <= 10um  2D

! NGRBAEODTO  - 207 aerosol total optical depth 2D
! NGRBAEODSS  - 208 optical depth sea salt      2D
! NGRBAEODDU  - 209 optical depth dust          2D
! NGRBAEODOM  - 210 optical depth organic matter2D
! NGRBAEODBC  - 211 optical depth black carbon  2D
! NGRBAEODSU  - 212 optical depth sulphate      2D
! NGRBAEODNI  - 250 optical depth Nitrate      2D
! NGRBAEODAM  - 251 optical depth Ammonium      2D

! NGRBAEODTO469  - 213 aerosol total optical depth 469 nm 2D
! NGRBAEODTO670  - 214 aerosol total optical depth 670 nm 2D
! NGRBAEODTO865  - 215 aerosol total optical depth 865 nm 2D
! NGRBAEODTO1240  - 216 aerosol total optical depth 1240 nm 2D
 
! NGRBAEODVSU - 243 optical depth volc.sulphate 2D
! NGRBAEODVFA - 244 optical depth volc.fly ash  2D

! NGRBAERMR16 - 247 aerosol mixing ratio 16
! NGRBAERMR17 - 248 aerosol mixing ratio 17
! NGRBAERMR18 - 249 aerosol mixing ratio 18
! NGRBAERMR19 - 252 aerosol mixing ratio 19
! NGRBAERMR20 - 253 aerosol mixing ratio 20

!-- aerosols in Table 215 -------------------------
! NGRBAEODSOA  - 215226 optical depth secondary organics     2D

!-- aerosols in Table 216 -------------------------

! NGRBAERSO2DD - 216006 dry deposition velocities from SUMO for SO2 2D
! NGRBAERFCA1  - 216043 calcite fraction of dust bin 1
! NGRBAERFCA2  - 216044 calcite fraction of dust bin 2
! NGRBAERDSF   - 216046 Dust source function       clim2D
! NGRBAERDSZ   - 216048 Dust size modulation at emission clim2D
! NGRBAERURBF  - 216122 Urban fraction

! NGRBUVBED     - 214002 UV Biologically Effective Dose 2D
! NGRBUVBEDCS   - 214003 UV Biologically Effective Dose - Clear Sky 2D

! ERA40 DIAGNOSTIC FIELDS
! NGRBMINERA    - 162100  Minimum identifier for ERA40 diagnostic fields
! NGRBMAXERA    - 162113  Maximum identifier for ERA40 diagnostic fields
! NGRBVIWVE     - 162071  Vertical integral of eastward water vapour flux
! NGRBVIWVN     - 162072  Vertical integral of northward water vapour flux

! Vertically-integrated variables for energy budget diagnostics
! NGRBVIKE      - 162059  Vert.-int. kinetic energy   
! NGRBVITHE     - 162060  Vertically-integrated thermal energy
! NGRBVIPIE     - 162061  Vert.-int. potential+internal energy
! NGRBVIPILE    - 162062  Vert.-int. potential+internal+latent energy
! NGRBVITOE     - 162063  Vert.-int. total energy
! NGRBVIWEN     - 162093  Vert.-int. water enthalpy

! Accumulated vertically-integrated variables for budget diagnostics
! NGRBTIVIEM  - 233015 time-int. vert.-int. eastward mass flux
! NGRBTIVINM  - 233016 time-int. vert.-int. northward mass flux
! NGRBTIVIEWV - 233022 time-int. vert.-int. eastward water vapour flux
! NGRBTIVINWV - 233023 time-int. vert.-int. northward water vapour flux
! NGRBTIVIELW - 233024 time-int. vert.-int. eastward cloud liquid water flux
! NGRBTIVINLW - 233025 time-int. vert.-int. northward cloud liquid water flux
! NGRBTIVIEIW - 233026 time-int. vert.-int. eastward cloud ice water flux
! NGRBTIVINIW - 233027 time-int. vert.-int. northward cloud ice water flux
! NGRBTIVIER  - 233028 time-int. vert.-int. eastward rain flux
! NGRBTIVINR  - 233029 time-int. vert.-int. northward rain flux
! NGRBTIVIES  - 233030 time-int. vert.-int. eastward snow flux
! NGRBTIVINS  - 233031 time-int. vert.-int. northward snow flux
! NGRBTIVIEOZ - 233032 time-int. vert.-int. eastward ozone flux
! NGRBTIVINOZ - 233033 time-int. vert.-int. northward ozone flux
! NGRBTIVISOZ - 233035 time-int. vert.-int. net source of ozone
! NGRBTIVIEEN - 233004 time-int. vert.-int. eastward enthalpy flux
! NGRBTIVINEN - 233005 time-int. vert.-int. northward enthalpy flux
! NGRBTIVIEG  - 233000 time-int. vert.-int. eastward geopotential flux
! NGRBTIVING  - 233001 time-int. vert.-int. northward geopotential flux
! NGRBTIVIEKE - 233006 time-int. vert.-int. eastward kinetic energy flux
! NGRBTIVINKE - 233007 time-int. vert.-int. northward kinetic energy flux
! NGRBTIVIETE - 233008 time-int. vert.-int. eastward total energy flux
! NGRBTIVINTE - 233009 time-int. vert.-int. northward total energy flux

! Accumulated divergence of vertically-integrated variables for diagnostics
! NGRBTIDVIM  - 233014 time-int. divergence of vert.-int. mass flux
! NGRBTIDVIWV - 233017 time-int. divergence of vert.-int. water vapour flux
! NGRBTIDVILW - 233018 time-int. divergence of vert.-int. cloud liquid water flux
! NGRBTIDVIIW - 233019 time-int. divergence of vert.-int. cloud ice water flux
! NGRBTIDVIR  - 233020 time-int. divergence of vert.-int. rain flux
! NGRBTIDVIS  - 233021 time-int. divergence of vert.-int. snow flux
! NGRBTIDVIOZ - 233034 time-int. divergence of vert.-int. ozone flux
! NGRBTIDVIEN - 233010 time-int. divergence of vert.-int. enthalpy flux
! NGRBTIDVIG  - 233003 time-int. divergence of vert.-int. geopotential flux
! NGRBTIDVIKE - 233011 time-int. divergence of vert.-int. kinetic energy flux
! NGRBTIDVITE - 233012 time-int. divergence of vert.-int. total energy flux
! NGRBTIDVIWEN- 233013 time-int. divergence of vert.-int. water enthalpy flux
! NGRBTIDVIWG - 233002 time-int. divergence of vert.-int. water geopotential flux

! NGRBCO2OF     - 210067 CO2 - ocean flux
! NGRBCO2NBF    - 210068 CO2 - biosphere flux
! NGRBCO2APF    - 210069 CO2 - anthropogenic emissions
! NGRBCO2FIRE   - 210080 CO2 - biomass burning
! NGRBCH4F      - 210070 CH4 surface fluxes - aggregated field
! NGRBCH4FIRE   - 210082 CH4 - fire emissions
! NGRBICH4WET   - 228104 Instantaenous CH4 emissions (wetlands)
! NGRBCH4WET    - 228109 Accumulated CH4 emissions (wetlands)
!
! Lightning fields
! NGRBLITOTI  - 228050 Instantaneous total lightning flash density
! NGRBLITOTA1 - 228051 1h averaged total lightning flash density
! NGRBLITOTA3 - 228057 3h averaged total lightning flash density
! NGRBLITOTA6 - 228058 6h averaged total lightning flash density
! NGRBLICGI   - 228052 Instantaneous cloud-to-ground lightning flash density
! NGRBLICGA1  - 228053 1h averaged cloud-to-ground lightning flash density
! NGRBLICGA3  - 228059 3h averaged cloud-to-ground lightning flash density
! NGRBLICGA6  - 228060 6h averaged cloud-to-ground lightning flash density

! Precipitation type most frequent (mode) and most severe
! NGRBPTYPESEVR1 - 260318 Precipitation type (most severe) in the last 1 hour
! NGRBPTYPESEVR3 - 260319 Precipitation type (most severe) in the last 3 hours
! NGRBPTYPESEVR6 - 260338 Precipitation type (most severe) in the last 6 hours
! NGRBPTYPEMODE1 - 260320 Precipitation type (most frequent) in the last 1 hour
! NGRBPTYPEMODE3 - 260321 Precipitation type (most frequent) in the last 3 hours
! NGRBPTYPEMODE6 - 260339 Precipitation type (most frequent) in the last 6 hours

! Ocean fields
! NGRBICETK  - 174098 Ice thickness
! NGRBMLD    - 151148 Mixed layer depth
! NGRBSL     - 151145 Sea lavel
! NGRB20D    - 151163 20 degree isotherm depth
! NGRBSSS0   - 151130 Sea surface salinity
! NGRBTEM300 - 151164 Mean temparature over 300m
! NGRBSAL300 - 151175 Mean salinity content over 300m

!---------------------------------------------------
! NGRBGHG(JPGHG)   - 210061 GHG1: Carbon dioxide
!                  - 210062 GHG2: Methane
!                  - 210063 GHG3: Nitrous oxide
! NGRBTCGHG(JPGHG) - 210064 Total column GHG1: Carbon Dioxide
!                  - 210065 Total column GHG2: Methane
!                  - 210066 Total column GHG3: Nitrous Oxide

!---------------------------------------------------

INTEGER(KIND=JPIB_K), PARAMETER :: NGRBSTRF  =  1
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBVP    =  2
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBPT    =  3
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBSRO   =  8
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBSSRO  =  9
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBALUVP = 15
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBALUVD = 16
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBALNIP = 17
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBALNID = 18

INTEGER(KIND=JPIB_K), PARAMETER :: NGRBALUVI = 210186
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBALNII = 210189
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBALUVV = 210187
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBALNIV = 210190
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBALUVG = 210188
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBALNIG = 210191

INTEGER(KIND=JPIB_K), PARAMETER :: NGRBPARCS = 20
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBUCTP  = 21
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBUCLN  = 22
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBUCDV  = 23
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBCL    = 26
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBCVL   = 27
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBCO2TYP= 129172
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBCVH   = 28
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBFWET  = 200026
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBCUR   = 200199
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTVL   = 29
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTVH   = 30
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBCI    = 31
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBASN   = 32
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBRSN   = 33
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBSSTK  = 34
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBISTL1 = 35
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBISTL2 = 36
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBISTL3 = 37
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBISTL4 = 38
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBSWVL1 = 39
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBSWVL2 = 40
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBSWVL3 = 41
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBSWVL4 = 42
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBSLT   = 43
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBES    = 44
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBSMLT  = 45
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB10FG  = 49
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBLSPF  = 50

INTEGER(KIND=JPIB_K), PARAMETER :: NGRBMONT  = 53
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBPRES  = 54

INTEGER(KIND=JPIB_K), PARAMETER :: NGRBUVB   = 57
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBPAR   = 58
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBCAPE  = 59
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBMUCAPE= 228235
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBMLCAPE50 = 228231
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBMLCAPE100= 228233
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBMUDEPL = 228237
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBCAPES = 228044
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBMXCAP6= 228035
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBMXCAPS6=228036
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBPV    = 60
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBLAIL  = 66
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBLAIH  = 67

INTEGER(KIND=JPIB_K), PARAMETER :: NGRBSDFOR = 74

INTEGER(KIND=JPIB_K), PARAMETER :: NGRBCRWC  = 75
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBCSWC  = 76

INTEGER(KIND=JPIB_K), PARAMETER :: NGRBETADOT= 77
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTCLW  = 78
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTCIW  = 79
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTCSLW = 228088
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTCRW  = 228089
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTCSW  = 228090

INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTROPOTP= 228045

! LARPEGE
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBSPD   = 80
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBSVD   = 81
! LECMWF
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB080   = 80
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB081   = 81
! LECV
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBALPHA= 90
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBSKTECV=91
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBSSHECV=92
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTSLECV=93
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBSDFORE=100

INTEGER(KIND=JPIB_K), PARAMETER :: NGRBMINXTRA  = 082
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBMAXXTRA  = 117

INTEGER(KIND=JPIB_K), PARAMETER :: NGRB082  = 082
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB083  = 083
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB084  = 084
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB085  = 085
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB086  = 086
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB087  = 087
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB088  = 088
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB089  = 089
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB090  = 090
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB091  = 091
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB092  = 092
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB093  = 093
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB094  = 094
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB095  = 095
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB096  = 096
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB097  = 097
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB098  = 098
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB099  = 099
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB100  = 100
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB101  = 101
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB102  = 102
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB103  = 103
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB104  = 104
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB105  = 105
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB106  = 106
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB107  = 107
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB108  = 108
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB109  = 109
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB110  = 110
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB111  = 111
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB112  = 112
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB113  = 113
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB114  = 114
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB115  = 115
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB116  = 116
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB117  = 117

INTEGER(KIND=JPIB_K), PARAMETER :: NGRB118  = 118
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB119  = 119
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB120  = 120

INTEGER(KIND=JPIB_K), PARAMETER :: NGRBMX2T3 = 228026
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBMN2T3 = 228027
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB10FG3 = 228028
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBI10FG = 228029
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBMX2T6 = 121
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBMN2T6 = 122
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB10FG6 = 123
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBEMIS  = 124

INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAT   = 127
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBBV   = 128
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBZ    = 129
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBT    = 130
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBU    = 131
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBV    = 132
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBUCUR = 262140
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBVCUR = 262139
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBSSS  = 151130
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBQ    = 133
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBSP   = 134
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBW    = 135
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTCW  = 136
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTCWV = 137
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBVO   = 138
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBSTL1 = 139

INTEGER(KIND=JPIB_K), PARAMETER :: NGRBSDSL = 141  ! back-comp single-layer
!!INTEGER(KIND=JPIB_K), PARAMETER :: NGRBSD   = 3066 ! grib2 multi-layer
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBSD   = 228141 ! grib2 multi-layer
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBWSN  = 228038

INTEGER(KIND=JPIB_K), PARAMETER :: NGRBLSP  = 142
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBCP   = 143
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBSF   = 144
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBFZRA = 228216
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBBLD  = 145
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBSSHF = 146
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBSLHF = 147
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBCHNK = 148
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBSNR  = 149
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTNR  = 150
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBMSL  = 151
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBLNSP = 152
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBSWHR = 153
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBLWHR = 154
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBD    = 155
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBGH   = 156
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBR    = 157
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTSP  = 158
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBBLH  = 159
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBSDOR = 160
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBISOR = 161
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBANOR = 162
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBSLOR = 163
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTCC  = 164
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB10U  = 165
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB10V  = 166
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBZUST = 228003
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBFDIR = 228021
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBCDIR = 228022
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBDSRP = 47
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBCBASE= 228023
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB0DEGL= 228024
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBM10DEGL= 228020
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBMUCIN  = 228236
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBMLCIN50 = 228232
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBMLCIN100= 228234
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBKINDEX = 260121
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTTINDEX= 260123
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBCBASEA= 260109
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBCTOPC = 228046
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBZTWETB0 = 228047
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBZTWETB1 = 228048
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBVISIH= 3020
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB10NU = 228131
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB10NV = 228132
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB100U = 228246
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB100V = 228247
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB100SI= 228249
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB200U = 228239
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB200V = 228240
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB200SI= 228241
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB2T   = 167
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB2D   = 168
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB2R   = 260242
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB2SH  = 174096
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBSSRD = 169
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBSTL2 = 170
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBLSM  = 172
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBSR   = 173
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAL   = 174
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBSTRD = 175
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBSSR  = 176
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBSTR  = 177
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTSR  = 178
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTTR  = 179
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBEWSS = 180
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBNSSS = 181
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBE    = 182
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBPEV  = 228251
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBSTL3 = 183
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBCCC  = 185
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBLCC  = 186
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBMCC  = 187
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBHCC  = 188
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBSUND = 189
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBEWOV = 190
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBNSOV = 191
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBNWOV = 192
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBNEOV = 193
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBBTMP = 194
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBCLBT = 260510
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBCSBT = 260511
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBCDRFL = 260512
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBCRRFL = 260513
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBLGWS = 195
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBMGWS = 196
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBGWD  = 197
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBSRC  = 198
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBVEG  = 199
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBVSO  = 200
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBMX2T = 201
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBMN2T = 202
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBO3   = 203
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBPAW  = 204
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBRO   = 205
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTCO3 = 206
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB10SI = 207
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTSRC = 208
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTTRC = 209
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBSSRC = 210
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBSTRC = 211

INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTISR = 212
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBVIMD = 213

INTEGER(KIND=JPIB_K), PARAMETER :: NGRBDHR  = 214
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBDHVD = 215
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBDHCC = 216
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBDHLC = 217
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBVDZW = 218
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBVDMW = 219
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBCTZW = 222
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBCTMW = 223
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBVDH  = 224
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBHTCC = 225
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBHTLC = 226
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBCRNH = 227
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTP   = 228
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBIEWS = 229
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBINSS = 230
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBISHF = 231
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBIE   = 232
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBLSRH = 234
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBSKT  = 235
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBSTL4 = 236
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTSN  = 238
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBCSF  = 239
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBLSF  = 240
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTPR    = 260048
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBILSPF  = 228217
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBCRR    = 228218
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBLSRR   = 228219
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBCSFR   = 228220
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBLSSFR  = 228221
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBMXTPR3 = 228222
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBMNTPR3 = 228223
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBMXTPR6 = 228224
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBMNTPR6 = 228225
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBMXTPR  = 228226
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBMNTPR  = 228227
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBPTYPE  = 260015

INTEGER(KIND=JPIB_K), PARAMETER :: NGRBACF  = 241
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBALW  = 242
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBFAL  = 243
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBFSR  = 244
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBFLSR = 245
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBCLWC = 246
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBCIWC = 247
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBCC   = 248
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAIW  = 249
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBICE  = 250
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBATTE = 251
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBATHE = 252
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBATZE = 253
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBATMW = 254
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB255  = 255
!-- lake depth ancillary + lake prognostics -- Table 228 ---
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBDL   = 228007
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBLMLT = 228008
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBLMLD = 228009
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBLBLT = 228010
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBLTLT = 228011
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBLSHF = 228012
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBLICT = 228013
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBLICD = 228014
!-- land carbon dioxide diagnostics -- Table 228 ---
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBNEE  = 228080
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBGPP  = 228081
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBREC  = 228082
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBINEE = 228083
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBIGPP = 228084
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBIREC = 228085
!-- ducting diagnostics -- Table 228 ---------------
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBDNDZN = 228015
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBDNDZA = 228016
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBDCTB  = 228017
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTPLB  = 228018
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTPLT  = 228019
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBASCAT_SM_CDFA  = 228253
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBASCAT_SM_CDFB  = 228254
!-- clearsky downward surface fluxes -- Table 228 ---------------
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBSSRDC= 228129
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBSTRDC= 228130
!-- ocean mixed layer -- Table 151 ----------------
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBOCT   = 151129
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBOCS   = 151130
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBOCU   = 262140
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBOCV   = 262139
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBOCVVS = 105
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBOCVDF = 106
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBOCDEP = 107
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBOCLDP = 108
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBOCLZ  = 109
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBADVT  = 110
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBADVS  = 111
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBOCUC  = 112
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBOCVC  = 113
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBUSTRC = 114
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBVSTRC = 115

!-- aerosols -- Table 210 --------------------------
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERMR01=210001
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERMR02=210002
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERMR03=210003
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERMR04=210004
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERMR05=210005
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERMR06=210006
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERMR07=210007
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERMR08=210008
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERMR09=210009
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERMR10=210010
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERMR11=210011
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERMR12=210012
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERMR13=210013
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERMR14=210014
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERMR15=210015

INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERGN01=210016
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERGN02=210017
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERGN03=210018
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERGN04=210019
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERGN05=210020
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERGN06=210021
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERGN07=210022
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERGN08=210023
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERGN09=210024
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERGN10=210025
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERGN11=210026
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERGN12=210027
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERGN13=210028
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERGN14=210029
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERGN15=210030

INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERLS01  =210031
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERLS02  =210032
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERLS03  =210033
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERLS04  =210034
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERLS05  =210035
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERLS06  =210036
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERLS07  =210037
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERLS08  =210038
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERLS09  =210039
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERLS10  =210040
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERLS11  =210041
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERLS12  =210042
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERLS13  =210043

INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERLS14=210044
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERLS15=210045

INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERPR  =210046
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERSM  =210047
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERLG  =210048
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAODPR  =210049
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAODSM  =210050
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAODLG  =210051
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERDEP =210052
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERLTS =210053
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERSCC =210054

INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAEPM1  =210072
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAEPM25 =210073
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAEPM10 =210074

INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAEODTO =210207
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAEODSS =210208
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAEODDU =210209
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAEODOM =210210
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAEODBC =210211
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAEODSU =210212

INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAEODTO469=210213
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAEODTO670=210214
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAEODTO865=210215
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAEODTO1240=210216

INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAEODVSU=210243
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAEODVFA=210244

INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTAEDEC550=210245
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTAEDAB550=210246

INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERMR16=210247
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERMR17=210248
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERMR18=210249

INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAEODNI =210250
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAEODAM =210251

INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERMR19=210252
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERMR20=210253

!--Injection height for biomass burning emissions---
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBINJFIRE = 210119

!-- aerosols -- Table 216 --------------------------
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERSO2DD =216006
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERFCA1  =216043
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERFCA2  =216044
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERDSF =216046
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERDSZ =216048
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERURBF  =216122

!---------------------------------------------------

INTEGER(KIND=JPIB_K), PARAMETER :: NGRBUVBED   = 214002
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBUVBEDCS = 214003

INTEGER(KIND=JPIB_K), PARAMETER :: NGRBMINERA  = 162100
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBMAXERA  = 162113
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBVIWVE   = 162071
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBVIWVN   = 162072

INTEGER(KIND=JPIB_K), PARAMETER :: NGRBVIKE    = 162059
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBVITHE   = 162060
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBVIPIE   = 162061
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBVIPILE  = 162062
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBVITOE   = 162063
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBVIWEN   = 162093

INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTIVIEM  = 233015
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTIVINM  = 233016
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTIVIEWV = 233022
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTIVINWV = 233023
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTIVIELW = 233024
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTIVINLW = 233025
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTIVIEIW = 233026
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTIVINIW = 233027
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTIVIER  = 233028
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTIVINR  = 233029
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTIVIES  = 233030
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTIVINS  = 233031
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTIVIEOZ = 233032
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTIVINOZ = 233033
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTIVISOZ = 233035
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTIVIEEN = 233004
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTIVINEN = 233005
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTIVIEG  = 233000
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTIVING  = 233001
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTIVIEKE = 233006
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTIVINKE = 233007
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTIVIETE = 233008
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTIVINTE = 233009

INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTIDVIM  = 233014
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTIDVIWV = 233017
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTIDVILW = 233018
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTIDVIIW = 233019
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTIDVIR  = 233020
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTIDVIS  = 233021
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTIDVIOZ = 233034
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTIDVIEN = 233010
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTIDVIG  = 233003
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTIDVIKE = 233011
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTIDVITE = 233012
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTIDVIWEN= 233013
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTIDVIWG = 233002

!---------------------------------------------------
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBCO2OF   = 210067
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBCO2NBF  = 210068
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBCO2APF  = 210069
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBCO2FIRE = 210080
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBCH4F    = 210070
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBCH4FIRE = 210082
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBICH4WET = 228104
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBCH4WET  = 228109
!---------------------------------------------------
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBFASGPPCOEF = 228078
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBFASRECCOEF = 228079
!---------------------------------------------------

INTEGER(KIND=JPIB_K), PARAMETER :: NGRBNOXLOG = 210121    ! use NO2 for C-IFS
!INTEGER(KIND=JPIB_K), PARAMETER :: NGRBNOXLOG = 210129    ! use NOX for coupled system

INTEGER(KIND=JPIB_K), PARAMETER :: NGRBLITOTI  = 228050
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBLITOTA1 = 228051
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBLITOTA3 = 228057
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBLITOTA6 = 228058
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBLICGI   = 228052
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBLICGA1  = 228053
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBLICGA3  = 228059
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBLICGA6  = 228060

INTEGER(KIND=JPIB_K), PARAMETER :: NGRBPTYPESEVR1 = 260318
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBPTYPESEVR3 = 260319
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBPTYPESEVR6 = 260338
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBPTYPEMODE1 = 260320
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBPTYPEMODE3 = 260321
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBPTYPEMODE6 = 260339

!--Further aerosol diagnostics -- Table 215 --------
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBACCAOD550 = 215089

INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAOT532  = 215093
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBNAOT532 = 215094
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAAOT532 = 215095

INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAEREXT355  = 215180
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAEREXT532  = 215181
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAEREXT1064 = 215182

INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERBACKSCATTOA355  = 215183
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERBACKSCATTOA532  = 215184
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERBACKSCATTOA1064 = 215185

INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERBACKSCATGND355  = 215186
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERBACKSCATGND532  = 215187
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAERBACKSCATGND1064 = 215188

INTEGER(KIND=JPIB_K), PARAMETER :: NGRBAEODSOA =215226

!-- Ocean model output on IFS grid ---

INTEGER(KIND=JPIB_K), PARAMETER :: NGRBICETK = 174098
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBMLD   = 151148
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBSL    = 151145
INTEGER(KIND=JPIB_K), PARAMETER :: NGRB20D   = 151163
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBSSSO  = 151130
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBTEM300= 151164
INTEGER(KIND=JPIB_K), PARAMETER :: NGRBSAL300= 151175

INTEGER(KIND=JPIB_K), PARAMETER, DIMENSION(3) :: NGRBGHG = (/&
 & 210061, 210062, 210063/)
INTEGER(KIND=JPIB_K),  PARAMETER,DIMENSION(3) :: NGRBTCGHG = (/&
 & 210064, 210065, 210066/)
END MODULE GRIB_CODES_MOD
