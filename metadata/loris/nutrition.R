foods_mg <- list(
  baseline = list(
    "biscuit"          = 13200,
    "invertebrates"    = 10000,
    "gum_arabic"       = 12000,
    "protein_rotate"   = 2100,
    "seasonal_veggies" = 15000
  ),
  biscuit_elim = list(
      "biscuit"          = 0,
      "invertebrates"    = 18000,
      "gum_arabic"       = 17000,
      "protein_rotate"   = 5000,
      "seasonal_veggies" = 30000
    ),
  lessBug_moreEgg = list(
      "biscuit"            = 13200,
      "invertebrates"      = 8000,
      "gum_arabic"         = 10000,
      "egg_whole_cooked"   = 5000,
      "sweet_potato_raw"   = 5000,
      "green_bean_fresh"   = 10000,
      "carrot"             = 5000,
      "canned_pumpkin"     = 5000
    ),
  seasonals = list(
      "biscuit"            = 13200,
      "invertebrates"      = 8000,
      "gum_arabic"         = 10000,
      "egg_whole_cooked"   = 5000,
      "sweet_potato_raw"   = 5000,
      "green_bean_fresh"   = 10000,
      "carrot"             = 5000,
      "canned_pumpkin"     = 5000
    ),
  oatgel = list(
      "biscuit"            = 13200,
      "invertebrates"      = 8000,
      "gum_arabic"         = 12000,
      "HDZ_oatgel"         = 5000,
      "protein_rotate"     = 2100,
      "seasonal_veggies"   = 20000
    ),
  low_lectin = list(
      "biscuit"            = 0,
      "invertebrates"      = 18000,
      "gum_arabic"         = 17000,
      "egg_white_cooked"   = 5000,
      "sweet_potato_cooked"= 5000,
      "carrot"             = 10000,
      "celery"             = 5000,
      "lettuce_romaine"    = 5000
    ),
  gum36_veg37_invert27 = list(
      "biscuit"            = 0,
      "invertebrates"      = 18000,
      "gum_arabic"         = 24000,
      "seasonal_veggies"   = 25000
    ),
  water31_root31_protein19_gum19 = list(
      "egg_whole_raw"           = 9000,
      "invertebrates"           = 9000,
      "gum_arabic"              = 18000,
      "seasonal_veggies_root"   = 30000,
      "seasonal_veggies_watery" = 30000
    )
)

nutrient_totals <- list(
  baseline                       = list(mg_fed = 52000      , mg_dry = 29000      , kcal = 90),
  biscuit_elim                   = list(mg_fed = 70000      , mg_dry = 28243      , kcal = 90),
  lessBug_moreEgg                = list(mg_fed = 53200      , mg_dry = 27982.29167, kcal = 92),
  seasonals                      = list(mg_fed = 61200      , mg_dry = 28454.5    , kcal = 90),
  oatgel                         = list(mg_fed = 60342.85714, mg_dry = 29904.58333, kcal = 90),
  low_lectin                     = list(mg_fed = 65000      , mg_dry = 29799.75   , kcal = 93),
  gum36_veg37_invert27           = list(mg_fed = 67000      , mg_dry = 32127.41667, kcal = 87),
  water31_root31_protein19_gum19 = list(mg_fed = 96000      , mg_dry = 30136.91071, kcal = 88)
)
  

proteins <- list(
  totals = list(
    baseline                       = list(mg_fed = 5100      , proportion = 0.172) ,
    biscuit_elim                   = list(mg_fed = 4444      , proportion = 0.2    ),
    lessBug_moreEgg                = list(mg_fed = 5692.84375, proportion = 0.20344),
    seasonals                      = list(mg_fed = 5132.76667, proportion = 0.18039),
    oatgel                         = list(mg_fed = 4826.53571, proportion = 0.1614 ),
    low_lectin                     = list(mg_fed = 7375.20417, proportion = 0.24749),
    gum36_veg37_invert27           = list(mg_fed = 3747.0375 , proportion = 0.11663),
    water31_root31_protein19_gum19 = list(mg_fed = 3765.82986, proportion = 0.12496)
    ),
  taurine = list(
    baseline                       = list(mg_fed = 20    , proportion = 0.0006 ),
    biscuit_elim                   = list(mg_fed = 0     , proportion = 0      ),
    lessBug_moreEgg                = list(mg_fed = 17.952, proportion = 0.00064),
    seasonals                      = list(mg_fed = 17.952, proportion = 0.00063),
    oatgel                         = list(mg_fed = 17.952, proportion = 0.0006 ),
    low_lectin                     = list(mg_fed = 0     , proportion = 0      ),
    gum36_veg37_invert27           = list(mg_fed = 0     , proportion = 0      ),
    water31_root31_protein19_gum19 = list(mg_fed = 0     , proportion = 0      )
  ),
  methionine = list(
    baseline                       = list(mg_fed = 90       , proportion = 0.0031     ),
    biscuit_elim                   = list(mg_fed = 63.79    , proportion = 0.002258657),
    lessBug_moreEgg                = list(mg_fed = 130.34071, proportion = 0.00466    ),
    seasonals                      = list(mg_fed = 101.63   , proportion = 0.00357    ),
    oatgel                         = list(mg_fed = 86.58715 , proportion = 0.0029     ),
    low_lectin                     = list(mg_fed = 198.55   , proportion = 0.00666    ),
    gum36_veg37_invert27           = list(mg_fed = 50.01429 , proportion = 0.00156    ),
    water31_root31_protein19_gum19 = list(mg_fed = 70.49786 , proportion = 0.00234    )
  )
)


fats <- list(
  totals = list(
    baseline                       = list(mg_fed = 2000      , proportion = 0.054      ),
    biscuit_elim                   = list(mg_fed = 1861      , proportion = 0.065905076),
    lessBug_moreEgg                = list(mg_fed = 2655.69415, proportion = 0.09491    ),
    seasonals                      = list(mg_fed = 1806.97933, proportion = 0.0635     ),
    oatgel                         = list(mg_fed = 1618.96429, proportion = 0.05414    ),
    low_lectin                     = list(mg_fed = 1018.84933, proportion = 0.03419    ),
    gum36_veg37_invert27           = list(mg_fed = 1016.60137, proportion = 0.03164    ),
    water31_root31_protein19_gum19 = list(mg_fed = 1655.85244, proportion = 0.05494    )
  ),
  omega3 = list(
    baseline                       = list(mg_fed = 635   , mg_g = 2.2    ),
    biscuit_elim                   = list(mg_fed = 1513  , mg_g = 0.0054 ),
    lessBug_moreEgg                = list(mg_fed = 634.49, mg_g = 2.2675 ),
    seasonals                      = list(mg_fed = 634.49, mg_g = 2.22984),
    oatgel                         = list(mg_fed = 634.49, mg_g = 2.12231),
    low_lectin                     = list(mg_fed = 634.49, mg_g = 0.00508),
    gum36_veg37_invert27           = list(mg_fed = 634.49, mg_g = 0.00665),
    water31_root31_protein19_gum19 = list(mg_fed = 634.49, mg_g = 0.00532)
  ),
  omega6 = list(
    baseline                       = list(mg_fed = 3370  , mg_g = 11.5    ),
    biscuit_elim                   = list(mg_fed = 5440  , mg_g = 0.0193  ),
    lessBug_moreEgg                = list(mg_fed = 3369.2, mg_g = 12.0405 ),
    seasonals                      = list(mg_fed = 3369.2, mg_g = 11.84066),
    oatgel                         = list(mg_fed = 3369.2, mg_g = 11.26864),
    low_lectin                     = list(mg_fed = 3369.2, mg_g = 0.01826 ),
    gum36_veg37_invert27           = list(mg_fed = 3369.2, mg_g = 0.02390 ),
    water31_root31_protein19_gum19 = list(mg_fed = 3369.2, mg_g = 0.01911 )
  )
)


CHOs <- list(
  totals = list(
    baseline                       = list(mg_fed = 2000      , proportion = 0.054),
    biscuit_elim                   = list(mg_fed = 3306      , proportion = 0.117051162),
    lessBug_moreEgg                = list(mg_fed = 991.69565 , proportion = 0.03544),
    seasonals                      = list(mg_fed = 2642.5    , proportion = 0.09287),
    oatgel                         = list(mg_fed = 2005.1413 , proportion = 0.06705),
    low_lectin                     = list(mg_fed = 2485.5    , proportion = 0.08341),
    gum36_veg37_invert27           = list(mg_fed = 2059.23913, proportion = 0.06410),
    water31_root31_protein19_gum19 = list(mg_fed = 5548.42500, proportion = 0.18411)
  ),
  WSC = list(
    baseline                       = list(mg_fed = 0, proportion = 0),
    biscuit_elim                   = list(mg_fed = 0, proportion = 0),
    lessBug_moreEgg                = list(mg_fed = 0, proportion = 0),
    seasonals                      = list(mg_fed = 0, proportion = 0),
    oatgel                         = list(mg_fed = 0, proportion = 0),
    low_lectin                     = list(mg_fed = 0, proportion = 0),
    gum36_veg37_invert27           = list(mg_fed = 0, proportion = 0),
    water31_root31_protein19_gum19 = list(mg_fed = 0, proportion = 0)
  ),
  starch = list(
    baseline                       = list(mg_fed = 3000      , proportion = 0.093),
    biscuit_elim                   = list(mg_fed = 1175.7    , proportion = 0.041628505),
    lessBug_moreEgg                = list(mg_fed = 2488.56667, proportion = 0.08893),
    seasonals                      = list(mg_fed = 2931.66667, proportion = 0.10303),
    oatgel                         = list(mg_fed = 3102.91905, proportion = 0.10376),
    low_lectin                     = list(mg_fed = 863.16667 , proportion = 0.02897),
    gum36_veg37_invert27           = list(mg_fed = 831.41667 , proportion = 0.02588),
    water31_root31_protein19_gum19 = list(mg_fed = 2619.25263, proportion = 0.08691)
  ),
  NDF = list(
    baseline                       = list(mg_fed = 9000       , proportion = 0.321),
    biscuit_elim                   = list(mg_fed = 9243.2     , proportion = 0.32727783),
    lessBug_moreEgg                = list(mg_fed = 8137.84222 , proportion = 0.29082),
    seasonals                      = list(mg_fed = 8539.042   , proportion = 0.30009),
    oatgel                         = list(mg_fed = 9629.23644 , proportion = 0.322),
    low_lectin                     = list(mg_fed = 9141.782   , proportion = 0.30677),
    gum36_veg37_invert27           = list(mg_fed = 12311.33756, proportion = 0.3832),
    water31_root31_protein19_gum19 = list(mg_fed = 9657.06145 , proportion = 0.32044)
  ),
  ADF = list(
    baseline                       = list(mg_fed = 2000      , proportion = 0.081),
    biscuit_elim                   = list(mg_fed = 763.86    , proportion = 0.027046144),
    lessBug_moreEgg                = list(mg_fed = 2196.54127, proportion = 0.0785),
    seasonals                      = list(mg_fed = 2387.31714, proportion = 0.0839),
    oatgel                         = list(mg_fed = 2435.20325, proportion = 0.08143),
    low_lectin                     = list(mg_fed = 681.28857 , proportion = 0.02286),
    gum36_veg37_invert27           = list(mg_fed = 711.17746 , proportion = 0.02214),
    water31_root31_protein19_gum19 = list(mg_fed = 798.50338 , proportion = 0.0265)
  ),
  crude_fiber = list(
    baseline                       = list(mg_fed = 12000, proportion = 0.422),
    biscuit_elim                   = list(mg_fed = 15300, proportion = 0.541733548),
    lessBug_moreEgg                = list(mg_fed = 10595, proportion = 0.37863),
    seasonals                      = list(mg_fed = 10595, proportion = 0.37235),
    oatgel                         = list(mg_fed = 12395, proportion = 0.41448),
    low_lectin                     = list(mg_fed = 15300, proportion = 0.51343),
    gum36_veg37_invert27           = list(mg_fed = 21600, proportion = 0.51343),
    water31_root31_protein19_gum19 = list(mg_fed = 16200, proportion = 0.53755)
  ),
  TDF = list(
    baseline                       = list(mg_fed = 1000     , proportion = 0.018),
    biscuit_elim                   = list(mg_fed = 1073.62  , proportion = 0.038014127),
    lessBug_moreEgg                = list(mg_fed = 270.69565, proportion = 0.00967),
    seasonals                      = list(mg_fed = 820.2    , proportion = 0.02882),
    oatgel                         = list(mg_fed = 606.16274, proportion = 0.02027),
    low_lectin                     = list(mg_fed = 874.2    , proportion = 0.02934),
    gum36_veg37_invert27           = list(mg_fed = 755.93913, proportion = 0.02353),
    water31_root31_protein19_gum19 = list(mg_fed = 1408.35  , proportion = 0.04673)
  )
)  

Ash <- list(
  totals = list(
    baseline                       = list(mg_fed = 2000      , proportion = 0.062      , ca_p_ratio  = 1.93),
    biscuit_elim                   = list(mg_fed = 1431      , proportion = 0.050675903, ca_p_ratio  = 1.93),
    lessBug_moreEgg                = list(mg_fed = 1721.86155, proportion = 0.06153    , ca_p_ratio  = 1.95096),
    seasonals                      = list(mg_fed = 1764.36233, proportion = 0.06201    , ca_p_ratio  = 1.86483),
    oatgel                         = list(mg_fed = 1816.25337, proportion = 0.06073    , ca_p_ratio  = 2.03403),
    low_lectin                     = list(mg_fed = 1531.76108, proportion = 0.05140    , ca_p_ratio  = 1.74059),
    gum36_veg37_invert27           = list(mg_fed = 1647.6457 , proportion = 0.05128    , ca_p_ratio  = 2.30977),
    water31_root31_protein19_gum19 = list(mg_fed = 1560.31388, proportion = 0.05177    , ca_p_ratio  = 2.37506)
  ),
  calcium = list(
    baseline                       = list(mg_fed = 321.7701592, proportion = 0.01096414),
    biscuit_elim                   = list(mg_fed = 257.32     , proportion = 0.009110887),
    lessBug_moreEgg                = list(mg_fed = 290.22518  , proportion = 0.01037),
    seasonals                      = list(mg_fed = 297.2016   , proportion = 0.01044),
    oatgel                         = list(mg_fed = 319.66248  , proportion = 0.01069),
    low_lectin                     = list(mg_fed = 254.7411   , proportion = 0.00855),
    gum36_veg37_invert27           = list(mg_fed = 332.46719  , proportion = 0.01035),
    water31_root31_protein19_gum19 = list(mg_fed = 254.52055  , proportion = 0.00845)
  ),
  phosphorus = list(
    baseline                       = list(mg_fed = 167.1484216, proportion = 0.00569549),
    biscuit_elim                   = list(mg_fed = 157.91     , proportion = 0.005591296),
    lessBug_moreEgg                = list(mg_fed = 148.76029  , proportion = 0.00532),
    seasonals                      = list(mg_fed = 159.3716   , proportion = 0.0056),
    oatgel                         = list(mg_fed = 157.15731  , proportion = 0.00526),
    low_lectin                     = list(mg_fed = 146.3536   , proportion = 0.00491),
    gum36_veg37_invert27           = list(mg_fed = 143.93931  , proportion = 0.00448),
    water31_root31_protein19_gum19 = list(mg_fed = 107.16394  , proportion = 0.00356)
  ),
  potassium = list(
    baseline                       = list(mg_fed = 322.688967, proportion = 0.00569549),
    biscuit_elim                   = list(mg_fed = 334.71    , proportion = 0.011851272),
    lessBug_moreEgg                = list(mg_fed = 264.31908 , proportion = 0.00945),
    seasonals                      = list(mg_fed = 320.23034 , proportion = 0.01125),
    oatgel                         = list(mg_fed = 322.43378 , proportion = 0.01078),
    low_lectin                     = list(mg_fed = 366.38577 , proportion = 0.01229),
    gum36_veg37_invert27           = list(mg_fed = 329.17668 , proportion = 0.01025),
    water31_root31_protein19_gum19 = list(mg_fed = 330.54289 , proportion = 0.01097)
  ),
  magnesium = list(
    baseline                       = list(mg_fed = 157.3827213, proportion = 0.005362729),
    biscuit_elim                   = list(mg_fed = 257.78     , proportion = 0.009127275),
    lessBug_moreEgg                = list(mg_fed = 113.78598  , proportion = 0.00407),
    seasonals                      = list(mg_fed = 51.3928    , proportion = 0.00181),
    oatgel                         = list(mg_fed = 190.93112  , proportion = 0.00638),
    low_lectin                     = list(mg_fed = 50.2238    , proportion = 0.00169),
    gum36_veg37_invert27           = list(mg_fed = 228.28999  , proportion = 0.00711),
    water31_root31_protein19_gum19 = list(mg_fed = 356.98761  , proportion = 0.01185)
  ),
  copper = list(
    baseline                       = list(mg_fed = 0.5    , mg_kg = 16),
    biscuit_elim                   = list(mg_fed = 0.1    , mg_kg = 5.0155),
    lessBug_moreEgg                = list(mg_fed = 0.42528, mg_kg = 15.1983),
    seasonals                      = list(mg_fed = 0.45428, mg_kg = 15.96503),
    oatgel                         = list(mg_fed = 0.45601, mg_kg = 15.24881),
    low_lectin                     = list(mg_fed = 0.1363 , mg_kg = 4.57389),
    gum36_veg37_invert27           = list(mg_fed = 0.13316, mg_kg = 4.14484),
    water31_root31_protein19_gum19 = list(mg_fed = 0.11077, mg_kg = 3.67543)
  ),
  iron = list(
    baseline                       = list(mg_fed = 5.5    , mg_kg = 188),
    biscuit_elim                   = list(mg_fed = 4.3    , mg_kg = 153.4236),
    lessBug_moreEgg                = list(mg_fed = 4.50299, mg_kg = 160.9229),
    seasonals                      = list(mg_fed = 4.92512, mg_kg = 173.08756),
    oatgel                         = list(mg_fed = 5.31891, mg_kg = 177.86267),
    low_lectin                     = list(mg_fed = 3.39732, mg_kg = 114.00498),
    gum36_veg37_invert27           = list(mg_fed = 3.6698 , mg_kg = 114.22634),
    water31_root31_protein19_gum19 = list(mg_fed = 2.53316, mg_kg = 84.05507)
  ),
  zinc = list(
    baseline                       = list(mg_fed = 2.5    , mg_kg = 86),
    biscuit_elim                   = list(mg_fed = 1.2    , mg_kg = 41.8857),
    lessBug_moreEgg                = list(mg_fed = 2.35165, mg_kg = 84.0407),
    seasonals                      = list(mg_fed = 2.44255, mg_kg = 85.84055),
    oatgel                         = list(mg_fed = 2.43729, mg_kg = 81.50225),
    low_lectin                     = list(mg_fed = 1.07555, mg_kg = 36.09249),
    gum36_veg37_invert27           = list(mg_fed = 1.09124, mg_kg = 33.96603),
    water31_root31_protein19_gum19 = list(mg_fed = 0.78669, mg_kg = 26.10390)
  ),
  manganese = list(
    baseline                       = list(mg_fed = 1.5    , mg_kg = 50),
    biscuit_elim                   = list(mg_fed = 0.4    , mg_kg = 12.4065),
    lessBug_moreEgg                = list(mg_fed = 1.38766, mg_kg = 49.5905),
    seasonals                      = list(mg_fed = 1.45356, mg_kg = 51.08364),
    oatgel                         = list(mg_fed = 1.46863, mg_kg = 49.11047),
    low_lectin                     = list(mg_fed = 0.30859, mg_kg = 10.3554),
    gum36_veg37_invert27           = list(mg_fed = 0.32665, mg_kg = 10.16727),
    water31_root31_protein19_gum19 = list(mg_fed = 0.26483, mg_kg = 8.78754)
  ),
  iodine = list(
    baseline                       = list(mg_fed = 0      , mg_kg = 0.7),
    biscuit_elim                   = list(mg_fed = 0      , mg_kg = 0),
    lessBug_moreEgg                = list(mg_fed = 0.02172, mg_kg = 0.7763),
    seasonals                      = list(mg_fed = 0.02172, mg_kg = 0.76344),
    oatgel                         = list(mg_fed = 0.02172, mg_kg = 0.72642),
    low_lectin                     = list(mg_fed = 0.00014, mg_kg = 0.0047),
    gum36_veg37_invert27           = list(mg_fed = 0      , mg_kg = 0),
    water31_root31_protein19_gum19 = list(mg_fed = 0      , mg_kg = 0)
  )
) 

vitamins <- list(
  vit_A = list(
    baseline                       = list(mg_fed = 641       , IU_g = 22),
    biscuit_elim                   = list(mg_fed = 732       , IU_g = 25.9006),
    lessBug_moreEgg                = list(mg_fed = 593.10984 , IU_g = 21.1959),
    seasonals                      = list(mg_fed = 2694.5646 , IU_g = 94.69731),
    oatgel                         = list(mg_fed = 760.41222 , IU_g = 25.42795),
    low_lectin                     = list(mg_fed = 2837.95   , IU_g = 95.23402),
    gum36_veg37_invert27           = list(mg_fed = 596.7381  , IU_g = 18.57411),
    water31_root31_protein19_gum19 = list(mg_fed = 2002.58571, IU_g = 66.44960)
  ),
  vit_D3 = list(
    baseline                       = list(mg_fed = 131    , IU_g = 4),
    biscuit_elim                   = list(mg_fed = 97.3   , IU_g = 3.4434),
    lessBug_moreEgg                = list(mg_fed = 131.596, IU_g = 4.7028),
    seasonals                      = list(mg_fed = 122.896, IU_g = 4.31904),
    oatgel                         = list(mg_fed = 130.858, IU_g = 4.37585),
    low_lectin                     = list(mg_fed = 94.452 , IU_g = 3.16956),
    gum36_veg37_invert27           = list(mg_fed = 133.344, IU_g = 4.15047),
    water31_root31_protein19_gum19 = list(mg_fed = 107.838, IU_g = 3.57827)
  ),
  vit_E = list(
    baseline                       = list(mg_fed = 6      , mg_kg = 190),
    biscuit_elim                   = list(mg_fed = 4.5    , mg_kg = 158.0895),
    lessBug_moreEgg                = list(mg_fed = 5.00225, mg_kg = 178.765),
    seasonals                      = list(mg_fed = 5.1554 , mg_kg = 181.18034),
    oatgel                         = list(mg_fed = 6.97048, mg_kg = 233.09054),
    low_lectin                     = list(mg_fed = 4.39583, mg_kg = 147.51238),
    gum36_veg37_invert27           = list(mg_fed = 5.68865, mg_kg = 177.06542),
    water31_root31_protein19_gum19 = list(mg_fed = 4.17978, mg_kg = 138.69307)
  ),
  vit_K = list(
    baseline                       = list(mg_fed = 0.048859, mg_kg = 1.7),
    biscuit_elim                   = list(mg_fed = 0       , mg_kg = 0.7201),
    lessBug_moreEgg                = list(mg_fed = 0.04627 , mg_kg = 1.6535),
    seasonals                      = list(mg_fed = 0.05074 , mg_kg = 1.78323),
    oatgel                         = list(mg_fed = 0.04953 , mg_kg = 1.65643),
    low_lectin                     = list(mg_fed = 0.02413 , mg_kg = 0.80957),
    gum36_veg37_invert27           = list(mg_fed = 0.02617 , mg_kg = 0.81465),
    water31_root31_protein19_gum19 = list(mg_fed = 0.02382 , mg_kg = 0.79047)
  ),
  vit_B1_thiamin = list(
    baseline                       = list(mg_fed = 0.196003, mg_kg = 7),
    biscuit_elim                   = list(mg_fed = 0.055   , mg_kg = 1.94),
    lessBug_moreEgg                = list(mg_fed = 0.19234 , mg_kg = 6.8737),
    seasonals                      = list(mg_fed = 0.19914 , mg_kg = 6.99854),
    oatgel                         = list(mg_fed = 0.19693 , mg_kg = 6.58529),
    low_lectin                     = list(mg_fed = 0.043   , mg_kg = 1.44297),
    gum36_veg37_invert27           = list(mg_fed = 0.04498 , mg_kg = 1.40008),
    water31_root31_protein19_gum19 = list(mg_fed = 0.0607  , mg_kg = 2.01405)
  ),
  vit_B2_riboflavin = list(
    baseline                       = list(mg_fed = 0.29455, mg_kg = 10),
    biscuit_elim                   = list(mg_fed = 0.213  , mg_kg = 7.5462),
    lessBug_moreEgg                = list(mg_fed = 0.31262, mg_kg = 11.1721),
    seasonals                      = list(mg_fed = 0.30534, mg_kg = 10.73082),
    oatgel                         = list(mg_fed = 0.27704, mg_kg = 9.26405),
    low_lectin                     = list(mg_fed = 0.30765, mg_kg = 10.32391),
    gum36_veg37_invert27           = list(mg_fed = 0.19895, mg_kg = 6.19260),
    water31_root31_protein19_gum19 = list(mg_fed = 0.17153, mg_kg = 5.69183)
  ),
  vit_B3_niacin = list(
    baseline                       = list(mg_fed = 1.957192, mg_kg = 67),
    biscuit_elim                   = list(mg_fed = 0.962   , mg_kg = 34.0582),
    lessBug_moreEgg                = list(mg_fed = 1.70526 , mg_kg = 60.9407),
    seasonals                      = list(mg_fed = 1.89955 , mg_kg = 66.75745),
    oatgel                         = list(mg_fed = 1.92509 , mg_kg = 64.37443),
    low_lectin                     = list(mg_fed = 0.81165 , mg_kg = 27.23681),
    gum36_veg37_invert27           = list(mg_fed = 0.79862 , mg_kg = 24.85802),
    water31_root31_protein19_gum19 = list(mg_fed = 0.7196  , mg_kg = 23.87755)
  ),
  choline = list(
    baseline                       = list(mg_fed = 37.545567, mg_kg = 1168.97601747296),
    biscuit_elim                   = list(mg_fed = 33.015   , mg_kg = 1168.976),
    lessBug_moreEgg                = list(mg_fed = 72.28271 , mg_kg = 2583.1591),
    seasonals                      = list(mg_fed = 48.8966  , mg_kg = 1718.41361),
    oatgel                         = list(mg_fed = 35.42382 , mg_kg = 1184.56164),
    low_lectin                     = list(mg_fed = 27.825   , mg_kg = 933.73267),
    gum36_veg37_invert27           = list(mg_fed = 28.27528 , mg_kg = 880.09808),
    water31_root31_protein19_gum19 = list(mg_fed = 46.25473 , mg_kg = 1534.8198)
  ),
  vit_B5_pantothenic_acid = list(
    baseline                       = list(mg_fed = 1.195088, mg_kg = 41),
    biscuit_elim                   = list(mg_fed = 0.499   , mg_kg = 17.6565),
    lessBug_moreEgg                = list(mg_fed = 1.27381 , mg_kg = 45.5222),
    seasonals                      = list(mg_fed = 1.25365 , mg_kg = 44.05806),
    oatgel                         = list(mg_fed = 1.17367 , mg_kg = 39.24709),
    low_lectin                     = list(mg_fed = 0.53635 , mg_kg = 17.99847),
    gum36_veg37_invert27           = list(mg_fed = 0.44874 , mg_kg = 13.96738),
    water31_root31_protein19_gum19 = list(mg_fed = 0.51371 , mg_kg = 17.04597)
  ),
  vit_B6_pyridoxine = list(
    baseline                       = list(mg_fed = 0.26034, mg_kg = 9),
    biscuit_elim                   = list(mg_fed = 0.096  , mg_kg = 3.403),
    lessBug_moreEgg                = list(mg_fed = 0.25503, mg_kg = 9.1139),
    seasonals                      = list(mg_fed = 0.27178, mg_kg = 9.55139),
    oatgel                         = list(mg_fed = 0.26179, mg_kg = 8.75416),
    low_lectin                     = list(mg_fed = 0.08041, mg_kg = 2.69834),
    gum36_veg37_invert27           = list(mg_fed = 0.08094, mg_kg = 2.51945),
    water31_root31_protein19_gum19 = list(mg_fed = 0.11476, mg_kg = 3.80793)
  ),
  vit_B7_biotin = list(
    baseline                       = list(mg_fed = 0.008388, mg_kg = 0.29),
    biscuit_elim                   = list(mg_fed = 0.006   , mg_kg = 0.2135),
    lessBug_moreEgg                = list(mg_fed = 0.00671 , mg_kg = 0.2399),
    seasonals                      = list(mg_fed = 0.00772 , mg_kg = 0.27124),
    oatgel                         = list(mg_fed = 0.00822 , mg_kg = 0.27481),
    low_lectin                     = list(mg_fed = 0.00613 , mg_kg = 0.20571),
    gum36_veg37_invert27           = list(mg_fed = 0.00603 , mg_kg = 0.18769),
    water31_root31_protein19_gum19 = list(mg_fed = 0.00302 , mg_kg = 0.10004)
  ),
  folic_acid = list(
    baseline                       = list(mg_fed = 0.24385, mg_kg = 8),
    biscuit_elim                   = list(mg_fed = 0.011  , mg_kg = 0.3983),
    lessBug_moreEgg                = list(mg_fed = 0.24073, mg_kg = 8.6028),
    seasonals                      = list(mg_fed = 0.2426 , mg_kg = 8.52589),
    oatgel                         = list(mg_fed = 0.2426 , mg_kg = 8.11247),
    low_lectin                     = list(mg_fed = 0.01125, mg_kg = 0.37752),
    gum36_veg37_invert27           = list(mg_fed = 0.01125, mg_kg = 0.35017),
    water31_root31_protein19_gum19 = list(mg_fed = 0.00563, mg_kg = 0.18665)
  ),
  vit_B12 = list(
    baseline                       = list(mg_fed = 0.0563012, ug_kg = 1.92),
    biscuit_elim                   = list(mg_fed = 0.1      , ug_kg = 3.5378),
    lessBug_moreEgg                = list(mg_fed = 0.02871  , ug_kg = 1.026),
    seasonals                      = list(mg_fed = 0.04525  , ug_kg = 1.59025),
    oatgel                         = list(mg_fed = 0.0452   , ug_kg = 1.51152),
    low_lectin                     = list(mg_fed = 0.09992  , ug_kg = 3.3532),
    gum36_veg37_invert27           = list(mg_fed = 0.09990  , ug_kg = 3.10949),
    water31_root31_protein19_gum19 = list(mg_fed = 0.05005  , ug_kg = 1.66075)
  ),
  vit_C = list(
    baseline                       = list(mg_fed = 11.7    , mg_kg = 400),
    biscuit_elim                   = list(mg_fed = 8.199   , mg_kg = 290.2894),
    lessBug_moreEgg                = list(mg_fed = 10.14728, mg_kg = 362.6323),
    seasonals                      = list(mg_fed = 10.1506 , mg_kg = 356.73092),
    oatgel                         = list(mg_fed = 12.60268, mg_kg = 421.42964),
    low_lectin                     = list(mg_fed = 2.784   , mg_kg = 93.4236),
    gum36_veg37_invert27           = list(mg_fed = 7.03945 , mg_kg = 219.11051),
    water31_root31_protein19_gum19 = list(mg_fed = 11.9245 , mg_kg = 395.67758)
  ),
  beta_carotene = list(
    baseline                       = list(mg_fed = 0.3    , mg_kg = 10),
    biscuit_elim                   = list(mg_fed = 0.365  , mg_kg = 12.9073),
    lessBug_moreEgg                = list(mg_fed = 0.21937, mg_kg = 7.8394),
    seasonals                      = list(mg_fed = 1.32197, mg_kg = 46.45908),
    oatgel                         = list(mg_fed = 0.33946, mg_kg = 11.35145),
    low_lectin                     = list(mg_fed = 1.5285 , mg_kg = 51.29238),
    gum36_veg37_invert27           = list(mg_fed = 0.30174, mg_kg = 9.39192),
    water31_root31_protein19_gum19 = list(mg_fed = 1.02329, mg_kg = 33.95471)
  ),
  lycopene = list(
    baseline                       = list(mg_fed = 0.0192825, ug_kg = 1),
    biscuit_elim                   = list(mg_fed = 0.039    , ug_kg = 1.3655),
    lessBug_moreEgg                = list(mg_fed = 0.01286  , ug_kg = 0.4594),
    seasonals                      = list(mg_fed = 0.00005  , ug_kg = 0.00176),
    oatgel                         = list(mg_fed = 0.02571  , ug_kg = 0.85973),
    low_lectin                     = list(mg_fed = 0.0001   , ug_kg = 0.00336),
    gum36_veg37_invert27           = list(mg_fed = 0.03214  , ug_kg = 1.00031),
    water31_root31_protein19_gum19 = list(mg_fed = 0.05936  , ug_kg = 1.9696)
  )
) 

diet_footnotes <- list(
  "Gum supplement is Mazuri Enrich Gum Arabic (5B35)"
)
