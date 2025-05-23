

nutrition <- tibble(
  formula = c(
    "5m1s",
    "5m1g",
    "5ma4",
    "5ma3",
    "5m6c",
    "5m02",
    "5ma2"
    ),
  sku = c(
    0048134,
    0043252,
    0007317,
    0007316,
    3005301701,
    0001472,
    0040996
  ),
  name = c(
    "Mazuri<sup>\u00AE</sup> Primate LS Biscuit Large",
    "Mazuri<sup>\u00AE</sup> Primate LS Biscuit - Small",
    "Mazuri<sup>\u00AE</sup> Primate Browse Biscuit",
    "Mazuri<sup>\u00AE</sup> Primate High Fiber Sticks",
    "Mazuri<sup>\u00AE</sup> Insectivore Diets",
    "Mazuri<sup>\u00AE</sup> Leaf-Eater Primate Diet",
    "Mazuri<sup>\u00AE</sup> Primate Maintenance Biscuit"
  ),
  description = c("Commercial Diet (Biscuit)"),
  protein = c(
    .2,
    .21,
    .18,
    .18,
    .28,
    .23,
    .15
  ),
  fat = c(
    .05,
    .05,
    .03,
    .05,
    .11,
    .05,
    .04
  ),
  fiber = c(
    .2,
    .2,
    .16,
    .145,
    .15,
    .14,
    .1
  ),
  moisture = c(
    .12,
    .12,
    .12,
    .12,
    .12,
    .12,
    .12
  ),
  ash      = c(
    .08,
    .08,
    .08,
    .08,
    .08,
    .09,
    .08
  ),
  info = c(
    "Mazuri® Primate LS Biscuit — Large (formerly Primate L/S Biscuit — Cinnamon) is formulated to provide a reduced-starch, high-fiber portion of a primate's complete diet. The cinnamon-flavored biscuits are the larger of the two Mazuri® Primate LS Biscuit offerings and are highly fortified to meet the needs of a variety of primates under managed care. The cinnamon flavor has been tested for palatability in a wide range of primates.",
    "Mazuri® Primate LS Biscuit – Small (previously Primate L/S Biscuit – Banana) is formulated to provide a reduced-starch, high-fiber portion of a primate’s complete diet. The bananaflavored biscuits are the smaller of the two Mazuri® Primate LS Biscuit offerings and are highly fortified to meet the needs of a variety of primates under managed care. The banana flavor has been tested for palatability in a wide range of primates.",
    "Mazuri® Primate Browse Biscuit is a supplemental primate food that's formulated to meet the needs of a variety of primates. These primate biscuits allow for natural browsing feeding behaviors.",
    "Mazuri® Primate High Fiber Sticks are designed as a supplemental biscuit for a variety of primates where additional fiber is valuable in the diet.",
    "Mazuri® Insectivore Diet is a highly palatable insectivore food that simulates the high-protein, high-fiber diet of a wide range of insectivores, including mammals, birds, reptiles and amphibians. This diet may be used as a replacement for all or some of the insect component of the diet. Although gut loaded insects may be part of the insectivore diet, no supplementation is needed when using this insectivore diet, which contains taurine at levels that meet the recommendations for carnivores.",
    "Mazuri® Leaf-Eater Primate Diets are complete lifecycle diets specially formulated to meet the needs of leaf-eating primates which are thought to require a high-fiber diet such as lemurs, langurs, and howlers. This diet can be fed to other species of primates such as gorillas and orangutans, when a high-fiber diet is desired.",
    "Mazuri® Primate Maintenance Biscuit is a primate food that's formulated to meet the needs of a variety of primates. These primate biscuits are especially suitable for feeding mature and overweight primates."
  ),
  ingredients = c(
    "Dehulled Soybean Meal, Ground Soybean Hulls, Dried Plain Beet Pulp, Corn Protein Meal, Oat Hulls, Dried Apple Pomace, Cane Molasses, Ground Flaxseed, Ground Whole Aspen, Carrageenan, Soybean Oil, Wheat Germ, Dicalcium Phosphate, Rice Flour, Sucrose, Fructose, Potassium Chloride, Brewers Dried Yeast, Pyridoxine Hydrochloride, Calcium Carbonate, Taurine, L-Ascorbyl2-Polyphosphate (Vitamin C), Choline Chloride, Salt, Cinnamon, Dried Lactobacillus acidophilus Fermentation Product, Calcium Pantothenate, d-Alpha Tocopheryl Acetate (Vitamin E), DL-Methionine, Dried Lactobacillus casei Fermentation Product, Folic Acid, Dried Bifidobacterium thermophilum Fermentation Product, Cholecalciferol (Vitamin D3), Zinc Proteinate, Dried Enterococcus faecium Fermentation Product, Vitamin A Acetate, Manganese Proteinate, Menadione Sodium Bisulfite Complex (Vitamin K), Nicotinic Acid, Copper Proteinate, Riboflavin-5-Phosphate, Thiamine Mononitrate, Preserved with Mixed Tocopherols, Vitamin B12 Supplement, Citric Acid (a Preservative), Beta Carotene, Manganese Sulfate, Copper Sulfate, Cobalt Proteinate, Ethylenediamine Dihydroiodide, Rosemary Extract, Sodium Selenite, Biotin",
    "Ground Soybean Hulls, Dehulled Soybean Meal, Corn Protein Meal, Dried Apple Pomace, Dried Plain Beet Pulp, Oat Hulls, Cane Molasses, Ground Flaxseed, Carrageenan, Ground Whole Aspen, Rice Flour, Soybean Oil, Wheat Germ, Dicalcium Phosphate, Soy Protein Concentrate, Sucrose, Fructose, Artificial Flavor, Potassium Chloride, Brewers Dried Yeast, Pyridoxine Hydrochloride, Salt, Taurine, L-Ascorbyl-2-Polyphosphate (Vitamin C), Choline Chloride, Calcium Carbonate, Dried Lactobacillus acidophilus Fermentation Product, Calcium Pantothenate, d-Alpha Tocopheryl Acetate (Vitamin E), Dried Lactobacillus casei Fermentation Product, DL-Methionine, Folic Acid, Zinc Proteinate, Dried Bifidobacterium thermophilum Fermentation Product, Cholecalciferol (Vitamin D3), Dried Enterococcus faecium Fermentation Product, Manganese Proteinate, Vitamin A Acetate, Copper Proteinate, Menadione Sodium Bisulfite Complex (Vitamin K), Nicotinic Acid, Riboflavin-5-Phosphate, Thiamine Mononitrate, Preserved with Mixed Tocopherols, Vitamin B12 Supplement, Citric Acid (a Preservative), Beta Carotene, Cobalt Proteinate, Ethylenediamine Dihydroiodide, Copper Sulfate, Rosemary Extract, Sodium Selenite, Biotin.",
    "Ground Corn, Dehulled Soybean Meal, Corn Gluten Meal, Dried Plain Beet Pulp, Ground Whole Aspen, Sucrose, Powdered Cellulose, Dried Apple Pomace, Fructose, Calcium Carbonate, Soybean Oil, Ground Flaxseed, Dicalcium Phosphate, Potassium Chloride, Brewers Dried Yeast, L-Lysine, Natural Flavor, Choline Chloride, Pyridoxine Hydrochloride, L-Ascorbyl-2-Polyphosphate (Vitamin C), Salt, Taurine, Iron Proteinate, DL-Methionine, Folic Acid, Zinc Proteinate, Manganese Proteinate, Cholecalciferol (Vitamin D3), d-Alpha Tocopheryl Acetate (Vitamin E), Calcium Pantothenate, Vitamin A Acetate, Magnesium Oxide, Menadione Sodium Bisulfite Complex (Vitamin K), Preserved with Mixed Tocopherols, Thiamine Mononitrate, Rosemary Extract, Vitamin B12 Supplement, Riboflavin Supplement, Nicotinic Acid, Citric Acid (a Preservative), Beta Carotene, Copper Proteinate, Cobalt Proteinate, Ethylenediamine Dihydroiodide, Sodium Selenite, Biotin.",
    "Ground corn, ground soybean hulls, dehulled soybean meal, ground oats, corn gluten meal, dried beet pulp, dried apple pomace, ground aspen, soybean oil, calcium carbonate, dehydrated alfalfa meal, flaxseed, sodium hexametaphosphate, dicalcium phosphate, brewers dried yeast, natural flavor, l-ascorbyl-2-polyphosphate (stabilized vitamin C), pyridoxine hydrochloride, salt, choline chloride, taurine, dl-methionine, folic acid, cholecalciferol (form of vitamin D3), d-alpha tocopheryl acetate (form of vitamin E), l-lysine, calcium pantothenate, vitamin A acetate, beta carotene, biotin, iron proteinate, menadione sodium bisulfite complex (source of vitamin K), preserved with mixed tocopherols (form of vitamin E), rosemary extract, thiamine mononitrate, vitamin B12 supplement, nicotinic acid, riboflavin supplement, citric acid, zinc proteinate, manganese proteinate, ground rice hulls, copper proteinate, cobalt proteinate, ethylenediamine dihydroiodide, sodium selenite.",
    "Chicken Meal, Ground Soybean Hulls, Ground Wheat, Dehulled Soybean Meal, Rice Flour, Poultry Fat Preserved with Mixed Tocopherols, Ground Whole Aspen, Powdered Cellulose, Dried Plain Beet Pulp, Brewers Dried Yeast, Dried Apple Pomace, Wheat Germ, Dried Whey, Fish Meal, Natural Poultry Flavor, Lecithin, Soybean Oil, Fish Oil, Phosphoric Acid, Calcium Propionate (a preservative), Salt, DL-Methionine, Taurine, Choline Chloride, Lactose, L-Ascorbyl-2Polyphosphate (Vitamin C), Pyridoxine Hydrochloride, Thiamine Mononitrate, Menadione Sodium Bisulfite Complex (Vitamin K), Manganous Oxide, d-Alpha Tocopheryl Acetate (Vitamin E), Preserved with Mixed Tocopherols, Rosemary Extract, Inositol, Biotin, Cobalt Carbonate, Vitamin B12 Supplement, Folic Acid, Citric Acid (a Preservative), Vitamin A Acetate, Riboflavin Supplement, Calcium Carbonate, Calcium Pantothenate, Nicotinic Acid, Zinc Oxide, Ferrous Carbonate, Calcium Iodate, Ferrous Sulfate, Copper Sulfate, Zinc Sulfate, Sodium Selenite",
    "Dehulled Soybean Meal, Ground Soybean Hulls, Ground Corn, Corn Gluten Meal, Ground Oats, Dried Plain Beet Pulp, Dried Apple Pomace, Soybean Oil, Dehydrated Alfalfa Meal, Dicalcium Phosphate, Calcium Carbonate, Ground Flaxseed, Brewers Dried Yeast, Salt, L-Ascorbyl-2-Polyphosphate (Vitamin C), DL-Methionine, Pyridoxine Hydrochloride, Choline Chloride, Folic Acid, Vitamin A Acetate, Cholecalciferol (Vitamin D3), d-Alpha Tocopheryl Acetate (Vitamin E), Calcium Pantothenate, Ferrous Sulfate, Menadione Sodium Bisulfite Complex (Vitamin K), Preserved with Mixed Tocopherols, Manganous Oxide, Zinc Oxide, Rosemary Extract, Ferrous Carbonate, Nicotinic Acid, Citric Acid (a Preservative), Thiamine Mononitrate, Vitamin B12 Supplement, Riboflavin Supplement, Copper Sulfate, Zinc Sulfate, Calcium Iodate, Cobalt Carbonate, Sodium Selenite, Biotin.",
    "Ground Corn, Dehulled Soybean Meal, Corn Gluten Meal, Dried Plain Beet Pulp, Ground Whole Aspen, Sucrose, Powdered Cellulose, Dried Apple Pomace, Fructose, Calcium Carbonate, Soybean Oil, Ground Flaxseed, Dicalcium Phosphate, Potassium Chloride, Brewers Dried Yeast, L-Lysine, Natural Flavor, Choline Chloride, Pyridoxine Hydrochloride, L-Ascorbyl-2-Polyphosphate (Vitamin C), Salt, Taurine, Iron Proteinate, DL-Methionine, Folic Acid, Zinc Proteinate, Manganese Proteinate, Cholecalciferol (Vitamin D3), d-Alpha Tocopheryl Acetate (Vitamin E), Calcium Pantothenate, Vitamin A Acetate, Magnesium Oxide, Menadione Sodium Bisulfite Complex (Vitamin K), Preserved with Mixed Tocopherols, Thiamine Mononitrate, Rosemary Extract, Vitamin B12 Supplement, Riboflavin Supplement, Nicotinic Acid, Citric Acid (a Preservative), Beta Carotene, Copper Proteinate, Cobalt Proteinate, Ethylenediamine Dihydroiodide, Sodium Selenite, Biotin."
  )
)

receptor <- c("alpha", "beta")

treatments <- c(
  control        = "Control" ,
  MeOH           = "Veh MeOH",
  DMSO           = "Veh DMSO",
  e2             = "E2"      ,
  e1             = "E1"      ,
  e3             = "E3"      ,
  genistein      = "gen",
  daidzein       = "daid"   ,
  coumestrol     = "cou"   ,
  equol          = "equol"   ,
  enterodiol     = "entdiol"   ,
  mazuri_5m1s    = "LS Cin",
  mazuri_5m1g    = "LS Ban",
  mazuri_5ma4    = "Browse Biscuit",
  mazuri_5ma3    = "High Fiber"    ,
  mazuri_5m6c    = "Insect",
  mazuri_5m02    = "Leafeater",
  mazuri_5m02    = "LE Primate",
  mazuri_5ma2    = "primate_osu"   ,
  lettuce        = "lettuce",
  WI             = "WI"
)


treat_type <- c(
  control        = "Control" ,
  vehicle        = "Veh MeOH",
  vehicle        = "Veh DMSO",
  standard       = "E2"      ,
  standard       = "E1"      ,
  standard       = "E3"      ,
  standard       = "gen",
  standard       = "daid"   ,
  standard       = "cou"   ,
  standard       = "equol"   ,
  standard       = "entdiol"   ,
  diet           = "LS Cin",
  diet           = "LS Ban",
  diet           = "Browse Biscuit",
  diet           = "High Fiber"    ,
  diet           = "Insect",
  diet           = "Leafeater",
  diet           = "LE Primate",
  diet           = "primate_osu"   ,
  diet           = "lettuce",
  diet           = "WI"
)

treat_subtype <- c(
  control        = "Control" ,
  vehicle        = "Veh MeOH",
  vehicle        = "Veh DMSO",
  estrogen       = "E2"      ,
  estrogen       = "E1"      ,
  estrogen       = "E3"      ,
  isoflavone     = "gen",
  isoflavone     = "daid"   ,
  coumestans     = "cou"   ,
  metabolite     = "equol"   ,
  metabolite     = "entdiol"   ,
  biscuit        = "LS Cin",
  biscuit        = "LS Ban",
  biscuit        = "Browse Biscuit",
  biscuit        = "High Fiber"    ,
  biscuit        = "Insect",
  biscuit        = "Leafeater",
  biscuit        = "LE Primate",
  biscuit        = "primate_osu"   ,
  produce        = "lettuce",
  produce        = "WI"
)

column <- c(
  control      = "Control" ,
  control      = "Veh MeOH",
  control      = "Veh DMSO",
  control      = "E2"      ,
  treat_e1     = "E1"      ,
  treat_e3     = "E3"      ,
  treat_gen    = "gen",
  treat_dai    = "daid"   ,
  treat_cou    = "cou"   ,
  treat_equ    = "equol"   ,
  treat_ent    = "entdiol"   ,
  treat_5m1s   = "LS Cin",
  treat_5m1g   = "LS Ban",
  treat_5ma4   = "Browse Biscuit",
  treat_5ma3   = "High Fiber"    ,
  treat_5m6c   = "Insect",
  treat_5m02   = "Leafeater",
  treat_5m02   = "LE Primate",
  treat_5ma2   = "primate_osu"   ,
  treat_let    = "lettuce",
  treat_wi     = "WI"
)


receptors <- c(
  alpha = "esr1",
  beta  = "esr2"
)

produce <- tibble(
  treat   = c("lettuce", "WI"),
  formula = c("lettuce", "WI"),
  name    = c("Lettuce", "WI"),
  description = c("Produce")
)

ingredients <- select(nutrition, formula, ingredients) %>%
  separate_longer_delim(ingredients, ", ") %>%
  mutate(rank = row_number(), .by = "formula") %>%
  mutate(ingredients = str_to_lower(str_squish(str_remove_all(ingredients, "\\.")))) %>%
  mutate(rel_rank  = percent_rank(desc(rank)), .by = "formula", .keep = "unused") %>%
  mutate(med_rank = median(rel_rank), .by = "ingredients") %>%
  pivot_wider(names_from  = "formula",
              values_from = "rel_rank",
              values_fill = 0) %>%
  mutate(across(where(is.numeric), ~round(.x, digits = 2))) %>%
  arrange(desc(med_rank))

standards <- tibble(
  treat = c(
    "e2",
    "e1",
    "e3",
    "genistein", 
    "daidzein",  
    "coumestrol",
    "equol",     
    "enterodiol"
  ),
  name = c(
    "Estradiol",
    "Estrone",
    "Estriol",
    "Genistein",
    "Daidzein",
    "Coumestrol",
    "Equol",
    "Enterodiol"
  ),
  description = c(
    "Endogenous estrogen",
    "Endogenous estrogen",
    "Endogenous estrogen",
    "Isoflavone plant metabolite",
    "Isoflavone plant metabolite",
    "Coumestan plant metabolite",
    "Microbial metabolite",
    "Microbial metabolite"),
  formula = c(
    "e2",
    "e1",
    "e3",
    "genistein", 
    "daidzein",  
    "coumestrol",
    "equol",     
    "enterodiol")
)

diet_key <- nutrition %>%
  select(formula, name, description) %>%
  mutate(treat = as.character(str_glue("mazuri_{formula}"))) %>%
  bind_rows(standards, produce)

