i_am("metadata/loris/diet_tables.R")
source(here("metadata/loris/factors.R"))

enframe_nutr_simple <- function(list, name) {
  enframe(list, name = "nutrient") %>%
    mutate(nutrient_class = factor(name, levels = nutrition_factors$classes),
           nutrient = factor(str_replace_all(nutrient, "totals", "total"), 
                             levels = nutrition_factors$all)) %>%
    unnest_longer(value, indices_to = "diet_name")  %>%
    mutate(diet_name = factor(diet_name, levels = diet_factors)) %>%
    arrange(nutrient_class, nutrient, diet_name) %>%
    unnest_wider(value) %>%
    mutate(relative_unit = "proportion",
           relative_fed  = proportion, .keep = "unused")
}

enframe_nutr_complex <- function(list, name, relative_vals) {
  enframe(list, name = "nutrient") %>%
    mutate(nutrient_class = factor(name, levels = nutrition_factors$classes),
           nutrient = factor(str_replace_all(nutrient, "totals", "total"), 
                             levels = nutrition_factors$all)) %>%
    unnest_longer(value, indices_to = "diet_name")  %>%
    mutate(diet_name = factor(diet_name, levels = diet_factors)) %>%
    arrange(nutrient_class, nutrient, diet_name)%>%
  unnest_wider(value) %>%
    pivot_longer(cols      = all_of(relative_vals),
                 names_to  = "relative_unit",
                 values_to = "relative_fed",
                 values_drop_na = T)
}

nutr_fat      <- enframe_nutr_complex(fats, "fats", c("proportion", "mg_g"))
nutr_protein  <- enframe_nutr_simple(proteins, "proteins")
nutr_CHOs     <- enframe_nutr_simple(CHOs, "CHOs")
nutr_Ash      <- enframe_nutr_complex(Ash, "Ash", c("proportion", "mg_kg"))  %>% select(-ca_p_ratio)
nutr_vitamins <- enframe_nutr_complex(vitamins, "vitamins", c("mg_kg", "IU_g", "ug_kg"))


nutrition <- bind_rows(nutr_protein,
                       nutr_fat,
                       nutr_CHOs,
                       nutr_Ash,
                       nutr_vitamins) %>%
  mutate(fed      = mg_fed,
         fed_unit = "mg", .keep = "unused") %>%  distinct()

food_totals <-  enframe(foods_mg, name = "diet_name") %>%
  unnest_longer(value, values_to = "mg_fed", indices_to = "food") %>%
  mutate(diet_name = factor(diet_name, levels = diet_factors)) %>%
  arrange(diet_name, food) %>%
  select(diet_name, food, mg_fed) %>%  distinct()


diet_totals <- enframe(nutrient_totals, name = "diet_name") %>%
  unnest_wider(value) %>%
  mutate(nutrient       = factor("total", levels = nutrition_factors$all), 
         nutrient_class = factor("total", levels = nutrition_factors$classes),
         diet_name      = factor(diet_name, levels = diet_factors)) %>%
  pivot_longer(cols = c("mg_fed", "mg_dry", "kcal"),
               names_to  = "fed_unit",
               values_to = "fed") %>%
  mutate(fed_unit      = str_remove_all(fed_unit, "_fed"),
         relative_fed  = 1,
         relative_unit = "proportion") %>%
  bind_rows(nutrition) %>%
  arrange(diet_name, nutrient_class, nutrient, fed_unit, relative_unit) %>%
  select(diet_name, 
         nutrient_class, 
         nutrient, 
         fed, 
         fed_unit, 
         relative_fed, 
         relative_unit) %>% distinct()

write.table(diet_totals, here(path$metadata$diet_totals), sep = "\t", row.names = F)

diet_nested <- diet_totals %>%
  group_by(diet_name, nutrient_class) %>%
  nest(.key = "nutrients") %>%
  pivot_wider(id_cols     = "diet_name", 
              names_from  = "nutrient_class",
              values_from = "nutrients") %>%
  unnest(total) %>%
  pivot_wider(id_cols     = !any_of(
    c("nutrient", 
      "fed", 
      "fed_unit", 
      "relative_fed", 
      "relative_unit")),
    names_from  = c("nutrient", "fed_unit"),
    names_sep   = "_",
    values_from = "fed") %>%
  nest_join(food_totals, by = join_by(diet_name), name = "foods") %>%
  relocate(all_of(c(starts_with("total_"))), .after = diet_name) %>%
  relocate(foods, .after = total_mg_dry) %>% distinct()
