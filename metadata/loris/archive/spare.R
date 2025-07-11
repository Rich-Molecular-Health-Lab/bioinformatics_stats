relative_foods    <- relative_nutrients %>% select(identifier, subject, any_of(nutrition_factors$foods)) %>% rename_with(~ paste0("rel_", .x), where(is.numeric)) %>% left_join(foods_wide, by = join_by(identifier, subject))
relative_proteins <- relative_nutrients %>% select(identifier, subject, any_of(nutrition_factors$proteins)) %>% rename_with(~ paste0("rel_", .x), where(is.numeric)) %>% left_join(proteins_wide, by = join_by(identifier, subject))
relative_fats     <- relative_nutrients %>% select(identifier, subject, any_of(nutrition_factors$fats    )) %>% rename_with(~ paste0("rel_", .x), where(is.numeric)) %>% left_join(fats_wide    , by = join_by(identifier, subject))
relative_CHOs     <- relative_nutrients %>% select(identifier, subject, any_of(nutrition_factors$CHOs    )) %>% rename_with(~ paste0("rel_", .x), where(is.numeric)) %>% left_join(CHOs_wide    , by = join_by(identifier, subject))
relative_Ash      <- relative_nutrients %>% select(identifier, subject, any_of(nutrition_factors$Ash     )) %>% rename_with(~ paste0("rel_", .x), where(is.numeric)) %>% left_join(Ash_wide     , by = join_by(identifier, subject))
relative_vitamins <- relative_nutrients %>% select(identifier, subject, any_of(nutrition_factors$vitamins)) %>% rename_with(~ paste0("rel_", .x), where(is.numeric)) %>% left_join(vitamins_wide, by = join_by(identifier, subject))


write.table(subtables         , here(path$metadata$subtables)    , sep = "\t", row.names = F)
write.table(relative_nutrients, here(path$metadata$subtables_rel), sep = "\t", row.names = F)
write.table(relative_foods    , here(path$metadata$foods)        , sep = "\t", row.names = F)
write.table(relative_proteins , here(path$metadata$proteins)     , sep = "\t", row.names = F)
write.table(relative_fats     , here(path$metadata$fats    )     , sep = "\t", row.names = F)
write.table(relative_CHOs     , here(path$metadata$CHOs    )     , sep = "\t", row.names = F)
write.table(relative_Ash      , here(path$metadata$Ash     )     , sep = "\t", row.names = F)
write.table(relative_vitamins , here(path$metadata$vitamins)     , sep = "\t", row.names = F)


relative_nutrients <- subtables %>% mutate(across(where(is.numeric), ~ relative_values(.x))) %>% rename_with(~ paste0("rel_", .x), where(is.numeric)) %>% left_join(subtables, by = join_by(identifier, subject))



foods_wide <- metadata %>% select(identifier, subject, foods) %>% 
  unnest(foods) %>% 
  select(identifier, subject, food, mg_fed) %>%
  pivot_wider(id_cols     = c("identifier", "subject"), 
              names_from  = "food", 
              values_from = "mg_fed") %>%
  mutate(HDZ_oatgel = replace_na(HDZ_oatgel, 0))
proteins_wide <- wide_subtables(metadata, "proteins")
fats_wide     <- wide_subtables(metadata, "fats")
CHOs_wide     <- wide_subtables(metadata, "CHOs") %>% select(-WSC)
Ash_wide      <- wide_subtables(metadata, "Ash")
vitamins_wide <- wide_subtables(metadata, "vitamins") %>% select(-vitamins_total)

subtables <- proteins_wide %>% 
  left_join(fats_wide    , by = join_by(identifier, subject)) %>% 
  left_join(CHOs_wide    , by = join_by(identifier, subject)) %>% 
  left_join(Ash_wide     , by = join_by(identifier, subject)) %>% 
  left_join(vitamins_wide, by = join_by(identifier, subject)) %>%
  left_join(foods_wide, by = join_by(identifier, subject))