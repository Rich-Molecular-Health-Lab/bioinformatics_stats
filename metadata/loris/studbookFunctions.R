enhance.locations <- function(df, location) {
  sample(palettes_d$palettesForR$Cranes, 
         size    = length(unique(pull(df, location))), 
         replace = FALSE) %>%
    as.list() %>%
    set_names(., map(as.list(unique(pull(df, location))), \(x) paste0(x))) %>%
    enframe(name = paste0(location), value = "colorLoc")  %>% 
    mutate(colorLoc = as.character(colorLoc)) %>%
    right_join(df, by = join_by(location)) %>%
    mutate(iconLoc = case_when(
      Country == "United States"             ~ "\U1F1FA\U1F1F8",
      Country == "United Kingdom"            ~ "\U1F1EC\U1F1E7",
      Country == "Vietnam"                   ~ "\U1F1FB\U1F1F3",
      Country == "Canada"                    ~ "\U1F1E8\U1F1E6",
      Country == "Denmark"                   ~ "\U1F1E9\U1F1F0",
      Country == "Germany"                   ~ "\U1F1E9\U1F1EA",
      Country == "Poland"                    ~ "\U1F1F5\U1F1F1",
      Country == "Russian Federation"        ~ "\U1F1F7\U1F1FA",
      Country == "Sweden"                    ~ "\U1F1F8\U1F1EA",
      Country == "Switzerland"               ~ "\U1F1E8\U1F1ED",
      Country == "Taiwan"                    ~ "\U1F1F9\U1F1FC",
      Country == "Thailand"                  ~ "\U1F1F9\U1F1ED",
      Country == "Laos"                      ~ "\U1F1F1\U1F1E6",
      Country == "Undetermined"              ~ "\U1F6A9",
      is.na(Country)                         ~ "\U1F6A9"))
}

clean.locations <- function(df) {
  df %>%
    select(Name  = Institution.Name, 
           Label = Mnemonic, 
           Country) %>%
    distinct() %>%
    mutate(across(where(is.character), ~ na_if(., "")),
           across(where(is.character), ~str_trim(str_replace_all(., "\\n|\\t", " ")))) %>%
    mutate(Label = if_else(is.na(Label), str_remove_all(Name, "(?<=.)(\\s\\w+){1,}"), Label)) %>%
    mutate(Label = str_to_upper(str_remove_all(Label, "[^\\w+]"))) %>%
    mutate(Label = str_to_upper(Label))  %>%
    mutate(
      Label   = if_else(Name == "SWITZRLND: Invalid use of GeoRef", "SWITZRLNDINVALIDUSEOFGEOREF", Label),
      Country = if_else(Label == "PUBLIC", "Undetermined", Country)
    ) %>%
    distinct() %>%
    arrange(Label) %>%
    mutate(Abbrev    = as.character(str_sub(Label,  1L,  3L)),
           Abbrev2   = as.character(str_sub(Label, -2L, -1L))) %>%
    mutate(LocAbbrev = if_else(!is.na(lead(Abbrev)) & Abbrev == lead(Abbrev), 
                               str_replace(Abbrev, "(?<=^\\w)\\w{2}$", 
                                           Abbrev2), Abbrev),
           NameLoc = Name, .keep = "unused") %>%
    arrange(Country, NameLoc) %>% filter(!is.na(NameLoc))
}

clean_studbook <- function(df, alive, locations) {
  df %>%
    select(ID  = Studbook.ID, 
           Sex = Sex.Type, 
           Sire, 
           Dam, 
           Date,
           TypeEvent = Event.Type, 
           Location) %>%
    fill(ID) %>%
    filter(str_detect(TypeEvent, "\\w+")) %>% 
    mutate(Date =  dmy(str_extract(Date, "\\d{1,2}.\\w{3}.\\d{2,4}")),
           across(c(Sire, Dam), as.character),
           across(where(is.character), ~ na_if(., "")),
           Sex       = str_sub(Sex, 1, 1),
           TypeEvent = fct_recode(str_to_lower(TypeEvent),
                                  Birth    = "birth/hatch",
                                  Capture  = "wild capture",
                                  Transfer = "transfer",
                                  LTF      = "go ltf",
                                  Death    = "death")) %>%
    mutate(Date = if_else(Date > today(), Date - years(100), Date)) %>%
    mutate(across(c(Sire, Dam), ~str_replace_all(., "WILD", "0"))) %>%
    group_by(ID) %>%
    fill(Sire, Dam, Sex) %>%
    mutate(across(c(Sire, Dam), as.integer)) %>%
    mutate(Sire = case_when(
      ID == 2108 ~ 12108,
      ID == 2717 ~ 12717,
      ID == 1047 ~ 1012,
      ID == 2645 ~ 1131,
      ID == 1045 | ID == 1046 ~ 11045,
      ID == 2423 | ID == 2424 ~ 12423,
      .default = Sire
    ), 
    Dam = if_else(ID == 2717, 22717, Dam)) %>%
    mutate(Status   = if_else(ID %in% alive, "A", "D"),
           Location = na_if(Location, ""),
           Location = str_to_upper(str_trim(str_remove_all(Location, "[^\\w+]")))) %>%
    left_join(select(locations, 
                     LocAbbrev, 
                     Label, 
                     NameLoc, 
                     Country), 
              by = join_by(Location == Label)) %>%
    mutate(Location = LocAbbrev, .keep = "unused") %>%
    mutate(Date = case_when(
      is.na(Date) & TypeEvent == "Capture" ~ lag(Date), 
      is.na(Date) & TypeEvent == "Birth" & lead(TypeEvent) == "Capture" ~ lead(Date), 
      .default = Date
    ),
    Location = case_when(
      (is.na(Location) | Location == "UND") & TypeEvent == "Capture" ~ lag(Location), 
      (is.na(Location) | Location == "UND") & TypeEvent == "Birth" & lead(TypeEvent) == "Capture" ~ lead(Location), 
      .default = Location
    )) %>%
    filter(!(TypeEvent == "Birth" & lead(TypeEvent) == "Capture")) %>%
    mutate(TypeEvent = fct_collapse(TypeEvent, 
                                    Birth = c("Birth", "Capture"),
                                    End   = c("Death", "LTF"))) %>%
    group_by(ID) %>%
    mutate(OrderLoc = consecutive_id(Location)) %>%
    ungroup() %>%
    distinct() %>% 
    mutate(TypeEvent = as.character(TypeEvent)) %>%
    select(ID,
           Sex,
           Status,
           Sire,
           Dam,
           OrderLoc,
           Location,
           Date,
           TypeEvent,
           NameLoc,
           Country
    )
}

generate_presence_list <- function(StartLoc, EndLoc, DateBirth, Location, Sex, ID) {
  if (is.na(StartLoc)) return(NULL)
  
  StartLoc <- floor_date(StartLoc, "month")
  
  if(is.na(EndLoc)) return(NULL)
  
  EndLoc <- ceiling_date(EndLoc, "month")
  
  if (StartLoc > EndLoc) return(NULL)
  

  tibble(Date     = seq(StartLoc, 
                        EndLoc, 
                        by = "months"),
         Location = Location,
         ID       = ID,
         Sex      = Sex,
         Age      = floor(
           as.numeric(
             as.period(
               interval(
                 DateBirth, 
                 seq(StartLoc, 
                     EndLoc, 
                     by = "months")), 
               unit = "years"), 
             "years"))
  ) %>% arrange(Sex, desc(Age))
  
}

inspect <- function(df, location, studbook) {
  left_join(df, select(studbook,
                       ID,
                       TypeEvent,
                       Location,
                       StartLoc,
                       EndLoc,
                       NameLoc,
                       Country,
                       Sire,
                       Dam,
                       DateBirth,
                       Status), 
            by = join_by(ID)) %>%
    filter(Location == location & TypeEvent != "End") %>%
    select(-TypeEvent) %>%
    distinct()
}


whoisthedaddy <- function(studbook, list) {
  subjects <- studbook %>%
  filter(is.na(Sire) & !is.na(Dam) & TypeEvent == "Birth") %>%
    mutate(Date = floor_date(Date, unit = "months")) %>%
    relocate(Sire, Dam, .after = Location)
  
  search <- as.list(deframe(select(subjects, Date, Location)))
  
  imap(search, \(x, idx) pluck(list, idx, x, 1)) %>%
    set_names(., as.list(deframe(select(subjects, ID)))) %>%
    map_depth(., 1, \(x) filter(x, Sex == "M" & !(ID %in% c(names)))) %>%
    map_depth(., 1, \(x) arrange(x, desc(Age))) %>%
    map2(., search, \(x, y) inspect(x, y, studbook))
}

whoisthemommy <- function(studbook, list) {
  subjects <- studbook %>%
    filter(is.na(Dam) & !is.na(Sire) & TypeEvent == "Birth") %>%
    mutate(Date = floor_date(Date, unit = "months")) %>%
    relocate(Sire, Dam, .after = Location)
  
  search <- as.list(deframe(select(subjects, Date, Location)))
  
  imap(search, \(x, idx) pluck(list, idx, x, 1)) %>%
    set_names(., as.list(deframe(select(subjects, ID)))) %>%
    map_depth(., 1, \(x) filter(x, Sex == "F" & !(ID %in% c(names)))) %>%
    map_depth(., 1, \(x) arrange(x, desc(Age))) %>%
    map2(., search, \(x, y) inspect(x, y, studbook))
}

