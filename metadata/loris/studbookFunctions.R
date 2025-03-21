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

enhance.locations <- function(df) {
  sample(palettes_d$palettesForR$Cranes, 
         size    = length(unique(pull(df, LocAbbrev))), 
         replace = FALSE) %>%
    as.list() %>%
    set_names(., map(as.list(unique(pull(df, LocAbbrev))), \(x) paste0(x))) %>%
    enframe(name = "LocAbbrev", value = "colorLoc")  %>% 
    mutate(colorLoc = as.character(colorLoc)) %>%
    right_join(df, by = join_by(LocAbbrev)) %>%
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


clean_studbook <- function(df, alive, locations) {
  df %>%
    select(ID        = Studbook.ID, 
           Sex       = Sex.Type, 
           Sire, 
           Dam, 
           Date,
           TypeEvent = Event.Type, 
           Location) %>%
    fill(ID) %>%
    mutate(across(c(Sire, Dam), as.character),
           across(where(is.character), ~ na_if(., "")),
           Date      =  dmy(str_extract(Date, "\\d{1,2}.\\w{3}.\\d{2,4}")),
           Sex       = str_sub(Sex, 1, 1),
           TypeEvent = fct_recode(str_to_lower(TypeEvent),
                                  Birth    = "birth/hatch",
                                  Capture  = "wild capture",
                                  Transfer = "transfer",
                                  LTF      = "go ltf",
                                  Death    = "death")) %>%
    mutate(Date            = if_else(Date > today(), Date - years(100), Date),
           Confirmed_Death = if_else(TypeEvent == "Death", "y", NA)) %>%
    mutate(across(c(Sire, Dam), ~as.integer(str_replace_all(., "WILD", "0")))) %>%
    mutate(across(c(Sire, Dam), ~na_if(., 0))) %>%
    group_by(ID) %>%
    fill(Confirmed_Death, .direction = "up") %>%
    mutate(Confirmed_Death = replace_na(Confirmed_Death, "n")) %>%
    fill(Sire, Dam, Sex) %>%
    mutate(Status   = if_else(ID %in% alive, "A", "D"),
           Location = na_if(Location, ""),
           Location = str_to_upper(str_trim(str_remove_all(Location, "[^\\w+]")))) %>%
    left_join(select(locations, 
                     LocAbbrev, 
                     Location = Label, 
                     NameLoc, 
                     Country), 
              by = join_by(Location)) %>%
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
    mutate(TypeEvent = fct_collapse(TypeEvent, 
                                    Birth = c("Birth", "Capture"),
                                    End   = c("Death", "LTF"))) %>%
    distinct() %>%
    group_by(ID) %>%
    mutate(
      Date          = if_else(TypeEvent == "Birth" & TypeEvent == lag(TypeEvent) & !is.na(lag(Date)), lag(Date), Date),
      remove        = if_else(max(row_number()) > 1 & TypeEvent == "Birth" & TypeEvent == lead(TypeEvent) & ID == lead(ID), "y", "n"),
      Confirmed_Age = if_else(TypeEvent == "Birth" & !is.na(Date), "y", NA),
      Sex           = if_else(ID == 2504 | ID == 2717, "F", Sex)) %>%
    fill(Confirmed_Age, remove, .direction = "downup") %>%
    filter(remove == "n") %>%
    mutate(OrderLoc      = consecutive_id(Location),
           Confirmed_Age = replace_na(Confirmed_Age, "n")) %>%
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
           Country,
           Confirmed_Death,
           Confirmed_Age
    )
}

calculate_age <- function(birth, date) {
  floor(as.numeric(as.period(interval(floor_date(birth, "month"), date), unit = "years"), "years"))
}

est_date_between   <- function(date1, date2) {
  date1 + days(floor(as.numeric(as.period((interval(date1, date2)), unit = "days"), "days")/2))
}

assemble_timeline <- function(studbook) {

  end.records <- studbook %>%
    select(ID,
           Status,
           OrderLoc,
           Location,
           TypeEvent,
           StartLoc,
           Date,
           EndLoc
    ) %>%
    arrange(ID, 
            OrderLoc,
            TypeEvent,
            Date) %>%
    slice_tail(n = 1, by = ID) %>%
    filter(TypeEvent != "End") %>%
    mutate(Date      = if_else(Status == "A", today(), NA),
           TypeEvent = "End") %>% 
    mutate(EndLoc    = Date) %>%
    select(-Status) %>% distinct()
  
  sires <- studbook %>%
    filter(TypeEvent == "Birth" & !is.na(Sire)) %>%
    select(ID      = Sire,
           OffspID = ID,
           Location,
           Date)
  
  dams <- studbook %>%
    filter(TypeEvent == "Birth" & !is.na(Dam)) %>%
    select(ID      = Dam,
           OffspID = ID,
           Location,
           Date)
  
  parents <- bind_rows(sires, dams)
  
  births <- studbook %>%
    select(ID,
           OrderLoc,
           Location,
           StartLoc,
           EndLoc) %>%
    right_join(parents, by = join_by(ID, Location)) %>%
    mutate(check = if_else(
      between(Date, StartLoc, EndLoc) | 
        is.na(StartLoc) & Date < EndLoc | 
        is.na(EndLoc) & Date > StartLoc | 
        (is.na(StartLoc) & is.na(EndLoc)) | is.na(Date),
      "keep",
      "discard"
    )) %>% filter(check == "keep") %>%
    mutate(TypeEvent = "Breed") %>%
    select(-c(check, OffspID)) %>% 
    distinct() %>% 
    arrange(ID, 
            OrderLoc,
            Date)
  
    studbook %>%
      select(
        ID,
        Sex,
        OrderLoc,
        Location,
        TypeEvent,
        StartLoc,
        Date,
        EndLoc) %>% 
      bind_rows(births) %>% 
      bind_rows(end.records) %>%
      mutate(TypeEvent = fct(TypeEvent, 
                             levels = c(
        "Birth",
        "Transfer",
        "Breed",
        "End"
      ))) %>%
      arrange(ID, 
              OrderLoc,
              TypeEvent,
              Date) %>%
    distinct() %>%
      group_by(ID) %>%
      fill(Sex, .direction = "downup") %>%
      mutate(OrderEvent = consecutive_id(TypeEvent)) %>%
      mutate(Cohort = if_else(OrderEvent == 1, year(Date), NA)) %>%
      fill(Cohort) %>%
      ungroup()
}

fill_dates_timeline <- function(df) {
  df %>%
    arrange(ID, 
            OrderLoc,
            TypeEvent,
            Date) %>%
    select(ID,
           OrderLoc,
           Location,
           StartLoc,
           TypeEvent,
           Date,
           EndLoc)  %>%
    group_by(ID) %>%
    mutate(Date = case_when(
      is.na(Date) & TypeEvent != "End"  & TypeEvent != "Breed" ~ StartLoc,
      is.na(Date) & TypeEvent == "End"   ~ EndLoc,
      is.na(Date) & TypeEvent == "Birth" ~ lead(Date) - years(1),
      is.na(Date) & TypeEvent == "Transfer" & lead(TypeEvent) != "End" ~  
        est_date_between(lag(Date), lead(Date)),
      .default = Date
    )) %>%
    mutate(Date = if_else(is.na(Date) & TypeEvent == "Transfer" & lead(TypeEvent) != "End", 
                          est_date_between(lag(Date), lead(Date)), Date)) %>%
    mutate(Date = if_else(
      is.na(Date) & lag(TypeEvent) == "Birth" & is.na(lag(Date)),
      lead(Date) - years(1), Date
    )) %>%
    mutate(Date = case_when(
      is.na(Date) & TypeEvent == "Birth"                  ~ lead(Date) - years(1),
      is.na(Date) & TypeEvent == "End" & !(ID %in% alive) ~ lag(Date) + years(1),
      .default = Date)) %>%
    mutate(
      StartLoc = case_when(
        TypeEvent == "Breed" ~ lag(StartLoc),
        TypeEvent != "End" &      TypeEvent != "Breed" ~ Date,
        TypeEvent == "End" & lag(TypeEvent) != "Breed" ~ lag(Date),
        .default = StartLoc
      ),
      EndLoc = case_when(
        TypeEvent       == "Breed" & Location == lead(Location)  ~ lead(EndLoc),
        lead(TypeEvent) == "Transfer" | lead(TypeEvent) == "End" ~ lead(Date),
        TypeEvent       == "End"                                 ~ Date,
        .default = EndLoc
      )
    ) %>% group_by(ID, Location) %>%
    fill(StartLoc, EndLoc, .direction = "updown") %>%
    ungroup() %>% distinct()
    
}

census.location <- function(df, interval) {
  if (interval == "month") {
    records <- df %>%
      mutate(StartLoc = floor_date(StartLoc, "month"),
             EndLoc   = floor_date(EndLoc, "month"),
             DateBirth = floor_date(DateBirth, "month"))
    seqby <- paste0("months")
  } else if (interval == "year") {
    records <- df %>%
      mutate(StartLoc  = floor_date(StartLoc, "year"),
             EndLoc    = floor_date(EndLoc, "year"),
             DateBirth = floor_date(DateBirth, "year"))
    seqby <- paste0("years")
  }
  
  list <-  list(pull(records, StartLoc),
                pull(records, EndLoc), 
                pull(records, DateBirth), 
                pull(records, Location), 
                pull(records, Sex), 
                pull(records, ID))
  
  pmap(list, \(u, v, w, x, y, z) tibble(
    Date     = seq(u, v, by = seqby),
    ID       = z,
    Location = x,
    Sex      = y,
    Age      =  calculate_age(w, seq(u, v, by = seqby))
  )) %>%
    map_depth(., 1, \(x) distinct(x)) %>%
    map_depth(., 1, \(x) arrange(x, Sex, ID)) %>% 
    bind_rows() %>% 
    group_by(Date, Location) %>%
    summarise(Individuals = list(
      tibble(ID, Sex, Age)), 
      .groups = "drop") %>%
    group_by(Date) %>%
    summarise(Locations = split(Individuals, 
                                Location), 
              .groups = "drop") %>%
    split(.$Date) %>%
    map(~ .x$Locations)
}

census <- function(df, interval, groups) {
  if (interval == "month") {
    records <- df %>%
      mutate(Birth = floor_date(Birth, "month"),
             End   = floor_date(End, "month"))
    seqby <- paste0("months")
  } else if (interval == "year") {
    records <- df %>%
      mutate(Birth = floor_date(Birth, "year"),
             End   = floor_date(End, "year"))
    seqby <- paste0("years")
  }
  
  list <-  list(pull(records, Birth),
                pull(records, End), 
                pull(records, Sex), 
                pull(records, ID))
  census <- pmap(list, \(w, x, y, z) tibble(
    Date   = seq(w, x, by = seqby),
    ID     = z,
    Sex    = y,
    Cohort = year(w),
    Age    =  calculate_age(w, seq(w, x, by = seqby))
  )) %>% map_depth(., 1, \(x) distinct(x)) %>%
    map_depth(., 1, \(x) arrange(x, Sex, ID)) %>% 
    bind_rows()
  
  if (groups == "age") {
    census %>%
      group_by(Date, Age) %>%
      summarise(Individuals = list(
        tibble(ID, Sex)), 
        .groups = "drop") %>%
      group_by(Date) %>%
      summarise(Ages = split(Individuals, 
                             Age), 
                .groups = "drop") %>%
      split(.$Date) %>%
      map(~ .x$Ages)
  } else if (groups == "cohort") {
    census %>%
      group_by(Date, Cohort) %>%
      summarise(Individuals = list(
        tibble(ID, Sex)), 
        .groups = "drop") %>%
      group_by(Date) %>%
      summarise(Cohorts = split(Individuals, 
                             Cohort), 
                .groups = "drop") %>%
      split(.$Date) %>%
      map(~ .x$Cohorts)
  } else if (groups == "none") {
    census %>%
      group_by(Date) %>%
      summarise(Individuals = list(tibble(ID, Sex, Age)),
                .groups = "drop") %>%
      split(.$Date)
  }
    
}

count_census_grouped <- function(census, groups) {
  bind_rows(
    map_dfr(names(census), function(date) {
      groups <- census[[date]]
      bind_rows(map_dfr(names(groups), function(group) {
        individuals <- groups[[group]][[1]] 
        tibble(Date           = as.Date(date),
               Group          = group,
               Total          = nrow(individuals),
               Males          = sum(individuals$Sex == "M", na.rm = TRUE),
               Females        = sum(individuals$Sex == "F", na.rm = TRUE),
               Undetermined   = sum(individuals$Sex == "U", na.rm = TRUE))
      }))
    })
  )
}


count_census <- function(census) {
  bind_rows(
    map_dfr(names(census), function(date) {
      individuals <- census[[date]]$Individuals[[1]]  
      tibble(Date           = as.Date(date),
             Total          = nrow(individuals),
             Males          = sum(individuals$Sex == "M", na.rm = TRUE),
             Females        = sum(individuals$Sex == "F", na.rm = TRUE),
             Undetermined   = sum(individuals$Sex == "U", na.rm = TRUE))
    }))
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
  filter(((is.na(Sire) & !is.na(Dam)) | (Status == "A" & is.na(Sire))) & TypeEvent == "Birth") %>%
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
    filter(((!is.na(Sire) & is.na(Dam)) | (Status == "A" & is.na(Dam))) & TypeEvent == "Birth") %>%
    mutate(Date = floor_date(Date, unit = "months")) %>%
    relocate(Sire, Dam, .after = Location)
  
  search <- as.list(deframe(select(subjects, Date, Location)))
  
  imap(search, \(x, idx) pluck(list, idx, x, 1)) %>%
    set_names(., as.list(deframe(select(subjects, ID)))) %>%
    map_depth(., 1, \(x) filter(x, Sex == "F" & !(ID %in% c(names)))) %>%
    map_depth(., 1, \(x) arrange(x, desc(Age))) %>%
    map2(., search, \(x, y) inspect(x, y, studbook))
}

add.hypotheticals <- function(studbook, ids, parent) {
  if (parent == "Sire" | parent == "sire" | parent == "dad" | parent == "Dad") {
      sex.add <- "M"
      add <- 10000
  } else if (parent == "Dam" | parent == "dam" | parent == "mom" | parent == "Mom") {
    sex.add <- "F"
      add <- 20000
  }
  
  hypSire <- min(ids) + 10000
  hypDam  <- min(ids) + 20000
  
  hypotheticals <-  studbook %>%
    filter(ID %in% ids & TypeEvent == "Birth") %>%
    mutate(ID = min(ID) + add,
           StartLoc = min(StartLoc - years(2)),
           Date     = min(Date     - years(2)),
           EndLoc   = max(StartLoc) + years(1),
           Status   = "H",
           Sex      = sex.add,
           Sire     = NA,
           Dam      = NA,
           DateBirth = min(DateBirth - years(2))) %>%
    distinct() %>%
    bind_rows(slice_head(., n = 1)) %>%
    mutate(TypeEvent = if_else(row_number() > 1, "End" , TypeEvent),
           Date      = if_else(row_number() > 1, EndLoc, Date))
  
  if (sex.add == "M") {
    
    studbook %>% bind_rows(hypotheticals) %>%
      mutate(TypeEvent = fct(TypeEvent, levels = c(
        "Birth",
        "Transfer",
        "Breed",
        "End"
      ))) %>%
      arrange(ID, OrderLoc, TypeEvent, Date) %>%
      mutate(Sire = if_else(ID %in% c(ids), hypSire, Sire))
    
  } else if (sex.add == "F") {
    
    studbook %>% bind_rows(hypotheticals) %>%
      mutate(TypeEvent = fct(TypeEvent, levels = c(
        "Birth",
        "Transfer",
        "Breed",
        "End"
      ))) %>%
      arrange(ID, OrderLoc, TypeEvent, Date) %>%
      mutate(Dam = if_else(ID %in% c(ids), hypDam, Dam))
    
  }
  
 
}

my_lambda <- function(years, df) {
  N_final   <- filter(df, Date == floor_date(today(), "year")) %>% pull(Population)
  N_initial <- filter(df, Date == (floor_date(today(), "year") - years(years))) %>% pull(Population)
  
  return((N_final / N_initial)^(1/years))
}

print_lambda <- function(lambda) {
 paste0(round((lambda*100) - 100, digits = 2), "% change")
}

cohort_grid <- function(df, maxAge, minYear) {
  
  cohorts <- minYear:2025
  years   <- cohorts
  sexes   <- c("Male", "Female", "Total")
  
 grid <-  expand_grid(
    Year   = years,
    Cohort = cohorts,
    Sex    = sexes
  )  %>%
   filter(Year >= Cohort, (Year - Cohort) <= maxAge) %>%
   mutate(Age = Year - Cohort) %>%
   select(Cohort, Year, Sex, Age) %>%
   arrange(Cohort, Sex, Age)
 
 df %>% 
   mutate(Year = year(Date)) %>%
   right_join(grid, by = join_by(
   Year, 
   Age, 
   Cohort, 
   Sex
 )) %>%
   mutate(across(where(is.numeric), ~ replace_na(., 0)),
          Date = make_date(year = Year, month = 1, day = 1)) %>%
   arrange(Cohort, Sex, Age) %>%
   relocate(Cohort, Sex, Year, Age)
}

mle_all <- function(lx, age) {

    df <- tibble(lx = lx, age = age) %>%
    filter(!is.na(lx))
  
  below <- df %>% filter(lx <= 0.5 & lx > 0) %>% slice_min(order_by = age, n = 1)
  above <- df %>% filter(lx > 0.5)           %>% slice_max(order_by = age, n = 1)
  
  if (nrow(below) == 0 || nrow(above) == 0) {
    return(NA_real_)
  }
  
  ageLow  <- above$age
  ageHigh <- below$age
  lxLow   <- above$lx
  lxHigh  <- below$lx
  
  ageMLE <- ageLow + ((0.5 - lxLow) / (lxHigh - lxLow)) * (ageHigh - ageLow)
  return(ageMLE)
  

}

mle_age1 <- function(lx, age) {
  df <- tibble(lx = lx, age = age) %>%
    filter(age >= 1, !is.na(lx), lx > 0)
  
  if (nrow(df) == 0) return(NA_real_)
  
  df <- df %>%
    mutate(lx1 = lx / first(lx))
  
  below <- df %>% filter(lx1 <= 0.5) %>% slice_min(order_by = age, n = 1)
  above <- df %>% filter(lx1 > 0.5)  %>% slice_max(order_by = age, n = 1)
  
  if (nrow(below) == 0 || nrow(above) == 0) return(NA_real_)
  
  ageLow  <- above$age
  ageHigh <- below$age
  lxLow   <- above$lx1
  lxHigh  <- below$lx1
  
  ageMLE1 <- ageLow + ((0.5 - lxLow) / (lxHigh - lxLow)) * (ageHigh - ageLow)
  return(ageMLE1)
}

lambda_5 <- function(Nx_vec, Year_vec) {
  sapply(seq_along(Year_vec), function(i) {
    y       <- Year_vec[i]
    current <- Nx_vec[i]
    
    past_value <- Nx_vec[which(Year_vec == (y - 5))]
    
    if (length(past_value) > 0 && !is.na(past_value) && past_value > 0) {
      (current / past_value)^(1/5)
    } else {
      NA_real_
    }
  })
}

build_leslie <- function(df, CohortStart, CohortEnd, Sex) {
  
  df <- filter(df, Cohort >= CohortStart & Cohort <= CohortEnd & Sex == Sex)  %>%
    group_by(Age) %>%
    summarise(
      mx = mean(mx, na.rm = TRUE),
      Px = mean(Px, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(Age)
  
  fecundity <- pull(df, mx)
  survival  <- pull(df, Px)
  n         <- length(fecundity)
  L         <- matrix(0, nrow = n, ncol = n)
  L[1, ]    <- fecundity
  if (n > 1) {
    for (i in 2:n) {
      L[i, i - 1] <- survival[i - 1]
    }
  }
  return(L)
}


