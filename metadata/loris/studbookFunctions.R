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

check_loc_dates <- function(df) {
 df %>%
   mutate(check = if_else(
    between(Date, StartLoc, EndLoc) |
      is.na(StartLoc) & Date < EndLoc |
      is.na(EndLoc) & Date > StartLoc |
      (is.na(StartLoc) & is.na(EndLoc)) | is.na(Date),
    "keep",
    "discard"
  )) %>%
   filter(check == "keep") %>%
   select(-check)
 
 return(df)
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
    mutate(Date      = case_when(Status == "A" ~ today(), 
                                 is.na(Date) & ID %in% deaths.21_24 ~ ymd("2023-1-1"),
                                 is.na(Date) & ID %in% deaths.24    ~ ymd("2025-1-1"),
                                 .default = Date),
           TypeEvent = "End") %>% 
    mutate(EndLoc    = Date) %>% 
    distinct()
  
  sires <- studbook %>%
    filter(TypeEvent == "Birth" & !is.na(Sire)) %>%
    select(ID      = Sire,
           OffspID = ID,
           Location,
           Date)
  
  parents <- studbook %>%
    filter(TypeEvent == "Birth" & !is.na(Dam)) %>%
    select(ID      = Dam,
           OffspID = ID,
           Location,
           Date) %>%
    bind_rows(sires)
  
  births <- studbook %>%
    select(ID,
           OrderLoc,
           Location,
           StartLoc,
           EndLoc) %>%
    right_join(parents, by = join_by(ID, Location)) %>%
    check_loc_dates() %>%
    mutate(TypeEvent = "Breed") %>%
    select(-OffspID) %>% 
    distinct() %>% 
    arrange(ID, 
            OrderLoc,
            Date)
  
  studbook %>%
    select(
      ID,
      OrderLoc,
      Location,
      TypeEvent,
      StartLoc,
      Date,
      EndLoc) %>% 
    filter(TypeEvent != "End") %>%
    bind_rows(births) %>% 
    bind_rows(select(end.records, -Status)) %>%
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
    distinct()
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



revise_studbook <- function(studbook, timeline) {
  deceased <- filter(studbook, Status == "D") %>% pull(ID) %>% unique()
  
  birth.dates <- filter(timeline, TypeEvent == "Birth") %>%
    select(ID,
           LocBirth  = Location,
           DateBirth = Date) %>%
    mutate(BirthYear = year(DateBirth))
  
  deaths   <- filter(timeline, TypeEvent == "End" & ID %in% deceased) %>%
    select(ID,
           LocDeath  = Location,
           DateDeath = Date) 
  
  studbook.revised <- studbook %>%
    select(
      ID,
      Status,
      Sex,
      Sire,
      Dam
    ) %>%
    distinct() %>%
    left_join(deaths, by = join_by(ID)) %>%
    left_join(birth.dates, by = join_by(ID)) %>%
    mutate(AgeDeath = calculate_age(DateBirth, DateDeath))
  
  return(studbook.revised)
}


expand_timeline <- function(timeline, period, studbook) {
  timeline <- timeline %>%
    select(ID,
           Location,
           StartLoc,
           EndLoc) %>%
    distinct()
  if (period == "months") {
    timeline.long <- timeline %>% 
      mutate(StartLoc = floor_date(StartLoc, "months"),
             EndLoc   = ceiling_date(EndLoc, "months")) %>%
      mutate(Months = pmap(list(StartLoc, EndLoc), \(x, y) seq(x, y, by = "months"))) %>%
      unnest(Months) %>%
      select(ID, 
             Date = Months, 
             Location) %>%
      left_join(select(
        studbook,
        ID,
        Sex,
        DateBirth
      ), by = join_by(ID)) %>%
      mutate(BirthYear = year(DateBirth),
             Age    = calculate_age(DateBirth, Date)) %>%
      select(ID,
             Sex,
             BirthYear,
             Date,
             Age,
             Location)
      
  } else if (period == "years") {
    timeline.new <- timeline %>% 
      mutate(StartLoc = floor_date(StartLoc, "years"),
             EndLoc   = ceiling_date(EndLoc, "years")) %>%
      mutate(Years = pmap(list(StartLoc, EndLoc), \(x, y) seq(x, y, by = "years"))) %>%
      unnest(Years) %>%
      select(ID, 
             Date = Years, 
             Location) %>%
      left_join(select(
        studbook,
        ID,
        Sex,
        DateBirth
      ), by = join_by(ID)) %>%
      mutate(BirthYear = year(DateBirth),
             Age    = calculate_age(floor_date(DateBirth, "years"), Date)) %>%
      select(ID,
             Sex,
             BirthYear,
             Date,
             Age,
             Location)
  }
  
  return(timeline.long)

}

census <- function(timeline, studbook, period) {
  
  if (period == "years") {
    counts <- timeline %>%
      filter(TypeEvent %in% c("Birth", "End")) %>%
      distinct(ID, Date, TypeEvent) %>%
      pivot_wider(id_cols     = "ID",
                  names_from  = "TypeEvent",
                  values_from = "Date") %>%
      distinct() %>%
      mutate(Birth = floor_date(Birth, "years"),
             End   = ceiling_date(End, "years")) %>%
      mutate(Dates = pmap(list(Birth, End), \(x, y) seq(x, y, by = "year"))) %>%
      unnest(Dates) %>%
      select(ID, Date = Dates) %>%
      mutate(Age = row_number() - 1, .by = ID) %>%
      left_join(select(studbook,
                       ID,
                       Sex), by = join_by(ID)) %>%
      summarize(N = n(), .by = c(Sex, Date)) %>%
      distinct() %>%
      arrange(Date)
  } else if (period == "months") {
    counts <- timeline %>%
      filter(TypeEvent %in% c("Birth", "End")) %>%
      distinct(ID, Date, TypeEvent) %>%
      pivot_wider(id_cols     = "ID",
                  names_from  = "TypeEvent",
                  values_from = "Date") %>%
      distinct() %>%
      mutate(Birth = floor_date(Birth, "months"),
             End   = ceiling_date(End, "months")) %>%
      mutate(Dates = pmap(list(Birth, End), \(x, y) seq(x, y, by = "month"))) %>%
      unnest(Dates) %>%
      select(ID, Date = Dates) %>%
      mutate(Age = row_number() - 1, .by = ID) %>%
      left_join(select(studbook,
                       ID,
                       Sex), by = join_by(ID)) %>%
      summarize(N = n(), .by = c(Sex, Date)) %>%
      distinct() %>%
      arrange(Date)
  }
  
  census <- counts %>% 
    pivot_wider(id_cols     = "Date",
                names_from  = "Sex",
                values_from = "N") %>%
    rename(Females      = F,
           Males        = M,
           Unidentified = U) %>%
    mutate(across(where(is.numeric), ~ replace_na(., 0)))
  
  return(census)
  
}

nest_timeline <- function(timeline, groupBy = NULL) {
  
  if (is.null(groupBy)) {
  nested <-  timeline %>%
      group_by(Date) %>%
      summarise(Individuals = list(tibble(ID, Sex, BirthYear, Age)),
                .groups = "drop") %>%
      split(.$Date)
    
  } else if (groupBy == "Location") {
  nested <-  timeline %>%
      group_by(Date, Location) %>%
      summarize(Individuals = list(tibble(ID, Sex, BirthYear, Age))) %>%
      group_by(Date) %>%
      summarize(Locations = split(Individuals, Location)) %>%
      split(.$Date) %>%
      map(~ .x$Locations)
  } else if (groupBy == "Age") {
    nested <-  timeline %>%
      group_by(Date, Age) %>%
      summarize(Individuals = list(tibble(ID, Sex, BirthYear, Location))) %>%
      group_by(Date) %>%
      summarize(Ages = split(Individuals, Age)) %>%
      split(.$Date) %>%
      map(~ .x$Ages)
  } else if (groupBy == "Sex") {
    nested <-  timeline %>%
      group_by(Date, Sex) %>%
      summarize(Individuals = list(tibble(ID, Age, BirthYear, Location))) %>%
      group_by(Date) %>%
      summarize(Sexes = split(Individuals, Sex)) %>%
      split(.$Date) %>%
      map(~ .x$Sexes)
  } else if (groupBy == "AgeSex") {
    nested <-  timeline %>%
      group_by(Date, Sex, Age) %>%
      summarize(Individuals = list(tibble(ID, BirthYear, Location))) %>%
      group_by(Date) %>%
      summarize(Classes = split(Individuals, Sex, Age)) %>%
      split(.$Date) %>%
      map(~ .x$Classes)
  }
  
  return(nested)

}



inspect <- function(df, studbook) {
  left_join(df, studbook, by = join_by(ID, BirthYear, Sex))
}


whoisthedaddy <- function(studbook, list) {
  subjects <- studbook %>%
  filter(((is.na(Sire) & !is.na(Dam)) | (Status == "A" & is.na(Sire)))) %>%
    mutate(Date = floor_date(DateBirth, unit = "months")) %>%
    relocate(Sire, Dam, .after = LocBirth)
  
  search <- as.list(deframe(select(subjects, Date, LocBirth)))
  
  imap(search, \(x, idx) pluck(list, idx, x, 1)) %>%
    set_names(., as.list(deframe(select(subjects, ID)))) %>%
    map_depth(., 1, \(x) filter(x, Sex == "M" & !(ID %in% c(names)))) %>%
    map_depth(., 1, \(x) arrange(x, desc(Age))) %>%
    map(., \(x) inspect(x, studbook))
}

whoisthemommy <- function(studbook, list) {
  subjects <- studbook %>%
    filter(((!is.na(Sire) & is.na(Dam)) | (Status == "A" & is.na(Dam)))) %>%
    mutate(Date = floor_date(DateBirth, unit = "months")) %>%
    relocate(Sire, Dam, .after = LocBirth)
  
  search <- as.list(deframe(select(subjects, Date, LocBirth)))
  
  imap(search, \(x, idx) pluck(list, idx, x, 1)) %>%
    set_names(., as.list(deframe(select(subjects, ID)))) %>%
    map_depth(., 1, \(x) filter(x, Sex == "F" & !(ID %in% c(names)))) %>%
    map_depth(., 1, \(x) arrange(x, desc(Age))) %>%
    map(., \(x) inspect(x, studbook))
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
    filter(ID %in% ids) %>%
    mutate(ID        = min(ID) + add,
           DateBirth = min(DateBirth - years(2)),
           DateDeath = max(DateBirth + years(2)),
           LocDeath  = LocBirth,
           AgeDeath  = calculate_age(DateBirth, DateDeath),
           Status    = "H",
           Sex       = sex.add,
           BirthYear = year(DateBirth),
           Sire      = NA,
           Dam       = NA) %>%
    distinct() %>%
    bind_rows(slice_head(., n = 1)) %>%
    distinct()
  
  if (sex.add == "M") {
    
    studbook %>% bind_rows(hypotheticals) %>%
      mutate(Sire = if_else(ID %in% c(ids), hypSire, Sire))
    
  } else if (sex.add == "F") {
    
    studbook %>% bind_rows(hypotheticals) %>%
      mutate(Dam = if_else(ID %in% c(ids), hypDam, Dam))
    
  }

}

update_timeline <- function(timeline, studbook.old, studbook.new) {
  new  <- setdiff(studbook.new, studbook.old)
  edit <- filter(new, !is.na(Sire) & !is.na(Dam))
  
  edit.sires <- select(edit,
                       ID       = Sire,
                       Date     = DateBirth,
                       Location = LocBirth) %>%
    mutate(TypeEvent = "Breed")
  
  edit.dams <- select(edit,
                       ID       = Dam,
                       Date     = DateBirth,
                       Location = LocBirth) %>%
    mutate(TypeEvent = "Breed")
  
  add  <- filter(new, is.na(Sire) & is.na(Dam))
  
  add.births <- add %>% 
    select(ID, 
           Location = LocBirth, 
           Date     = DateBirth,
           EndLoc   = DateDeath) %>%
    mutate(OrderLoc = 1,
           StartLoc = Date,
           TypeEvent = "Birth")
  add.deaths <- add %>% 
    select(ID, 
           Location = LocDeath, 
           Date     = DateDeath,
           StartLoc = DateBirth) %>%
    mutate(OrderLoc  = 1,
           EndLoc    = Date,
           TypeEvent = "End")
  
  new.timeline <- bind_rows(edit.sires, 
                            edit.dams, 
                            add.births, 
                            add.deaths,
                            timeline) %>%
    arrange(ID, Date) %>%
    group_by(ID, Location) %>%
    fill(StartLoc, OrderLoc, EndLoc) %>%
    ungroup() %>%
    arrange(ID, OrderLoc, Date) %>%
    distinct() %>%
    select(
      ID,
      OrderLoc,
      Location,
      StartLoc,
      TypeEvent,
      Date,
      EndLoc
    )
  
  write.table(new.timeline, here(path$AZAstudbooks$timeline), sep = "\t", row.names = F)
  
  return(new.timeline)
  
}

my_lambda <- function(years, df) {
  N_final   <- filter(df, Date == floor_date(today(), "year")) %>% pull(Nx)
  N_initial <- filter(df, Date == (floor_date(today(), "year") - years(years))) %>% pull(Nx)
  
  return((N_final / N_initial)^(1/years))
}



MLE_age1 <- function(lx, age) {
  df <- tibble(lx = lx, age = age) %>%
    filter(age >= 1, !is.na(lx), lx > 0)
  
  if (nrow(df) == 0) return(0)
  
  lx1_start <- df$lx[1]
  df <- df %>% mutate(lx1 = lx / lx1_start)
  
  below <- df %>% filter(lx1 <= 0.5) %>% slice_min(order_by = age, n = 1, with_ties = FALSE)
  above <- df %>% filter(lx1 >  0.5) %>% slice_max(order_by = age, n = 1, with_ties = FALSE)
  
  if (nrow(below) == 0 || nrow(above) == 0) return(0)
  
  ageLow  <- above$age[1]
  ageHigh <- below$age[1]
  lxLow   <- above$lx1[1]
  lxHigh  <- below$lx1[1]
  
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

build_leslie_vec <- function(mx, Px) {
  n <- length(mx)
  L <- matrix(0, n, n)
  L[1, ] <- mx
  if (n > 1) {
    for (i in 2:n) {
      L[i, i - 1] <- Px[i - 1]
    }
  }
  return(L)
}

build_leslie <- function(df, BirthYrStart, BirthYrEnd, sex) {
  
  df <- df %>% filter(BirthYear >= BirthYrStart & Cohort <= BirthYrEnd & Sex == sex)  %>%
    group_by(Age) %>%
    summarise(
      Mx = mean(Mx, na.rm = TRUE),
      Px = mean(Px, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(Age)
  
  fecundity <- pull(df, Mx)
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

make_cohorts <- function(df, minYear, maxYear, span, maxAge, include_sex = TRUE) {
  N_letters <- (maxYear - minYear + 1)/span
  
  cohorts <- expand_grid(
    Age       = 0:maxAge, 
    Sex       = c("M", "F"), 
    BirthYear = 1985:2024) %>%
    full_join(tibble(BirthYear   = minYear:maxYear, 
                     BirthCohort = rep(LETTERS[1:N_letters], each = span)), 
              by = join_by(BirthYear)) %>%
    mutate(CohortMin = min(BirthYear),
           CohortMax = max(BirthYear), .by = BirthCohort) %>%
    mutate(CohortLabel = case_when(
      CohortMin <= 2013 & CohortMax >= 2013 ~ as.character(str_glue("{CohortMin}", "-", "{CohortMax}", "\n(Culi)")),
      CohortMin <= 2017 & CohortMax >= 2013  ~ as.character(str_glue("{CohortMin}", "-", "{CohortMax}", "\n(Warble)")),
             .default = as.character(str_glue("{CohortMin}", "-", "{CohortMax}"))), 
           .keep = "unused") %>%
    select(CohortLabel, BirthCohort, BirthYear, Sex, Age) %>%
    arrange(BirthCohort, BirthYear, Sex, Age) %>%
    filter(BirthYear + Age <= 2025)
  
  if (include_sex == TRUE) {
    df <- df %>%
      right_join(cohorts, by = join_by(BirthYear, Sex, Age)) %>%
      mutate(Cohort = as.character(str_glue("{Sex}", "{BirthCohort}")),
             across(where(is.numeric), ~ replace_na(., 0)))
  } else if (include_sex == FALSE) {
    df <- df %>%
      right_join(distinct(cohorts,
                          Age,
                          BirthYear,
                          BirthCohort,
                          CohortLabel), by = join_by(BirthYear, Age)) %>%
      mutate(Cohort = BirthCohort,
             across(where(is.numeric), ~ replace_na(., 0)),
             Sex = "Total") %>%
      relocate(CohortLabel, BirthCohort, Sex, Age)
  }
  
  return(df)
}

count_births <- function(timeline, studbook) {
  births <- timeline %>%
    filter(TypeEvent == "Breed") %>%
    select(ID, Date) %>% distinct() %>%
    mutate(Year = year(Date)) %>%
    summarize(Births = n(), .by = c(ID, Year)) %>%
    ungroup()
  
  counts <- studbook %>%
    select(
      ID,
      Sex,
      BirthYear,
      Start     = DateBirth,
      End       = DateDeath
    ) %>%
    filter(Sex != "U") %>%
    mutate(Start = floor_date(Start, "years"),
           End   = if_else(!is.na(End), 
                           floor_date(End, "years"), 
                           floor_date(today(), "years"))
    ) %>%
    mutate(Years = pmap(list(Start, End), \(x, y) seq(x, y, by = "years"))) %>%
    unnest(Years) %>%
    mutate(Year = year(Years),
           Age  = calculate_age(Start, Years)) %>%
    select(ID,
           Sex,
           BirthYear,
           Age,
           Year) %>%
    left_join(births, by = join_by(ID, Year)) %>%
    mutate(Births = replace_na(Births, 0)) %>% 
    distinct() %>%
    select(ID,
           BirthYear,
           Sex,
           Age,
           Births)
}

lifeTab <- function(df) {
  
  life.table <- df %>% arrange(Cohort, Age) %>%
    mutate(across(c(Births, Nx), ~ replace_na(., 0)),
           RiskQx = Nx,
           RiskMx = Nx) %>%
    group_by(Cohort) %>%
    mutate(Deaths = if_else(Age == max(Age), Nx, Nx - lead(Nx)),
           N0     = if_else(Age == 0, Nx, NA),
           N1     = if_else(Age == 1, Nx, NA),
           Px     = if_else(Nx == 0 | Age == max(Age), 0, lead(Nx)/Nx)) %>%
    fill(N0, N1, .direction = "downup")  %>%
    ungroup() %>%
    mutate(Lx1 = if_else(    N1 == 0, 0, Nx/N1),
           Lx  = if_else(    N0 == 0, 0, Nx/N0),
           Qx  = if_else(RiskQx == 0, 0, Deaths/RiskQx),
           Mx  = if_else(RiskMx == 0, 0, (Births/RiskMx)/2)) %>%
    mutate(Qx1 = if_else(Age == 0, Qx, NA),
           Fx  = Mx * Lx) %>%
    mutate(numT  = Age * Fx) %>%
    group_by(Cohort) %>%
    mutate(MLE        = if_else(max(N1) < 1, 0, MLE_age1(Lx1, Age)),
           FirstRepro = min(Age[Mx>0]),
           LastRepro  = max(Age[Mx>0]),
           MaxLongev  = max(Age[Deaths>0])
    ) %>%
    mutate(R0   = sum(Fx), 
           Tnum = sum(numT),
           .keep = "unused") %>%
    fill(Qx1) %>%
    ungroup() %>%
    mutate(T = if_else(R0 > 0, Tnum/R0, 0)) %>%
    mutate(lambda = if_else(R0 > 0 & T > 0, R0^(1/T), 0)) %>%
    select(-Tnum) %>%
    relocate(
      RiskQx,
      RiskMx,
      N0,
      Nx,
      Px,
      Lx,
      Lx1,
      Qx,
      Qx1,
      Mx,
      R0,
      T,
      MLE,
      lambda,
      FirstRepro,
      LastRepro,
      MaxLongev,
      .after = Age
    )
  
  return(life.table)
}

annotate_lambda <- function(df, lambda = lambda) {
  df <- df %>%
    mutate(hover_lambda = abs(round((lambda - 1)*100, digits = 1))) %>%
    mutate(
      hover_lambda = if_else(
        lambda >= 1,
        as.character(str_glue("Population growing by ", "{hover_lambda}", "%")),
        as.character(str_glue("Population declining by ", "{hover_lambda}", "%"))
      )
    )
  
  return(df)
}

lifeTab_static <- function(df) {
  df <- df  %>%
    mutate(across(c(Px:lambda), ~ round(., digits = 3))) %>%
    select(
      Cohort,
      CohortLabel,
      Sex,
      N0,
      Qx1,
      R0,
      T,
      MLE,
      lambda,
      FirstRepro,
      LastRepro,
      MaxLongev
    ) %>%
    filter(N0 > 0) %>%
    distinct() %>%
    arrange(Cohort, Sex)
    
  return(df)
}

hline <- function(y = 0, color = "#444444") {
  list(
    type = "line",
    x0   = 0,
    x1   = 1,
    xref = "paper",
    y0   = y,
    y1   = y,
    line = list(color   = color, 
                width   = 0.9,
                dash    = "dot")
  )
}


lambda.annotation <-  list(
  x          = 0.9,
  xref       = "paper",
  y          = 1,
  yref       = "y",
  text       = "Stable if \u03BB = 1.0",
  showarrow  = TRUE,
  arrowhead  = 4,
  arrowcolor = "#444444",
  arrowsize  = 1,
  arrowwidth = 0.7,
  ax         = 10,
  ay         = 40,
  font       = list(color  = "#444444",
                    size   = 12,
                    style  = "italic")
)


lighten_palette <- function(palette, hex) {
  if (is.list(palette)) {
    new <-  map_depth(palette, 1, \(x) gsub("FF", hex, x))
  } else {
    new <- gsub("FF", hex, palette)
  }
  
  return(new)

}

living <- function(studbook) {
  ids  <- filter(studbook, Status == "Alive" | Status == "A") %>% pull(ID) %>% unique()
  return(ids)
}

living.males <- function(studbook) {
  ids  <- filter(studbook, Status == "Alive" & (Sex == "M" | Sex == "Male")) %>% 
    pull(ID) %>% unique()
  return(ids)
}

living.females <- function(studbook) {
  ids  <- filter(studbook, Status == "Alive" & (Sex == "F" | Sex == "Female")) %>% 
    pull(ID) %>% unique()
  return(ids)
}

studbook_visual <- function(timeline, studbook, location.key) {
  end.records <- filter(timeline, TypeEvent == "End") %>%
    select(ID, LocLast = Location)
  
  locations <- location.key %>%
    mutate(Label = str_glue("{NameLoc}", ", ", "{Country}")) %>%
    select(LocAbbrev,
           Label,
           colorLoc,
           iconLoc)
  
  studbook.new <- studbook %>% left_join(end.records, by = join_by(ID)) %>%
    mutate(DateLast = if_else(is.na(DateDeath), today(), DateDeath),
           AgeLast  = if_else(is.na(AgeDeath), 
                              calculate_age(DateBirth, today()), 
                              AgeDeath),
           Status      = case_match(Status,
                                 "D" ~ "Deceased",
                                 "A" ~ "Alive",
                                 "H" ~ "Hypothetical Parent"),
           color       = case_match(Sex,
                                 "F" ~ colors$f,
                                 "M" ~ colors$m,
                                 "U" ~ colors$u),
           sex_ped     = case_match(Sex,
                                 "F" ~ 2,
                                 "M" ~ 1,
                                 "U" ~ 0),
           sex_kinship = case_match(Sex,
                                   "F" ~ 2,
                                   "M" ~ 1,
                                   "U" ~ 3)) %>%
    mutate(YearLast = year(DateLast), .keep = "unused") %>%
    left_join(locations, by = join_by(LocBirth == LocAbbrev)) %>%
    rename(
      LocBirth_name      = Label,
      LocBirth_color     = colorLoc,
      LocBirth_icon      = iconLoc
    ) %>%
    left_join(locations, by = join_by(LocLast == LocAbbrev)) %>%
    rename(
      LocLast_name      = Label,
      LocLast_color     = colorLoc,
      LocLast_icon      = iconLoc
    ) %>%
    arrange(Status, DateBirth, LocBirth) %>%
    select(
      Status,
      ID,
      LocBirth      ,
      Sire          ,
      Dam           ,
      LocLast       ,
      AgeLast       ,
      Sex           ,
      color         ,
      DateBirth     ,
      BirthYear     ,
      DateDeath     ,
      YearLast      ,
      LocBirth_icon ,
      LocLast_icon  ,
      LocLast_color ,
      LocBirth_name ,
      LocBirth_color,
      LocLast_name  ,
      sex_ped       ,
      sex_kinship
    )  %>%
    mutate(MonthBirth = month(DateBirth, label = TRUE, abbr = TRUE),
           YearDeath  = if_else(Status == "Alive", Status, as.character(YearLast))) 
  
  write.table(studbook.new, here(path$AZAstudbooks$reactable_ready), sep = "\t", row.names = F)  
  
  return(studbook.new)
}

locations.color <- function(x) {
  list_assign(x, color = "#444444")
}

pedigree_levels <- function(pedigree, studbook) {
  studbook <- studbook %>%
    mutate(across(c(Sire, Dam), ~ as.character(.)))
  
  levels <- as.list(gen_numbers(pedigree)) %>%
    set_names(pedigree$ID) %>%
    enframe(name = "id", value = "level") %>%
    mutate(level = as.integer(level)) %>%
    mutate(level = if_else(id %in% founders(pedigree), level, level + 2)) %>%
    mutate(level = if_else(id %in% leaves(pedigree), level + 1, level)) %>% 
    distinct()
  
  return(levels)
}

pedigree_pairs   <- function(pedigree, studbook) {
  studbook <- studbook %>%
    mutate(across(c(Sire, Dam), ~ as.character(.)))
  
  pairs <- map(as.list(nonfounders(pedigree)), \(x) as.list(parents(pedigree, id = x))) %>%
    enframe(name = NULL) %>%
    unnest_wider(value, names_sep = "_") %>%
    distinct() %>%
    mutate(pid = paste0("0", as.character(row_number()))) %>%
    left_join(studbook, by = join_by(value_1 == Sire, value_2 == Dam)) %>%
    select(pid, 
           fid = value_1, 
           mid = value_2, 
           color = LocBirth_color, 
           label = LocBirth) %>% 
    distinct() %>% 
    group_by(pid) %>% 
    summarize(across(everything(), ~ dplyr::first(.x)), .groups = "drop") %>% 
    ungroup()
  
  return(pairs)
  
}

pedigree_edges  <- function(pedigree, studbook) {
  pairs  <- pedigree_pairs(pedigree, studbook)
  sire   <- pull(pairs, fid, pid) %>% as.list()
  dam    <- pull(pairs, mid, pid) %>% as.list()
  
  pairs.edf <- pairs  %>%
    pivot_longer(cols = c("fid", "mid"), values_to = "from", names_to = NULL) %>%
    select(from, to = pid, color) %>%
    mutate(dashes = TRUE, 
           shadow = FALSE,
           width  = 0.5,
           arrows = "to") %>% distinct()
  
  combined         <- c(sire, dam)
  combined_grouped <- split(combined, names(combined)) %>% map(unlist)
  
  offspring.edf <- combined_grouped %>%
    imap(function(x, idx) {
      list(idx, as.list(commonDescendants(pedigree, ids = x, maxGen = 2)))
    }) %>%
    enframe(name = NULL, value = "id") %>%
    unnest_wider(id, names_sep = "_") %>%
    unnest_longer(id_2, values_to = "id") %>%
    select(from = id_1, to = id) %>%
    arrange(from, to) %>%
    distinct() %>%
    left_join(select(pairs, from = pid, color), by = join_by(from)) %>%
    mutate(dashes = FALSE, 
           shadow = TRUE,
           width  = 0.7,
           arrows = NULL) %>%
    distinct()
  
  edges <- bind_rows(pairs.edf, offspring.edf) %>%
    distinct()
  
  return(edges)
}

pedigree_nodes  <- function(pedigree, studbook) {
  levels    <- pedigree_levels(pedigree, studbook)
  pairs.ndf <- pedigree_pairs(pedigree, studbook)  %>%
    pivot_longer(cols = c("fid", "mid"), values_to = "mateID", names_to = NULL) %>%
    left_join(levels, by = join_by(mateID == id)) %>%
    distinct() %>%
    group_by(pid, color, label) %>%
    mutate(level = sum(max(level), 1)) %>% ungroup() %>% 
    group_by(pid) %>% 
    summarize(across(everything(), ~ dplyr::first(.x)), .groups = "drop") %>% 
    ungroup() %>%
    mutate(
      group = "pair",
      shape = "icon",
      icon  = map(color, \(x) list(code = "\uf068", color = x, face = "'Font Awesome 5 Free'", weight = 700, size = 35)),
      font  = map(color, \(x) list(size = 18, color = x, strokeColor = "#FFFFFF", strokeWidth = 3))
    ) %>%
    select(id = pid, level, group, shape, icon, label, font) %>%
    distinct()
  
  nodes <- as.data.frame(pedigree) %>% 
    mutate(id    = as.character(id),
           group = as.character(sex)) %>% 
    select(id, group) %>%
    distinct() %>% 
    group_by(id) %>% 
    summarize(across(everything(), ~ dplyr::first(.x)), .groups = "drop") %>% 
    ungroup() %>% 
    left_join(levels, by = join_by(id)) %>%
    distinct() %>% 
    group_by(id) %>% 
    summarize(across(everything(), ~ dplyr::first(.x)), .groups = "drop") %>% 
    ungroup() %>% 
    mutate(color = case_when(
      group == "0" ~ colors$u,
      group == "1" ~ colors$m,
      group == "2" ~ colors$f),
      code = case_when(
        group == "0" ~ "\uf04b",
        group == "1" ~ "\uf0c8",
        group == "2" ~ "\uf111"),
      shape = "icon",
      label = id
    ) %>% 
    mutate(icon = map2(color, code, \(x, y) list(code = y, 
                                                 color = x, 
                                                 face = "'Font Awesome 5 Free'",
                                                 weight = 700, 
                                                 size = 30)),
           font = map(color, \(x) list(size = 12, 
                                       color = x, 
                                       strokeColor = "#FFFFFF", 
                                       strokeWidth = 3))
    ) %>% 
    select(id, level, group, shape, icon, label, font) %>% 
    bind_rows(pairs.ndf) %>% 
    distinct(id, .keep_all = TRUE) %>% 
    ungroup()

  return(nodes)
}


ped_network <- function(pedigree, studbook) {
  edges <- pedigree_edges(pedigree, studbook) %>% distinct()
  nodes <- pedigree_nodes(pedigree, studbook) %>% distinct()
  
  legend <- nodes %>%
    select(group, shape, icon) %>%
    distinct() %>%
    mutate(icon = if_else(group %in% c("0", "1", "2"), icon, map(icon, \(x) locations.color(x)))) %>%
    mutate(label = case_when(
      group == "0" ~ "Sex Undet",
      group == "1" ~ "Male",
      group == "2" ~ "Female",
      .default = "Pair colored/labeled by Location"
    )) %>% distinct()
  
  ped.net <- visNetwork(nodes, edges, width = "100%", height = "700px") %>% 
    addFontAwesome(version = "5.13.0") %>%
    visNodes(shadow = TRUE, fixed  = list(x = FALSE, y = FALSE))  %>%
    visInteraction(dragNodes = TRUE, dragView = TRUE) %>%
    visLegend(addNodes = legend, ncol = 1, useGroups = FALSE)
  
  visSave(ped.net, file = paste0("Pedigree_Hierarchical_", params$group, ".html"))
  
  return(ped.net)
}

founder_reps <- function(pedigree, pedigree.living, studbook) {
  founder.descendants <- map(as.list(founders(pedigree)), \(x) as.list(descendants(pedigree, x, inclusive = TRUE))) %>%
    compact()
  names(founder.descendants) <- map(founder.descendants, \(x) x[[1]])
  
  founderReps <- keep(founder.descendants, function(sublist) {
  flat        <- unlist(sublist)
    any(flat %in% living(studbook))
  }) %>% list_flatten(name_spec = "") %>% unique() %>%
    intersect(founders(pedigree.living))
  return(founderReps)
}

founder_contributions <- function(studbook, pedigree.living) {
  living         <- living(studbook)
  dp             <- descentPaths(pedigree.living)
  
  founderContribution <- map_dbl(founders(pedigree.living), function(f) {
    paths_list <- dp[[f]]
    valid_paths <- keep(paths_list, ~ tail(.x, 1) %in% living)
    sum(map_dbl(valid_paths, ~ 0.5^(length(.x) - 1)))
  })
  names(founderContribution) <- founders(pedigree.living)
  
  return(founderContribution)
}

rel_founder_contributions <- function(studbook, pedigree.living) {
  founderContribution <- founder_contributions(studbook, pedigree.living)
  result              <- founderContribution / sum(founderContribution)
  
  return(result)
}

founder_summary <- function(pedigree, pedigree.living, studbook) {
  founderContribution <- founder_contributions(studbook, pedigree.living)
  living              <- living(studbook)
  founderReps         <- founder_reps(pedigree, pedigree.living, studbook)
  
  n_founder_reps <- length(founderReps)
  
  founderRepsIDs <- founderReps %>% list_c()
  
  dp <- descentPaths(pedigree.living)
  
  p     <- founder_contributions(studbook, pedigree.living)
  p.tbl <- enframe(p, 
                   name  = "ID", 
                   value = "Rel_Contribution") %>%
    mutate(ID = as.integer(ID))
  
  founder.summary <- enframe(founderContribution, 
                             name = "ID", 
                             value = "Contribution")  %>%
    mutate(ID = as.integer(ID)) %>%
    left_join(p.tbl) %>%
    left_join(studbook) %>%
    select(Status           ,
           ID               ,
           LocBirth         ,
           AgeDeath = AgeLast ,
           LocLast          ,
           Sire             ,
           Dam              ,
           Rel_Contribution ,
           DateDeath        ,
           DateBirth        ,
           Sex              ,
           color            ,
           BirthYear        ,
           MonthBirth       ,
           YearLast         ,
           YearDeath        ,
           LocBirth_icon    ,
           LocBirth_color   ,
           LocLast_icon     ,
           LocLast_color    ,
           LocBirth_name    ,
           LocLast_name     ,
           sex_ped          ,
           sex_kinship      
    )  %>%
    arrange(Status, LocLast, desc(Rel_Contribution))
  
  return(founder.summary)
}

gen_numbers <- function(pedigree.living) {
  gen_numbers <- generations(pedigree.living, what = "indiv")
  if (length(gen_numbers) == 0) {
    warning("No generation numbers returned using what = 'indiv'. Trying what = 'depth' instead.")
    gen_numbers <- generations(pedigree.living, what = "depth")
  }
  
  return(gen_numbers)
}


gen_numbers_living <- function(pedigree.living, studbook) {
  living      <- living(studbook)
  gens        <- gen_numbers(pedigree.living)
  living_gen  <- gens[names(gens) %in% living]
  return(living_gen)
}


kin_matrix_living <- function(pedigree.living, studbook) {
  living         <- living(studbook)
  kinship.ped    <- kinship(pedigree.living)
  living.ped     <- intersect(living, rownames(kinship.ped))
  living.kinship <- kinship.ped[living.ped, living.ped]
}

family_history <- function(pedigree.living, studbook) {
  generations <- enframe(gen_numbers(pedigree.living), 
                         name  = "ID", 
                         value = "Generations") %>%
    mutate(ID = as.integer(ID))
  
  inbred.df <- inbreeding(pedigree.living) %>% 
    enframe(name = "ID", value = "inbred")
  
  kin.matrix <- kin_matrix_living(pedigree.living, studbook)
  
  inbred.df <- inbred.df  %>%
    mutate(ID = as.integer(ID))
  
  ancestors <- as.list(rownames(kin.matrix)) %>%
    set_names(map(., \(x) x)) %>%
    map(., \(x) as.list(ancestors(pedigree.living, x))) %>%
    enframe(name = "ID", value = "Ancestors") %>%
    mutate(ID = as.integer(ID))
  
  children <- as.list(rownames(kin.matrix)) %>%
    set_names(map(., \(x) x)) %>%
    map(., \(x) as.list(children(pedigree.living, x))) %>%
    enframe(name = "ID", value = "Children") %>%
    mutate(ID = as.integer(ID))
  
  descendants <- as.list(rownames(kin.matrix)) %>%
    set_names(map(., \(x) x)) %>%
    map(., \(x) as.list(descendants(pedigree.living, x))) %>%
    enframe(name = "ID", value = "Descendants") %>%
    mutate(ID = as.integer(ID))
  
  siblings <- as.list(rownames(kin.matrix)) %>%
    set_names(map(., \(x) x)) %>%
    map(., \(x) as.list(siblings(pedigree.living, x))) %>%
    enframe(name = "ID", value = "Siblings") %>%
    mutate(ID = as.integer(ID))
  
  df <- ancestors %>% 
    left_join(descendants) %>% 
    left_join(children) %>% 
    left_join(siblings) %>%
    left_join(generations) %>%
    rowwise() %>%
    mutate(N_Children    = length(Children),
           N_Descendants = length(Descendants),
           N_Siblings    = length(Siblings),
           N_Ancestors   = length(Ancestors)) %>%
    select(ID, starts_with("N_")) %>%
    left_join(inbred.df) %>%
    left_join(studbook) %>%
    select(
           LocCurrent = LocLast,
           ID               ,
           LocBirth         ,
           Age = AgeLast    ,
           Sire             ,
           Dam              ,
           DateBirth        ,
           Sex              ,
           color            ,
           BirthYear        ,
           MonthBirth       ,
           YearLast         ,
           LocBirth_icon    ,
           LocBirth_color   ,
           LocLast_icon     ,
           LocLast_color    ,
           LocBirth_name    ,
           LocLast_name     ,
           sex_ped          ,
           sex_kinship      ,
           inbred           ,
           starts_with("N_")) %>%
    arrange(LocCurrent, Sex, Age)
  
  return(df)
  
}

F_vector <- function(pedigree.living, studbook) {
  kin.matrix     <- kin_matrix_living(pedigree.living, studbook)
  F_vec          <- 2 * diag(kin.matrix) - 1
  
  return(F_vec)
}

kinship_summary <- function(pedigree, pedigree.living, studbook) {
  p              <- rel_founder_contributions(studbook, pedigree.living)
  living         <- living(studbook)
  n_founder_reps <- length(founder_reps(pedigree, pedigree.living, studbook))
  kin.matrix     <- kin_matrix_living(pedigree.living, studbook)
  mean_gen       <- mean(gen_numbers_living(pedigree.living, studbook), na.rm = TRUE)
  F_vec          <- F_vector(pedigree.living, studbook)
  F_mean         <- mean(F_vec)
  delta_F        <- 1 - (1 - F_mean)^(1/mean_gen)
  Ne             <- 1 / (2 * delta_F)
  N              <- length(living)
  Ne_over_N      <- Ne / N
  FGE            <- 1 / sum(p^2)
  GD             <- 1 - sum(p^2)
  MK             <- mean(kin.matrix[upper.tri(kin.matrix)])
  
  result <- tibble(N, 
                   n_founder_reps, 
                   FGE, 
                   delta_F, 
                   Ne, 
                   Ne_over_N, 
                   mean_gen, 
                   F_mean, 
                   MK, 
                   GD)
  return(result)
}



subset_matrix_living <- function(matrix, studbook) {
  living_males   <- intersect(as.character(living.males(studbook))  , rownames(matrix))
  living_females <- intersect(as.character(living.females(studbook)), rownames(matrix))
  result         <- matrix[living_males, living_females]
  return(result)
}

annotate_kin_matrix <- function(matrix, pedigree.living, studbook) {
  F           <- F_vector(pedigree.living, studbook) %>%
    enframe(name  = "ID", value = "F_vec") %>%
    mutate(ID = as.integer(ID))
  
  living.data <- family_history(pedigree.living, studbook)
  
  annotate    <- living.data %>%
    mutate(ID = as.integer(ID)) %>%
    left_join(F, by = join_by(ID)) %>%
    mutate(hoverText = str_glue(
             "{LocCurrent}", "<br>", 
             "{Age}", " yrs (Born ", "{DateBirth}", ")<br>",
             "Mother: ", "{Dam}", ", Father: ", "{Sire}", "<br>",
             "{N_Siblings}"," Siblings, ", "{N_Children}", " Offspring, ", "{N_Descendants}", " Descendants"
           )
    ) %>% arrange(ID)
  
  kinship_btp  <- subset_matrix_living(matrix, studbook)
  text_males   <- intersect(as.character(living.males(studbook))  , rownames(kinship_btp))
  text_females <- intersect(as.character(living.females(studbook)), colnames(kinship_btp))
  
  annotate.m   <- filter(annotate, Sex == "Male") %>% 
    select(ID, hoverText) %>%
    filter(ID %in% text_males) %>% 
    arrange(match(ID, text_males))
  
  annotate.f   <- filter(annotate, Sex == "Female") %>% 
    select(ID, hoverText) %>%
    filter(ID %in% text_females) %>% 
    arrange(match(ID, text_females))
  
  hover_matrix <- outer(
    annotate.m$hoverText, 
    annotate.f$hoverText, 
    FUN = function(m_text, f_text) {
      I(paste0("<br>Male:<br>", m_text, "<br><br>",
               "Female:<br>", f_text))
    }
  )
  rownames(hover_matrix) <- annotate.m$ID
  colnames(hover_matrix) <- annotate.f$ID
  
  return(hover_matrix)
}

subset_network_btp <- function(pedigree.living, pair, studbook) {
  edges        <- pedigree_edges(pedigree.living, studbook) %>% distinct()
  nodes        <- pedigree_nodes(pedigree.living, studbook) %>% distinct()
  result       <- verbalise(pedigree.living, ids = pair)
  related.pair <- setdiff(pedigree.living[["ID"]], 
              c(unrelated(pedigree.living, male), 
                unrelated(pedigree.living, female))) %>% unique() 
  connections <- edges %>%
    filter(to %in% c(pair, result[[1]][["v1"]], result[[1]][["v2"]])) %>%
    pull(from) %>% unique()
  paths       <- edges %>% 
    filter(to %in% c(related.pair, pair) | from %in% c(related.pair, pair)) %>%
    filter(to %in% c(pair, result[[1]][["v1"]], result[[1]][["v2"]], connections)) %>%
    mutate(color  = colors$emph,
           shadow = TRUE,
           width  = 3) %>%
    distinct()
  pair.edges <- edges %>% 
    filter(to %in% c(related.pair, pair) | from %in% c(related.pair, pair)) %>%
    anti_join(paths) %>%
    bind_rows(paths) %>%
    distinct()
  node.ids <- c(pull(pair.edges, from), pull(pair.edges, to)) %>% unique()
  
  icons.size <- function(x, group) {
    if (group == "match.m" | group == "match.f") {
      list_assign(x, size = 55, )
    } else if (group == "match.anc") {
      list_assign(x, size = 45)
    } else {
      return(x)
    }
  }
  
  nodes.color <- function(group) {
    if (group == "match.m" | group == "match.f" | group == "match.anc") {
      list(background = colors$emph, border = "#000000")
    } else {
      list(NULL)
    }
  }
  
  font.match <- function(x, group) {
    if (group == "match.m" | group == "match.f") {
      list_assign(x, size = 18, background = colors$emph)
    } else if (group == "match.anc") {
      list_assign(x, size = 18, background = colors$emph)
    } else if (group == "match.rel.m" | group == "match.rel.f") {
      list_assign(x, size = 14)
    } else {
      return(x)
    }
  }
  pair.nodes <- filter(nodes, id %in% node.ids) %>%
    mutate(
      label = case_when(
        id %in% male                 ~ "Male Partner",
        id %in% female               ~ "Female Partner",
        id %in% result[[1]][["anc"]] ~ "Shared Ancestor",
        .default = label),
      group = case_when(
        id %in% male                 ~ "match.m",
        id %in% female               ~ "match.f",
        id %in% result[[1]][["anc"]] ~ "match.anc",
        id %in% result[[1]][["v1"]]  ~ "match.rel.m",
        id %in% result[[1]][["v2"]]  ~ "match.rel.f",
        .default = group)) %>%
    mutate(icon = map2(icon, group, \(x, y) icons.size(x, y)),
           font = map2(font, group, \(x, y) font.match(x, y)),
           color= map(group, \(x) nodes.color(x)),
           borderWidth = case_when(
             group %in% c("match.m"    , "match.f")     ~ 3,
             group %in% c("match.rel.m", "match.rel.f") ~ 1.5,
             group     == "match.anc"                   ~ 2,
             .default = 1
           )) %>% distinct()
  subtitle <- paste0(male, " & ", female, " are ", result[[1]][["rel"]],
                     "<br>Path Connecting Pair: ", result[[1]][["path"]])
  pair.net <- visNetwork(pair.nodes, 
                         pair.edges, 
                         width   = "100%", 
                         height  = "700px",
                         main    = "Relatives of Proposed Match",
                         submain = subtitle) %>% 
    addFontAwesome(version = "5.13.0") %>%
    visNodes(shadow = TRUE, fixed  = list(x = FALSE, y = FALSE))  %>%
    visInteraction(dragNodes = TRUE, dragView = TRUE)
  
  visSave(pair.net, file = paste0("Pedigree_Network_PairLineage_", params$group, ".html"))
  
  return(pair.net)
}
  

  
