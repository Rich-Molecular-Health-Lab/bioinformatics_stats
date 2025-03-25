ID <- function(df) {
  colDef(
    header = tippy("ID", tooltip = "Studbook ID color-coded by sex (maroon = F, blue = M, green = Undetermined)"),
    maxWidth = 70,
    cell = color_tiles(
      data       = df,
      color_ref  = "color",
      box_shadow = TRUE,
      bold_text  = TRUE
    )
  )
}

Status <- function() {
  colDef(header = tippy("Status", tooltip = "Alive or Deceased"),
         maxWidth = 100,
         style    = JS("function(rowInfo, column, state) {
                        const firstSorted = state.sorted[0]
                        if (!firstSorted || firstSorted.id === 'Status') {
                          const prevRow = state.pageRows[rowInfo.viewIndex - 1]
                          if (prevRow && rowInfo.values['Status'] === prevRow['Status']) {
                            return { visibility: 'hidden' }
                          }
                        }
                      }")
  )
}

DateBirth <- function(df) {
  colDef(header   = tippy("Birthdate", tooltip = "Date of birth (captive-born) or capture (wild-born)"),
         maxWidth = 200,
         cell     = color_tiles(
           data                = df,
           colors              = paletteer_d(colors$seq),
           opacity             = 0.4,
           color_by            = "BirthYear",
           brighten_text_color = "black",
           box_shadow          = TRUE,
           number_fmt          = label_date()
           )
         )
}

DateDeath <- function(df) {
  colDef(header = tippy("Death Date", tooltip = "Date of death (NA for living individuals)"),
         cell = color_tiles(
           data       = df,
           colors     =  paletteer_d(colors$seq),
           opacity    = 0.4,
           color_by   = "YearLast",
           brighten_text_color = "black",
           box_shadow = TRUE,
           number_fmt = label_date()
         ), maxWidth = 200)
}

LocBirth <- function(df) {
  colDef(header = tippy("Born",
                        tooltip = "Location of birth (captive-born) or capture (wild-born)"),
         maxWidth     = 100,
         cell     = function(value, index) {
           flag <- df$LocBirth_icon[index]
           name <- span(style = "text-decoration: underline; text-decoration-style: dotted",
                       tippy(value, df$LocBirth_name[index]))
           date <- div(style="display:inline",
                       span(style = "font-weight: 600; font-size:12pt", df$BirthYear[index]),
                       span(style = "font-size:10pt", " (", df$MonthBirth[index], ")"))
           
           div(style="display:grid; row-gap:2px", div(style="display:inline-block", flag, name), date)
           
         }
  )
}

LocLast <- function(df) {
  colDef(header = tippy("Last",
                        tooltip = "Current institution (Alive) or institution and date of death (Deceased)"),
         maxWidth = 100,
         cell     = function(value, index) {
           flag <- df$LocLast_icon[index]
           name <- span(style = "text-decoration: underline; text-decoration-style: dotted",
                        tippy(value, df$LocLast_name[index]))
           date <- div(style = "font-weight: 600; font-size:12pt", df$YearDeath[index])
           
           div(style="display:grid; row-gap:2px", div(style="display:inline-block", flag, name), date)
 
         }
  )
}


Current_Location <- function(df) {
  colDef(header = tippy("Current Location",
                        tooltip = "Current institution"),
         maxWidth = 70,
         cell     = function(value, index) {
           flag <- df$LocLast_icon[index]
           name <- span(style = "text-decoration: underline; text-decoration-style: dotted",
                        tippy(value, df$LocLast_name[index]))
           
           div(style="display:inline-block", flag, name)
           
         },
        style = JS("function(rowInfo, column, state) {
                        const firstSorted = state.sorted[0]
                        if (!firstSorted || firstSorted.id === 'LocLast') {
                          const prevRow = state.pageRows[rowInfo.viewIndex - 1]
                          if (prevRow && rowInfo.values['LocLast'] === prevRow['LocLast']) {
                            return { visibility: 'hidden' }
                          }
                        }
                      }")
  )
}

bubble_count <- function(df, name) {
  colDef(name     = name,
         maxWidth = 250,
         align    = "center",
         cell     = bubble_grid(
           data   = df,
           text_color    = "#ffffff",
           bold_text     = TRUE,
           brighten_text = TRUE,
           colors  = paletteer_d(colors$seq),
         )
  )
}

AgeLast <- function(df) {
  colDef(header = tippy("Age", tooltip = "Now (Alive) or at time of death (Deceased)"),
         maxWidth = 50,
         align    = "center",
         cell = pill_buttons(
           data       = df,
           colors     =  paletteer_d(colors$seq),
           opacity    = 0.4,
           brighten_text_color = "black",
           box_shadow = TRUE
           )
         )
}

Sire <- function(df) {
  colDef(header = tippy("Father", tooltip = "Studbook ID of Sire (0 if wildborn or unknown)"),
         maxWidth = 70,
         cell = pill_buttons(
           data       = df,
           colors     = colors$sire,
           opacity    = 0.6,
           brighten_text_color = "black",
           box_shadow = TRUE
         )
  )
}

Dam <- function(df) {
  colDef(header = tippy("Mother", tooltip = "Studbook ID of Dam (0 if wildborn or unknown)"),
         maxWidth = 70,
         cell = pill_buttons(
           data       = df,
           colors     = colors$dam,
           opacity    = 0.6,
           brighten_text_color = "black",
           box_shadow = TRUE
         )
  )
}

Rel_Contribution <- function(df) {
  colDef(header = tippy("Relative Contribution",
                        tooltip = "Individual's contribution to living population relative to total founder representation in current population"),
         cell = data_bars(
           data = df,
           text_position = "outside-base",
           fill_color = paletteer_d(colors$seq),
           number_fmt = label_percent(),
           background = "white",
           box_shadow = TRUE
         ), maxWidth = 200)
}

inbred <- function(df) {
  colDef(header = tippy("F",
        tooltip = "Inbreeding coefficient (the probability that, at a random autosomal locus, the two alleles carried by the member are identical by descent relative to the pedigree.)"),
         align    = "center",
         maxWidth = 300,
         cell     = data_bars(
           data          = df,
           text_position = "inside-end",
           box_shadow    = TRUE,
           fill_color    = paletteer_d(colors$div),
           background    = "#ffffff00",
           text_color    = "#ffffff",
           bold_text     = TRUE,
           brighten_text = TRUE,
           number_fmt    = label_number(accuracy = 0.001)
             )
         )
}


studbook.cols <- function(df) {
list(
  Status            = Status(),
  ID                = ID(df),
  LocBirth          = LocBirth(df),
  AgeLast           = AgeLast(df),
  LocLast           = LocLast(df),
  Sire              = Sire(df),
  Dam               = Dam(df),
  DateDeath         = colDef(show = FALSE),
  DateBirth         = colDef(show = FALSE),
  Sex               = colDef(show = FALSE),
  color             = colDef(show = FALSE),
  BirthYear         = colDef(show = FALSE),
  MonthBirth        = colDef(show = FALSE),
  YearLast          = colDef(show = FALSE),
  YearDeath         = colDef(show = FALSE),
  LocBirth_icon     = colDef(show = FALSE),
  LocLast_icon      = colDef(show = FALSE),
  LocBirth_name     = colDef(show = FALSE),
  LocLast_name      = colDef(show = FALSE)
  )
}

founder.cols <- function(df) {
  list(
    Status            = Status(),
    ID                = ID(df),
    LocBirth          = LocBirth(df),
    AgeLast           = AgeLast(df),
    LocLast           = LocLast(df),
    Sire              = Sire(df),
    Dam               = Dam(df),
    Rel_Contribution  = Rel_Contribution(df),
    DateDeath         = colDef(show = FALSE),
    DateBirth         = colDef(show = FALSE),
    Sex               = colDef(show = FALSE),
    color             = colDef(show = FALSE),
    BirthYear         = colDef(show = FALSE),
    MonthBirth        = colDef(show = FALSE),
    YearLast          = colDef(show = FALSE),
    YearDeath         = colDef(show = FALSE),
    LocBirth_icon     = colDef(show = FALSE),
    LocLast_icon      = colDef(show = FALSE),
    LocBirth_name     = colDef(show = FALSE),
    LocLast_name      = colDef(show = FALSE)
  )
}

living.cols <- function(df) {
  list(
    ID                = ID(df),
    LocBirth          = LocBirth(df),
    DateBirth         = DateBirth(df),
    AgeLast           = AgeLast(df),
    LocLast           = Current_Location(df),
    Sire              = Sire(df),
    Dam               = Dam(df),
    inbred            = inbred(df),
    N_Ancestors       = bubble_count(df, "Ancestors"),
    N_Descendants     = bubble_count(df, "Descendants"),
    N_Children        = bubble_count(df, "Offspring"),
    N_Siblings        = bubble_count(df, "Siblings"),
    Sex               = colDef(show = FALSE),
    color             = colDef(show = FALSE),
    BirthYear         = colDef(show = FALSE),
    YearLast          = colDef(show = FALSE),
    LocBirth_color    = colDef(show = FALSE),
    LocLast_color     = colDef(show = FALSE),
    LocBirth_icon     = colDef(show = FALSE),
    LocLast_icon      = colDef(show = FALSE),
    LocBirth_name     = colDef(show = FALSE),
    LocLast_name      = colDef(show = FALSE)
  )
}

living.groups <- list(
  colGroup(name    = "Cummulative Counts",
           columns = c("N_Ancestors", "N_Children", "N_Siblings"))
)


studbook.react <- function(df, cols, ...) {
 df.react <- df %>%
    reactable(
      theme               = flatly(centered = TRUE),
      height              = 900,
      sortable            = TRUE,
      resizable           = TRUE,
      filterable          = TRUE,
      defaultExpanded     = TRUE,
      defaultPageSize     = 20,
      showPageSizeOptions = TRUE,
      highlight           = TRUE,
      columns             = cols,
      ...
    )
  
 save_reactable_test(df.react, paste0("StudbookA_", params$group, ".html"))
 return(df.react)
}

lambda.hover <- "Growth rate: Finite rate of population growth calculated as the geometric mean of changes in total population size over the past 5 years"

pva.cols <- list(
  N = colDef(header = tippy("N",
    tooltip = "Demographic Population size: Number of currently living individuals in the population")
  ),
  Ne = colDef(header = tippy("Ne",
     tooltip = "Effective population size (estimated from the rate of inbreeding per generation)"
     ),
     format = colFormat(digits = 0)
  ),
  Ne_over_N = colDef(header = tippy("Ne/N",
      tooltip = "Effective population size relative to population size"
      ),
      format = colFormat(digits = 1)
  ),
  n_founder_reps = colDef(header = tippy("Founders",
      tooltip = "Number of founders represented in the current population" )
  ),
  FGE = colDef(header = tippy("FGE",
      tooltip = "Founder genome equivalents: expressed in units of the number of wild-caught, unrelated individuals (“founders”) that would produce the same gene diversity as the current population"
      ),
      format = colFormat(digits = 1)
  ),
  F_mean = colDef(header = tippy("F",
      tooltip = "Mean inbreeding level" )
  ),
  mean_gen = colDef(header = tippy("Generations",
     tooltip = "Mean generation for living individuals"
     ),
     format = colFormat(digits = 1)
  ),
  delta_F = colDef(header = tippy("ΔF",
    tooltip = "ΔF is the per–generation increase in inbreeding" )
  ),
  GD = colDef(header = tippy("GD %",
    tooltip = "Gene diversity: current percentage of founding gene diversity retained"
    ),
    format = colFormat(percent = TRUE)
  ),
  MK = colDef(header = tippy("MK",
      tooltip = "Population mean kinship" )
  ),
  lambda2 = colDef(header = tippy("\u03BB",
    tooltip =  lambda.hover)
  )
)






