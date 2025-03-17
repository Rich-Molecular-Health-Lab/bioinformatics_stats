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
  colDef(header = tippy("Status", tooltip = "Alive (A), Deceased (D), or Hypothetical ID (H) created to fill missing parentage data"),
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

Date <- function(df) {
  colDef(header   = tippy("Date", tooltip = "Date of Event"),
         maxWidth = 200,
         cell     = color_tiles(
           data                = df,
           colors              = paletteer_d(colors$seq),
           opacity             = 0.4,
           color_by            = "Year",
           brighten_text_color = "black",
           box_shadow          = TRUE,
           number_fmt          = label_date_short()
         )
  )
}

Location <- function(df) {
  colDef(header = tippy("Location", tooltip = "Institution or other Location for record."),
         html   = TRUE,
         cell = function(value, index) {
           Location <- df$Location[index]
           Location <- if (!is.na(Location)) Location else ""
           NameLoc  <- df$NameLoc[index]
           NameLoc  <- if (!is.na(NameLoc)) NameLoc else ""
           iconLoc  <- df$iconLoc[index]
           iconLoc  <- if (!is.na(iconLoc)) iconLoc else ""
           colorLoc <- df$colorLoc[index]
           colorLoc <- if (!is.na(colorLoc)) colorLoc else ""
           
           div(class          = "location-cell", 
               span(class     = "flag"         , iconLoc),
               div(class      = "cell-text"    , 
                   span(class = "LocAbbrev"    , color = colorLoc, Location),
                   span(class = "Names"        , NameLoc)))
         }
      )
}

studbook.cols <- function(df) {
  list(
      ID         =  ID(      df),  
      Location   =  Location(df),  
      Age        =  ,
      StartLoc   =  , 
      TypeEvent  =  ,  
      Date       =  , 
      EndLoc     =  ,  
      Status     =  Status(   df), 
      Sire       =  , 
      Dam        =  ,  
      Sex        =  colDef(show = FALSE),  
      NameLoc    =  colDef(show = FALSE),  
      Country    =  colDef(show = FALSE), 
      DateBirth  =  colDef(show = FALSE), 
      OrderLoc   =  colDef(show = FALSE), 
      colorLoc   =  colDef(show = FALSE),  
      iconLoc    =  colDef(show = FALSE),   
      color      =  colDef(show = FALSE),
      Year       =  colDef(show = FALSE)
  )
}

