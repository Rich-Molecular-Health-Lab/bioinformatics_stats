


ID <- function(df) {
  colDef(
    header = tippy("ID", tooltip = "Studbook ID color-coded by sex (maroon = F, blue = M, green = Undetermined)"),
    maxWidth = 60,
    cell = color_tiles(
      data       = df,
      color_ref  = "color",
      text_color = "white",
      box_shadow = TRUE,
      bold_text  = TRUE
    )
  )
}

Status <- function() {
  colDef(header = tippy("Status", tooltip = "Alive (A), Deceased (D), or Hypothetical ID (H) created to fill missing parentage data"),
         maxWidth = 70,
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
  colDef(
    header   = tippy("Date", tooltip = "Date of Event"),
    align    = "left",
    maxWidth = 450,
    cell     = data_bars(
      data          = df,
      fill_by       = "Year",
      fill_opacity  = 0.3,
      number_fmt    = date_format(),
      align_bars    = "left",
      bar_height    = 15,
      background    = "#DAD9DCFF",
      fill_color    = "#DAD9DCFF",
      text_position = "outside-end",
      min_value     = 1965,
      max_value     = 2025,
      icon          = "grip-lines-vertical",
      icon_color    = "#80848EFF",
      text_color    = "#80848EFF",
      text_size     = 12,
      icon_size     = 15
      )
    )
}


Location <- function(df) {
  colDef(header   = tippy("Location", tooltip = "Institution or other Location at the time of row-record."),
         maxWidth = 200,
         html     = TRUE,
         cell     = function(value, index) {
           
           colorLoc <- df$colorLoc[index]
           iconLoc  <- df$iconLoc[index]
           Location <- span(class = "bungee-inline-regular", df$Location[index])
           NameLoc  <- div(class = "news-cycle-regular", df$NameLoc[index])
          
    tagList(
      div(class = "text-rows",
          div(class = "text-cols", iconLoc, Location),
          div(class = "wrap-text", NameLoc)
      )
    )
  }
 )
}


Age <- function(df) {
  colDef(
    header   = tippy("Age", tooltip = "Age at the time of row-record"),
    align    = "center",
    maxWidth = 50,
    cell     = color_tiles(
      data       = df,
      opacity    = 0.5,
      bold_text  = TRUE,
      box_shadow = TRUE
    )
  )
}

Sire <- function(df) {
  colDef(
    header   = tippy("Sire", tooltip = "Studbook ID of Father (Empty for wild-born founders)"),
    align    = "center",
    maxWidth = 60,
    cell = color_tiles(
      data       = df,
      colors     = colors$m,
      opacity    = 0.3,
      box_shadow = TRUE
    )
  )
}

Dam <- function(df) {
  colDef(
    header   = tippy("Dam", tooltip = "Studbook ID of Mother (Empty for wild-born founders)"),
    align    = "center",
    maxWidth = 60,
    cell = color_tiles(
      data       = df,
      colors     = colors$f,
      opacity    = 0.3,
      box_shadow = TRUE
    )
  )
}

TypeEvent <- function(df) {
  colDef(
    header   = tippy("Event", tooltip = "Event represented by row."),
    align    = "center",
    maxWidth = 70,
    cell = color_tiles(
      data       = df,
      color_ref  = "colorEvent",
      opacity    = 0.5,
      box_shadow = TRUE
    )
  )
}

studbook.cols <- function(df) {
  list(
      ID         =  ID(       df),  
      Location   =  Location( df),  
      TypeEvent  =  TypeEvent(df),  
      Date       =  Date(     df), 
      Status     =  Status(     ), 
      Sire       =  Sire(     df), 
      Dam        =  Dam(      df),  
      Age        =  colDef(show = FALSE),
      Sex        =  colDef(show = FALSE),  
      NameLoc    =  colDef(show = FALSE),  
      Country    =  colDef(show = FALSE), 
      DateBirth  =  colDef(show = FALSE), 
      EndLoc     =  colDef(show = FALSE),  
      StartLoc   =  colDef(show = FALSE), 
      OrderLoc   =  colDef(show = FALSE), 
      colorLoc   =  colDef(show = FALSE),  
      iconLoc    =  colDef(show = FALSE),   
      color      =  colDef(show = FALSE),
      Year       =  colDef(show = FALSE),
      colorEvent =  colDef(show = FALSE)
  )
}

studbook.react <- function(df, cols, ...) {
  df %>%
    reactable(
      theme               = flatly(),
      fullWidth           = TRUE,
      height              = 900,
      sortable            = TRUE,
      resizable           = TRUE,
      filterable          = TRUE,
      defaultPageSize     = 20,
      showPageSizeOptions = TRUE,
      highlight           = TRUE,
      columns             = cols,
      ...
    )
}
