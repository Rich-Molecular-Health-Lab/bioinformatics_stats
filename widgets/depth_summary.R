

depth_summary = function(dataset) {

  depth.plot <- dataset %>%
    ggplot(aes(depth, after_stat(count))) +
    geom_histogram(binwidth = 1000, fill = "#8785B2FF", color = "black") +
    geom_text(
      stat = "bin",
      binwidth = 1000,
      aes(label = after_stat(count), fontface = "bold"),
      vjust = -0.5,
      color = "black",
      size = 4
    ) +
    theme_classic() +
    scale_x_continuous(
      breaks = seq(0, max(depth$depth, na.rm = TRUE), by = 1000),  
      labels = scales::comma,
      limits = c(0, 15000)
    ) +
    labs(
      title = "Frequency of Read Counts",
      x = "Depth (Read Count)",
      y = "Frequency"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for clarity
    )
  
  depth_list <- as.list(dataset$depth) %>% set_names(dataset$SampleID)
  
  max_depth <- as.numeric(max(dataset$depth))
  
  thresholds <- list(
    "3000" = keep(depth_list, \(x) all(x < 3000)),
    "4000" = keep(depth_list, \(x) all(x < 4000)),
    "5000" = keep(depth_list, \(x) all(x < 5000)),
    "6000" = keep(depth_list, \(x) all(x < 6000)),
    "7000" = keep(depth_list, \(x) all(x < 7000)),
    "8000" = keep(depth_list, \(x) all(x < 8000)),
    "9000" = keep(depth_list, \(x) all(x < 9000)),
    "Total_N" = keep(depth_list, \(x) all(x <= max_depth))
  )
  
  total <- c(as.numeric(nrow(dataset)))
  
  depth_summary <- map(thresholds, \(x) length(x)) %>% set_names(names(thresholds)) %>%
    enframe(name = "Threshold", value = "Samples_Lost") %>%
    unnest_longer("Samples_Lost") %>%
    rowwise() %>%
    mutate(Prop_Lost = Samples_Lost/total)
  
  gt_depth_summary <- depth_summary %>%
    gt(rowname_col = "Threshold") %>%
    fmt_percent(columns = "Prop_Lost") %>%
    tab_header("Total Samples Lost by Rarefaction Threshold") %>%
    tab_stubhead("Rarefaction Thresholds for SRS") %>%
    tab_spanner("Loss in Sample Size",
                columns = c("Samples_Lost", "Prop_Lost")) %>%
    tab_style(style = list(
      cell_text(weight    = "bold", 
                transform = "uppercase", 
                stretch   = "condensed",
                style     = "oblique")),
      locations = list(cells_body(rows = Threshold == "Total_N"),
                       cells_stub(rows = Threshold == "Total_N"))) %>%
    cols_label(Samples_Lost = "Count (N)",
               Prop_Lost    = "Percent of Total N") %>%
    cols_width(stub() ~ px(80)) %>%
    text_case_match("Total_N" ~ "Total N", .locations = cells_stub()) %>%
    opt_align_table_header("left") %>%
    opt_stylize(style = 2, color = "gray")
  
  
  shinyApp(
    ui = page_fluid(
      accordion(
        accordion_panel(
          title = "Summary of Rarefaction Thresholds",
          icon = bsicons::bs_icon("bar-chart"),
          layout_column_wrap(
            plotOutput("depth_plot"),
            gt_output("depth_summary")
          )
        ),
        accordion_panel(
          title = "Sequencing Depth Across All Samples",
          icon = bsicons::bs_icon("calendar-date"),
          reactableOutput("depth_table")
        )
      )
    ),
  server = function(input, output, session) {
    output$depth_plot    <- renderPlot(depth.plot) 
    output$depth_summary <- render_gt(gt_depth_summary) 
    output$depth_table   <- renderReactable(reactable(
      dataset,
      groupBy             = c("CollectionDate"),
      defaultPageSize     = 25,
      sortable            = TRUE,
      resizable           = TRUE,
      height              = 900,
      fullWidth           = TRUE,
      compact             = TRUE, 
      showPagination      = TRUE,
      showPageSizeOptions = TRUE,
      columns = list(
        CollectionDate = colDef.CollectionDate ,
        Subject        = colDef.Subject        ,
        SampleID       = colDef.SampleID       ,
        depth          = colDef.depth          ,
        mean_coverage  = colDef.mean_coverage  ,
        probiotic      = colDef.probiotic      ,
        oatgel         = colDef.oatgel         ,
        steroid        = colDef.steroid        ,
        cauliflower    = colDef.cauliflower    ,
        tomatoes       = colDef.tomatoes       ,
        broccoli       = colDef.broccoli       ,
        med_type       = colDef.med_type       ,
        med_name       = colDef.med_name       ,
        med_dose       = colDef.med_dose       ,
        enclosure      = colDef.enclosure      ,
        access         = colDef.access         ,
        estrus         = colDef.estrus         ,
        pregnant       = colDef.pregnant       ,
        bristol_mean   = colDef.bristol_mean   ,
        health_note    = colDef.health_note    ,
        repro_note     = colDef.repro_note     ,
        SampleNotes    = colDef.SampleNotes    ,
        study_day      = colDef(show = FALSE)
      ),
      columnGroups = list(
        colGroup(name = "Diet Trial"             , columns = c("probiotic"  , "oatgel"    , "steroid")),
        colGroup(name = "Foods in Rotation"      , columns = c("cauliflower", "tomatoes"  , "broccoli")),
        colGroup(name = "Medication Administered", columns = c("med_type"   , "med_name"  , "med_dose")),
        colGroup(name = "Misc. Notes"            , columns = c("health_note", "repro_note", "SampleNotes"))
      )
    ))
    
  }
  )
}
