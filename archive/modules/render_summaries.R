depth_list <- as.list(depth$depth) %>% set_names(depth$SampleID)

max_depth <- as.numeric(max(depth$depth))

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

total <- c(as.numeric(nrow(depth)))

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

gtsave(gt_depth_summary, paste0(params$sampleset, "_depth_summary.png"), global$visuals)

depth.plot <- ggplot(depth, aes(depth)) +
  geom_dotplot(method="histodot", binwidth = 1000) +
  xlim(0, 30000)

depth_plot <- renderPlot({depth.plot})

ggsave(paste0(params$sampleset, "_depth_histogram.png"),
       depth.plot,
       path = global$visuals,
       dpi  = 320)