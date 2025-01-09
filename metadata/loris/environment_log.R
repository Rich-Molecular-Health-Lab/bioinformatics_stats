environment.log <- tribble(
  ~category,     ~subject,      ~event_state     ,  ~location,                        ~interval,  
  "housing"    ,     "culi"  ,  "state"          ,  "old_enclosure"               ,   interval(make_date(2023, 10, 01),       make_date(2024, 02, 27))    , 
  "housing"    ,     "warble",  "state"          ,  "old_enclosure"               ,   interval(make_date(2023, 10, 01),       make_date(2024, 02, 28))    ,
  "housing"    ,     "culi"  ,  "state"          ,  "new_enclosure"               ,   interval(make_date(2024, 02, 28),                       today())    ,
  "housing"    ,     "warble",  "state"          ,  "new_enclosure"               ,   interval(make_date(2024, 02, 29),                       today())    ) %>%
  mutate(start = int_start(interval),
         end   =   int_end(interval),
         subject = factor(subject, levels = c("warble",
                                              "culi",
                                              "unknown",
                                              "ntc")))
housing.dt <- environment.log %>% filter(event_state == "state") %>%
  dplyr::select(-c("interval", 
                   "category",
                   "event_state")) %>%
  mutate(start = decimal_date(start),
         end   = decimal_date(end)) %>% as.data.table()
setkey(housing.dt, subject, start, end)


housing.table <- environment.log %>%
  arrange(subject, start) %>%
  dplyr::select(Subject   = subject,
                Start     = start,
                Enclosure = location) %>%
  mutate(Subject  = str_to_title(Subject),
         Enclosure     = str_to_title(str_remove(Enclosure, "_enclosure"))) %>%
  kable() %>%
  kable_styling(full_width = T,
                fixed_thead = T,
                c("hover", "responsive"),
                position = "center") %>%
  column_spec(1, bold = T) %>%
  collapse_rows(columns = 1, valign = "top") %>%
  scroll_box(width = "800px", height = "400px")