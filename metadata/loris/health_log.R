
health.log    <- tribble(
  ~category,      ~event_state,   ~subject,  ~title,                           ~interval,  
  "symptom"    ,   "event"      ,   "culi"  ,  "dried blood around anus/tail",   interval(make_date(2023, 11, 16)   ,  make_date(2023, 11, 16))    ,
  "behavior"   ,   "event"      ,   "culi"  ,  "coprophagy observed"         ,   interval(make_date(2023, 12, 17)   ,  make_date(2023, 12, 17))    ,
  "weight"     ,   "event"      ,   "culi"  ,  "weight - 451 g"              ,   interval(make_date(2024, 04, 14)   ,  make_date(2024, 04, 14))    ,
  "behavior"   ,   "event"      ,   "culi"  ,  "coprophagy observed"         ,   interval(make_date(2024, 01, 21)   ,  make_date(2024, 01, 21))    ,
  "behavior"   ,   "event"      ,   "culi"  ,  "left biscuit/gum behind"     ,   interval(make_date(2024, 02, 10)   ,  make_date(2024, 02, 10))    ,
  "repro"      ,   "event"      ,   "warble",  "gave birth to healthy twins" ,   interval(make_date(2024, 10, 09)   ,  make_date(2024, 10, 09))    ,
  "repro"      ,   "event"      ,   "warble",  "killed/ate both twins"       ,   interval(make_date(2024, 10, 10)   ,  make_date(2024, 10, 10))    ) %>%
  mutate(start = int_start(interval),
         end   = int_end(interval)) %>% 
  mutate(subject = 
           factor(subject, 
                  levels = 
                    c("warble",
                      "culi",
                      "unknown",
                      "ntc")))

health.table <- health.log %>%
  arrange(start, subject) %>%
  dplyr::select(Subject  = subject,
                Date     = start,
                Category = category,
                Note     = title) %>%
  mutate(Subject  = str_to_title(Subject),
         Category = str_to_title(Category),
         Note     = str_to_sentence(Note)) %>%
  kable() %>%
  kable_styling(full_width = T,
                fixed_thead = T,
                c("hover", "responsive"),
                position = "center") %>%
  column_spec(1, bold = T) %>%
  collapse_rows(columns = 1, valign = "top") %>%
  scroll_box(width = "800px", height = "400px")