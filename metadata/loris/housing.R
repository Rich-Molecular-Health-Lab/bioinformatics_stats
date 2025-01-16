housing <- list(
  warble = tribble(
    ~start_day,        ~last_day        , ~enclosure,
    ymd(loris$day1)    , ymd("2024-02-28"), "old",
    ymd("2024-02-29")  , ymd(loris$last)  , "new"
  ),
  culi = tribble(
    ~start_day,        ~last_day        , ~enclosure,
    ymd(loris$day1)    , ymd("2024-02-27"), "old",
    ymd("2024-02-28")  , ymd(loris$last)  , "new"
  )
)
