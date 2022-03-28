library(tidyverse)
library(gt)
library(palmerpenguins)

penguins %>% 
  select(-year) %>% 
  group_by(species) %>% 
  summarise(across(is.numeric, .fns =  ~ mean(.x, na.rm = T))) %>% 
  gt(rowname_col = "species") %>% 
  tab_header(title = md("The `penguins` dataset summarised"),
             subtitle = md("**Statistics** off *three* islands")) %>% 
  opt_align_table_header("left") %>% 
  cols_label(bill_length_mm = md("Bill Length<br>(mm)"),
             bill_depth_mm = md("Bill Depth<br>(mm)"),
             flipper_length_mm = md("Flipper Length<br>(mm)"),
             body_mass_g = md("Body Mass<br>(kg)")) %>% 
  fmt_number(everything()) %>% 
  fmt_number(body_mass_g, scale_by = 1/1000) %>% 
  cols_width(
    bill_length_mm ~px(100),
    bill_depth_mm ~px(100),
    flipper_length_mm ~px(100),
    body_mass_g ~px(100)
  ) %>% 
  tab_source_note(md("Data from `palmerpenguins` **R** package")) %>% 
  tab_footnote(footnote = md("The *largest* penguin species studied"),
               locations = cells_stub(rows = "Gentoo")
  ) %>% 
  tab_footnote(footnote = md("flippers are like wings"),
               locations = cells_column_labels("flipper_length_mm")) %>% 
  tab_footnote(footnote = md("flippers are cool"),
               locations = cells_column_labels("flipper_length_mm")) %>% 
  tab_footnote(footnote = md("flippers are black and white"),
               locations = cells_column_labels("flipper_length_mm")) %>% 
  opt_footnote_marks(marks = letters) %>% 
  # tab_style(
  #   locations = list(
  #     cells_body(rows = "Gentoo"),
  #     cells_column_labels(bill_depth_mm),
  #     cells_stub(rows = "Gentoo")),
  #   style = list(
  #     cell_fill(color = "#FF5599"),
  #     cell_text(color = "white")
  #   )
  # ) %>% 
  opt_table_font(google_font("Annie Use Your Telescope"),
                weight = 600) %>% 
  data_color(
    columns = c(bill_depth_mm, body_mass_g),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::red_material"
      ) %>% as.character(),
      domain = NULL
    )
  )

