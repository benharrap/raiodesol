make_table <- function(sheet) {
  # Read in the spreadsheet, using `sheet` to target the required sheet
  read_excel(
    "songs.xlsx",
    sheet = sheet
  ) |>
    mutate(
      across(
        c(section:bar),
        str_to_title
      ),
      # Attempt to show the 'everyone' rows when specific instruments are searched
      search_col = case_when(
        instrument %in% c("Response", "Everyone") ~
          "Timbal/Surdo/Agogo/Tam/Shaker/Rep/Snare",
        .default = NA
      )
    ) |>
    group_by(section, instrument) |>
    # Add a row number, to attach footnotes to the different timbal patterns
    mutate(
      row = row_number(),
    ) |>
    ungroup() |>
    #### Table start ####
    gt(
      groupname_col = "section"
    ) |>
    # Use empty string for empty cells
    sub_missing(missing_text = "") |>
    # Hide these columns
    cols_hide(columns = c(row)) |>
    #### Fonts ####
    # Raio font
    opt_table_font(
      size = 14,
      font = google_font("Montserrat")
    ) |>
    tab_style(
      style = cell_text(
        size = px(1),
        color = "white"
        ),
      locations = cells_body(columns = search_col)
    ) |>
    #### Width ####
    # Make the notation narrow relative to the other columns
    cols_width(
      c(instrument) ~ px(100),
      bar ~ px(55),
      search_col ~ px(15),
      everything() ~ px(20)
    ) |>
    # Center align the box notation
    cols_align(
      align = "center",
      columns = matches("\\d")
    ) |>
    #### Labels ####
    # Make the column labels title case
    cols_label_with(
      fn = str_to_title
    ) |>
    # Remove the numbers from e&a as we add a spanner
    cols_label(
      ends_with("e") ~ "e",
      ends_with("&") ~ "&",
      ends_with("a") ~ "a",
      search_col = ""
    ) |>
    # Add a column spanner for each count
    tab_spanner(
      label = "1 ",
      columns = starts_with("1")
    ) |>
    tab_spanner(
      label = "2 ",
      columns = starts_with("2")
    ) |>
    tab_spanner(
      label = "3 ",
      columns = starts_with("3")
    ) |>
    tab_spanner(
      label = "4 ",
      columns = starts_with("4")
    ) |>
    #### Borders ####
    # Add the vertical black bars
    tab_style(
      cell_borders(
        sides = "l",
        color = "black",
        weight = px(2)
      ),
      locations = cells_body(columns = matches("\\d$"))
    ) |>
    # Add one on the right of the last col too
    tab_style(
      cell_borders(
        sides = "r",
        color = "black",
        weight = px(2)
      ),
      locations = cells_body(columns = matches("4a"))
    ) |>
    #### Interactive ####
    # Make the table interactive
    opt_row_striping() |>
    opt_interactive(
      use_pagination = FALSE,
      use_search = TRUE,
      use_compact_mode = TRUE
    )
}
