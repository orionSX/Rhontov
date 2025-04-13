library(rvest)
library(dplyr)


url <- "https://ru.wikipedia.org/wiki/Список_музеев_Ростовской_области"
page <- read_html(url)


tables <- html_elements(page, "table.wikitable")


museum_tables <- lapply(tables, function(tbl) {
  html_table(tbl, fill = TRUE)
})
df_raw <- bind_rows(museum_tables)

name_col <- grep("Название|Наименование", names(df_raw), value = TRUE)
addr_col <- grep("Местонахождение", names(df_raw), value = TRUE)
desc_col <- grep("Примечания", names(df_raw), value = TRUE)


rows <- unlist(lapply(tables, function(tbl) html_elements(tbl, "tr")[-1]), recursive = FALSE)

museum_links <- sapply(rows, function(row) {
  link <- html_element(row, "a")
  if (!is.na(link)) {
    href <- html_attr(link, "href")
    if (!is.na(href)) paste0("https://ru.wikipedia.org", href) else NA
  } else {
    NA
  }
})
img_links <- sapply(rows, function(row) {
  link <- html_element(row, "img")
  if (!is.na(link)) {
    href <- html_attr(link, "src")
    if (!is.na(href)) paste0("https:", href) else NA
  } else {
    NA
  }
})


df_museums <- tibble(
  Название = df_raw[[name_col[1]]],
  Адрес = if (length(addr_col) > 0) df_raw[[addr_col[1]]] else NA,
  Описание = if (length(desc_col) > 0) df_raw[[desc_col[1]]] else NA,
  Ссылка = museum_links[1:nrow(df_raw)],
  Картинка=img_links[1:nrow(df_raw)]
)


