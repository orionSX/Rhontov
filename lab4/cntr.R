library(rvest)


countries_target <- c("Finland", "Denmark", "France", "Germany", "Romania")
years_range <- 2014:2021


data_filtered_list <- lapply(years_range, function(yr) {
  url <- paste0("https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=", yr)
  file_path <- paste0("quality_of_life_", yr, ".html")
  download.file(url, destfile = file_path, quiet = TRUE)
  
  html <- read_html(file_path)
  table_nodes <- html_nodes(html, "table")
  data_raw <- html_table(table_nodes[[2]], fill = TRUE) |> as.data.frame()
  
  
  data_cleaned <- data_raw[, -1]
  colnames(data_cleaned) <- c(
    "Country", "QualityOfLife", "PurchasingPower", "Safety", "HealthCare", 
    "CostOfLiving", "PropertyToIncome", "TrafficTime", "Pollution", "Climate", "Extra"
  )[1:ncol(data_cleaned)]
  
  data_filtered <- subset(data_cleaned, Country %in% countries_target)
  data_filtered$Year <- yr
  data_filtered
})


data_combined <- do.call(rbind, data_filtered_list)


indices_meta <- list(
  list(var = "PurchasingPower", title = "Индекс покупательной способности"),
  list(var = "Pollution", title = "Индекс загрязнения"),
  list(var = "PropertyToIncome", title = "Отношение цены на жилье к доходу"),
  list(var = "CostOfLiving", title = "Индекс прожиточного минимума"),
  list(var = "Safety", title = "Индекс безопасности"),
  list(var = "HealthCare", title = "Индекс медицинского обслуживания"),
  list(var = "TrafficTime", title = "Индекс времени движения на дороге"),
  list(
    var = "Climate",
    title = "Климатический индекс",
    filter = function(df) {
      df_sub <- subset(df, Year >= 2016)
      df_sub$Climate <- as.numeric(as.character(df_sub$Climate))
      df_sub
    }
  )
)


colors_palette <- rainbow(length(countries_target))


plot_index <- function(index_meta) {
  df_to_plot <- if (!is.null(index_meta$filter)) {
    index_meta$filter(data_combined)
  } else {
    data_combined
  }
  
  plot(NA,
       xlim = range(df_to_plot$Year),
       ylim = range(as.numeric(df_to_plot[[index_meta$var]]), na.rm = TRUE),
       xlab = "Год", ylab = "Индекс",
       main = paste0(index_meta$title, " (", min(df_to_plot$Year), "-", max(df_to_plot$Year), ")")
  )
  
  invisible(lapply(seq_along(countries_target), function(i) {
    country_name <- countries_target[i]
    country_data <- subset(df_to_plot, Country == country_name)
    lines(country_data$Year, as.numeric(country_data[[index_meta$var]]),
          type = "o", col = colors_palette[i], lwd = 2)
  }))
  
  legend("topright", legend = countries_target, col = colors_palette,
         lty = 1, lwd = 2, bty = "n")
}


par(mfrow = c(1, 1))
invisible(lapply(indices_meta, plot_index))
