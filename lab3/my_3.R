install.packages("dplyr")
library(dplyr)
data18 <- read.delim("~/my_git/dataR/lab3/data18.txt", header=FALSE)
data_c=data18[,-1]
athlete_events <- read.csv("my_git/dataR/lab3/athlete_events.csv")
gymn=athlete_events[athlete_events$Sport %in% c('Rhythmic Gymnastics'),]
x=athlete_events[athlete_events$Sport %in% c('Rhythmic Gymnastics') & athlete_events$Team %in% c('Russia'),]
x=x[order(x$Year),]
women=x[x$Sex=='F',]
men=x[x$Sex=='M',]
results <- x %>%
  group_by(Year,City, Sex) %>%
  summarize(
    First = sum(Medal=='Gold'),
    Second = sum(Medal=='Silver'),
    Third = sum(Medal=='Bronze')
  ) %>%
  mutate(Total=First+Second+Third)%>%
  ungroup()

numeric_results <- results %>%
  select(-Sex)  

rownames(numeric_results) <- paste(numeric_results$Year, numeric_results$City, sep = " - ")
rownames_c=data_c[,1]
numeric_matrix_c=as.matrix(t(data_c%>%select(-V2)))
colnames(numeric_matrix_c)=rownames_c
numeric_matrix <- as.matrix(t(numeric_results %>% select(-Year, -City,-Total)))

par(mar=c(11,4,2,2))
barplot(numeric_matrix_c[-4,], beside = TRUE, col = rainbow(8),
        main = "Места и их кол-во", 
        ylab = "Количество мест", 
       
        legend.text = paste("Место", 1:8), 
        args.legend = list(x = "topright"),
        las = 2, 
        names.arg = rownames_c) 

data_only_first <- numeric_matrix_c[1, ]
valid_indices <- which(data_only_first != 0 & !is.na(data_only_first))
data_filtered <- data_only_first[valid_indices]
labels_filtered <- paste(data_filtered,' шт.', sep = "")
labels_filtered_l <- paste( colnames(numeric_matrix_c)[valid_indices], sep = "")
pie(data_filtered, 
    main = "Количество первых мест России по худ. гимнастике",
    col = rainbow(length(data_filtered)),  
    labels = labels_filtered)

legend("topright", 
       legend = labels_filtered_l, 
       fill = rainbow(length(data_filtered)), 
       cex = 0.8)  
female_trends <- results %>% select(Year, City, Total)

years_trends <- as.numeric(female_trends$Year)
female_trends[is.na(female_trends)] <- 0
plot(years_trends, female_trends$Total, type = "o", col = "red", 
     main = "Тенденции изменения количества призовых мест по Олимпиадам (Женщины РФ)",
     xlab = "Год", ylab = "Количество призовых мест", ylim = c(0, max(female_trends$Total, na.rm = TRUE) + 1), 
     xaxt = "n")
axis(1, at = years_trends, labels = years_trends, las = 1)
data_female <- gymn[gymn$Sex == 'F',]
filtered_gold_female <- subset(data_female, Medal == "Gold")
medals_by_year_country_female <- aggregate(Medal ~ Team+Year, data = filtered_gold_female, FUN = length)
countries_female <- unique(medals_by_year_country_female$Team)
years_female <- unique(medals_by_year_country_female$Year)
all_combinations <- expand.grid(Team = countries_female, Year = years_female)
medals_by_year_country_female_complete <- merge(all_combinations, medals_by_year_country_female, 
                                                by = c("Team", "Year"), 
                                                all.x = TRUE)
medals_by_year_country_female_complete$Medal[is.na(medals_by_year_country_female_complete$Medal)] <- 0

plot(NULL, 
     xlim = range(medals_by_year_country_female_complete$Year+1), 
     ylim = c(0, max(medals_by_year_country_female_complete$Medal, na.rm = TRUE) + 1), 
     xlab = "Год", 
     ylab = "Количество золотых медалей", 
     main = "График изменения спортивных достижений по золотым медалям (Женщины)")
for (country in countries_female) {
  country_data_female <- subset(medals_by_year_country_female_complete, Team == country)
  lines(country_data_female$Year, country_data_female$Medal, type = "o", 
        col = which(country == countries_female), lwd = 2, pch = 19)
}
legend("topright", legend = countries_female, col = seq_along(countries_female), 
       lwd = 2, pch = 19, title = "Страна")


filtered_prize_female <- subset(data_female, !is.na(Medal))
medals_by_year_country_female <- aggregate(Medal ~ Team + Year, data = filtered_prize_female, FUN = length)
medals_by_country_female <- aggregate(Medal ~ Team, data = medals_by_year_country_female, FUN = sum)
top_countries_female <- head(medals_by_country_female[order(-medals_by_country_female$Medal), ], 7)
countries_female <- top_countries_female$Team
years_female <- unique(medals_by_year_country_female$Year)
all_combinations <- expand.grid(Team = countries_female, Year = years_female)
medals_by_year_country_female_complete <- merge(all_combinations, medals_by_year_country_female, 
                                                by = c("Team", "Year"), 
                                                all.x = TRUE)
medals_by_year_country_female_complete$Medal[is.na(medals_by_year_country_female_complete$Medal)] <- 0
plot(NULL, 
     xlim = range(medals_by_year_country_female_complete$Year + 1), 
     ylim = c(0, max(medals_by_year_country_female_complete$Medal, na.rm = TRUE) + 1), 
     xlab = "Год", 
     ylab = "Количество золотых медалей", 
     main = "График изменения спортивных достижений по призовым (Женщины)")
for (country in countries_female) {
  country_data_female <- subset(medals_by_year_country_female_complete, Team == country)
  lines(country_data_female$Year, country_data_female$Medal, type = "o", 
        col = which(country == countries_female), lwd = 2, pch = 19)
}
legend("topright", legend = countries_female, col = seq_along(countries_female), 
       lwd = 2, pch = 19, title = "Страна")
