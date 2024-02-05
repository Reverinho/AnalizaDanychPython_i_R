library(psych)
library(ggplot2)
library(dplyr)
library(mice)
imdb_movies <- read.csv("C:/Users/gabri/OneDrive/Pulpit/Rpism_Projekt/Projekt-2.-Filip-Filus-Grabowski/Projekt 2. - Filip, Filus, Grabowski/imdb_movies.csv")

imdb_movies <- select(imdb_movies, names, date_x, score, genre, status, orig_lang, budget_x, revenue, country)

str(imdb_movies)
install.packages("mice")

head(imdb_movies,5)
tail(imdb_movies,5)


imdb_movies$date_x <- as.Date(imdb_movies$date_x,format = "%m/%d/%Y")


imdb_movies %>%
  group_by(status) %>%
  summarise(count = n())

imdb_movies$status <- as.factor(imdb_movies$status)
levels(imdb_movies$status)


imdb_movies %>%
  group_by(orig_lang) %>%
  summarise(count = n())


imdb_movies %>%
  group_by(country) %>%
  summarise(count = n())


md.pattern(imdb_movies)


describe(imdb_movies$score)
describe(imdb_movies$budget_x)
describe(imdb_movies$revenue)

ggplot(imdb_movies, aes(x = score)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Wykres gęstości punktacji użytkowników", x = "Punktacja użytkowników")

ggplot(imdb_movies, aes(x = budget_x)) +
  geom_density(fill = "green", alpha = 0.5) +
  labs(title = "Wykres gęstości budżetu", x = "Budżet")

ggplot(imdb_movies, aes(x = revenue)) +
  geom_density(fill = "purple", alpha = 0.5) +
  labs(title = "Wykres gęstości przychodu", x = "Przychód")



comedy_scores <- imdb_movies$score[grepl("Comedy", imdb_movies$genre, ignore.case = TRUE)]
horror_scores <- imdb_movies$score[grepl("Horror", imdb_movies$genre, ignore.case = TRUE)]


result <- wilcox.test(comedy_scores, horror_scores)


print(result)

count_horror <- length(horror_scores)
count_comedy <- length(comedy_scores)
cat("Liczba wyników zakwalifikowanych do Horror:", count_horror, "\n")
cat("Liczba wyników zakwalifikowanych do Comedy:", count_comedy, "\n")

mean_comedy <- mean(comedy_scores)
mean_horror <- mean(horror_scores)


cat("Średnia ocen dla gatunku Comedy:", mean_comedy, "\n")
cat("Średnia ocen dla gatunku Horror:", mean_horror, "\n")

median_comedy <- median(comedy_scores)
median_horror <- median(horror_scores)


cat("Średnia ocen dla gatunku Comedy:", median_comedy, "\n")
cat("Średnia ocen dla gatunku Horror:", median_horror, "\n")

comedy_horror_data <- data.frame(
  Genre = rep(c("Comedy", "Horror"), each = length(c(comedy_scores, horror_scores))),
  Score = c(comedy_scores, horror_scores)
)

# Narysuj pudełkowy wykres
ggplot(comedy_horror_data, aes(x = Genre, y = Score, fill = Genre)) +
  geom_boxplot() +
  labs(title = "Porównanie ocen między gatunkiem komedii a gatunkiem horroru",
       x = "Gatunek",
       y = "Ocena") +
  theme_minimal()

ggplot(comedy_horror_data, aes(x = Genre, y = Score, color = Genre)) +
  geom_jitter() +
  labs(title = "Rozkład ocen między gatunkiem komedii a gatunkiem horroru",
       x = "Gatunek",
       y = "Ocena") +
  theme_minimal()
