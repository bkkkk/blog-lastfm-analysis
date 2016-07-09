library(dplyr)
library(lubridate)
library(ggplot2)
library(zoo)
library(hrbrmisc)
library(viridis)

source("R/func.R")
source("R/last-fm-api.R")

load("data/tidy_scrobbles_with_genres.RData")

clean_scrobbles %>%
  count(yrmth = as.yearmon(date)) %>%
  complete(yrmth = full_seq_yearmon(yrmth)) %>%
  ggplot(aes(yrmth, n)) + geom_line() + geom_point() +
  scale_x_yearmon() +
  labs(x = "Month", y = "Plays", title = "Play counts per month",
       caption = "source: last.fm scrobbles") +
  theme_hrbrmstr()

ggsave("output/overall.png", width = 6, height = 3)

top_artists <- clean_scrobbles %>%
  count(artist, genre, sort = T) %>%
  ungroup() %>%
  top_n(20)

top_artists_translate <- top_artists %>%
  mutate(artist = recode(
    artist,
    `소녀시대` = "Girls' Generation",
    `HOME MADE 家族` = "HOME MADE KAZOKU")
  )

top_artists_translate %>%
  ggplot(aes(reorder(artist, n), n, color = genre)) +
   geom_segment(aes(xend = reorder(artist, n)), yend = 0, color = 'grey50') +
     geom_point(size = 3) +
  coord_flip() +
  labs(y = "Plays", x = "", title = "My taste is all over the place",
       subtitle = "Top 20 Artist by plays", caption = "source: last.fm scrobbles") +
  theme_hrbrmstr() +
  scale_color_viridis(discrete = T, name = "Genre") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = 'grey60', linetype = 'dashed'))

ggsave("output/top_artists.png", width = 5, height = 6)

## Top artists over time (Dont use)
tops <- 10

artist_ranking <- clean_scrobbles %>%
  mutate(artist = recode(artist,
                         `소녀시대` = "Girls' Generation",
                         `HOME MADE 家族` = "HOME MADE KAZOKU")) %>%
  count(year = year(date), artist) %>%
  group_by(year) %>%
  mutate(rank = row_number(-n)) %>%
  filter(rank < tops + 1)

artist_end_tags <- artist_ranking %>%
  ungroup() %>%
  filter(year == max(year)) %>%
  mutate(year = as.numeric(year) + 0.25)

artist_start_tags <- artist_ranking %>%
  ungroup() %>%
  filter(year == min(year)) %>%
  mutate(year = as.numeric(year) - 0.25)

colors <- c("John Mayer" = "#6a40fd", "Netsky" = "#198ce7", "Chet Baker" = "#563d7c", "Jorge Drexler" = "#f1e05a",
            "Joe Satriani" = "#b07219", "Pendulum" = "#e44b23", "Antoine Dufour" = "green")

othertags <- artist_ranking %>% distinct(artist) %>% filter(!artist %in% names(colors)) %>% .$artist

colors <- c(colors, setNames(rep("gray", length(othertags)), othertags))

highlights <- filter(artist_ranking, artist %in% names(colors)[colors != "gray"])

ggplot(data = artist_ranking, aes(year, rank, color = artist, group = artist, label = artist)) +
  geom_line(size = 1.7, alpha = 0.25) +
  geom_line(size = 2.5, data = highlights) +
  geom_point(size = 4, alpha = 0.25) +
  geom_point(size = 4, data = highlights) +
  geom_point(size = 1.75, color = "white") +
  geom_text(data = artist_start_tags, x = 2003.8, size = 4.5) +
  geom_text(data = artist_end_tags, x = 2017, size = 4.5) +
  scale_y_reverse(breaks = 1:tops) +
  scale_x_continuous(
    breaks = seq(min(artist_ranking$year), max(artist_ranking$year)),
    limits = c(min(artist_ranking$year) - 1.5, max(artist_ranking$year) + 1.6)) +
  scale_color_manual(values = colors) +
  theme_hrbrmstr() + theme(
    legend.position = "",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_line(color = 'grey60', linetype = 'dashed')) +
  labs(x = "Year", y = "Rank", title = "Sorry, Pendulum",
       subtitle = "Top 10 artists per year by plays")

ggsave("output/artists_over_time.png", width = 13, height = 5.6)

# Top 10 Songs

clean_scrobbles %>%
  count(song, artist, sort = T) %>%
  ungroup() %>%
  top_n(10) %>%
  mutate(artist = recode(artist, `소녀시대` = "Girls' Generation"),
         song = recode(song, `소원을 말해봐 (Genie)` = "Genie")) %>%
  unite(fullname, artist, song, sep = " - ", remove = F) %>%
  ggplot(aes(reorder(fullname, n), n)) +
  geom_segment(aes(xend = reorder(fullname, n)), yend = 0, color = 'grey50') +
  geom_point(size = 3, color = viridis(1)) +
  scale_color_viridis() + coord_flip() + theme_hrbrmstr() + theme(legend.position = "") +
  labs(x = "", y = "Plays", title = "Hey there K-pop", subtitle = "Top 10 songs by plays",
       caption = "source: last.fm scrobbles")

ggsave("output/top-20-songs.png", width = 5, height = 6)

# Subway-style Genre Ranking

tops <- 10

clear_genres <- function(data) {
  data$genre[data$artist == "Nicole"] <- "metal"
  return(data)
}

cleaner_scrobbles <- clear_genres(clean_scrobbles)

ranking <- cleaner_scrobbles %>%
  filter(is.na(genre) != TRUE) %>%
  count(year = year(date), genre) %>%
  group_by(year) %>%
  mutate(rank = row_number(-n)) %>%
  filter(rank < tops + 1)

end_tags <- ranking %>%
  ungroup() %>%
  filter(year == max(year)) %>%
  mutate(year = as.numeric(year) + 0.25)

start_tags <- ranking %>%
  ungroup() %>%
  filter(year == min(year)) %>%
  mutate(year = as.numeric(year) - 0.25)

to_grey <- ranking %>%
  filter(!genre %in% highlight)

colors <- c("metal" = "#6a40fd", "rock" = "#198ce7", "jazz" = "#563d7c", "electronic" = "#f1e05a",
            "melodic death metal" = "#b07219", "singer-songwriter" = "#e44b23", "acoustic" = "green")

othertags <- ranking %>% distinct(genre) %>% filter(!genre %in% names(colors)) %>% .$genre

colors <- c(colors, setNames(rep("gray", length(othertags)), othertags))

highlights <- filter(ranking, genre %in% names(colors)[colors != "gray"])

ggplot(data = ranking, aes(year, rank, color = genre, group = genre)) +
  geom_line(size = 1.7, alpha = 0.25) +
  geom_line(size = 2.5, data = highlights) +
  geom_point(size = 4, alpha = 0.25) +
  geom_point(size = 4, data = highlights) +
  geom_point(size = 1.75, color = "white") +
  geom_text(data = start_tags, aes(label = genre), x = 2003.8, size = 4.5) +
  geom_text(data = end_tags, aes(label = genre), x = 2017, size = 4.5) +
  scale_y_reverse(breaks = 1:tops) +
  scale_x_continuous(
    breaks = seq(min(ranking$year), max(ranking$year)),
    limits = c(min(ranking$year) - 1.5, max(ranking$year) + 1.6)) +
  scale_color_manual(values = colors) +
  theme_hrbrmstr() + theme(
    legend.position = "",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_line(color = 'grey60', linetype = 'dashed')) +
  labs(x = "Year", y = "Rank", title = "I've mellowed down. A bit",
       subtitle = "Top 10 genres over the years")

ggsave("output/genre_overtime.png", height = 6, width = 12)

