fill_missing_albums <- function(data) {
  data %>%
    filter(is.na(album)) %>%
    distinct(song, artist, album) %>%
    mutate(album = fetch_tracks_albums(artist, song)) %>%
    left_join(data, ., by = c("artist", "song")) %>%
    transmute(date, artist, song, album = coalesce(album.x, album.y))
}

check_health <- function(data) {
  unique_tracks <- unique(select(data, -date))
  n <- nrow(unique_tracks)

  print(100 / n * sum(is.na(unique_tracks$album)))
  print(100 / n * sum(is.na(unique_tracks$artist)))
  print(100 / n * sum(is.na(unique_tracks$song)))
}
