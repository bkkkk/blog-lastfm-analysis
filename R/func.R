get_genres <- function(data) {
  all_artists_names <- sort(unique(data$artist))

  all_genres <- fetch_all_artists_toptag(all_artists_names) # Takes ages!

  arrange(data_frame(artist = all_artists_names, genre = all_genres), artist)
}

tidy_genres <- function(data) {
  data$genre[data$artist == "Jorge Drexler"] <- "singer-songwriter"
  data$genre[data$artist == "HOME MADE 家族"] <- "hip-hop"
  return(data)
}

find_missing_albums <- function(data) {
  unique(filter(data, is.na(album)))
}

full_seq_yearmon <- function(x) {
  as.yearmon(full_seq(as.numeric(x), 1/12))
}

clean_bad_names <- function(scrobbles) {
  scrobbles$artist[scrobbles$artist == "æ¾¤é‡Žå¼˜ä¹‹"] <- "Hiroyuki Sawano"
  scrobbles$artist[scrobbles$artist == "ì†Œë…€ì‹œëŒ€"] <- "Girls' Generation"
  scrobbles$artist[scrobbles$artist == "HOME MADE å®¶æ—\u008f"] <- "HOME MADE KAZOKU"

  scrobbles$song[scrobbles$song == "ì†Œì›\u0090ì\u009d„ ë§\u0090í•´ë´\u0090 (Genie)"] <- "Genie"
  scrobbles$song[scrobbles$song == "ì†Œë…€ì‹œëŒ€ (Girls' Generation)"] <- "Girls' Generation"

  scrobbles$album[scrobbles$album == "ì†Œì›\u0090ì\u009d„ ë§\u0090í•´ë´\u0090 (Genie) - EP"] <- "Genie - EP"
  scrobbles$album[scrobbles$album == "ì†Œë…€ì‹œëŒ€ (Girls' Generation)"] <- "Girls' Generation"

  scrobbles$album[scrobbles$artist == "Roadrunner United"] <- "Roadrunner United"

  return(scrobbles)
}

merge_genres <- function(x) {
  from <- c("cantautor")
  from_replace <- which(x %in% from)

  to <- c("singer-songwriter")

  return(replace(x, from_replace, to))
}

clean_genres <- function(data) {
  data[data$artist == "Nicole", ]$genre <- "metal"
  data[data$artist == "Electric Mantis", ]$genre <- "trap"
  data[data$artist == "Girl's Generation", ]$genre <- "k-pop"
  data[data$artist == "HOME MADE KAZOKU", ]$genre <- "hip-hop"
  data
}
