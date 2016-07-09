tidy_scrobbles <- select(raw_scrobbles, date, song, artist, album)
tidy_scrobbles <- mutate(tidy_scrobbles, date = as.POSIXct(date, origin = "1970-01-01"))
tidy_scrobbles <- arrange(tidy_scrobbles, date)
tidy_scrobbles <- fill_missing_albums(tidy_scrobbles)

save(tidy_scrobbles, file = "data/tidy_scrobbles.Rdata")

## Fetching Artist Top Tag

raw_genres_all_artists <- get_genres(tidy_scrobbles)
save(raw_genres_all_artists, file = "data/raw_genres.RData")

tidy_genres <- tidy_genres(raw_genres_all_artists)
save(tidy_genres, file = "data/tidy_genres.RData")

## Merging Artist Tags

clean_scrobbles <- left_join(tidy_scrobbles, tidy_genres, by = "artist")
save(clean_scrobbles, file = "data/tidy_scrobbles_with_genres.RData")

## Cleaning up

rm(raw_scrobbles, tidy_scrobbles, raw_genres_all_artists, tidy_genres, clean_scrobbles)
