library(urltools)

build_tracks_fetch_query <- function(user, page, tracktype = "recenttracks", baseurl = 'http://ws.audioscrobbler.com/2.0/?') {
  baseurl <- param_set(baseurl, 'method', paste0('user.get', tracktype))
  baseurl <- param_set(baseurl, 'api_key', 'e38cc7822bd7476fe4083e36ee69748e')
  baseurl <- param_set(baseurl, 'user', user)
  baseurl <- param_set(baseurl, 'page', as.character(page))
  baseurl <- param_set(baseurl, 'limit', '50')
  baseurl <- param_set(baseurl, 'format', 'json')

  return(baseurl)
}

fetch_tracks_page <- function(user, page) {
  require(jsonlite)
  response <- fromJSON(build_tracks_fetch_query(user, page))

  return(response)
}

get_track_list <- function(response) {
  return(response$recenttracks$track)
}

parse_tracks <- function(tracklist) {
  tracks <- flatten(tracklist)
  if (!is.null(tracks$`@attr.nowplaying`)) {
    tracks <- tracks[-which(tracks$`@attr.nowplaying` == "true"), ]
  }

  tracks <- select(tracks, `date.#text`, name, `artist.#text`, `album.#text`, mbid)
  rename(tracks, date = `date.#text`, artist = `artist.#text`, album = `album.#text`)
}

get_total_page_count <- function(response) {
  if (is.null(response)) {
    return(0)
  }

  if (is.null(response$recenttracks)) {
    return(0)
  }

  return(response$recenttracks$`@attr`$totalPages)
}

fetch_all_user_scrobbles <- function(user, max_pages = 0) {
  response <- fetch_tracks_page(user, 1)
  total_pages <- get_total_page_count(response)

  if (max_pages != 0) {
    total_pages <- min(max_pages, total_pages)
  }

  all_tracks <- parse_tracks(get_track_list(response))

  print(paste0("Exporting page 1/", total_pages))

  if (total_pages == 1 | max_pages == 1) return(all_tracks)

  for (page in 2:total_pages) {
    print(paste0("Exporting page ", page, "/", total_pages))
    response <- fetch_tracks_page(user, page)
    tracklist <- get_track_list(response)
    temp <- parse_tracks(tracklist)
    all_tracks <- bind_rows(all_tracks, temp)
  }

  return(all_tracks)
}

check_scrobbles <- function(data) {
  missing_albums <- length(which(is.na(testss$album)))
  missing_artist <- length(which(is.na(testss$artist)))
  missing_song <- length(which(is.na(testss$name)))
  missing_date <- length(which(is.na(testss$date)))

  if (missing_albums != 0 | missing_artist != 0 | missing_song != 0 |  missing_date != 0) {
    print("Missing albums: ", missing_albums,
          ", Missing artists: ", missing_artist,
          ", Missing song: ", missing_song,
          ", Missing date: ", missing_date)
    return(FALSE)
  } else {
    return(TRUE)
  }
}


export_scrobbles <- function(user, output, max_pages = 0) {
  export <- fetch_all_user_scrobbles(user, max_pages)
  write_csv(export, output, col_names = T)
}

