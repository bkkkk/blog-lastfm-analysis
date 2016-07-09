library(urltools)
library(jsonlite)
library(memoise)

build_artist_toptags_query <- function(artist, base = "http://ws.audioscrobbler.com/2.0/") {
  base <- param_set(base, "method", "artist.gettoptags")
  base <- param_set(base, "artist", URLencode(artist))
  base <- param_set(base, "api_key", "96f7c58dc14f575249f97b5f01282316")
  base <- param_set(base, "format", "json")

  return(base)
}


build_track_toptags_query <- function(artist, track, base = "http://ws.audioscrobbler.com/2.0/") {
  base <- param_set(base, "method", "track.gettoptags")
  base <- param_set(base, "artist", URLencode(artist))
  base <- param_set(base, "track", URLencode(track))
  base <- param_set(base, "api_key", "96f7c58dc14f575249f97b5f01282316")
  base <- param_set(base, "format", "json")

  return(base)
}


build_track_info_query <- function(artist, track, base = "http://ws.audioscrobbler.com/2.0/") {
  base <- param_set(base, "method", "track.getInfo")
  base <- param_set(base, "artist", URLencode(artist))
  base <- param_set(base, "track", URLencode(track))
  base <- param_set(base, "api_key", "96f7c58dc14f575249f97b5f01282316")
  base <- param_set(base, "format", "json")

  return(base)
}

fetch_artist_toptags <- function(artist) {
  print(paste0("Fetching ", artist))
  json <- fromJSON(build_artist_toptags_query(artist))

  if (length(json$toptags$tag) == 0) return(NA)

  return(as.vector(json$toptags$tag[,"name"]))
}

fetch_track_toptags <- function(artist, track) {
  print(paste0("Fetching ", artist, " song: ", track))
  json <- fromJSON(build_track_toptags_query(artist, track))

  if (length(json$toptags$tag) == 0) return(NA)

  return(as.vector(json$toptags$tag[,"name"]))
}

fetch_track_album <- function(artist, track) {
  print(paste0("Fetching ", artist, " song: ", track))
  json <- fromJSON(build_track_info_query(artist, track))

  if (is.null(json$track$album)) return(NA)

  return(json$track$album$title)
}


memfetch_artist_toptags <- memoise(fetch_artist_toptags)
memfetch_track_toptags <- memoise(fetch_track_toptags)
memfetch_track_album <- memoise(fetch_track_album)

fetch_all_artists_toptag <- function(artists) {
  toptags <- sapply(artists, memfetch_artist_toptags)
  toptags <- unname(sapply(toptags, function(x) x[1]))

  return(tolower(toptags))
}

fetch_songs_genres <- function(artists, tracks) {
  if (length(artists) != length(tracks)) {
    stop("Cannot fetch genres for songs because inputs")
  }

  mapply(memfetch_track_toptags, artist = artists, track = tracks)
}

fetch_tracks_albums <- function(artists, tracks) {
  if (length(artists) != length(tracks)) {
    stop("Cannot fetch genres for songs because inputs")
  }

  mapply(memfetch_track_album, artist = artists, track = tracks)
}
