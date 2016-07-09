library(readr)
library(tibble)

raw_scrobbles <- read_tsv("data-raw/raw_scrobbles.csv", col_names = c("date", "song", "artist", "album", "id1", "id2", "id3"))
