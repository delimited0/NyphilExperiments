library(tidyverse)
library(stringr)
library(ggplot2)
library(ggrepel)

load_ideal_point_data

# where are these guys ----
# conductor_set <- c("Gilbert, Alan",
#                    "van Zweden, Jaap",
#                    "Bernstein, Leonard",
#                    "Mehta, Zubin", 
#                    "Boulez, Pierre", 
#                    "Maazel, Lorin",
#                    "Abbado, Claudio",
#                    "Mahler, Gustav",
#                    "Toscanini, Arturo",
#                    "Salonen, Esa-Pekka",
#                    "Masur, Kurt",
#                    "Stokowski, Leopold",
#                    "LaGuardia, Fiorello",
#                    "Rostropovich, Mstislav",
#                    "Dutoit, Charles",
#                    "Stransky, Josef"
# )
conductor_set <- c("Stravinsky, Igor",
                   "Stockhausen, Karlheinz",
                   # "Bernstein, Leonard",
                   # "Boulez, Pierre",
                   "van Zweden, Jaap",
                   "Gergiev, Valery",
                   "Solti, Georg",
                   "Muti, Riccardo",
                   "Klemperer, Otto",
                   "Rodgers, Richard", 
                   "Ravel, Maurice",
                   "Gardiner, John Eliot",
                   "Abbado, Claudio"
                   )
music_directors <- c("Hill, Urelli Corelli",
                     "Eisfeld, Theodore",
                     "Bergmann, Carl",
                     "Damrosch, Leopold",
                     "Thomas, Theodore",
                     "Seidl, Anton",
                     "Paur, Emil",
                     "Damrosch, Walter",
                     "Safonoff, Wassily",
                     "Mahler, Gustav",
                     "Stransky, Josef",
                     "Mengelberg, Willem",
                     "Toscanini, Arturo",
                     "Barbirolli, John",
                     "Rodzinski, Artur",
                     "Stokowski, Leopold",
                     "Mitropolous, Dmitri",
                     "Bernstein, Leonard",
                     "Boulez, Pierre",
                     "Mehta, Zubin",
                     "Masur, Kurt",
                     "Maazel, Lorin",
                     "Gilbert, Alan")

# pca piece counts ----
pca <- prcomp(piece_count_mat, rank = 2, scale = TRUE)
pc_tbl <- pca$x %>%
  as_tibble %>%
  mutate(conductor = piece_counts$conductor)
loading_tbl <- pca$rotation %>%
  as_tibble %>%
  mutate(piece = colnames(piece_counts)[-1])

ggplot(data = filter(pc_tbl, conductor %in% conductor_set), aes(PC1, PC2)) + 
  geom_point() +
  geom_text_repel(data = filter(pc_tbl, conductor %in% conductor_set),
                  aes(label = conductor)) +
  geom_point(data = filter(loading_tbl, conductor == "Stravinsky, Igor"),
             aes(label = piece))

# pca composers counts ----
pca <- prcomp(composer_freq_mat, rank = 2, scale = TRUE)
pc_tbl <- pca$x %>%
  as_tibble %>%
  mutate(conductor = composer_counts$conductor)
loading_tbl <- (pca$rotation * 1000) %>%
  as_tibble %>%
  mutate(composer = colnames(composer_counts)[-1] %>% str_replace("  ", " "))

# stockhausen test
ggplot(data = filter(pc_tbl, conductor == "Stockhausen, Karlheinz"), aes(PC1, PC2)) + 
  geom_point() +
  geom_text_repel(data = filter(pc_tbl, conductor == "Stockhausen, Karlheinz"),
                  aes(label = conductor)) +
  geom_point(data = filter(loading_tbl, composer == "Stockhausen, Karlheinz")) + 
  geom_text_repel(data = filter(loading_tbl, composer == "Stockhausen, Karlheinz"),
             aes(label = composer, color = "blue"))

ggplot(data = filter(pc_tbl, conductor %in% conductor_set), aes(PC1, PC2)) + 
  geom_point() +
  geom_text_repel(data = filter(pc_tbl, conductor %in% conductor_set),
                  aes(label = conductor))

# pca played composer or not data ----
piece_played_mat <- piece_count_mat > 0
composer_played_mat <- composer_count_mat > 0

pca <- prcomp(composer_played_mat, rank = 2, scale = TRUE)
# pca <- prcomp(composer_played_mat, rank = 1, scale = TRUE)
pc_tbl <- pca$x %>%
  as_tibble %>%
  mutate(conductor = composer_counts$conductor)
loading_tbl <- (pca$rotation * 1000) %>%
  as_tibble %>%
  mutate(composer = colnames(composer_counts)[-1] %>% str_replace("  ", " "))

ggplot(data = filter(pc_tbl, conductor %in% music_directors), aes(PC1, PC2)) + # 2e
# pc_tbl$index <- rep(0, nrow(pc_tbl))
# ggplot(data = filter(pc_tbl, conductor %in% conductor_set), aes(PC1, index)) +
  geom_point() +
  geom_text_repel(data = filter(pc_tbl, conductor %in% music_directors),
                  aes(label = conductor))
