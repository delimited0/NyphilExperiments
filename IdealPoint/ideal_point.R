# set up ----
library(ggplot2)
library(ggrepel)
library(pscl)
library(knitr)

source("load_ideal_point_data.R")

composer_played_mat <- composer_count_mat > 0
composer_played_mat[composer_played_mat == 0] = -1
piece_played_mat <- piece_count_mat > 0

# directorship info ----
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
                     "Gilbert, Alan", 
                     "van Zweden, Jaap")
director_season <- c(1842,
                     1848,
                     1855,
                     1876,
                     1877,
                     1891,
                     1898,
                     1902,
                     1906,
                     1909,
                     1911,
                     1922,
                     1928,
                     1936,
                     1943,
                     1949,
                     1949,
                     1958,
                     1971,
                     1978,
                     1991,
                     2002,
                     2009,
                     2018)

# IRT using ideal, composers as items ----
set.seed(1)
composer_rollcall <- rollcall(data = composer_played_mat, yea = 1, nay = -1, 
                              legis.names = composer_counts$conductor, 
                              vote.names = colnames(composer_played_mat))

# 1d ----

composer_irt_1d <- ideal(object = composer_rollcall, d = 1, store.item = TRUE, verbose = TRUE)

x <- composer_irt_1d$xbar %>%
  as_tibble %>%
  mutate(conductor = rownames(composer_irt_1d$xbar), y = 0)

ggplot(data = filter(x, conductor %in% music_directors), aes(D1, y)) +
  geom_point() +
  geom_text_repel(data = filter(x, conductor %in% music_directors),
                  aes(label = conductor))

# 2d ----
composer_irt <- ideal(object = composer_rollcall, d = 2, store.item = TRUE, 
                      verbose = TRUE)

x <- composer_irt$xbar %>%
  as_tibble %>%
  mutate(conductor = rownames(composer_irt$xbar)) 

ggplot(data = filter(x, conductor %in% music_directors), aes(D1, D2)) +
  geom_point() +
  geom_text_repel(data = filter(x, conductor %in% music_directors),
                  aes(label = conductor))

params <- composer_irt$betabar %>%
  as_tibble %>%
  mutate(composer = rownames(composer_irt$betabar))

top_composers <- data %>% 
  count(composer) %>%
  top_n(500)

params %>% arrange(Difficulty) %>% View # what composers are frequently played?
params %>% arrange(`Discrimination D2`) %>% View # what composers are modern?
params %>% 
  arrange(`Discrimination D2`) %>%
  filter(composer %in% top_composers$composer) %>%
  View
params %>% arrange(`Discrimination D1`) %>% View # wtf is this dimension?
params %>% 
  arrange(`Discrimination D1`) %>% 
  filter(composer %in% top_composers$composer) %>%
  View

# IRT with ideal, pieces as items ----
set.seed(1)
piece_rollcall <- rollcall(data = piece_played_mat, yea = 1, nay = 0, 
                              legis.names = piece_counts$conductor, 
                              vote.names = colnames(piece_played_mat))
piece_irt <- ideal(object = piece_rollcall, d = 2, 
                   store.item = TRUE, verbose = TRUE)
piece_irt <- postProcess(piece_irt, 
                   constraints = list(`Boulez, Pierre` = c(-.2, .2),
                                      `Toscanini, Arturo` = c(-.1, -.2),
                                      `Eisfeld, Theodore` = c(.3, -.5)),
                   debug = TRUE)
save(piece_irt, file = "piece_irt.RData")

first_director_season <- tibble(conductor = music_directors, 
                                year = director_season)

x <- piece_irt$xbar %>%
  as_tibble %>%
  mutate(conductor = rownames(piece_irt$xbar)) %>%
  left_join(first_director_season, by = "conductor")

ggplot(data = filter(x, conductor %in% music_directors), aes(D1, D2)) +
  geom_point() +
  geom_text_repel(data = filter(x, conductor %in% music_directors),
                  aes(label = paste(conductor, year, sep = "\n")),
                  size = 3.4) +
  labs(x = "Pop", 
       y = "Modern", 
       title = "NY Phil Music Directors' Ideal Point Posterior Means",
       subtitle = "With first year as music director") +
  theme_bw()

params <- piece_irt$betabar %>%
  as_tibble %>%
  mutate(piece = rownames(piece_irt$betabar)) %>%
  rename(Piece = piece)

params %>% arrange(Difficulty) %>% View
params %>% arrange(`Discrimination D2`) %>% View  # time dimension
params %>% arrange(`Discrimination D1`) %>% View  # art - pop? instrumental - vocal? dimension

params %>% 
  arrange(Difficulty) %>%
  top_n(n = 10, wt = -Difficulty) %>%
  select(Piece, Difficulty) %>% 
  kable(digits = 2)

params %>% 
  arrange(`Discrimination D2`) %>% 
  mutate(total = nrow(params)) %>%
  mutate(rank = rank(`Discrimination D2`)) %>%
  filter(rank <= 10) %>%
  select(Piece, `Discrimination D2`) %>%
  kable(digits = 2, caption = "Top 10 most negative D2 values")

params %>% 
  arrange(`Discrimination D2`) %>% 
  mutate(total = nrow(params)) %>%
  mutate(rank = rank(`Discrimination D2`)) %>%
  filter((total - rank) <= 10) %>%
  select(Piece, `Discrimination D2`) %>%
  kable(digits = 2, caption = "Top 10 most positive D2 values")


params %>% 
  arrange(`Discrimination D1`) %>% 
  mutate(total = nrow(params)) %>%
  mutate(rank = rank(`Discrimination D1`)) %>%
  filter(rank <= 10) %>%
  select(Piece, `Discrimination D1`) %>%
  kable(digits = 2, caption = "Top 10 most negative D1 values")

params %>% 
  arrange(`Discrimination D1`) %>% 
  mutate(total = nrow(params)) %>%
  mutate(rank = rank(`Discrimination D1`)) %>%
  filter((total - rank) <= 10) %>%
  select(Piece, `Discrimination D1`) %>%
  kable(digits = 2, caption = "Top 10 most positive D1 values")

