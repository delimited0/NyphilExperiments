library(ggplot2)
library(ggrepel)
library(pscl)
library(emIRT)

source("load_ideal_point_data.R")

composer_played_mat <- composer_count_mat > 0
composer_played_mat[composer_played_mat == 0] = -1
piece_played_mat <- piece_count_mat > 0

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

# IRT using ideal, composers as items ----
set.seed(1)
composer_rollcall <- rollcall(data = composer_played_mat, yea = 1, nay = -1, 
         legis.names = composer_counts$conductor, vote.names = colnames(composer_played_mat))

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
composer_irt <- ideal(object = composer_rollcall, d = 2, store.item = TRUE, verbose = TRUE)

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

View(params %>% arrange(Difficulty)) # what composers are frequently played?
View(params %>% arrange(`Discrimination D2`)) # what composers are modern?
View(params %>% arrange(`Discrimination D1`)) # wtf is this dimension?

# IRT with ideal, pieces as items ----
set.seed(1)
piece_rollcall <- rollcall(data = piece_played_mat, yea = 1, nay = 0, 
                              legis.names = piece_counts$conductor, 
                              vote.names = colnames(piece_played_mat))
piece_irt <- ideal(object = piece_rollcall, d = 2, store.item = TRUE, verbose = TRUE)

x <- piece_irt$xbar %>%
  as_tibble %>%
  mutate(conductor = rownames(piece_irt$xbar)) 

ggplot(data = filter(x, conductor %in% music_directors), aes(D1, D2)) +
  geom_point() +
  geom_text_repel(data = filter(x, conductor %in% music_directors),
                  aes(label = conductor))

params <- piece_irt$betabar %>%
  as_tibble %>%
  mutate(piece = rownames(piece_irt$betabar))

View(params %>% arrange(Difficulty))

# wordfish using emPois, composers as items ----
K <- nrow(composer_count_mat)
J <- ncol(composer_count_mat)

Dim <- 1 # latent space dimension

starts <- list()
starts$alpha <- matrix(data = rnorm(J), nrow = J, ncol = 1)
starts$psi <- matrix(data = rnorm(K), nrow = K, ncol = 1)
starts$beta <- matrix(data = rnorm(J), nrow = J, ncol = 1)
starts$x <- matrix(data = rnorm(K), nrow = K, ncol = 1)

priors <- list()
priors$x$mu <- 0
priors$x$sigma2 <- 1
priors$beta$mu <- 0
priors$beta$sigma2 <- 25
priors$alpha$mu <- 0
priors$alpha$sigma2 <- 25
priors$psi$mu <- 0
priors$psi$sigma2 <- 25

composer_pirt <- poisIRT(.rc = t(composer_count_mat), .starts = starts, .priors = priors,
                         .control = list(verbose = TRUE))


