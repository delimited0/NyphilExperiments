library(tidyverse)
library(tidytext)
library(stringr)
library(devtools)
library(widyr)
library(magrittr)
library(gutenbergr)
library(PythonInR)

set.seed(1)

# taken from the tidy text book topic modeling chapter:
# http://tidytextmining.com/topicmodeling.html
books <- gutenberg_works(title == "Great Expectations") %>%
  gutenberg_download(meta_fields = "title")

# divide into documents, each representing one chapter
by_chapter <- books %>%
  group_by(title) %>%
  mutate(chapter = cumsum(str_detect(text, regex("^chapter ", ignore_case = TRUE)))) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  unite(document, title, chapter)

# split into words
by_chapter_word <- by_chapter %>%
  unnest_tokens(word, text)

# find document-word counts
word_counts <- by_chapter_word %>%
  anti_join(stop_words) %>%
  count(document, word, sort = TRUE) %>%
  ungroup()

# latent dirichlet allocation
D <- word_counts$document %>% unique %>% length
N <- by_chapter_word %>% count(document) %>% select(n) %>% as_vector()
K <- 10
V <- by_chapter_word %>% count(word) %>% nrow

if (!pyIsConnected()) pyConnect()
pyImport(import = "os")
pyImport(import = "numpy", as = "np")
pyImport(import = "tensorflow", as = "tf")
pyImport(import = "edward", as = "ed")
pyImport(from = "edward.models", import = c("Dirichlet", "Multinomial", "Categorical"))

edward_lda <- 
"
theta = Dirichlet(alpha = tf.zeros([D, K]) + 0.1)
phi = Dirichlet(alpha = tf.zeros([K, V]) + 0.05)
z = [[0] * np.max(N)] * D
w = [[0] * np.max(N)] * D
for d in range(D):
  for n in range(N[d]):
    z[d][n] = Categorical(logits = ed.logit(tf.gather(theta, d)))
    w[d][n] = Categorical(logits = ed.logit(tf.gather(phi, z[d][n])))
"

pySet("D", D)
pySet("N", as.matrix(N), useNumpy = TRUE)
pyExec("N = N.flatten()")
pySet("K", K)
pySet("V", V)
pyExec("baz = [0] * np.array([2, 2])")
pyExecp("np.max(N)")
pyExec(edward_lda)
