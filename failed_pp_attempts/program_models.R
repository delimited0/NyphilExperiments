library(PythonInR)
library(Rcpp)
source("load_data.R")
sourceCpp("biterm_vectors.cpp")

K <- 10
composer_biterms <- biterm_vectors(program_tbl$composerID-1, program_tbl$programID-1, K)
piece_biterms <- biterm_vectors(program_tbl$pieceID-1, program_tbl$programID-1, K)

set.seed(1)

if (!pyIsConnected()) pyConnect()
pyImport(import = "os")
pyImport(import = "numpy", as = "np")
pyImport(import = "tensorflow", as = "tf")
pyImport(import = "edward", as = "ed")
pyImport(from = "edward.models", import = c("Dirichlet", "Multinomial", "Categorical"))

# model 1 ----
a_c <- .1
a_p <- .1
e_p <- .01
e_c <- .01
B <- length(composer_biterms$w1s)
C <- program_tbl %>% distinct(composerID) %>% nrow
P <- program_tbl %>% distinct(pieceID) %>% nrow

edward_model_1 <- 
"
theta_c = Dirichlet(alpha = tf.zeros([1, K]) + a_c)
beta_c = Dirichlet(alpha = tf.zeros([C, K]) + e_c) 
beta_p = Dirichlet(alpha = tf.zeros([P, K]) + e_p) 
z = Categorical(logits = tf.tile(theta_c, [B, 1]))
w1 = Categorical(logits = tf.gather(beta_c, z))
w2 = Categorical(logits = tf.gather(beta_c, z))
p1 = Categorical(logits = tf.gather(beta_p, z))
p2 = Categorical(logits = tf.gather(beta_p, z))

qtheta_c = Dirichlet(alpha = tf.zeros([1, K]))
qbeta_c = Dirichlet(alpha = tf.zeros([C, K])) 
qbeta_p = Dirichlet(alpha = tf.zeros([P, K])) 
qz = Categorical(logits = tf.zeros([B, K]))
"

pySet("a_c", a_c); pySet("a_p", a_p); pySet("e_p", e_p); pySet("e_c", e_c)
pySet("B", B); pySet("C", C); pySet("P", P); pySet("K", K)
pyExec(edward_model_1)

pySet("comp1", composer_biterms$w1s, useSetPoly = FALSE); pySet("comp2", composer_biterms$w2s, useSetPoly = FALSE)
pySet("piec1", piece_biterms$w1s, useSetPoly = FALSE); pySet("piec2", piece_biterms$w2s, useSetPoly = FALSE)
to_nparray <- 
"
comp1 = np.array(comp1),
comp2 = np.array(comp2),
piec1 = np.array(piec1),
piec2 = np.array(piec2)
"
pyExec(to_nparray)

model_1_inf <- 
"
inf1 = ed.KLqp({theta_c: qtheta_c, beta_c: qbeta_c, beta_p: qbeta_p, z: qz}, 
                       {w1: comp1, w2: comp2, p1: piec1, p2: piec2})
inf1.run(n_iter = 1000, n_samples = 20)
"
pyExec(model_1_inf)




