#include "util.h"

// [[Rcpp::export]]
NumericMatrix term_score(NumericMatrix beta) {
  // calculate term score (Blei and Lafferty 2009)
  int W = beta.rows();
  int K = beta.cols();
  NumericMatrix term_score(W, K);
  
  for (int w = 0; w < W; w++) {
    double product = 0.0;
    for (int k = 0; k < K; k++) {
      product *= beta(w, k);
    }
    for (int k = 0; k < K; k++) {
      term_score(w, k) = beta(w, k) * log(beta(w, k) / pow(product, (1 / K)));
    }
  }
  
  return term_score;
}

// [[Rcpp::export]]
NumericVector calc_coherence(List ctot, NumericMatrix top_words_topics) {
  int K = top_words_topics.cols();
  int M = top_words_topics.rows();
  NumericMatrix cond_topic_count = ctot["cond_topic_count"];
  NumericVector coherence_scores(K);
  NumericVector composer_id = ctot["composer_id"];
  for (int k = 0; k < K; k++) {
    for (int m = 2; m < M; m++) {
      for (int l = 0; l < m-1; l++) {
        double doc_freq = 0.0;
        double co_doc_freq = 0.0;
        for (int i = 0; i < composer_id.size(); i++) {
          doc_freq += (double) (composer_id[i] == top_words_topics(l, k));
          co_doc_freq += (double) (composer_id[i] == top_words_topics(m, k) &&
                                   composer_id[i] == top_words_topics(l, k));
        }
        coherence_scores[k] += log((co_doc_freq + 1.0) / doc_freq);
      }
    }
  }
  return coherence_scores;
}