#include "btm.h"

// compute p(z_i=k|z/i, B)
NumericVector Btm::sample_prob(Biterm& bi) {
  NumericVector Q(K);
  for (int k = 0; k < K; k++) {
    int subtract = 0;
    if (bi.get_z() == k)
      subtract = 1.0;
    
    // Rcout << "subtr: " << subtract << ", ";
    // Rcout << "topic count word: " << topic_count_wd[k] << ", ";
    // Rcout << "denom: " << std::pow(topic_count_wd[k] - (2.0 * subtract) + (V * eta), 2.0) << ", ";
    Q[k] = (alpha + topic_count_bt[k] - subtract) * 
      (eta + word_topic_count(bi.get_wi(), k) - subtract) *
      (eta + word_topic_count(bi.get_wj(), k) - subtract) /
      ((topic_count_wd[k] - subtract + V * eta + 1) * 
        (topic_count_wd[k] - subtract + V * eta));
    if (k != 0)
      Q[k] = Q[k] + Q[k-1];
    // Rcout << "Q_" << k << ": " << Q[k] << " " << std::endl;
  }
  // Rcout << std::endl;
  return Q;
}

void Btm::update_counts(Biterm& bi, int k) {
  word_topic_count(bi.get_wi(), bi.get_z())--;
  word_topic_count(bi.get_wj(), bi.get_z())--;
  topic_count_wd(bi.get_z()) -= 2;
  topic_count_bt(bi.get_z())--;
  bi.set_z(k);  
  word_topic_count(bi.get_wi(), k)++;
  word_topic_count(bi.get_wj(), k)++;
  topic_count_wd(k) += 2;
  topic_count_bt(k)++;
}

arma::mat Btm::calc_beta() {
  arma::mat beta(V, K, arma::fill::zeros);
  for (int k = 0; k < K; k++) {
    for (int w = 0; w < V; w++) {
      beta(w, k) = (word_topic_count(w, k) + eta) / (topic_count_wd[k] + V * eta);
    }
  }
  return beta;
}

arma::rowvec Btm::calc_theta() {
// B - number of biterms
  arma::rowvec theta(K, arma::fill::zeros);
  for (int k = 0; k < K; k++) {
    theta[k] = (topic_count_bt[k] + alpha) / (B + K * alpha);
  }
  return theta;
}

// [[Rcpp::export]]
List btm_gibbs(NumericVector token_ids, NumericVector doc_ids, int K, 
         double alpha, double eta, int iter) {
  NumericVector docs = unique(doc_ids);
  int D = docs.size();
  int V = unique(token_ids).size();
  
  std::vector<Biterm> bs;
  for (int d = 0; d < D; d++) {
    NumericVector doc_words = token_ids[doc_ids == docs[d]];
    for (int i = 0; i < doc_words.size(); i++) {
      for (int j = i+1; j < doc_words.size(); j++) {
        bs.push_back(Biterm(doc_words[i], doc_words[j], K));
      }
    }
  }
  
  Btm btm = Btm(V, K, alpha, eta, bs);
  
  int B = bs.size();
  arma::mat theta_trace(iter, K);
  theta_trace.zeros();
  arma::cube beta_trace(V, K, iter);
  
  for (int j = 0; j < iter; j++) {
    NumericVector u = runif(B);
    if ((j+1) % 100 == 0)
      Rcout << "Iteration: " << j << std::endl;
    
    // sample latent topic assignments
    for (int i = 0; i < B; i++) {
      // Rcout << "Biterm " << i << std::endl;
      NumericVector Q = btm.sample_prob(bs[i]);
      // Rcout << "----" << std::endl;
      for (int k = 0; k < K; k++) {
        if (u[i] < (Q[k] / Q[K-1])) {
          // Rcout << "assigned topic: " << k << std::endl;
          btm.update_counts(bs[i], k);
          break;
        }
      }
    }
    
    theta_trace.row(j) = btm.calc_theta();
    beta_trace.slice(j) = btm.calc_beta();
  }
  
  NumericVector zs(B);
  for (int b = 0; b < B; b++) 
    zs[b] = bs[b].get_z();
  
  List result;
  result["beta_trace"] = beta_trace;
  result["theta_trace"] = theta_trace;
  result["word_topic_count"] = btm.get_word_topic_count();
  result["topic_count_wd"] = btm.get_topic_count_wd();
  result["topic_count_bt"] = btm.get_topic_count_bt();
  result["z"] = zs;
  CharacterVector class_names(2); 
  class_names[0] = "btm"; 
  class_names[1] = "lda";
  result.attr("class") = class_names;
  
  return result;
}