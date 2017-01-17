#ifndef BTM_H
#define BTM_H

#include "biterm.h"

class Btm {
private:
  int K;
  int V;
  int B;
  double alpha;
  double eta;
  NumericMatrix word_topic_count;
  NumericVector topic_count_wd;
  NumericVector topic_count_bt;
  
public:
  Btm(int V, int K, double alpha, double eta, 
      std::vector<Biterm> bs) : K(K), V(V), alpha(alpha), eta(eta) {
    word_topic_count = NumericMatrix(V, K);
    topic_count_wd = NumericVector(K);
    topic_count_bt = NumericVector(K);
    
    this->B = bs.size();
    for (int i = 0; i < B; i++) {
      int z = bs[i].get_z();
      word_topic_count(bs[i].get_wi(), z)++;
      word_topic_count(bs[i].get_wj(), z)++;
      topic_count_wd(z) += 2;
      topic_count_bt(z)++;
    }
  }
  
  // compute sampling probability
  NumericVector sample_prob(Biterm& bi);
  
  // update counts
  void update_counts(Biterm& bi, int k);
  
  NumericMatrix calc_beta();
  
  NumericVector calc_theta();
  
  NumericMatrix get_word_topic_count() { return word_topic_count;}
  
  NumericVector get_topic_count_wd() { return topic_count_wd;}
  
  NumericVector get_topic_count_bt() {return topic_count_bt;}
};

List btm_gibbs(NumericVector token_ids, NumericVector doc_ids, int K, 
               double alpha, double eta, int iter);

#endif