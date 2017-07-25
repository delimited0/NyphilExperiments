#ifndef DBTM_H
#define DBTM_H

#include "biterm.h"

class Dbtm : public Btm {
private:
  int K;
  int V;
  int B;
  double eta;
  arma::cube word_topic_count;
  arma::mat topic_count_wd;
  arma::mat topic_count_bt;
  arma::mat alpha;
  
public:
  Dbtm(int V, int K, double eta, std::vector<Biterm> bs, NumericVector t) {
    int T = t.size();
    word_topic_count = arma::cube(V, K, T, arma::fill::zeros);
    topic_count_wd = arma::mat(T, K, arma::fill::zeros);
    topic_count_bt = arma::mat(T, K, arma::fill::zeros);
    
    
    this->B = bs.size();
    for (int i = 0; i < B; i++) {
      int z = bs[i].get_z();
      word_topic_count(bs[i].get_wi(), z)++;
      word_topic_count(bs[i].get_wj(), z)++;
      topic_count_wd(z) += 2;
      topic_count_bt(z)++;
    }
  }
};

#endif
