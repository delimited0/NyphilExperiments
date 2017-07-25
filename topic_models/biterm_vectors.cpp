#include "biterm.h"

// [[Rcpp::export]]
List biterm_vectors(NumericVector token_ids, NumericVector doc_ids, int K) {
  NumericVector docs = unique(doc_ids);
  int D = docs.size();
  
  std::vector<Biterm> bs;
  for (int d = 0; d < D; d++) {
    NumericVector doc_words = token_ids[doc_ids == docs[d]];
    for (int i = 0; i < doc_words.size(); i++) {
      for (int j = i+1; j < doc_words.size(); j++) {
        bs.push_back(Biterm(doc_words[i], doc_words[j], K));
      }
    }
  }
  
  NumericVector w1s(bs.size());
  NumericVector w2s(bs.size());
  for (int b = 0; b < bs.size(); b++) {
    w1s[b] = bs[b].get_wi();
    w2s[b] = bs[b].get_wj();
  }
  
  List result;
  result["w1s"] = w1s;
  result["w2s"] = w2s;
  
  return result;
}