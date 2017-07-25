// Composer Topic over Time
#include "util.h"

void psi_update(NumericMatrix& psi, NumericVector dates, NumericVector topic_count,
                NumericVector z) {
  int K = topic_count.size();
  int N = z.size();
  NumericVector t_mean(K);
  NumericVector t_var(K);
  for (int i = 0; i < N; i++) {
    t_mean[z[i]] += dates[i];
  }
  for (int k = 0; k < K; k++) 
    t_mean[k] = t_mean[k] / topic_count[k];
  for (int i = 0; i < N; i++) {
    t_var[z[i]] += pow(dates[i] - t_mean[z[i]], 2.0);
  }
  for (int k = 0; k < K; k++) {
    t_var[k] = t_var[k] / topic_count[k];
    psi(0, k) = t_mean[k] * (((t_mean[k] * (1.0 - t_mean[k])) / t_var[k]) - 1.0);
    psi(1, k) = (1.0 - t_mean[k]) * (((t_mean[k]) * (1.0 - t_mean[k]) / t_var[k]) - 1.0);
  }
}

NumericVector sample_prob(double z, double date, NumericVector topic_count, 
                          NumericVector comp_topic_count, NumericVector cond_topic_count, 
                          NumericMatrix psi, int K, int C, double eta, double alpha) {
  NumericVector Q(K);
  for (int k = 0; k < K; k++) {
    int subtract = 0;
    if (z == k)
      subtract = 1;
    Q[k] = (eta + comp_topic_count[k] - subtract) * 
      (alpha + cond_topic_count[k] - subtract) *
      (pow(date, psi(0, k) - 1.0) * pow(1.0 - date, psi(1, k))) /
    ((C * eta + topic_count[k] - subtract) * boost::math::beta(psi(0, k), psi(1, k)));
    if (k != 0)
      Q[k] = Q[k] + Q[k-1];
  }
  
  return Q;
}

/*
 * ctot_gibbs: fit composer topic over time model.
 * composers - 0 indexed composer id
 * conductors - 0 indexed conductor id
 * dates - 0-1 normalized date
 */

// [[Rcpp::export]]
List ctot_gibbs(NumericVector composers, NumericVector conductors, 
                NumericVector dates, int K, double alpha, double eta,
                double nu, int iter) {
  int A = unique(conductors).size();
  int C = unique(composers).size();
  int N = dates.size();
  NumericMatrix cond_topic_count(A, K);
  NumericMatrix comp_topic_count(C, K);
  NumericVector topic_count(K);
  NumericMatrix psi(2, K);
  std::fill(psi.begin(), psi.end(), nu);
  
  NumericVector z = floor(runif(N) * K);
  for (int i = 0; i < N; i++) {
    cond_topic_count(conductors[i], z[i])++;
    comp_topic_count(composers[i], z[i])++;
    topic_count(z[i])++;
  }
  
  for (int j = 0; j < iter; j++) {
    NumericVector u = runif(N);
    if ((j+1) % 100 == 0)
      Rcout << "Iteration: " << j << std::endl;
    
    // sample latent topic assignments
    for (int i = 0; i < N; i++) {
      NumericVector Q = sample_prob(z[i], dates[i], topic_count, 
                                    comp_topic_count(composers[i], _), 
                                    cond_topic_count(conductors[i], _),
                                    psi, K, C, eta, alpha);
      
      for (int k = 0; k < K; k++) {
        if (u[i] < (Q[k] / Q[K-1])) {
          comp_topic_count(composers[i], z[i])--;
          cond_topic_count(conductors[i], z[i])--;
          topic_count[z[i]]--;
          z[i] = k;
          comp_topic_count(composers[i], k)++;
          cond_topic_count(conductors[i], k)++;
          topic_count[k]++;
          break;
        }
      }
    }
    
    // update psi by method of moments
    psi_update(psi, dates, topic_count, z);
  }
  
  NumericMatrix beta(C, K);
  NumericMatrix theta(A, K);
  for (int k = 0; k < K; k++) {
    for (int c = 0; c < C; c++) {
      beta(c, k) = (comp_topic_count(c, k) + eta) / (topic_count(k) + C * eta);  
    } 
    for (int a = 0; a < A; a++) {
      theta(a, k) = (cond_topic_count(a, k) + alpha) / (sum(cond_topic_count(a, _)) + K * alpha);
    }
  }
  
  List result;
  result["beta"] = beta;
  result["theta"] = theta;
  result["psi"] = psi;
  result["z"] = z;
  result["cond_topic_count"] = cond_topic_count;
  result["comp_topic_count"] = comp_topic_count;
  result["topic_count"] = topic_count;
  result["composer_id"] = composers;
  result["conductor_id"] = conductors;
  CharacterVector class_names(2); 
  class_names[0] = "ctot"; 
  class_names[1] = "lda";
  result.attr("class") = class_names;
  
  return result;
}