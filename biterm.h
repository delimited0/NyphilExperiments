#ifndef BITERM_H
#define BITERM_H

#include "util.h"

class Biterm {
private:
  int wi;
  int wj;
  int z;
  
public:
  Biterm(int w1, int w2, int K) {
    wi = std::min(w1, w2);
    wj = std::max(w1, w2);
    z = floor(R::runif(0, 1) * K);
  }
  
  int get_wi() const {return wi;}
  int get_wj() const {return wj;}
  int get_z() const {return z;}
  void set_z(int k) {z = k;}
  void reset_z() {z = -1;}
};

#endif