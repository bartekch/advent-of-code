#include <Rcpp.h>
using namespace Rcpp;

//[[Rcpp::export]]
IntegerVector playCombatCpp(int maxit, Rcpp::IntegerVector pl1, Rcpp::IntegerVector pl2) {
  
  int l1 = pl1.length();
  int l2 = pl2.length();
  
  int i = 0;
  
  int pl1_card, pl2_card;
  
  while (l1 > 0 && l2 > 0 && i < maxit) {
    
    pl1_card = pl1[0];
    pl1.erase(0);
    
    pl2_card = pl2[0];
    pl2.erase(0);
    
    if (pl1_card > pl2_card) {
      pl1.push_back(pl1_card);
      pl1.push_back(pl2_card);
      l1++;
      l2--;
    } else {
      pl2.push_back(pl2_card);
      pl2.push_back(pl1_card);
      l1--;
      l2++;
    }
    
    i++;
  }
  
  IntegerVector win;
  if (l1 > 0) {
    win = pl1;
  } else {
    win = pl2;
  }
  
  return win;
}
