#include <Rcpp.h>
using namespace Rcpp;


bool compareRounds(IntegerVector r11, IntegerVector r12, IntegerVector r21, IntegerVector r22) {

  if (r11.length() != r21.length()) {
    return false;
  } else if (r12.length() != r22.length()) {
    return false;
  } else if (is_true(any(r11 != r21))) {
    return false;
  } else if (is_true(any(r12 != r22))) {
    return false;
  }

  return true;
}




//[[Rcpp::export]]
IntegerVector playRecursiveCombatCpp(int maxit, Rcpp::IntegerVector pl1, Rcpp::IntegerVector pl2, int level) {

  int l1 = pl1.length();
  int l2 = pl2.length();

  int i = 0;

  int pl1_card, pl2_card, winner;

  List previous_rounds = List::create();
  List previous_round;

  while (l1 > 0 && l2 > 0 && i < maxit) {

    for (int j = 0; j < previous_rounds.length(); j++) {
      previous_round = previous_rounds[j];
      if (compareRounds(previous_round[0], previous_round[1], pl1, pl2)) {
        if (level == 1) {
          return pl1;
        } else {
          return IntegerVector({1});
        }
      }
    }
    previous_rounds.push_back(List::create(pl1, pl2));


    pl1_card = pl1[0];
    pl1.erase(0);

    pl2_card = pl2[0];
    pl2.erase(0);


    if (l1 > pl1_card && l2 > pl2_card) {
      winner = playRecursiveCombatCpp(maxit, head(pl1, pl1_card), head(pl2, pl2_card), level + 1)[0];
    } else {
      if (pl1_card > pl2_card) {
        winner = 1;
      } else {
        winner = 2;
      }
    }


    if (winner == 1) {
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

  if (l1 > 0 && l2 > 0) {
    stop("maxit hit");
  }


  IntegerVector win;
  if (level == 1) {
    if (l1 > 0) {
      win = pl1;
    } else {
      win = pl2;
    }
  } else {
    if (l1 > 0) {
      win = IntegerVector({1});
    } else {
      win = IntegerVector({2});
    }
  }

  return win;
}
