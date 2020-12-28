#include <Rcpp.h>
using namespace Rcpp;


int which(IntegerVector x, int v) {
  Rcpp::IntegerVector s = Rcpp::seq(0, x.size()-1);
  IntegerVector res = s[x == v];
  return res[0];
}



//[[Rcpp::export]]
IntegerVector playCupsCpp(IntegerVector cups, int maxit = 100) {
  int l = cups.length() - 1;

  int current_cup_position = 0;

  IntegerVector picked_cups;
  int current_cup_value, destination_cup_value, destination_cup_position;


  for (int i = 1; i <= maxit; i++) {

    current_cup_value = cups[current_cup_position];

    //  pick up three cups
    if (current_cup_position <= l - 3) {
      picked_cups = cups[Range(current_cup_position + 1, current_cup_position + 3)];

      cups.erase(current_cup_position + 1, current_cup_position + 4);

    } else if (current_cup_position == l - 2) {
      picked_cups = cups[Range(current_cup_position + 1, current_cup_position + 2)];
      picked_cups.push_back(cups[0]);

      cups.erase(current_cup_position + 1, current_cup_position + 3);
      cups.erase(0);

      current_cup_position--;

    } else if (current_cup_position == l - 1) {
      picked_cups = cups[Range(current_cup_position + 1, current_cup_position + 1)];
      picked_cups.push_back(cups[0]);
      picked_cups.push_back(cups[1]);

      cups.erase(current_cup_position + 1, current_cup_position + 2);
      cups.erase(0, 2);

      current_cup_position -= 2;

    } else {
      picked_cups = head(cups, 3);
      cups.erase(0, 3);

      current_cup_position -= 3;
    }

    // find destination cup
    destination_cup_value = current_cup_value - 1;
    while (true) {
      if (destination_cup_value == 0) {
        destination_cup_value = max(cups);
      } else if (is_true(any(picked_cups == destination_cup_value))) {
        destination_cup_value--;
      } else {
        break;
      }
    }
    destination_cup_position = which(cups, destination_cup_value);
    if (destination_cup_position < current_cup_position) {
      current_cup_position += 3;
    }

    // insert picked cups
    for (int j = 1; j <= 3; j++) {
      cups.insert(destination_cup_position + j, picked_cups[j-1]);
    }

    // find the position of the new current cup
    current_cup_position++;
    if (current_cup_position > l) {
          current_cup_position = 0;
    }
  }

  // shift to set 1 as the first cup

  return cups;
}







int which2(std::vector<int> x, int v) {
  for (int i = 0; i < x.size(); i ++) {
    if (x[i] == v) {
      return i;
    }
  }
  return -1;
}



//[[Rcpp::export]]
List playCupsCpp2(IntegerVector cupsR, int maxit = 100) {
  std::vector<int>  cups = as<std::vector<int>>(cupsR);

  int l = cups.size() - 1;

  std::vector<int>::iterator current_cup_position = cups.begin();

  std::vector<int> picked_cups = {0,0,0};
  int current_cup_value, destination_cup_value;
  std::vector<int>::iterator destination_cup_position;

  for (int i = 1; i <= maxit; i++) {

    current_cup_value = *current_cup_position;

    //  pick up three cups
    if (current_cup_position <= cups.end() - 4) {
      picked_cups[0] = *(current_cup_position + 1);
      picked_cups[1] = *(current_cup_position + 2);
      picked_cups[2] = *(current_cup_position + 3);

      cups.erase(current_cup_position + 1, current_cup_position + 4);

    } else if (current_cup_position == cups.end() - 3) {
      picked_cups[0] = *(current_cup_position + 1);
      picked_cups[1] = *(current_cup_position + 2);
      picked_cups[2] = *cups.begin();


      cups.erase(current_cup_position + 1, current_cup_position + 3);
      cups.erase(cups.begin());

      current_cup_position--;

    } else if (current_cup_position == cups.end() - 2) {
      picked_cups[0] = *(current_cup_position + 1);
      picked_cups[1] = *cups.begin();
      picked_cups[2] = *(cups.begin() + 1);

      cups.erase(current_cup_position + 1, current_cup_position + 2);
      cups.erase(cups.begin(), cups.begin() + 2);

      current_cup_position--;current_cup_position--;

    } else {
      picked_cups[0] = *cups.begin();
      picked_cups[1] = *(cups.begin() + 1);
      picked_cups[2] = *(cups.begin() + 2);

      cups.erase(cups.begin(), cups.begin() + 3);

      current_cup_position--;current_cup_position--;current_cup_position--;
    }

    // find destination cup
    destination_cup_value = current_cup_value - 1;
    while (true) {
      if (destination_cup_value == 0) {
        destination_cup_value = l + 1;
      } else if (destination_cup_value == picked_cups[0] || destination_cup_value == picked_cups[1] || destination_cup_value == picked_cups[2]) {
        destination_cup_value--;
      } else {
        break;
      }
    }
    destination_cup_position = std::find(cups.begin(), cups.end(), destination_cup_value);
    // insert picked cups

    //std::cout << "round" << std::endl;
    //std::cout << i << std::endl;
    //std::cout << *current_cup_position << std::endl;
    cups.insert(destination_cup_position + 1, picked_cups.begin(), picked_cups.end());
    //std::cout << *current_cup_position << std::endl;
    if (destination_cup_position < current_cup_position) {
      current_cup_position++;current_cup_position++;current_cup_position++;
    }
    //std::cout << *current_cup_position << std::endl;

    // find the position of the new current cup
    if (current_cup_position == (cups.end() - 1)) {
      current_cup_position = cups.begin();
    } else {
      current_cup_position++;
    }
    //std::cout << *current_cup_position << std::endl;
  }

  // shift to set 1 as the first cup
  List L = List::create(wrap(cups), *current_cup_position);
  return L;
}






//[[Rcpp::export]]
IntegerVector playCupsCppOpt(IntegerVector cupsR, int maxit = 100) {
  int current_cup = cupsR[0] - 1;

  int max_cup = cupsR.size() - 1;

  // store index of the next cup under each index
  IntegerVector cups (max_cup + 1);
  for (int i = 0; i < max_cup; i++) {
    cups[cupsR[i] - 1] = cupsR[i + 1] - 1;
  }
  cups[cupsR[max_cup] - 1] = current_cup;


  IntegerVector picked_cups = {0,0,0};

  int destination_cup;

  for (int i = 1; i <= maxit; i++) {

    // pick up three cups
    picked_cups[0] = cups[current_cup];
    picked_cups[1] = cups[picked_cups[0]];
    picked_cups[2] = cups[picked_cups[1]];


    // remove picked cups
    cups[current_cup] = cups[picked_cups[2]];


    // find destination cup
    destination_cup = current_cup - 1;
    while (true) {
      if (destination_cup < 0) {
        destination_cup = max_cup;
      } else if (destination_cup == picked_cups[0] || destination_cup == picked_cups[1] || destination_cup == picked_cups[2]) {
        destination_cup--;
      } else {
        break;
      }
    }

    // insert picked cups
    cups[picked_cups[2]] = cups[destination_cup];
    cups[picked_cups[1]] = picked_cups[2];
    cups[picked_cups[0]] = picked_cups[1];
    cups[destination_cup] = picked_cups[0];

    // find the position of the new current cup
    current_cup = cups[current_cup];
  }

  // recreateList starting from 0
  IntegerVector res (max_cup + 1);
  res[0] = 1;
  for (int i = 0; i < max_cup; i++) {
    res[i + 1] = cups[res[i] - 1] + 1;
  }

  return res;
}
