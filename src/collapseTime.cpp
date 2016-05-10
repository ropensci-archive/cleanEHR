#include <Rcpp.h>
#include <iostream>
#include <ctime>
#include <algorithm>
#include <unordered_map>
#include <string>
using namespace Rcpp ;


// select the last value of the time period.
// [[Rcpp::export]]
DataFrame reallocateTime_(DataFrame d, const float t_discharge, const float frequency){
  NumericVector tin = d["time"];
  CharacterVector valin = d["item2d"];

  //create a map with tin and tvalin acting as a table.
  std::unordered_map<float, std::string> df_in;
  for (int i = 0; i < tin.size(); ++i)
    df_in.insert(std::pair<float, std::string>(tin[i], as<std::string>(valin[i])));


  std::vector<float> tnew;
  std::vector<std::string> vnew;
  float tt = 0;
  while(tt <= t_discharge) {
    vnew.push_back("NA");
    tnew.push_back(tt);
    tt += frequency;
  }
  tnew.push_back(1e127);
  std::sort(tin.begin(), tin.end());

  auto tnew_iter = tnew.begin();
  int ind;

  for ( auto t = tin.begin(); t != tin.end(); ++t ) {  
    while ( tnew_iter != tnew.end() ) {
      ind = tnew_iter - tnew.begin();
      if ( *t >= tnew[ind] && *t < tnew[ind + 1] ) {
        vnew[ind] = df_in[*t];
        break;
      }
      else 
        tnew_iter++;
    }
  }
 
  tnew.pop_back();
  return DataFrame::create(_["time"]=tnew, _["val"]=vnew);
}


