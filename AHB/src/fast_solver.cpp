//author: Xian Sun, Duke University, 2020

#include <Rcpp.h>
#include <algorithm>
#include <chrono>
#include <map>
using namespace Rcpp;
using std::find;


double get_greedy_CATE(IntegerVector MG, LogicalVector test_treatments,
                       NumericVector test_outcomes) {
  // Computes the CATE, given a MG
  double CATE;
  double control_outcomes = 0;
  double treated_outcomes = 0;
  int n_treated_in_MG = 0;
  int n_control_in_MG = 0;
  for (int i = 0; i < MG.size(); i++) {
    if (test_treatments[MG[i]]) {
      treated_outcomes += test_outcomes[MG[i]];
      n_treated_in_MG += 1;
    }
    else {
      control_outcomes += test_outcomes[MG[i]];
      n_control_in_MG += 1;
    }
  }
  CATE = treated_outcomes / n_treated_in_MG - control_outcomes / n_control_in_MG;
  return(CATE);
}
// get variance of units for the same cov
double get_var(NumericVector array){
  double var = 0;
  double mean = 0;
  for(auto &i:array){mean += i;}
  mean = mean/array.size();
  for(int j = 0; j < array.size(); j++){
    var += (array[j] - mean)*(array[j] - mean);
  }
  var = var/(array.size()-1);
  return var;
}
// get mean of all units in the box
double get_mean(NumericVector array){
  double mean = 0;
  for(auto &i:array){mean += i;}
  mean = mean/array.size();
  return mean;
}

// [[Rcpp::export]]
List greedy_cpp(StringVector names,StringVector black_box ,NumericMatrix test_treated_covs, IntegerVector test_control, IntegerVector test_treated,
                NumericMatrix test_covs, LogicalVector test_treatments, NumericVector test_outcomes,
                int variation, int n_req_matches, double multiplier, SEXP bart_fit0, SEXP bart_fit1,
                NumericVector fhat0, NumericVector fhat1, Function expansion_variance_tmp,
                Function preprocess_cand,Function preprocess_covs,int n_prune) {

  int n_test_treated = test_treated_covs.nrow();
  int p = test_covs.ncol();
  int n_test = test_outcomes.size();

  NumericVector CATE(n_test_treated);
  // contrainer to get upper, lower bounds and MG groups
  List all_A = List::create();
  List all_B = List::create();
  List all_MGs =  List::create();

  // NumericVector variances;
  double prev_var;
  int n_matched_controls;
  IntegerVector all_units = seq(0, n_test - 1);

  // For each test-treated unit
  for (int i = 0; i < n_test_treated; i++) {
    Rcpp::Rcout  << "Matching unit " << i + 1 << " of " << n_test_treated << "\r" << std::flush;

    // Initialize the unit to be trivially in its own MG
    IntegerVector MG(1, test_treated[i]);
    n_matched_controls = 0;
    // Lower and upper bounds initialized to be unit's covariate values
    NumericVector A = test_treated_covs(i, _);
    NumericVector B = test_treated_covs(i, _);
    NumericVector bin_var(p, 10000.0); // Variance of expansion along each cov
    double bin_error = 0; // error of expansion units in the whole box

    //Preprocessing Data to only select those units near the treated unit
    int index = test_treated[i] + 1;
    IntegerVector cands = preprocess_cand(index, test_covs, test_treatments, bart_fit1, bart_fit0, fhat1, fhat0, n_prune);
    NumericMatrix test_covs_pre = preprocess_covs(cands, test_covs);
    cands = cands - 1;
    n_test = test_covs_pre.nrow();

    // While we haven't matched enough units
    do {
      //count_times = count_times+1;
      //auto start = std::chrono::steady_clock::now();
      prev_var = min(bin_var) + 0.01;
      NumericVector potential_matches;
      // Find units closest along each axis
      if (variation != 2) {
        // Don't consider expanding to any units already in the MG
        // Don't take union every time
        potential_matches = setdiff(all_units, MG);

      }
      //else { exit(-1);}// To do

      // traverse each cov to find the best direction to expand the box
      NumericVector proposed_bin(p); // Proposed bin endpoint for each cov
      for (int j = 0; j < p; j++) { // Find variance of expanding each cov
        NumericVector test_df_col = test_covs_pre(_, j);
        double dist_to_bin; // Distance from covariate value to current bin
        double min_dist_to_bin = R_PosInf;
        double closest_val_to_bin; // Value of j'th cov for closest_unit

        ///////// Test for potential_matches being empty /////////
        // 1. Find the closest unit we are allowed to match to
        for (int k = 0; k < n_test; k++) {
          bool contains = false;
          for (int m = 0; m < potential_matches.size(); m++) {
            if (potential_matches[m] == k) {
              contains = true;
              break;
            }
          }
          if (!contains) {
            continue;
          }
          // if(potential_matches[cands[k]] == 0){continue;}
          // Consider expanding upper bound
          dist_to_bin = test_df_col[k] - B[j];
          if (dist_to_bin < min_dist_to_bin &&  // Closest option so far
              dist_to_bin > 0) {// Located outside the bin
            min_dist_to_bin = dist_to_bin;
            closest_val_to_bin = test_df_col[k];
          }
          // Consider expanding lower bound
          dist_to_bin = A[j] - test_df_col[k];
          if (dist_to_bin < min_dist_to_bin &&  // Closest option so far
              dist_to_bin > 0) { // Located outside the bin
            min_dist_to_bin = dist_to_bin;
            closest_val_to_bin = test_df_col[k];
          }
        }
        if (traits::is_infinite<REALSXP>(min_dist_to_bin)) {
          bin_var[j] = R_PosInf;
          continue;
        }
        // 2. Change the bin to the closest unit's covariate value
        proposed_bin[j] = closest_val_to_bin;
      }
      // 3. Test this new bin
      // Find a sample of data in the box we have now, and get the predicted outcomes
      NumericMatrix pred_outcomes = Rcpp::as<NumericMatrix>(expansion_variance_tmp(names,black_box,A, B, proposed_bin, bart_fit0, bart_fit1, 3));

      //get bin variance of the box
      for(int x = 0; x < p; x++){if(bin_var[x] != R_PosInf){bin_var[x] = get_var(pred_outcomes(x, _)) + get_var(pred_outcomes(x+p, _));}}

      // get average predicted outcomes in the box
      // get bin_error
      double average_pred_outcome = get_mean(pred_outcomes);
      double treated_outcome =  test_outcomes[test_treated[i]];
      bin_error = treated_outcome - average_pred_outcome;
      if(bin_error < 0){bin_error = - bin_error;}


      int expand_along = which_min(bin_var); // what if all equal
      // Update bin
      if (min(bin_var)+ bin_error< multiplier * prev_var || n_matched_controls < 1) {
        if (proposed_bin[expand_along] < A[expand_along]) {// Expanded downwards
          A[expand_along] = proposed_bin[expand_along];
        }else {B[expand_along] = proposed_bin[expand_along];}
      }
      else {break;}

      // Find units matched, given the unit's new bin
      LogicalVector in_MG(n_test, true);

      for (int k = 0; k < n_test; k++) {
        NumericVector test_unit = test_covs_pre(k, _);
        for (int j = 0; j < p; j++) {
          if (test_unit[j] > B[j] || test_unit[j] < A[j]) {
            in_MG[k] = false;
            break;
          }
        }
        if (in_MG[k]) {
          int id = cands[k];
          if (std::binary_search(test_control.begin(), test_control.end(), id)) {
            n_matched_controls += 1;
            //if(n_matched_controls == 1){std::cout<<id<<std::endl;}
          }
          MG.push_back(id);
        }
      }

      MG = unique(MG); // Can also get CATE in running fashion
      CATE[i] = get_greedy_CATE(MG, test_treatments, test_outcomes);
    }
    while (min(bin_var)+ bin_error < multiplier * prev_var || n_matched_controls < 1);

    all_MGs.push_back(MG+1);
    all_A.push_back(A);
    all_B.push_back(B);
  }

  List ret = List::create(CATE, all_A, all_B,all_MGs);
  return(ret);
}
