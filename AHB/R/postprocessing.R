#' ATE of a matched dataset
#'
#' \code{ATE} computes the average treatment effect (ATE) of a matched dataset.
#'
#' The ATE is computed as the difference between the weighted treated and the
#' weighted control outcomes in the dataset. A unit's weight is the number of
#' times it was matched.
#'
#' @param AHB_out An object returned by running \code{\link{AHB_fast_match}} and
#'   \code{\link{AHB_MIP_match}}
#' @export

ATE <- function(AHB_out){
  CATEs <- AHB_out$CATE
  MGs <- AHB_out$MGs
  weight <- vector('numeric', length = nrow(AHB_out$data))
  for(i in 1:length(MGs)){
    weight[MGs[[i]]] <- weight[MGs[[i]]] + 1
  }

  weight_sum <- 0
  weighted_CATE_sum <- 0

  for (j in 1:length(MGs)) {
    MG_weight <- sum(weight[MGs[[j]]])
    weight_sum <- weight_sum + MG_weight
    weighted_CATE_sum <- weighted_CATE_sum + MG_weight * CATEs[[j]]
  }
  ATE <- weighted_CATE_sum / weight_sum

  return (ATE)
}

#' ATT of a matched dataset
#'
#' \code{ATT} computes the average treatment effect on the treated (ATT) of a
#' matched dataset.
#'
#' The counterfactual outcome of each treated unit is estimated via the mean
#' outcome of control units in its matched group. This value is then averaged
#' across all treated units to compute the ATT.
#' @param AHB_out An object returned by running \code{\link{AHB_fast_match}} and
#'   \code{\link{AHB_MIP_match}}
#' @export
ATT <- function(AHB_out) {
  ind_treated <- which(colnames(AHB_out$data) == AHB_out$verbose[1])
  ind_outcome <- which(colnames(AHB_out$data) == AHB_out$verbose[2])
  controls <-  which(AHB_out$data[, ind_treated] == 0)
  treated <- which(AHB_out$data[, ind_treated] == 1)
  outcomes <- AHB_out$data[,ind_outcome]
  MGs <- AHB_out$MGs

  weight <- vector('numeric', length = nrow(AHB_out$data))
  for(i in 1:length(MGs)){
    weight[MGs[[i]]] <- weight[MGs[[i]]] + 1
  }

  weight_sum <- 0
  weighted_TT_sum <- 0
  for (j in 1:length(MGs)) {
    MG_controls <- MGs[[j]][MGs[[j]] %in% controls]
    MG_treated <- MGs[[j]][MGs[[j]] %in% treated]

    MG_weight <- sum(weight[MG_controls])
    weight_sum <- weight_sum + MG_weight
    mean_control_outcome <- mean(outcomes[MG_controls])

    for (k in seq_along(MG_treated)) {
      weighted_TT_sum <-
        weighted_TT_sum +
        MG_weight * (outcomes[MG_treated[k]] - mean_control_outcome)/length(MG_treated)
    }
  }

  ATT <- weighted_TT_sum / weight_sum
  return(ATT)
}


#'
#'
#' \code{print_box}  prints the box info on original covariates
#'  constructed based on a specific treated unit.
#'
#' @param AHB_out An object returned by running \code{\link{AHB_fast_match}} and
#'   \code{\link{AHB_MIP_match}}
#' @param treated_unit_id is the treated unit id
#' @param returnValue is if we return the info or not
#' @return a list that contains the box info
#' @export
print_box<-function(AHB_out, treated_unit_id, returnValue = F ){
  treated_unit_ids <-AHB_out$treated_unit_ids
  if(!(treated_unit_id %in% treated_unit_ids)){
    stop("Please input a valid treated unit id.\n")
  }
  infoList = list ()
  cat(paste0("Unit ",treated_unit_id, " matches to units with: " ,"\n"))
  data <- AHB_out$data
  bins <- AHB_out$bins
  data_dummy <- AHB_out$data_dummy
  list_dummyCols <- AHB_out$list_dummyCols
  treated_column_name <- AHB_out$verbose[[1]]
  outcome_column_name <- AHB_out$verbose[[2]]
  lowerBound <- bins[which(treated_unit_ids == treated_unit_id), ,1]
  higherBound<-bins[which(treated_unit_ids == treated_unit_id), ,2]
  unit_dummy <-  data_dummy[treated_unit_id,]
  count <- 1
  colNames <- colnames(data)
  for(cov in colNames){
    if(cov == treated_column_name ||cov == outcome_column_name) next
    if(is.factor(data[, cov])){
      info <- mapCategoricalBoundsToOriginal(lowerBound,higherBound,cov,list_dummyCols[[count]],unit_dummy)
      infoList[cov] <-info
      count = count + 1
    }
    else{
      lower = lowerBound[which(colnames(unit_dummy) == cov)]
      higher = higherBound[which(colnames(unit_dummy) == cov)]
      info <- printOneLineNumeric(cov,lower,higher);
      infoList[cov] <-info
    }
  }
  if(returnValue){
    return (infoList)
  }
}

mapCategoricalBoundsToOriginal<-function(lb,hb,colName,colsDummy,unit_dummy){
  container <- c()
  for(col in colsDummy){
    index <- which(colnames(unit_dummy) == paste0(colName,"_",col))
    if(unit_dummy[index] >= lb[index] && unit_dummy[index] <= hb[index]){
      container<-append(container, col)
    }
  }
  info <- printOneLineForFactor(colName, container)
  return (info)
}

printOneLineForFactor <- function(colName, container){
  info = paste(colName, "in {")
  delimit = ""
  i <- 1
  for(c in container){
    info = paste(info , c)
    if(i != length(container)){
      info = paste(info , ",")
    }
    i = i +1
  }
  info = paste(info , "}")
  cat(paste("  ",info, "\n"))
  return (info)
}

printOneLineNumeric <- function(colName, lower,higher){
  info = paste(colName, "from ")
  info = paste(info, sprintf("%.3f", lower), "to", sprintf("%.3f", higher))
  cat(paste("  ", info, "\n"))
  return(info)
}
