# Bin interpolation utilities for ACS-style binned frequency tables.
# Bin labels are expected to follow "<prefix>_<upperbound>",
# e.g. "rent_549" = count in the interval ending at $549.
# Sourced by data_prep.R.

# Estimate a median by linear interpolation across the median-containing bin.
# Returns the estimated median (numeric).
# Note: the mp == 1 branch indexes name[mp+1] and is suspicious for the
# lowest-bin edge case. Preserving original behavior; revisit under Goal #4
# (risk methodology update).
med_lin_est <- function(name, value) {
  tot_value = as.numeric(value)
  mp = min(which(cumsum(tot_value)/sum(tot_value) > .5))
  bin_1 = as.numeric(str_extract(name[mp],"(?<=_)[0-9]+"))
  bin_0 = as.numeric(str_extract(name[min(which(cumsum(tot_value)/sum(tot_value) > .5))-1],"(?<=_)[0-9]+"))
  inc_width = bin_1 - bin_0
  inc_ratio = (sum(tot_value)/2 - sum(tot_value[1:(mp-1)]))/tot_value[mp]
  inc_ratio_1 <- .5/(sum(tot_value[1])/sum(tot_value))
  median_value = ifelse(mp > 1, bin_0+inc_ratio*inc_width,
                        as.numeric(str_extract(name[mp],"(?<=_)[0-9]+"))[mp+1]*inc_ratio_1)
  return(median_value)
}

# Estimate the count of renters with income under an FMI cutoff
# by linear interpolation across the cutoff-containing bin.
# Returns an integer.
# Note: errors if the cutoff falls in the very first bin (mp == 1, no mp-1).
# Preserving original behavior; revisit under Goal #4.
renter_adj <- function(name, value, fmi) {
  inc_bins = as.numeric(str_extract(name,"(?<=_)[0-9]+"))
  mp = min(which(inc_bins>fmi))
  bin_1 = inc_bins[mp]
  bin_0 = inc_bins[mp-1]
  inc_width = bin_1 - bin_0
  bin_ratio = (inc_width - (bin_1 - fmi))/inc_width
  adjusted_pop = round(sum(value[1:mp-1])+value[mp]*bin_ratio)
  return(adjusted_pop)
}
