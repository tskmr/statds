Low_Cycle_Fatigue_Data_on_a_Nickel_Base_Superalloy =data.frame(
  pseudostress = c(80.3, 80.6, 80.8, 84.3, 85.2, 85.6, 85.8, 86.4, 86.7, 87.2, 87.3, 89.7, 91.3,
                   99.8, 100.1, 100.5, 113.0, 114.8, 116.4, 118.0, 118.4, 118.6, 120.4, 142.5, 144.5, 145.9),
  k_cycles = c(211.629, 200.027, 57.923, 155.000, 13.949, 112.968, 152.680, 156.725, 138.114, 56.723, 121.075, 122.372, 112.002,
               43.331, 12.076, 13.181, 18.067, 21.300, 15.616, 13.030, 8.489, 12.434, 9.750, 11.865, 6.705, 5.733),
  status = c("failure", "failure", "censored", "failure", "failure", "censored", "failure", "failure", "censored", "failure", "failure", "censored", "failure",
             "failure", "failure", "failure", "failure", "failure", "failure", "failure", "failure", "failure", "failure", "failure", "failure", "failure")
)
x1 = Low_Cycle_Fatigue_Data_on_a_Nickel_Base_Superalloy[1:13, ]
x2 = Low_Cycle_Fatigue_Data_on_a_Nickel_Base_Superalloy[14:26,]
print(xtable::xtable(cbind(x1, x2)), include.rownames=FALSE)
# library(tidyverse)
# Low_Cycle_Fatigue_Data_on_a_Nickel_Base_Superalloy %>% 
#   ggplot(aes(pseudostress, k_cycles, shape=status))+geom_point()
usethis::use_data(Low_Cycle_Fatigue_Data_on_a_Nickel_Base_Superalloy, overwrite = TRUE) # data/pois_mat.rda ができる.
