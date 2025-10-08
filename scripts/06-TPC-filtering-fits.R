#### script for filtering models and datasets for analysis ####
#### load packages and data ####
####have not done this script yet, left off with the fitted valid data that i have not figured out how to filter yet ###

library(here)
library(dplyr)
library(ggplot2)
library(ggforce)
library(tidyverse)
curves <- readRDS(here('processed-data', 'wild-tpcs.RdS'))
model_preds <- readRDS(here('processed-data', 'all_model_predictions_01_10_25.RDS'))
params <- readRDS(here('processed-data', 'all_model_params_01_10_25.RdS'))
model_evaluations <- readRDS(here('processed-data', 'model_fit_evaluations_01_10_25.RDS'))

#### DATASET FILTRATION ####
#ideally first we can filter the datasets that won't be curves or have ctmins or maxes or optimums before they are plotted with a model
#could normalize the values all first and do like less than 0.05 of max response# 

near_zero_threshold <- 0.05

filtered <- curves %>%
  group_by(curve_ID) %>%
  summarise(
    min_resp = min(response_value, na.rm = TRUE),
    max_resp = max(response_value, na.rm = TRUE),
    resp_range = max_resp - min_resp,
    .groups = "drop") %>%
  mutate(irregular = case_when(min_resp > near_zero_threshold ~ TRUE,
                               resp_range < 0.1 ~ TRUE,
                               TRUE ~ FALSE))

breadth_curves <- c("18", "59", "78", "173", "226", "228", "232", "234", "265", "267", "325", "366", "367", "368", "369", "401", "402", "403", "416", "359", "253", "20", "412", "309", "311", "270", "351", "409", "74", "75") #30 full breadth curves

ctmin <- c("22", "11", "51", "52", "58", "62", "53", "67", "87", "88", "123", "148", "159", "163", "128", "295", "296", "298", "299", "300", "301", "302", "303", "304", "305", "365", "364", "400", "25", "26", "27", "28", "29", "30", "73", "82", "158", "137", "396", "411") #40 ctmin bound datasets

ctmin_and_topt <- c("257", "385", "81", "86", "31", "33", "37", "221", "219", "255", "312", "313", "314") #13 ctmin bound with topt datasets

ctmax <- c("138", "140", "362", "361", "414", "417") #6 ctmax bound datasets

ctmax_and_topt <- c("225", "230", "382", "252", "383", "419") #6 ctmax bound with topt datasets
topt <- c("413", "418", "415", "350", "171", "174", "184", "211", "355", "358", "15", "16", "19", "42", "43", "44", "45", "47", "61", "63", "65", "69", "72", "77", "80", "83", "85", "89", "91", "94", "96", "97", "98", "102", "104", "106", "108", "109", "110", "114", "129", "130", "142",  "143", "147", "151", "164", "167", "168", "165", "170","169", "76", "161", "179", "180", "181", "182", "183", "185", "186", "187", "189", "190", "191", "194", "195", "196", "197", "198", "199", "200", "201", "203", "204", "205", "206", "208", "212", "216", "217", "218", "220", "222", "223", "227", "229", "231", "233", "235", "236", "237", "242", "249", "254", "260", "261", "262", "263", "266", "269", "275", "276", "274", "278", "279", "288", "289", "310", "324", "326", "327", "331", "334", "333", "332", "335", "336", "342", "344", "346", "347", "349", "356", "357", "370", "371", "353", "354", "360", "372", "386", "389", "390", "391", "392", "393", "394", "395", "404", "405", "408", "410") #150 topt datasets

irregular <- c("337", "307","160", "387", "308", "250", "377277",  "297", "306", "293", "293", "283", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "13", "14", "17", "21", "23", "24", "32", "34", "35", "36", "38", "39", "40", "41", "46", "48", "49", "50", "54", "55", "56", "57", "60", "64", "68", "70", "71", "79", "84", "90", "92", "93", "95", "99", "100", "101", "103", "105", "107", "111", "112", "113", "115", "116", "117", "118", "119", "120", "121", "122", "124", "125", "126", "127", "131", "132", "133", "134", "135", "136", "144", "145", "146", "149", "150", "152", "153", "154", "155", "139", "317", "328", "388", "407", "157", "162", "166", "172", "175", "176", "12", "66", "141", "156", "177", "178", "188", "192", "193", "202", "207", "209" ,"210", "213", "214", "215", "224", "238", "239", "240", "241", "243", "244", "245", "246", "247", "248", "251", "256", "258", "259", "264", "268", "271", "272", "273", "280", "281", "282", "284", "285", "286", "287", "290", "291", "292", "315", "316", "318", "319", "320", "321", "322", "323", "330", "294", "338", "340", "341", "343", "345","348", "352", "374", "375", "363", "373", "376", "378", "379", "380", "381", "384", "396", "398", "399", "406", "423", "425", "427", "397") #179 with no extractable parameters -- some could be good with linear fit

#### FIRST MODEL FILTER: models that predict outside of 1sd of the data, happens to all dataset categories ####
curves_sd <- curves %>%
  group_by(curve_ID) %>%
  mutate(sd_response = sd(response_value, na.rm = TRUE),
         min_1sd = min(response_value, na.rm = TRUE) - sd_response,
         max_1sd = max(response_value, na.rm. = TRUE) + sd_response) %>%
  ungroup() %>%
  mutate(curve_ID = as.numeric(curve_ID)) %>%
  select(curve_ID, response_value, test_temp, sd_response, max_1sd, min_1sd) 
#Attach bounds to fitted data ###
model_preds_with_bounds <- model_preds %>%
  left_join(
    curves_sd %>% distinct(curve_ID, response_value, test_temp, sd_response, max_1sd, min_1sd),
    by = "curve_ID"
  )
#filter valid models within 1 SD of raw data and get valid models/preds ###
valid_models <- model_preds_with_bounds %>%
  group_by(curve_ID, model) %>%
  summarise(valid = all(.fitted >= min_1sd & .fitted <= max_1sd), .groups = "drop") %>%
  filter(valid) %>%
  select(-valid)
valid_preds <- model_preds %>%
  semi_join(valid_models, by = c("curve_ID", "model"))

unique(valid_preds$curve_ID) #still have all of the datasets

#### SECOND MODEL FILTER: out-of-bounds ctmins and maxes, can only do this for full breadth curves ####
# first need to join the params with the preds # 
valid_preds_with_params <- valid_preds %>%
  left_join(params, join_by(curve_ID, model))
valid_preds_with_params <- valid_preds_with_params %>%
  group_by(curve_ID, model) %>%
  mutate(min_temp = min(test_temp)) %>%
  mutate(max_temp = max(test_temp)) %>%
  mutate(poor_ctmax = ifelse(abs(ctmax - max_temp) > 10,
                             "yes",
                             "no")) %>%
  mutate(poor_ctmin = ifelse(abs(ctmin - min_temp) > 10, 
                             "yes",
                             "no"))
#### breadth curve model over prediction filter ####
breadth_valid_preds_with_params <- valid_preds_with_params %>%
  filter(curve_ID %in% breadth_curves) 
length(unique(breadth_valid_preds_with_params$curve_ID)) #30
breadth_valid_preds_with_params <- breadth_valid_preds_with_params %>%
  filter(poor_ctmax == "no") %>%
  filter(poor_ctmin == "no")
length(unique(breadth_valid_preds_with_params$curve_ID)) #30

saveRDS(breadth_valid_preds_with_params, here('processed-data', 'full_curve_filtered_model_params_and_preds.RDS'))



responses <- curves %>%
  select(curve_ID, response_type, response_unit) %>%
  distinct()
curve_labels <- responses %>%
  mutate(label = paste0(response_type, " (", curve_ID, ")")) %>%
  select(curve_ID, label) %>%
  deframe()
library(ggforce)
#breadth?
ggplot() +
  geom_point(data = curves %>%
               filter(curve_ID %in% breadth_curves),
             aes(x = test_temp, y = response_value)) +
  geom_point(data = valid_preds_with_params %>%
               filter(curve_ID %in% breadth_curves),
             aes(x = ctmax, y = y_value_ctmax, color = model)) +
  geom_point(data = valid_preds_with_params %>%
               filter(curve_ID %in% breadth_curves),
             aes(x = topt, y = y_value_topt, color = model)) +
  geom_point(data = valid_preds_with_params %>%
             filter(curve_ID %in% breadth_curves),
           aes(x = ctmin, y = y_value_ctmin, color = model))+
  geom_line(data = valid_preds_with_params %>%
              filter(curve_ID %in% breadth_curves), 
            aes(x = test_temp, y = .fitted, color = model), linewidth = .2) +
  facet_wrap_paginate(~curve_ID, scales = "free", ncol = 4, nrow = 4, page =2,
                      labeller = labeller(curve_ID = curve_labels)) +
  scale_color_manual(
    values = c(
      "johnsonlewin" = "slateblue", 
      "lactin2" = "#4DAF4A",  
      "oneill"= "magenta", 
      "ratkowsky" = "yellow",  
      "rezende" = "#A65628",  
      "spain" = "royalblue3",  
      "thomas" = "#999999",  
      "weibull" = "black"  ,
      "hinshelwood" = "aquamarine",
      "briere" = "lightblue", 
      "gaussian" = "maroon",
      "quadratic" = "green"
    )
  ) +
  theme_minimal() +
  labs(x = "Test Temperature", y = "Response", color = "Model")






responses <- curves %>%
  select(curve_ID, response_type, response_unit) %>%
  distinct()
curve_labels <- responses %>%
  mutate(label = paste0(response_type, " (", curve_ID, ")")) %>%
  select(curve_ID, label) %>%
  deframe()
library(ggforce)
#breadth?
ggplot() +
  geom_point(data = raw_data_with_params %>%
               filter(ctmax_valid == "yes") %>%
               filter(ctmin_valid == "no"),
             aes(x = test_temp, y = response_value)) +
  geom_point(data = top_3_models %>%
               filter(ctmax_valid == "yes") %>%
               filter(ctmin_valid == "no"),
             aes(x = CTmax, y = y_value_ctmax, color = model)) +
  geom_point(data = top_3_models %>%
               filter(ctmax_valid == "yes") %>%
               filter(ctmin_valid == "no"),
             aes(x = Topt, y = y_value_topt, color = model)) +
  geom_line(data = top_3_models %>%
              filter(ctmax_valid == "yes") %>%
              filter(ctmin_valid == "no") %>%
              filter(best_mod == "no"), 
            aes(x = test_temp, y = .fitted, color = model), linewidth = .2) +
  geom_line(data = top_3_models %>%
              filter(ctmax_valid == "yes") %>%
              filter(ctmin_valid == "no") %>%
              filter(best_mod == "yes"), 
            aes(x = test_temp, y = .fitted, color = model), linewidth = 1) +
  facet_wrap_paginate(~curve_ID, scales = "free", ncol = 4, nrow = 4, page =1,
                      labeller = labeller(curve_ID = curve_labels)) +
  scale_color_manual(
    values = c(
      "johnsonlewin" = "slateblue", 
      "lactin2" = "#4DAF4A",  
      "oneill"= "magenta", 
      "ratkowsky" = "yellow",  
      "rezende" = "#A65628",  
      "spain" = "royalblue3",  
      "thomas" = "#999999",  
      "weibull" = "black"  ,
      "hinshelwood" = "aquamarine",
      "briere" = "lightblue", 
      "gaussian" = "maroon",
      "quadratic" = "green"
    )
  ) +
  theme_minimal() +
  labs(x = "Test Temperature", y = "Response", color = "Model")


ggplot() +
  geom_point(data = raw_data_with_params %>%
               filter(curve_ID %in% breadth_curves),
             aes(x = test_temp, y = response_value)) +
   geom_point(data = top_3_models %>%
                filter(curve_ID %in% breadth_curves),
             aes(x = Topt, y = y_value_topt, color = model)) +
  geom_point(data = top_3_models %>%
               filter(curve_ID %in% breadth_curves),
             aes(x = CTmin, y = y_value_ctmin, color = model)) +
  geom_point(data = top_3_models %>%
               filter(curve_ID %in% breadth_curves),
             aes(x = CTmax, y = y_value_ctmax, color = model)) +
  geom_line(data = top_3_models %>%
              filter(curve_ID %in% breadth_curves) %>%
              filter(best_mod == "yes"), 
            aes(x = test_temp, y = .fitted, color = model), linewidth = 1) +
  geom_line(data = top_3_models %>%
              filter(curve_ID %in% breadth_curves) %>%
              filter(best_mod == "no"), 
           aes(x = test_temp, y = .fitted, color = model), linewidth = .2) +
  facet_wrap_paginate(~curve_ID, scales = "free", ncol = 4, nrow = 4, page = 2,
                      labeller = labeller(curve_ID = curve_labels)) +
  scale_color_manual(
    values = c(
      "johnsonlewin" = "slateblue", 
      "lactin2" = "#4DAF4A",  
      "oneill"= "magenta", 
      "ratkowsky" = "yellow",  
      "rezende" = "#A65628",  
      "spain" = "royalblue3",  
      "thomas" = "#999999",  
      "weibull" = "black"  ,
      "hinshelwood" = "aquamarine",
      "briere" = "lightblue", 
      "gaussian" = "maroon",
      "quadratic" = "green"
    )
  ) +
  theme_minimal() +
  labs(x = "Test Temperature", y = "Response", color = "Model")


##inspect flinn and oneil

curves_breadth <- curves %>%
  filter(curve_ID %in% breadth_curves)
curves_breadth_fres <- curves_breadth %>%
  filter(habitat_water == "freshwater")
length(unique(curves_breadth_fres$curve_ID)) #12
curves_breadth_marine <- curves_breadth %>%
  filter(habitat_water == "marine")
length(unique(curves_breadth_marine$curve_ID)) #12
curves_breadth_brackish <- curves_breadth %>%
  filter(habitat_water == "brackish")
length(unique(curves_breadth_brackish$curve_ID)) #4


breadth_curves <- c("18", "59", "78", "173", "226", "228", "232", "234", "265", "267", "325", "366", "367", "368", "369", "401", "402", "403", "416", "359", "253", "20", "412", "309", "311", "270", "226", "351", "409", "74", "75")

#has ctmin, ctmax, and topt (so don't overlap in the other datasets)

ctmin <- c("22", "11", "51", "52", "58", "62", "53", "67", "87", "88", "123", "148", "159", "163", "128", "295", "296", "298", "299", "300", "301", "302", "303", "304", "305", "400", "25", "26", "27", "28", "29", "30", "73", "82", "158", "137", "396", "411") #41

ctmin_and_topt <- c("257", "364", "365", "385", "81", "86", "31", "33", "37", "221", "219", "312", "313", "314") #13

ctmax <- c("138", "140", "414", "417") #6

ctmax_and_topt <- c("225", "230", "382", "252", "383", "419", "362", "361") #7

topt <- c("413", "418", "415", "350", "171", "174", "184", "211", "355", "358", "15", "16", "19", "44", "47", "61", "63", "65", "69", "72", "77", "80", "83", "85", "89", "91", "94", "96", "97", "98", "102", "104", "106", "108", "109", "110", "114", "129", "130", "142",  "143", "147", "151", "164", "167", "168", "165", "170","169", "76", "161", "179", "180", "181", "182", "183", "185", "186", "187", "189", "190", "191", "194", "195", "196", "197", "198", "199", "200", "201", "203", "204", "205", "206", "208", "212", "216", "217", "218", "220", "222", "223", "227", "229", "231", "233", "235", "236", "237", "242", "249", "254", "260", "261", "262", "263", "266", "269", "275", "276", "274", "278", "279", "288", "289", "310", "324", "326", "327", "331", "334", "333", "332", "335", "336", "342", "344", "346", "347", "349", "356", "357", "370", "371", "353", "354", "360", "372", "377", "386", "389", "390", "391", "392", "393", "394", "395", "404", "405", "408", "410", "12") #150
irregular <- c("255", "307", "45", "339", "43","329","42", "387", "308", "297", "306", "293", "293", "283", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "13", "14", "17", "21", "23", "24", "32", "34", "35", "36", "38", "39", "40", "41", "46", "48", "49", "50", "54", "55", "56", "57", "60", "64", "68", "70", "71", "79", "84", "90", "92", "93", "95", "99", "100", "101", "103", "105", "107", "111", "112", "113", "115", "116", "117", "118", "119", "120", "121", "122", "124", "125", "126", "127", "131", "132", "133", "134", "135", "136", "144", "145", "146", "149", "150", "152", "153", "154", "155", "139", "317", "328", "388", "407", "157", "162", "166", "172", "175", "176", "12", "66", "141", "156", "177", "178", "188", "192", "193", "202", "207", "209" ,"210", "213", "214", "215", "224", "238", "239", "240", "241", "243", "244", "245", "246", "247", "248", "251", "256", "258", "259", "264", "268", "271", "272", "273", "280", "281", "282", "284", "285", "286", "287", "290", "291", "292", "315", "316", "318", "319", "320", "321", "322", "323", "330", "294", "338", "340", "341", "343", "345","348", "352", "374", "375", "363", "373", "376", "378", "379", "380", "381", "384", "396", "398", "399", "406", "423", "425", "427", "397")

all <- c(breadth_curves, ctmin, ctmin_and_topt, ctmax, ctmax_and_topt, topt, irregular)
length(unique(all))

all <- as.numeric(all)
not_yet <- setdiff(curve_IDs, all)
print(not_yet)



####filtering datasets to how much of a curve they cover (curve coverage?), using topt####
#add topt to highrescurvedf so we can filter with it
topt <- all_params_deutsch_2008_highres %>%
  select(topt, curve_ID)
high_res_ds_fitted <- left_join(high_res_ds, topt, join_by(curve_ID))

#### plot some with raw data, fitted data, and params to see ####
ggplot() + 
  geom_point(
    data = high_res_ds_fitted %>% filter(curve_ID == "332"), 
    aes(x = test_temp, y = response_value)) +
  geom_line(data = all_preds_deutsch_2008_highres %>% filter(curve_ID == "332"),
            aes(x = test_temp, y = .fitted), 
            linewidth = 1) +
  geom_point(data = all_param_points_deutsch_2008_highres %>% filter(curve_ID == "332"),
             aes(x = test_temp, y = y_value),
             size = 3, color = "red")


#### overlaying some quality deutsch curves ####
#filter to those where RSS is less than 0.01 (ask jenn about this, not sure if this is arbitrary)
good_fit_deutsch <- rss_deutsch_2008_highres %>%
  filter(RSS <= 0.01) %>%
  filter(!curve_ID %in% c("223", "210", "89", "332")) %>% #datasets where deutsch brings the curve down / fits really weird because the equation needs a ctmax
  pull(curve_ID) #25


good_fits_deutsch_preds <- all_preds_deutsch_2008_highres %>%
  filter(curve_ID %in% good_fit_deutsch)
length(unique(good_fits_deutsch_preds$curve_ID)) #25
topt_deutsch_2008 <- all_params_deutsch_2008_highres %>%
  select(topt, curve_ID)
good_fits_deutsch_preds <- left_join(good_fits_deutsch_preds, topt_deutsch_2008, join_by(curve_ID))
length(unique(good_fits_deutsch_preds$curve_ID)) #25
info <- curves %>%
  select(curve_ID, curve_type, latitude, longitude, response_type_group, response_type)
good_fits_deutsch_preds <- left_join(good_fits_deutsch_preds, info, join_by(curve_ID))
#normalize resp
good_fits_deutsch_preds <- good_fits_deutsch_preds %>%
  group_by(curve_ID) %>%
  mutate(response_scaled = .fitted / max(.fitted)) %>%
  ungroup()
length(unique(good_fits_deutsch_preds$curve_ID)) #25
#plot
ggplot(good_fits_preds, aes(x = test_temp, y = response_scaled, color = response_type_group, group = curve_ID)) +
  geom_line(linewidth = .5) +
  theme_minimal() +
  labs(x = "Temperature (C)",
       y = "Normalized Response",
       title = "deutsch 5+ good RSS curves") +
  theme(legend.position = "bottom")
#25 curves (out of the 200...)


ggplot() + 
  geom_point(
    data = low_res_ds %>% filter(curve_ID == "371"), 
    aes(x = test_temp, y = response_value)) +
  geom_line(data = all_preds_flinn_1991_lowres %>% filter(curve_ID == "371"),
            aes(x = test_temp, y = .fitted), 
            linewidth = 1) +
  geom_point(data = all_param_points_flinn_1991_lowres %>% filter(curve_ID == "371"),
             aes(x = test_temp, y = y_value),
             size = 3, color = "red")
#### combine RSS for 3 parameter models to see which model is best for each curve ####
rss_4_param_models <- left_join(rss_flinn_1991_lowres, rss_quadratic_2008_lowres, by = "curve_ID")
rss_4_param_models <- left_join(rss_4_param_models, rss_gaussian_1987_lowres, by = "curve_ID")
rss_4_param_models <- rss_4_param_models %>%
  mutate(best_model = names(select(., RSS_flinn:RSS_gaussian))[max.col(-select(., RSS_flinn:RSS_gaussian))])
flinn_bestrss <- rss_4_param_models %>%
  filter(best_model == "RSS_flinn")
gaussian_bestrss <- rss_4_param_models %>%
  filter(best_model == "RSS_gaussian")
quadratic_bestrss <- rss_4_param_models %>%
  filter(best_model == "RSS_quadratic")
print(unique(quadratic_bestrss$curve_ID))

ggplot() + 
  geom_point(
    data = curves %>% filter(curve_ID == "11"), 
    aes(x = test_temp, y = response_value)) +
  geom_line(data = all_preds_oneill_1972_highres %>% filter(curve_ID == "11"),
            aes(x = test_temp, y = .fitted), 
            linewidth = 1)


good_fit_deutsch_curve_IDs <- c("351")
good_fit_briere2_curve_IDs <- "311"
good_fit_quadratic_curve_IDs <- c("206", "208", "198", "236","269","390", '392', "392")
good_fit_gaussian_curve_IDs <- c("285","288", "289", "246", "393", "194","397", "377", "391", "33", "267", "276", "249","229", "180","179","174", "362","361","205", "370", "130", "173", "371", "18", "163", "99", "69", "67", "98", "108", "96", "106", "104", "37")
good_fit_flinn_curve_IDs <- c("319", "324", "344", "372", "15", "16", "44", "45", "47", "42", "43", "97", "102", "94", "114", "109", "110", "129", "147", "151", "204", "203", "196", "195", "200", "197", "201", "31", "216", "237", "235", "233", "279", "272", "274", "296", "394", "395", "413")
not_opt_datasets <- curves %>%
  filter(!(curve_ID %in% c(
    good_fit_quadratic_curve_IDs,
    good_fit_gaussian_curve_IDs,
    good_fit_flinn_curve_IDs,
    good_fit_deutsch
  )))
###flinn###
good_fits_flinn_1991_preds <- all_preds_flinn_1991_lowres %>%
  filter(curve_ID %in% good_fit_flinn_curve_IDs)
length(unique(good_fits_flinn_1991_preds$curve_ID)) #39
topt_flinn <- all_params_flinn_1991_lowres %>%
  select(topt, curve_ID)
good_fits_flinn_1991_preds <- left_join(good_fits_flinn_1991_preds, topt_flinn, join_by(curve_ID))
info <- curves %>%
  select(curve_ID, curve_type, latitude, longitude, response_type_group, response_type)
good_fits_flinn_1991_preds <- left_join(good_fits_flinn_1991_preds, info, join_by(curve_ID))
good_fits_flinn_1991_preds <- good_fits_flinn_1991_preds %>%
  group_by(curve_ID) %>%
  mutate(response_scaled = .fitted / max(.fitted)) %>%
  ungroup()
length(unique(good_fits_flinn_1991_preds$curve_ID)) #39

###gaus###
good_fits_gaussian_preds <- all_preds_gaussian_1987_lowres %>%
  filter(curve_ID %in% good_fit_gaussian_curve_IDs)
length(unique(good_fits_gaussian_preds$curve_ID)) #35
topt_gaus <- all_params_gaussian_1987_lowres %>%
  select(topt, curve_ID)
good_fits_gaussian_preds <- left_join(good_fits_gaussian_preds, topt_gaus, join_by(curve_ID))
info <- curves %>%
  select(curve_ID, curve_type, latitude, longitude, response_type_group, response_type)
good_fits_gaussian_preds <- left_join(good_fits_gaussian_preds, info, join_by(curve_ID))
good_fits_gaussian_preds <- good_fits_gaussian_preds %>%
  group_by(curve_ID) %>%
  mutate(response_scaled = .fitted / max(.fitted)) %>%
  ungroup()
length(unique(good_fits_gaussian_preds$curve_ID)) #35

###quadratic###
good_fits_quadratic_preds <- all_preds_quadratic_2008_lowres %>%
  filter(curve_ID %in% good_fit_quadratic_curve_IDs)
topt_quad <- all_params_quadratic_2008_lowres %>%
  select(topt, curve_ID)
good_fits_quadratic_preds <- left_join(good_fits_quadratic_preds, topt_quad, join_by(curve_ID))
info <- curves %>%
  select(curve_ID, curve_type, latitude, longitude, response_type_group, response_type)
good_fits_quadratic_preds <- left_join(good_fits_quadratic_preds, info, join_by(curve_ID))
good_fits_quadratic_preds <- good_fits_quadratic_preds %>%
  group_by(curve_ID) %>%
  mutate(response_scaled = .fitted / max(.fitted)) %>%
  ungroup()
length(unique(good_fits_quadratic_preds$curve_ID)) #7

ggplot(good_fits_gaussian_preds, aes(x = test_temp, y = response_scaled, color = response_type_group, group = curve_ID)) +
  geom_line(linewidth = .5) +
  theme_minimal() +
  labs(x = "Temperature (C)",
       y = "Normalized Response",
       title = "3 param gaus") +
  theme(legend.position = "bottom")



ggplot(good_fits_quadratic_preds, aes(x = test_temp, y = response_scaled, color = response_type_group, group = curve_ID)) +
  geom_line(linewidth = .5) +
  theme_minimal() +
  labs(x = "Temperature (C)",
       y = "Normalized Response",
       title = "3 param quad") +
  theme(legend.position = "bottom")

good_fits_deutsch_preds <- good_fits_preds %>%
  mutate(model_name = "deutsch")
good_fits_flinn_1991_preds <- good_fits_flinn_1991_preds %>%
  mutate(model_name = "flinn")
good_fits_gaussian_preds <- good_fits_gaussian_preds %>%
  mutate(model_name = "gaussian")
good_fits_quadratic_preds <- good_fits_quadratic_preds %>%
  mutate(model_name = "quadratic")
all_preds <- rbind(good_fits_quadratic_preds, good_fits_deutsch_preds, good_fits_flinn_1991_preds, good_fits_gaussian_preds)

ggplot(all_preds, aes(x = test_temp, y = response_scaled, color = response_type_group, group = curve_ID)) +
  geom_line(linewidth = .35) +
  theme_minimal() +
  labs(x = "Temperature (C)",
       y = "Normalized Response",
       title = "Overlaid TPCs") +
  scale_color_manual(
    values = c(
      "metabolism" = "orange",        
      "feeding" = "lightpink",   
      "growth" = "springgreen3", 
      "swimming"  = "royalblue",
      "reproduction" = "purple3",
      "survival" = "red",
      "predation" = "black"
    )
  ) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  theme(legend.position = "bottom")

length(unique(all_preds$curve_ID)) #106
#make some sort of codified way to indicate if a dataset has min, max, opt, breadth, etc to indicate curve fullness
fits <- all_preds %>%
  select(curve_ID, topt, curve_type, latitude, longitude, response_type_group, response_type, model_name ) %>%
  distinct() %>%
  mutate(abs_latitude = abs(latitude))
ggplot(fits, aes(x = abs_latitude, y = topt, color = response_type_group)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Absolute Latitude",
    y = "Thermal Optimum",
    title = "Scatter of Topt and Absolute Latitude"
  ) +
  scale_color_manual(
    values = c(
      "metabolism" = "orange",        
      "feeding" = "lightpink",   
      "growth" = "springgreen3", 
      "swimming"  = "royalblue",
      "reproduction" = "purple3",
      "survival" = "red",
      "predation" = "black"
    )
  )
theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())


regression <- lm(topt ~ abs_latitude, data = fits)
summary(regression)
summary(regression)$r.squared


####violin plot of tpc topt on y and dif response types on x ####
library(hrbrthemes)
install.packages("viridis")
library(viridis)
sample <- fits_unique %>% group_by(response_type_group) %>% summarize(num=n())
fits_unique %>%
  left_join(sample) %>%
  mutate(myaxis = paste0(response_type_group, "\n", "n=", num)) %>%
  ggplot(aes(x = myaxis, y = topt, fill = response_type_group)) +
  geom_violin(width = 1.4) + 
  geom_boxplot(width = .1, color = "grey", alpha = .2) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11)) +
  ggtitle("Violin plot of thermal optimums for each response group") +
  xlab("")

#only feeding, growth, metabolism, and swimming
fits_filtered <- fits_unique %>%
  filter(response_type_group %in% c("feeding", "growth", "metabolism", "swimming"))
sample <- fits_filtered %>% group_by(response_type_group) %>% summarize(num=n())
fits_filtered %>%
  left_join(sample) %>%
  mutate(myaxis = paste0(response_type_group, "\n", "n=", num)) %>%
  ggplot(aes(x = myaxis, y = topt, fill = response_type_group)) +
  geom_violin(width = 1.4) + 
  geom_boxplot(width = .1, color = "grey", alpha = .2) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11)) +
  ggtitle("Violin plot of thermal optimums for each response group") +
  xlab("")

sample <- fits_filtered %>% group_by(response_type_group) %>% summarize(num=n())
fits_filtered %>%
  left_join(sample) %>%
  mutate(
    response_type_group = factor(response_type_group,
                                 levels = c("metabolism", "feeding", "growth", "swimming")),
    myaxis = paste0(response_type_group, "\n", "n=", num)
  ) %>%
  ggplot(aes(x = myaxis, y = topt, fill = response_type_group)) +
  geom_violin(trim = FALSE, width = 0.9, alpha = 0.8, color = "black") +
  geom_boxplot(width = 0.15, color = "black", outlier.shape = NA, alpha = 0.3) +
  geom_jitter(width = 0.15, alpha = 0.5, size = 1, color = "black") +
  
  scale_fill_manual(values = c("metabolism" = "#F4D03F",
                               "feeding" = "#1ABC9C",
                               "growth" = "#7D3C98",
                               "swimming" = "blue")) +
  
  labs(
    x = "Response type",
    y = expression(T[opt]~"(°C)"),
    title = "Thermal optima across physiological performance"
  ) +
  theme_ipsum(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )






habitat <- curves %>%
  select(curve_ID, habitat, habitat_water)

fits <- fits %>%
  left_join(habitat, join_by(curve_ID))
fits_unique <- fits %>%
  group_by(curve_ID) %>%
  distinct()
sample <- fits_unique %>% group_by(habitat_water) %>% summarize(num=n())
fits_unique %>%
  left_join(sample) %>%
  mutate(
    habitat_water = factor(habitat_water,
                           levels = c("marine", "freshwater", "brackish")),
    myaxis = paste0(habitat_water, "\n", "n=", num)
  ) %>%
  ggplot(aes(x = myaxis, y = topt, fill = habitat_water)) +
  geom_violin(trim = FALSE, width = 0.9, alpha = 0.8, color = "black") +
  geom_boxplot(width = 0.15, color = "black", outlier.shape = NA, alpha = 0.3) +
  geom_jitter(width = 0.15, alpha = 0.5, size = 1, color = "black") +
  
  scale_fill_manual(values = c("marine" = "#F4D03F",
                               "freshwater" = "#1ABC9C",
                               "brackish" = "#7D3C98")) +
  
  labs(
    x = "Habitat type",
    y = expression(T[opt]~"(°C)"),
    title = "Thermal optima across aquatic habitats"
  ) +
  theme_ipsum(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )
print(unique(fits_unique$habitat))
sample_hab <- fits_unique %>% group_by(habitat) %>% summarize(num=n())



#one point to the right of topt by a degree or two that is lower than topt and one point to the left of topt that the response value is lower than topt

#classify what side of the curve a point is on
curves <- curves %>%
  group_by(curve_ID) %>%
  mutate(
    side = case_when(
      test_temp < topt ~ "increasing",
      test_temp > topt ~ "decreasing",
      TRUE ~ "opt"
    ),
    n_increasing = sum(side == "increasing"),
    n_decreasing = sum(side == "decreasing"),
    curve_symmetry = case_when(
      n_increasing >= 2 & n_decreasing >= 2 ~ "full curve",
      n_increasing >= 1 & n_decreasing >= 1 ~ "full curve_ish",
      n_increasing > 0 & n_decreasing == 0 ~ "increasing only",
      n_increasing == 0 & n_decreasing > 0 ~ "decreasing only",
      TRUE ~ "incomplete"
    )
  ) %>%
  ungroup()

increasing_side <- high_res_ds_fitted %>%
  filter(curve_symmetry == "increasing only")
length(unique(increasing_side$curve_ID)) #27
decreasing_side <- high_res_ds_fitted %>%
  filter(curve_symmetry == "decreasing only")
print(unique(decreasing_side$curve_ID)) #5
full_curves <- high_res_ds_fitted %>%
  filter(curve_symmetry == "full curve")
print(unique(full_curves$curve_ID)) #120
full_curves_ish <- high_res_ds_fitted %>%
  filter(curve_symmetry == "full curve_ish")
length(unique(full_curves_ish$curve_ID)) #16








##this didt work and the flinn model wasnt captured

#sort - after join with valid preds -- group by curve_id and within that sort ot find best model
rss_5_param_models <- rss_5_param_models %>%
  mutate(best_model = pmap_chr(select(., deutsch_rss:weibull_rss),
                               ~ names(list(...))[which.min(c(...))]))
best_models <- rss_5_param_models %>%
  select(curve_ID, best_model) %>%
  mutate(best_model = gsub("_rss$", "", best_model))


#top 3 models for each curve
rss_top3_long <- rss_5_param_models %>%
  pivot_longer(cols = deutsch_rss:weibull_rss,
               names_to = "model",
               values_to = "rss") %>%
  group_by(curve_ID) %>%
  arrange(rss, .by_group = TRUE) %>%
  slice_head(n = 3) %>%
  ungroup() %>%
  mutate(curve_ID = as.numeric(curve_ID))
rss_top3_long <- rss_top3_long %>%
  mutate(model = gsub("_rss$", "", model))





all_preds_top3 <- all_preds_long %>%
  inner_join(rss_top3_long %>% select(curve_ID, model), by = c("curve_ID", "model"))
all_preds_long_best <- all_preds_long %>%
  semi_join(best_models, by = c("curve_ID", "model" = "best_model"))
all_preds_top_3_metabolism <- all_preds_top3 %>%
  filter(response_type_group == "metabolism")
length(unique(high_res_ds_metabolism$curve_ID)) #44
high_res_ds_metabolism <- high_res_ds %>%
  filter(response_type_group == "metabolism")

all_preds_top_3_growth <- all_preds_top3 %>%
  filter(response_type_group == "growth")
length(unique(high_res_ds_growth$curve_ID)) #63
high_res_ds_growth <- high_res_ds %>% 
  filter(response_type_group == "growth")

###can i test if the relationsihp is mostly latitudinally driver? or if also by water type?


full_curves <- curves %>%
  filter(curve_ID %in% breadth_curves)
full_models <- filtered %>%
  filter(curve_ID %in% breadth_curves)

saveRDS(full_curves, here("processed-data", "full_curves.RDS"))
saveRDS(full_models, here("processed-data", "full_models.RDS"))
