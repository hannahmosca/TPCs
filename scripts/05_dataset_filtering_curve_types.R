#### this script is to subset the data into curve types ####
rm(list=ls())
curves <- readRDS(here('processed-data', 'wild-tpcs.RdS'))

#### 01.normalize all of the datasets so can work with scaled values ####
data_scaled <- curves %>%
  select(curve_ID, test_temp, response_value, response_type, response_unit) %>%
  group_by(curve_ID, test_temp) %>%
  mutate(mean_response = mean(response_value, na.rm = TRUE)) %>%  # mean at each temp, handles ind response curves
  ungroup() %>%
  group_by(curve_ID) %>%
  mutate(response_scaled = mean_response / max(mean_response, na.rm = TRUE)) %>%  # scale within curve
  ungroup()
#### 02. add columns for datasets that are left bounded, right bounded, and reach an optimum ####

#optimum: curves that have a max response sandwiched by responses that are less on both sides ...ie go up and come down

# The values rise before the peak.
# The values fall after the peak.
# The peak is not at the edges.

data_scaled <- data_scaled %>%
  group_by(curve_ID) %>%
  arrange(test_temp) %>%
  mutate(
    peak_pos = which.max(response_scaled),
    has_optimum = peak_pos > 1 & peak_pos < n()
  ) %>%
  ungroup()

# 2. Compute left and right bounds
data_scaled <- data_scaled %>%
  group_by(curve_ID) %>%
  arrange(test_temp) %>%
  mutate(
    first_temp = first(test_temp),
    first_response = first(response_scaled),
    left_bound  = ifelse(first_response <= 0.10, "yes", "no"),
    last_temp = last(test_temp),
    last_response = last(response_scaled),
    right_bound = ifelse(last_response <= 0.10, "yes", "no")
  ) %>%
  ungroup()

ctmin <- data_scaled %>%
  filter(left_bound == "yes") %>%
  filter(has_optimum == FALSE)
ctmin_auto_curves <- unique(ctmin$curve_ID)

only_in_auto <- setdiff(ctmin_auto_curves, ctmin_vis)
only_in_vis <- setdiff(ctmin_vis, ctmin_auto_curves)
in_both <- intersect(ctmin_auto_curves, ctmin_vis)

ctmin_and_opt <- data_scaled %>%
  filter(left_bound == "yes") %>%
  filter(right_bound == "no") %>%
  filter(has_optimum = TRUE)
ctmin_and_opt_auto_curves <- unique(ctmin_and_opt$curve_ID)

only_in_auto <- setdiff(ctmin_and_opt_auto_curves, ctmin_and_topt_vis)
only_in_vis <- setdiff(ctmin_and_topt_vis, ctmin_and_opt_auto_curves)
in_both <- intersect(ctmin_and_opt_auto_curves, ctmin_and_topt_vis)

ctmax <- data_scaled %>%
  filter(right_bound == "yes") %>%
  filter(has_optimum = FALSE)
ctmax_auto_curves <- unique(ctmax$curve_ID)

ctmax_and_opt <- data_scaled %>%
  filter(right_bound == "yes") %>%
  filter(left_bound == "no")
  filter(has_optimum = TRUE)
ctmax_and_opt_auto_curves <- unique(ctmax_and_opt$curve_ID)

topt <- data_scaled %>%
  filter(right_bound == "no") %>%
  filter(left_bound == "no") %>%
  filter(has_optimum == TRUE)
topt_auto_curves <- unique(topt$curve_ID)

breadth <- data_scaled %>%
  filter(right_bound == "yes") %>%
  filter(left_bound == "yes") %>%
  filter(has_optimum == TRUE)
breadth_auto_curves <- unique(breadth$curve_ID)

irregular <- data_scaled %>%
  filter(right_bound == "no") %>%
  filter(left_bound == "no") %>%
  filter(has_optimum == FALSE)
irregular_auto_curves <- unique(irregular$curve_ID)

vis_topt <- c(topt, ctmin_and_topt, ctmax_and_topt, breadth_curves)

# in filtered_curves but not in vis_topt
only_in_filtered <- setdiff(data_filter_topt, vis_topt)

# in vis_topt but not in filtered_curves
only_in_vis <- setdiff(vis_topt, data_filter_topt)
# the only ones that filter auto didnt capture were the ind ones like 385 and 386, doesn't seem to know what to do with the ind. response ones

# in both
in_both <- intersect(data_filter_topt, vis_topt)

only_in_filtered  # see the IDs
only_in_vis

responses <- curves %>%
  select(curve_ID, response_type, response_unit) %>%
  distinct()
curve_labels <- responses %>%
  mutate(label = paste0(response_type, " (", curve_ID, ")")) %>%
  select(curve_ID, label) %>%
  deframe()

ggplot() +
  geom_point(data = data_scaled %>%
               filter(curve_ID %in% breadth_curves_vis),
             aes(x = test_temp, y = mean_response)) +
  facet_wrap_paginate(~curve_ID, scales = "free", ncol = 4, nrow = 4, page = 2,
                      labeller = labeller(curve_ID = curve_labels)) +
  theme_minimal() +
  labs(x = "Test Temperature", y = "Response", color = "Model")

##visual ones


breadth_curves_vis <- c("18", "59", "173", "226", "228", "232", "234", "265", "267", "325", "366", "367", "368", "369", "401", "402", "403", "416", "359", "253", "20", "412", "309", "311", "270", "226", "351", "409", "74", "75")


## the just ctmins are somewhat finalized ## 
ctmin_vis <- c("32", "257", "87", "88", "148", "159", "295", "296", "298", "299", "300", "301", "302", "303", "304", "305", "26", "27", "28", "29", "30", "158")

ctmin_and_topt_vis <- c("77", "11", "22","364", "365", "86", "31", "33", "37", "312", "313", "314", "67") #13

ctmax_vis <- c("138", "140", "414", "417", "53", "51", "62", "137") #6

ctmax_and_topt_vis <- c("225", "230", "382", "252", "383", "419", "362", "361") #7

topt_vis <- c("163", "385", "221", "219","25", "58", "413", "418", "415", "350", "171", "174", "184", "211", "355", "358", "15", "16", "19", "44", "47", "61", "63", "65", "69", "72", "80", "83", "85", "89", "91", "94", "96", "97", "98", "102", "104", "106", "108", "109", "110", "114", "129", "130", "142",  "143", "147", "151", "164", "167", "168", "165", "170","169", "76", "161", "179", "180", "181", "182", "183", "185", "186", "187", "189", "190", "191", "194", "195", "196", "197", "198", "199", "200", "201", "203", "204", "205", "206", "208", "212", "216", "217", "218", "220", "222", "223", "227", "229", "231", "233", "235", "236", "237", "242", "249", "254", "260", "261", "262", "263", "266", "269", "275", "276", "274", "278", "279", "288", "289", "310", "324", "326", "327", "331", "334", "333", "332", "335", "336", "342", "344", "346", "347", "349", "356", "357", "370", "371", "353", "354", "360", "372", "377", "386", "389", "390", "391", "392", "393", "394", "395", "404", "405", "408", "410", "12") #150
irregular_vis <- c("411", "52", "255", "307", "45", "339", "43","329","42", "387", "308", "297", "306", "293", "293", "283", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "13", "14", "17", "21", "23", "24", "32", "34", "35", "36", "38", "39", "40", "41", "46", "48", "49", "50", "54", "55", "56", "57", "60", "64", "68", "70", "71", "79", "84", "90", "92", "93", "95", "99", "100", "101", "103", "105", "107", "111", "112", "113", "115", "116", "117", "118", "119", "120", "121", "122", "124", "125", "126", "127", "131", "132", "133", "134", "135", "136", "144", "145", "146", "149", "150", "152", "153", "154", "155", "139", "317", "328", "388", "407", "157", "162", "166", "172", "175", "176", "12", "66", "141", "156", "177", "178", "188", "192", "193", "202", "207", "209" ,"210", "213", "214", "215", "224", "238", "239", "240", "241", "243", "244", "245", "246", "247", "248", "251", "256", "258", "259", "264", "268", "271", "272", "273", "280", "281", "282", "284", "285", "286", "287", "290", "291", "292", "315", "316", "318", "319", "320", "321", "322", "323", "330", "294", "338", "340", "341", "343", "345","348", "352", "374", "375", "363", "373", "376", "378", "379", "380", "381", "384", "396", "398", "399", "406", "423", "425", "427", "397", "73", "82", "123", "128", "400", "81", "78")



###for now i am being conservative on what ctmin. and maxes we have--so if want to be more lenient can refilter the topt/irreg categories for thme later


#### curves i am removing from topt ####
irregular: 11, 17, 23, 39, 60, 66, 67, 71, 84, 87, 99, 100, 103, 112, 152, 156, 176, 192, 209, 210, 224, 264, 307, 308, 323, 343, 345, 373, 374,  375, 376, 407, 400, 406

confused: 22, 243, 246, 247, 271, 397
