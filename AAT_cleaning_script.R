library(dplry)

clean_task_data <- function (df){ 
  
task_data <- df |> 
  #takes out unnecessary columns
  select(!(1:10)) |> 
  select(!c(expName, psychopyVersion, frameRate, OS, task_loop.thisRepN, task_loop.thisTrialN, task_loop.thisN, task_loop.thisIndex, task_loop.ran)) |> 
  #filters first 150 trials (last 28 are stimulus ratings)
  filter(trial <= 150) |> 
  #cleans choice data to be in one column vs. 3 different ones
  mutate( choice = case_when(!is.na(`left image outcome` ) ~'left', 
                             !is.na(`middle image outcome`) ~'middle',
                             !is.na(`right image outcome`) ~'right')) |> 
  #filters out rows that do not include current choice, cleans image outcome data to be in one column vs. 3
  filter (!is.na(choice)) |> 
  mutate( image_outcome = case_when (!is.na(`left image outcome` ) ~ `left image outcome`, 
                                     !is.na(`middle image outcome`) ~ `middle image outcome`,
                                     !is.na(`right image outcome`) ~ `right image outcome`)) |> 
  #selects only relevant variables
  select(c(ID, date, trial, 2, 4:6, 46,47)) |> 
  #renames reaction time variable to be appropriate label
  rename(reaction_time = choose_trial_end.rt) |> 
  #lubridate to clean up date
  mutate(date = lubridate::ymd_hms(date))   %>% 
  #identifies whether the task data is complete or not, adds a column to identify completeness
  group_by(ID, date) %>% 
  mutate(n = max(trial)) %>% 
  mutate(complete = ifelse(n == 150, 1, 0)) %>% 
  select(!n) %>% 
  ungroup()  

#cleans up image names to take out file path
task_data$image_outcome <- stringr::str_remove(task_data$image_outcome,'stimuli/')
task_data$image_outcome <- stringr::str_remove(task_data$image_outcome, '.jpg')

return(task_data)
}

#cleans the arousal and valence data

clean_arousal_data <- function (df) {
#picks out only trials > #150 (last 28 trials, which are the valence and arousal ratings)
arousal_data <- df |> 
  filter(trial >150) |> 
  select(c(ID, trial, session, date, 51:58, 64)) |> 
  select (!c(session, valence_end_key.keys, arousal_end_key.keys, valence_end_key.rt, arousal_end_key.rt)) |> 
  mutate(date = lubridate::ymd_hms(date))
#cleans up image names to take out file path
arousal_data$ImageFile <- stringr::str_remove(arousal_data$ImageFile,'stimuli/')
arousal_data$ImageFile <- stringr::str_remove(arousal_data$ImageFile, '.jpg')
#returns tibble of arousal and valence data
return(arousal_data)
}

#creates a dataframe which binds together all .csv files in the current working directory
lapply_read_csv_bind_rows <- function() {
  files = list.files('.', '*.csv', full.names = TRUE)
  lapply(files, read_csv) %>% 
  bind_rows()
}

#puts three above functions together to output the task data and arousal/valence data in two tibbles
clean_aat_task <- function() {
  df <- lapply_read_csv_bind_rows()
  empty_vec <- vector(mode = 'list', length = (2))
  tibbles <- empty_vec |> 
    purrr::map(~.x, as_tibble())
  tibbles[[1]] <- clean_task_data(df)
  tibbles[[2]] <- clean_arousal_data(df)
  return(tibbles)
}

# runs the clean_aat_task function in the current working directory, saves the output files as .csv files
data <- clean_aat_task()
aat_task_data <- data[[1]]
aat_stimulus_data <- data[[2]]
write.csv(aat_task_data, 'clean/aat_cleaned.csv')
write.csv(aat_stimulus_data, 'clean/aat_cleaned_stimulus.csv')

