
clean_task_data <- function (df){ 
  
task_data <- df |> 
  select(!(1:10)) |> 
  select(!c(expName, psychopyVersion, frameRate, OS, task_loop.thisRepN, task_loop.thisTrialN, task_loop.thisN, task_loop.thisIndex, task_loop.ran)) |> 
  filter(trial <= 150) |> 
  mutate( choice = case_when(!is.na(`left image outcome` ) ~'left', 
                             !is.na(`middle image outcome`) ~'middle',
                             !is.na(`right image outcome`) ~'right')) |> 
  filter (!is.na(choice)) |> 
  mutate( image_outcome = case_when (!is.na(`left image outcome` ) ~ `left image outcome`, 
                                     !is.na(`middle image outcome`) ~ `middle image outcome`,
                                     !is.na(`right image outcome`) ~ `right image outcome`)) |> 
  select(c(ID, date, trial, 2, 4:6, 46,47)) |> 
  rename(reaction_time = choose_trial_end.rt) |> 
  mutate(date = lubridate::ymd_hms(date)) 

task_data$image_outcome <- stringr::str_remove(task_data$image_outcome,'stimuli/')
task_data$image_outcome <- stringr::str_remove(task_data$image_outcome, '.jpg')



return(task_data)
}


clean_arousal_data <- function (df) {
  
arousal_data <- df |> 
  filter(trial >150) |> 
  select(c(ID, trial, session, date, 51:58, 64)) |> 
  select (!c(session, valence_end_key.keys, arousal_end_key.keys, valence_end_key.rt, arousal_end_key.rt)) |> 
  mutate(date = lubridate::ymd_hms(date))

arousal_data$ImageFile <- stringr::str_remove(arousal_data$ImageFile,'stimuli/')
arousal_data$ImageFile <- stringr::str_remove(arousal_data$ImageFile, '.jpg')


return(arousal_data)
}

lapply_read_csv_bind_rows <- function() {
  files = list.files('.', '*.csv', full.names = TRUE)
  lapply(files, read_csv) %>% 
  bind_rows()
}

clean_aat_task <- function() {
  df <- lapply_read_csv_bind_rows()
  empty_vec <- vector(mode = 'list', length = (2))
  tibbles <- empty_vec |> 
    purrr::map(~.x, as_tibble())
  tibbles[[1]] <- clean_task_data(df)
  tibbles[[2]] <- clean_arousal_data(df)
  return(tibbles)
}

data <- clean_aat_task()

aat_task_data <- data[[1]]
aat_stimulus_data <- data[[2]]

write.csv(aat_task_data, 'clean/aat_cleaned.csv')
write.csv(aat_stimulus_data, 'clean/aat_cleaned_stimulus.csv')

