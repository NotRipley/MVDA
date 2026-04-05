load_data <- function(){
  
  e <- new.env()
  load(file = "./Assignment3_Data.RData", envir = e)
  
  return(list(
    data = t(e$data_hypertension),
    replicates = e$replicate_names,
    times = e$time_names,
    treatments = e$treatment_names
  ))
  
}

treatment_colours <- c(
  "Control" <- "grey",
  "Lisinopril" <- "steelblue",
  "Amlodipine" <- "tomato"
)