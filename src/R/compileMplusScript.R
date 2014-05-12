#!/usr/bin/env Rscript
#

library(reshape2)

create_mplus_script <- function(model_type, score_type, test_name, estimator, ncategories = 4, items, item_groups){
  to_save_dir <- "data/work/mplus"
  input_dir <- "src/mplus"
  data_dir <- "data/work/prepped"
  
  identifier <- sprintf("%s.%s.%s", test_name, model_type, estimator)
  file_model <- paste(input_dir, "/", identifier, ".inp", sep = "")
  
  demo_vars <- c("sid", "cid", "tid", "scid", "lname")
  
  
  title <- paste("title: ", identifier, ";\n")
  data <- get_data_command(data_dir, test_name, score_type)
  variables <- get_variable_command(model_type, demo_vars, items)
  analysis <- get_analysis_command(model_type, estimator)
  model <- get_model_command(model_type, items, ncategories, item_groups)
  output <- "\noutput:tech1;\n"
  savedata <- get_savedata_command(model_type, identifier, to_save_dir)
  
  options("width" = 90)
  cat(title,  file = file_model, append = FALSE, sep="")
  cat(data, variables, analysis, model, output, savedata, file = file_model, fill = TRUE, append = TRUE)
}
get_data_command <- function(data_dir, test_name, score_type){
  data_file_name <- sprintf("%s/%s.%s.dat", data_dir, test_name, score_type)
  data_file <- paste("\ndata: file = ", shQuote(data_file_name), ";\n")
  return(data_file)
}

data_scales <- function(data_scale){
  if (data_scale == "NOMINAL")
    return("nominal")
  else if (data_scale == "CATEGORICAL")
    return("categorical")
  else if (data_scale == "CONTINUOUS")
    return("")
  else return("ERROR: NOT A VALID DATA SCALE")
}
get_variable_command <- function(model_type, demo_vars, items){
  data_scale <- data_scales(model_type)
  names <- paste0(c("names =" , c(demo_vars, items), ";\n"))
  usevariables <- paste0(c("\tusevariables =", items, ";\n"))
  data_types <- ""
  if (data_scale != ""){
    data_types <- paste0(c("\t", data_scale,  " = ", items, ";\n"))
  }
  missing <- "\tmissing = .;\n"
  variables <- paste(c("\nvariable:", names, usevariables, data_types, missing), sep = "")
  return(variables)
}

get_analysis_command <- function(model_type, estimator){
  estimator = paste0("estimator = ", estimator, ";\n")
  if (grepl("EFA", model_type)){
    type = paste0(c("type = efa 1 ", NUM_FACTORS, ";\n"))
  }else if (model_type == "LC"){
    type = "type = mixture;\n"
  }else {
    type = ""
  }
  analysis = paste(c("\nanalysis: ", estimator, type))
  return(analysis)
}

get_model_command <- function(model_type, items, ncategories, item_groups){
  if (model_type == "Rasch"){
    model = c("\n model: \n"#factor loadings
              , sprintf("f1 by %s@1 (1);\n", items)
              , "\n"
              , "f1@1;\n[f1@0];\n"
              , "\n" #items thresholds
              , sprintf("[%s$1*];\n", items))
  }else if (model_type == "1PL"){
    model = c("\n model: \n"#factor loadings
              , sprintf("f1 by %s* (1);\n", items)
              , "\n"
              , "f1@1;\n[f1@0];\n"
              , "\n" #common items thresholds
              , "\n" #items thresholds
              , sprintf("[%s$1*];\n", items))
  }else if (model_type == "2PL"){
    model = c("\n model: \n"#factor loadings
              , sprintf("f1 by %s*;\n", items)
              , "\n"
              , "f1@1;\n[f1@0];\n"
              , "\n" # items thresholds
              , sprintf("[%s$1*];\n", items))
  }else if (model_type == "Nominal"){
    nitems = length(items)
    as <- NULL
    cs <- NULL
    for(i in 1:(ncategories-1)){
      as <- c(as, sprintf("\tf1 by  %s#%d* (a%d);\n", items, i, ((i-1)*nitems+1):(i*nitems)))
    }
    for(i in 1:(ncategories-1)){
      cs <- c(cs, sprintf("\t[%s#%d] (c%d);\n", items, i, ((i-1)*nitems+1):(i*nitems)))
    }
    model <- c("\nmodel: \n", as, cs, "\tf1@1;\n\t[f1@0];\n")
  }else if (model_type == "CFA"){
    model = ""
    ngroups <- length(unique(item_groups))
    all_f <- unique(item_groups)
    fs <- paste("f", all_f, sep = "")
    covs <- NULL
    for (i in 1:ngroups){
      f <- all_f[i]
      cur_items <- items[item_groups %in% f]
      model <- c(model, sprintf("\tf%d by %s@1;\n", f, cur_items[1])
                 , sprintf("\tf%d by %s*;\n", f, cur_items[2:length(cur_items)])
                 , sprintf("\tf%d*;\n \t[f%d*];\n", f, f)
                 , sprintf("\t[%s$1@0];\n", cur_items[1])
                 , sprintf("\t[%s$1*];\n", cur_items[2:length(cur_items)]))
    }
    for (i in 1:(ngroups - 1)){
      covs <- c(covs, paste("\t", fs[i], " with ", fs[(i+1):(length(fs))], ";\n"))
    }
    model <- paste0(c("\nmodel: \n" ,model, covs))
  }else {
    model = ""
  } 
  return(model)
}

get_savedata_command <- function(model_type, identifier, to_save_dir){
  results <- fscores <- probs <- estimates <- NULL
  if (!grepl("EFA", model_type)){
    results <- paste0("results= ", shQuote(sprintf("%s/item_parameters/results.%s.dat", to_save_dir, identifier)), ";\n")
    fscores <- paste0("file= ", shQuote(sprintf("%s/factor_scores/fscores.%s.dat", to_save_dir, identifier))
                        , ";\n", "save = fscores;\n") 
    estimates <- paste0("estimates= ", shQuote(sprintf("%s/estimates/estimates.%s.dat", to_save_dir, identifier)), ";\n")
  }
  if (model_type == "LC"){
    probs <- paste0("file= ", shQuote(sprintf("%s/class_probabilities/cprobs.%s.dat", to_save_dir, identifier))
                        , ";\n", "save = cprob;\n") 
    fscores <- NULL
    
  }
  savedata <- paste0("\nsavedata: \n", results, fscores, probs, estimates)
  savedata
}

main <- function(time_point, input_file, info_file, score_type, model_type, estimator){
  input_data <- read.csv(input_file, stringsAsFactors = FALSE)
  input_data <- subset(input_data, lesson_name == time_point)
  ncategories <- length(levels(factor(input_data$correct_answer)))
  
  item_info <- unique(input_data[, c("item_name", "passage_id")])
  item_info <- item_info[order(as.numeric(gsub('[a-zA-Z]+', "", item_info$item_name))),]
  items <- tolower(substr(item_info$item_name, 0, 8))  
  item_groups <- item_info$passage_id
  test_name <- sprintf("read.comp.%s", time_point)
  create_mplus_script(model_type, score_type, test_name, estimator, ncategories = ncategories,  items, item_groups)
}

DEFAULT_TIME_POINT = 'post'
DEFAULT_SCORE_TYPE = 'raw'
DEFAULT_MODEL_TYPE = 'CFA'
DEFAULT_ESTIMATOR = "WLSMV"
DEFAULT_INPUT_FILE = 'data/work/prepped/read.comp.scored.csv'
args <- commandArgs(trailingOnly=TRUE);
time_point <- nvl(args[1], DEFAULT_TIME_POINT)
score_type <- nvl(args[2], DEFAULT_SCORE_TYPE)
model_type <- nvl(args[3], DEFAULT_MODEL_TYPE)
estimator <- nvl(args[4], DEFAULT_ESTIMATOR)
input_file <- nvl(args[5], DEFAULT_INPUT_FILE)

main(time_point, input_file, info_file, score_type, model_type, estimator)
