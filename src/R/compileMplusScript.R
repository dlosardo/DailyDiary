#!/usr/bin/env Rscript
#

create_mplus_script <- function(model_type, data_type, estimator, items, item_groups = 0
                                , ncategories = 4, demo_vars = "", data_scale = "CONTINUOUS"){
  to_save_dir <- "."
  input_dir <- "src/mplus"
  data_dir <- "../../../data/work/prepped"
  identifier <- model_type
  file_model <- paste(input_dir, "/", identifier, ".inp", sep = "")
  
  title <- paste("title: ", identifier, ";\n")
  data <- get_data_command(data_dir, data_type)
  variables <- get_variable_command(model_type, data_scale, demo_vars, items)
  analysis <- get_analysis_command(model_type, estimator)
  model <- get_model_command(model_type, items, ncategories, item_groups)
  output <- "\noutput:tech1;\n"
  savedata <- get_savedata_command(model_type, identifier, to_save_dir)
  
  options("width" = 90)
  cat(title,  file = file_model, append = FALSE, sep="")
  cat(data, variables, analysis, model, output, savedata, file = file_model, fill = TRUE, append = TRUE)
}
get_data_command <- function(data_dir, data_type){
  data_file_name <- sprintf("%s/%s.dat", data_dir, data_type)
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
get_variable_command <- function(model_type, data_scale, demo_vars, items){
  data_scale <- data_scales(data_scale)
  names <- paste0(c("names =" , c(demo_vars, items), ";\n"))
  usevariables <- paste0(c("\tusevariables =", items, ";\n"))
  data_types <- ""
  if (data_scale != ""){
    data_types <- paste0(c("\t", data_scale,  " = ", items, ";\n"))
  }
  missing <- "\tmissing = .;\n"
  cluster <- ""
  if (model_type == "MLM"){
    cluster <- paste0(c("within = x;\nbetween = ;\ncluster = id;\n"))
  }
  variables <- paste(c("\nvariable:", names, usevariables, data_types, missing, cluster), sep = "")
  return(variables)
}

get_analysis_command <- function(model_type, estimator){
  estimator = paste0("estimator = ", estimator, ";\n")
  if (grepl("EFA", model_type)){
    type = paste0(c("type = efa 1 ", NUM_FACTORS, ";\n"))
  }else if (model_type == "LC"){
    type = "type = mixture;\n"
  }else if (model_type == "MLM"){
    type = "type = twolevel random;\n"  
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
    model <- paste0(c("\nmodel: \n", model, covs))
  }else if(model_type == "LCM") {
    nt <- max(grep("[0-9]+", items))
    model <- c(
      paste0("int by ", paste0(items, "@1", collapse = " "), ";\n"),
      paste0("slp by ", paste0(items, "@", 0:(nt-1), collapse =" "), ";\n"),
      paste0("int with slp;\nint slp;\n[int slp];\n"),
      paste0("[", paste0(items, "@0", collapse = " "), "];\n"),
      paste0(paste0(items, collapse = " "), ";\n"))
    model <- paste0(c("\nmodel: \n", model))
  }else if(model_type == "AR"){
    nt <- max(grep("[0-9]+", items))
    model <- c(
      paste0("!initial status\n inity by ", items[1], "@0;\n inity*;\n [inity@0]; \n ", items[1], " on inity@1;"),
      unlist(sapply(1:nstates, function(s) get_ar_lags(s + 1, items, nt))),
      paste0("ksi", 1:nt, " by ", items[1:nt], "@1;\n"),
      paste0("ksi", 1, "-ksi", nt, " (svars);\n"),
      paste0(items[1], "-", items[nt], "@0;\n"),
      paste0("[", items[1], "-", items[nt], "@0];\n")
    )
    covs <- NULL
    for (i in 1:(nt - 1)){
      covs <- c(covs, paste0("ksi", i, " with ksi", (i+1):(nt), "@0;\n"))
    }
    covs <- c(covs, paste0("inity with ksi", 1:nt, "@0;\n"))
    model <- paste0(c("\nmodel: \n", model, covs))
    
  }else if(model_type == "MA"){
    model <- c(
      paste0("!initial status\n inity by ", items[1], "@0;\n inity*;\n [inity@0]; \n ", items[1], " on inity@1;"),      
      paste0("ksi", 1:nt, " by ", items[1:nt], "@1;\n"),
      sapply(2:nt, function(x) paste0(items[x], " on ksi", x - 1, " (1);\n")),
      paste0("ksi", 1, "-ksi", nt, " (2);\n"),
      paste0(items[1], "-", items[nt], "@0;\n"),
      paste0("[", items[1], "-", items[nt], "@0];\n")
    )
    covs <- NULL
    for (i in 1:(nt - 1)){
      covs <- c(covs, paste0("ksi", i, " with ksi", (i+1):(nt), "@0;\n"))
    }
    covs <- c(covs, paste0("inity with ksi", 1:nt, "@0;\n"))
    model <- paste0(c("\nmodel: \n", model, covs))
  } else if (model_type == "ARMA"){
    nt <- max(grep("[0-9]+", items))
    model <- c(
      paste0("!initial status\n inity by ", items[1], "@0;\n inity*;\n [inity@0]; \n ", items[1], " on inity@1;"),
      unlist(sapply(1:p, function(s) get_ar_lags(s + 1, items, nt))),
      paste0("ksi", 1:nt, " by ", items[1:nt], "@1;\n"),
      paste0("ksi", 1, "-ksi", nt, " (svars);\n"),
      paste0(items[1], "-", items[nt], "@0;\n"),
      paste0("[", items[1], "-", items[nt], "@0];\n"),
      sapply(2:nt, function(x) paste0(items[x], " on ksi", x - 1, " (ma);\n"))
    )
    covs <- NULL
    for (i in 1:(nt - 1)){
      covs <- c(covs, paste0("ksi", i, " with ksi", (i+1):(nt), "@0;\n"))
    }
    covs <- c(covs, paste0("inity with ksi", 1:nt, "@0;\n"))
    model <- paste0(c("\nmodel: \n", model, covs))
    
  } else if (model_type == "MLM"){
    model <- c("\n model: %WITHIN%\ns | y on x;\n%BETWEEN%\ny with s;\n")
  } else {
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
  return(savedata)
}