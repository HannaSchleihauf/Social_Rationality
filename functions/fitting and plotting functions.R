
# inverse logit function
inv_logit <- function(x){
  return(exp(x)/(1+exp(x)))
}


################################ GLM Functions #################################

glm_prep <- function(dat, formula, dummies, trialVar){
  # Takes data, formula for GLM, list of dummy variable(s), and name of trial variable
  # Outputs data in correct format for GLM
  
  xx.fe.re <- fe.re.tab(fe.model = formula,
                        re = "(1|Chimpanzee)", data = dat)
  t.data <- xx.fe.re$data
  # Center dummy-coded variable, scale trial
  for (dummy in dummies){
    dummy_code_name <- paste(dummy, ".code", sep="")
    t.data[[dummy_code_name]] <-  t.data[[dummy]] - mean(t.data[[dummy]])
  }
  t.data[["z.Trial"]] <- scale(t.data[[trialVar]])
  return(list(xx.fe.re, t.data))
}

glm_assumptions <- function(model, contr, vif_test=T){
  # Takes GLM model, checks model assumptions
  
  # Overdispersion
  overdispersion <- overdisp.test(model)
  
  # Model stability
  m.stab.b <- glmm.model.stab(model.res = model, contr = contr, use = c("Chimpanzee"))
  # only with models that did not give a warning message
  m.stab.b$detailed <- m.stab.b$detailed %>%
    filter(warnings == "none")
  m.stab.output <- cbind(apply(m.stab.b$detailed, 2, min), apply(m.stab.b$detailed, 2, max))
  
  # VIF test
  if (vif_test){
    vif_output <- vif(model)
    assumptions_output <- list(overdisp = overdispersion, vif = vif_output, stab = m.stab.output)
  } else {
    assumptions_output <- list(overdisp = overdispersion, stab = m.stab.output)
  }
  
  return(assumptions_output)
}

glm_bootstraps <- function(model, nBoots, vars){
  # Takes model, number of bootstraps, and variables to include
  # Runs bootstraps
  
  boot <- boot.glmm.pred(model.res = model, excl.warnings = T,
                         nboots = nBoots, para = F, level = 0.95,
                         use = vars)
  
  boot_output <- list(estimate = round(boot$ci.estimates, 3), 
                      stab = m.stab.plot(round(boot$ci.estimates, 3)), 
                      ci = boot$ci.predicted)
  return(boot_output)
}

glm_many_models <- function(N = 100, n_boots = 10, model_data, cells_to_change, response_var, store_df) {
  # For models with complete separation issues, run adjusted analysis: 
  # fit N models with problematic data perturbed by a small amount at a time. 
  # Then, compile average results from these 100 models as an estimate of the
  # true model values.
  
  # Returns a list containing:
  
  # 1. number of models that converged (out of N)
  # 2. data-frame containing model information for all perturbed models
  # 3. data-frame of model comparisons (for each run of N)
  # 4. data-frame of averaged coefficients across all N full models
  # 5. list of data-frames containing bootstraps for all models
  
  ### Run models
  
  # set variables for use in loop
  contr <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
  resultNames <- c("(Intercept)", "Conditionweak_first", "z.Trial", "Conditionweak_first:z.Trial")
  columnNames <- c("intercept", "Condition", "Trial", "Condition.Trial")
  null_valNames <- c("Chisq", "Df", "Pr(>Chisq)")
  null_colNames <- c("Chisq", "Df", "Pr..Chisq.")
  colNames <- c("Chisq", "Chi.Df", "Pr..Chisq.", "n.opt.warnings", "n.fun.warnings")
  
  # create DFs to save bootstraps
  boot.full.values <- data.frame()
  boot.full.estimates <- data.frame()
  boot.Cond.values <- data.frame()
  boot.Cond.estimates <- data.frame()
  
  # run N models
  for (i in 1:N) {
    set.seed(i)
    
    # Create a new response variable to store modified responses
    model_data$new.resp <- model_data[[response_var]]
    
    # First, change a random response in each of the problem cells
    for (problem in 1:length(cells_to_change)){
      
      # sample 1 random element of this problem cell
      new <- sample(cells_to_change[[problem]], size = 1) 
      
      # save sampled row number to data storage
      store_df[,paste("to.change.",problem,sep="")][i] <- new
      
      # change response for that cell
      model_data$new.resp[new] <- ifelse(model_data$new.resp[new] == 0, 1, 0)
    }
    
    # Next, run models
    # full model
    full1 <- keepWarnings(glmer(new.resp ~ Condition * z.Trial + (1 + Condition * z.Trial | Chimpanzee), 
                                data = model_data, control = contr, family = binomial(link = "logit")))
    # reduced model (without interaction)
    red1 <- keepWarnings(glmer(new.resp ~ Condition + z.Trial + (1 + Condition * z.Trial | Chimpanzee), 
                               data = model_data, control = contr, family = binomial(link = "logit")))
    # null model
    null1 <- keepWarnings(glmer(new.resp ~ 1 + (1 + Condition * z.Trial | Chimpanzee),
                                data = model_data, control = contr,family = binomial(link = "logit")))
    
    # Save model outputs
    if (length(full1$warnings) == 0 & length(red1$warnings) == 0 & length(null1$warnings) == 0) {
      full <- full1$value
      red <- red1$value
      null <- null1$value
      
      store_df$warnings[i] <- "no"
      
      # save full model parameters and SDs to DF
      for (x in 1:length(resultNames)){
        store_df[[paste("all.full.coeffs.",columnNames[x],sep="")]][i] <- summary(full)$coefficients[resultNames[x], 1]
        store_df[[paste("all.full.sd.",columnNames[x],sep="")]][i] <-summary(full)$coefficients[resultNames[x], 2]
      }
      
      # Model Comparisons
      
      # full vs. null 
      test.full.null <- as.data.frame(anova(null, full, test = "Chisq"))["full", null_valNames]
      for (x in 1:length(null_colNames)){
        store_df[[paste("test.full.null.",null_colNames[x],sep="")]][i] <- test.full.null[[null_valNames[x]]]
      }
      
      # full vs. reduced 
      tests.full.red <- drop1p(model.res = full, para = F, contr = contr, n.cores = c("all-1", "all"), to.del = NULL)
      test.2.way.int <- as.data.frame(tests.full.red$drop1.res[2, colNames])
      
      # testing assumptions
      # collinearity
      store_df$collinearity.test.Condition[i] <- vif(red)[1]
      store_df$collinearity.test.Trial[i] <- vif(red)[2]
      
      # reduced vs. main 
      tests.red.main <- drop1p(model.res = red, para = F, contr = contr,n.cores = c("all-1", "all"), to.del = NULL)
      test.main.Condition <- as.data.frame(tests.red.main$drop1.res[2, colNames])
      test.main.Trial <- as.data.frame(tests.red.main$drop1.res[3, colNames])
      
      # save comparison stats to DF
      for (colName in colNames){
        store_df[[paste("test.2.way.Condition.Trial.",colName,sep="")]][i] <- test.2.way.int[[colName]]
        store_df[[paste("test.main.Condition.",colName,sep="")]][i] <- test.main.Condition[[colName]]
        store_df[[paste("test.main.Trial.",colName,sep="")]][i] <- test.main.Trial[[colName]]
      }
      
      # Carry out bootstraps
      
      # full model
      boot.full <- boot.glmm.pred(model.res = full, excl.warnings = T, nboots = n_boots, para = F, resol = 100, level = 0.95, use = c("Condition", "z.Trial"))
      boot.full$ci.predicted$name <- paste(boot.full$ci.predicted$Condition,boot.full$ci.predicted$z.Trial, sep = ".")
      boot.full.values <- rbind(boot.full.values, 
                               data.frame(term = boot.full$ci.predicted$Condition,
                                          fitted = boot.full$ci.predicted$fitted,
                                          lower.cl = boot.full$ci.predicted$lower.cl,
                                          upper.cl = boot.full$ci.predicted$upper.cl))
      boot.full.estimates <- rbind(boot.full.estimates,
                                  data.frame(term = rownames(boot.full$ci.estimates),
                                             orig = boot.full$ci.estimates$orig, 
                                             X2.5. = boot.full$ci.estimates$X2.5.,
                                             X97.5. = boot.full$ci.estimates$X97.5.))
      
      # condition only
      boot.Cond <- boot.glmm.pred(model.res = full, excl.warnings = T, nboots = n_boots, para = F, resol = 100, level = 0.95, use = c("Condition"))
      boot.Cond.values <- rbind(boot.Cond.values, 
                                data.frame(term = boot.Cond$ci.predicted$Condition,
                                           fitted = boot.Cond$ci.predicted$fitted,
                                           lower.cl = boot.Cond$ci.predicted$lower.cl,
                                           upper.cl = boot.Cond$ci.predicted$upper.cl))
      boot.Cond.estimates <- rbind(boot.Cond.estimates,
                                   data.frame(term = rownames(boot.Cond$ci.estimates),
                                              orig = boot.Cond$ci.estimates$orig,
                                              X2.5. = boot.Cond$ci.estimates$X2.5.,
                                              X97.5. = boot.Cond$ci.estimates$X97.5.))
    } else {
      store_df$warnings[i] <- "yes"
    }
  }
  
  ### Evaluation of model results
  
  # how many models converged?
  sum_converge <- sum(store_df$warnings == "no", na.rm = T)
  
  # 1. Model comparisons
  
  # set variables
  tests <- c("test.full.null.", "collinearity.test.", 
             "test.2.way.Condition.Trial.", "test.main.Trial.", 
             "test.main.Condition.")
  allCols <- list(c("Chisq", "Df", "Pr..Chisq."), c("Condition", "Trial"),
                  c("Chisq", "Chi.Df", "Pr..Chisq."), 
                  c("Chisq", "Chi.Df", "Pr..Chisq."),
                  c("Chisq", "Chi.Df", "Pr..Chisq."))
  colNames <- c("test","Chisq.mean", "Chisq.range.low", "Chisq.range.high","Df", 
                "Pr..Chisq..mean", "Pr..Chisq..range.low", "Pr..Chisq..range.high",
                "Condition.mean", "Condition.range.low", "Condition.range.high",
                "Trial.mean", "Trial.range.low", "Trial.range.high")
  
  # compile results
  models_results <- data.frame(matrix(ncol = length(colNames), nrow = 0))
  colnames(models_results) <- colNames
  for (i in 1:length(tests)){
    thisTest <- data.frame(matrix(ncol = length(colNames), nrow = 1))
    colnames(thisTest) <- colNames
    thisTest$test[1] <- tests[i]
    cols <- allCols[[i]]
    for (col in cols){
      colName <- paste(tests[i], col, sep = "")
      if (col == "Df" | col == "Chi.Df"){
        thisTest$Df[1] <- round(mean(store_df[[colName]], na.rm = T), 10)
      } else {
        thisTest[[savename_mean <- paste(col, ".mean", sep="")]][1] <- round(mean(store_df[[colName]], na.rm = T), 10)
        thisTest[[paste(col, ".range.low", sep = "")]][1] <- round(range(store_df[[colName]], na.rm = T), 10)[1]
        thisTest[[paste(col, ".range.high", sep = "")]][1] <- round(range(store_df[[colName]], na.rm = T), 10)[2]
      }
    }
    models_results <- rbind(models_results, thisTest)
  }
  
  
  # 2. Calculate average coefficients for the full model
  
  # select only full models
  all.coeffs <- store_df %>%
    select(vars_select(names(store_df), starts_with("all.full", ignore.case = TRUE)))
  
  # compile coefficients
  models_coeffs <- data.frame(min = sapply(all.coeffs, min, na.rm = T),
                              mean = sapply(all.coeffs, mean, na.rm = T),
                              max = sapply(all.coeffs, max, na.rm = T))
  
  
  # 3. Save summary of bootstraps
  
  all_boots <- list()
  boots_mods <- list(boot.full.values, boot.full.estimates, boot.Cond.values, boot.Cond.estimates)
  for (i in 1:length(boots_mods)){
    thisMod <- boots_mods[[i]]
    thisBoot <- mapply(FUN = tapply, X = thisMod[, -1], MoreArgs = list(INDEX = thisMod$term, FUN = mean))
    modDF <- data.frame(round(thisBoot, 3))
    modDF$Condition <- rownames(modDF)
    all_boots[[i]] <- modDF
  }
  
  # Finally, return all outputs of the analysis
  outputs <- list(sum_converge, store_df, models_results, models_coeffs, all_boots)
  return(outputs)
  
}

view_random_models <- function(n_samples = 10, n_models = 100, model_data, saved_models, cells_to_change, response_var){
  # View results of random n_samples models (out of n_models total)
  
  for (i in sample(seq(1:n_models), n_samples)){
    # duplicate response variable
    model_data$new.resp <- model_data[[response_var]]
    
    # change response cells based on stored model info
    for (problem in 1:length(cells_to_change)){
      # select row of this var that was changed in this model
      thisRow <- saved_models[,paste("to.change.",problem,sep="")][i]
      # reverse response for this cell
      model_data$new.resp[thisRow] <- ifelse(model_data$new.resp[thisRow] == 0, 1, 0)
    }
    
    # run model
    full_randomModel <- glmer(new.resp ~ Condition * z.Trial + (1 + Condition * z.Trial | Chimpanzee), 
                                   data = model_data, control = contr,family = binomial(link = "logit"))
    
    # view model
    print(summary(full_randomModel)$varcor)
    ranef.diagn.plot(full_randomModel)
    print(round(summary(full_randomModel)$coefficients, 3))
  }
}






########################## Bayesian Model Functions ############################

## Model fitting

fit_stan_models <- function(data, experiment){
  # Takes first choice data and test trials as input
  # Fits all Experiment 1/2 Bayesian models and outputs model fits and LLs
  
  if (experiment == 1 | experiment == 2){
    # create data structures
    stan_full_data <- list(n = nrow(data), # number of trials
                           nsubj = length(unique(data$subjID)),
                           subj = data$subjID,
                           condition = data$cond,  # 1 = strong first
                           choice1 = data$choice1, # 1 = strong, 0 = weak
                           choice2 = data$choice2)

    # Fit Rationality model
    stan_full_fit <- stan(file="./Stan Models/rational_model_exp1_2.stan", 
                          data=stan_full_data, 
                          iter=20000, chains=2, cores=2, 
                          control=list(adapt_delta = .999,
                                       max_treedepth = 11))
    
    # Fit Belief perseveration model
    stan_persev_fit <- stan(file="./Stan Models/persev_model_exp1_2.stan", 
                            data=stan_full_data, 
                            iter=20000, chains=2, cores=2,
                            control=list(adapt_delta=.999,
                                         max_treedepth = 11))
    
    # Fit Recency bias model
    stan_recency_fit <- stan(file="./Stan Models/recency_model_exp1_2.stan", 
                             data=stan_full_data, 
                             iter=20000, chains=2, cores=2, 
                             control=list(adapt_delta = .999,
                                          max_treedepth = 11))
    
    # Fit Cue saliency model
    stan_cue_fit <- stan(file="./Stan Models/cue_model_exp1_2.stan", 
                         data=stan_full_data, 
                         iter=20000, chains=2, cores=2, 
                         control=list(adapt_delta = .999))
    
    # Fit null model
    stan_null_fit <- stan(file="./Stan Models/null_model_exp1_2.stan", 
                         data=stan_full_data, 
                         iter=20000, chains=2, cores=2, 
                         control=list(adapt_delta = .999))
    
    stan_models <- list("full" = stan_full_fit, "persev" = stan_persev_fit, 
                        "recency" = stan_recency_fit, "cue" = stan_cue_fit,
                        "null" = stan_null_fit,
                        "full_ll" = extract_log_lik(stan_full_fit),
                        "persev_ll" = extract_log_lik(stan_persev_fit),
                        "recency_ll" = extract_log_lik(stan_recency_fit),
                        "cue_ll" = extract_log_lik(stan_cue_fit),
                        "null_ll" = extract_log_lik(stan_null_fit))
    
  } else if (experiment == 3){
    
    # split data into experiment versions
    data_v1 <- subset(data, Version == 1)
    data_v2 <- subset(data, Version == 2)
    
    # create data structures
    stan_data_v1 <- list(n = nrow(data_v1), # number of trials
                         nsubj = length(unique(data_v1$subjID)), # number of subjects
                         subj = data_v1$subjID, # subject ID
                         choice = data_v1$choice1) # 0 = no evidence, 1 = weak evidence
    
    stan_data_v2 <- list(n = nrow(data_v2), # number of trials
                         nsubj = length(unique(data_v2$subjID)), # number of subjects
                         subj = data_v2$subjID, # subject ID
                         choice1 = data_v2$choice1, # 1 = strong evidence, 2 = weak evidence, 3 = no evidence
                         choice2 = data_v2$choice2) # if choice1 = 1 (strong), then 1 = WEAK evidence, 2 = NO evidence
                                                    # if choice1 = 2 (weak), then 1 = STRONG evidence, 2 = NO evidence
                                                    # if choice1 = 3 (no), then 1 = STRONG evidence, 2 = WEAK evidence
    
    
    # Fit Rationality models
    stan_full_fit_v1 <- stan(file="./Stan Models/rational_model_exp3_v1.stan", 
                             data = stan_data_v1, 
                             iter = 20000, chains = 2, cores = 2, 
                             control = list(adapt_delta = .999, max_treedepth = 15))
    
    stan_full_fit_v2 <- stan(file="./Stan Models/rational_model_exp3_v2.stan", 
                             data = stan_data_v2, 
                             iter = 20000, chains = 2, cores = 2, 
                             control = list(adapt_delta = .999, max_treedepth = 15))
    
    # Fit Cue saliency model
    stan_cue_fit_v2 <- stan(file="./Stan Models/cue_model_exp3_v2.stan", 
                            data = stan_data_v2, 
                            iter=20000, chains=2, cores=2, 
                            control = list(adapt_delta = .999))
    
    # Fit null models
    stan_null_fit_v1 <- stan(file="./Stan Models/null_model_exp3_v1.stan", 
                             data = stan_data_v1, 
                             iter = 20000, chains = 2, cores = 2, 
                             control = list(adapt_delta = .999, max_treedepth = 15))
    
    stan_null_fit_v2 <- stan(file="./Stan Models/null_model_exp3_v2.stan", 
                             data = stan_data_v2, 
                             iter = 20000, chains = 2, cores = 2, 
                             control = list(adapt_delta = .999, max_treedepth = 15))
    
    stan_models <- list("full_v1" = stan_full_fit_v1, "full_v2" = stan_full_fit_v2,
                        "cue_v2" = stan_cue_fit_v2, 
                        "null_v1" = stan_null_fit_v1, "null_v2" = stan_null_fit_v2,
                        "full_v1_ll" = extract_log_lik(stan_full_fit_v1),
                        "full_v2_ll" = extract_log_lik(stan_full_fit_v2),
                        "cue_v2_ll" = extract_log_lik(stan_cue_fit_v2),
                        "null_v1_ll" = extract_log_lik(stan_null_fit_v1),
                        "null_v2_ll" = extract_log_lik(stan_null_fit_v2))
    
  } else if (experiment == 4){
    
    # create data structure
    stan_full_data <- list(n = nrow(data),
                           nsubj = length(unique(data$subjID)),
                           subj = data$subjID,
                           condition = data$cond,  # 0 = new, 1 = redundant
                           choice1 = data$choice1, # 1 = location supported by strong evidence, and 0 = weak evidence
                           choice2 = data$choice2)
    
    # Fit Rationality model
    stan_full_fit <- stan(file="./Stan Models/rational_model_exp4.stan", 
                          data=stan_full_data, 
                          iter=20000, chains=2, cores=2, 
                          control=list(adapt_delta = .999,
                                       max_treedepth = 11))
    
    # Fit Cue saliency model
    stan_cue_fit <- stan(file="./Stan Models/cue_model_exp4.stan", 
                         data=stan_full_data, 
                         iter=20000, chains=2, cores=2, 
                         control=list(adapt_delta = .999))
    
    # Fit null model
    stan_null_fit <- stan(file="./Stan Models/null_model_exp4.stan", 
                         data=stan_full_data, 
                         iter=20000, chains=2, cores=2, 
                         control=list(adapt_delta = .999))
    
    stan_models <- list("full" = stan_full_fit,
                        "cue" = stan_cue_fit,
                        "null" = stan_null_fit,
                        "full_ll" = extract_log_lik(stan_full_fit),
                        "cue_ll" = extract_log_lik(stan_cue_fit),
                        "null_ll" = extract_log_lik(stan_null_fit))
    
  } else if (experiment == 5){
    
    # split into defeater alone & test trials
    d_alone <- subset(data, condition == 2)
    fullData <- subset(data, condition != 2)
    
    # create data structures
    stan_full_data <- list(n = nrow(fullData),
                           nsubj = length(unique(fullData$subjID)),
                           subj = fullData$subjID,
                           condition = fullData$condition,  # 0 = non-defeater, 1 = defeater
                           choice1 = fullData$choice1, # 1 = location supported by strong evidence, and 0 = weak evidence
                           choice2 = fullData$choice2)
    
    stan_dAlone_data <- list(n = nrow(d_alone), # number of trials
                             nsubj = length(unique(d_alone$subjID)), # number of subjects
                             subj = d_alone$subjID, # subject ID
                             choice1 = d_alone$choice1)
    
    # Fit Rationality model
    stan_full_fit <- stan(file="./Stan Models/rational_model_exp5.stan", 
                          data=stan_full_data, 
                          iter=20000, chains=2, cores=2, 
                          control=list(adapt_delta = .999, max_treedepth = 11))
    
    # Fit Defeater Alone model
    stan_dAlone_fit <- stan(file="./Stan Models/d_alone_model_exp5.stan", 
                            data = stan_dAlone_data, 
                            iter=20000,chains=2, cores=2, 
                            control=list(adapt_delta = .999, max_treedepth = 15))
    
    # Fit Cue saliency model
    stan_cue_fit <- stan(file="./Stan Models/cue_model_exp5.stan", 
                         data=stan_full_data, 
                         iter=20000,chains=2, cores=2, 
                         control=list(adapt_delta = .999))
    
    # Fit null model
    stan_null_fit <- stan(file="./Stan Models/null_model_exp5.stan", 
                         data=stan_full_data, 
                         iter=20000,chains=2, cores=2, 
                         control=list(adapt_delta = .999))
    
    # save to output list
    stan_models <- list("full" = stan_full_fit, "d_alone" = stan_dAlone_fit, 
                        "cue" = stan_cue_fit, "null" = stan_null_fit,
                        "full_ll" = extract_log_lik(stan_full_fit),
                        "d_alone_ll" = extract_log_lik(stan_dAlone_fit),
                        "cue_ll" = extract_log_lik(stan_cue_fit),
                        "null_ll" = extract_log_lik(stan_cue_fit))
  }
  
  return(stan_models)
}


## Log Likelihood

med_post_lik <- function(mod.nlls){
  # Takes LL object from model, calculates median posterior likelihood
  
  sums <- c()
  for (i in 1:ncol(mod.nlls)){
    sums <- c(sums, sum(mod.nlls[i,]))
  }
  med.nll <- median(sums)
  return(med.nll)
}

collate_med_post_lik <- function(nll.list, modelnames){
  # Takes list of LLs for models
  # Creates DF containing median posterior likelihoods for all models
  
  med.nlls <- data.frame()
  x <- 1
  for (mod.nll in nll.list){
    modname <- modelnames[x]
    
    # calculate median
    med.nll <- med_post_lik(mod.nlls = mod.nll)
    
    # save
    med.nlls <- rbind(med.nlls, data.frame(mod = modname, med.nll))
    x <- x + 1
  }
  return(med.nlls)
}


## Extracting model parameters

extract_params <- function(stan_models, subj_names, model_names, paramList){
  # Takes Bayesian model fits, subject names, model names, and list containing lists of parameters in each model
  # Extracts parameter fits for all subjects for all models
  
  # create summary objects to access parameters
  mod_sums <- list()
  x <- 1
  for (mod in stan_models){
    mod_sum <- data.frame(summary(mod)$summary)
    mod_sum$param <- rownames(mod_sum)
    mod_sums[[model_names[x]]] <- mod_sum
    x <- x + 1
  }
  
  # extract group parameters
  group_params <- data.frame()
  x <- 1
  for (x in 1:length(model_names)){
    model <- model_names[x]
    params <- paramList[[x]]
    mod_sum <- mod_sums[[model]]
    for (param in params){
      param_mean <- mod_sum$mean[mod_sum$param == param]
      param_2.5 <- mod_sum$X2.5.[mod_sum$param == param]
      param_97.5 <- mod_sum$X97.5.[mod_sum$param == param]
      group_params <- rbind(group_params, 
                            data.frame(model = model, param, 
                                       param_mean, param_2.5, param_97.5))
    }
  }
  
  # Save individual fits
  
  # create column names for output dataframe
  column_names <- c("Chimpanzee")
  for (x in 1:length(model_names)){
    model <- model_names[x]
    params <- paramList[[x]]
    for (param in params){
      for (col in c("groupMean", "group2.5", "group97.5", "subj")){
        if (col == "subj"){
          colname <- paste(model, param, sep="_")
        } else {
          colname <- paste(model, param, col, sep = "_")
        }
        column_names <- c(column_names, colname)
      }
    }
  }
  
  # create DF to save individual fits
  indiv_fits <- data.frame(matrix(nrow = 0, ncol = length(column_names)))
  
  for (id in 1:length(subj_names)){
    # extract name
    subj <- subj_names[id]
    
    # create subj vector
    thisSubj <- c()
    
    # calculate this subj's parameters for each model
    for (mod in 1:length(model_names)){
      model_name <- model_names[i]
      mod_adj <- mod_sums[[model_names[mod]]]
      this_mod <- subset(group_params, model == model_names[mod])
      params <- paramList[[mod]]
      
      for (param in params){
        # Define column names
        name_mean <- paste(model_name, param,"groupMean",sep="_")
        name_2.5 <- paste(model_name, param,"group2.5",sep="_")
        name_97.5 <- paste(model_name, param,"group97.5",sep="_")
        name_subj <- paste(model_name, param, sep = "_")
        
        # define row name
        rowName <- paste("subj_", param, "_adj[", id, "]",sep="")
        
        # calculate subj value for this parameter
        subj_adj <- mod_adj$mean[mod_adj$param == rowName]
        thisSubjVal <- this_mod$param_mean[this_mod$param == param] + subj_adj
        
        # save to subj vector
        thisSubj <- c(thisSubj, this_mod$param_mean[this_mod$param == param],
                      this_mod$param_2.5[this_mod$param == param],
                      this_mod$param_97.5[this_mod$param == param],
                      thisSubjVal)
      }
    }
    # save this subj to output
    thisRow <- data.frame(t(thisSubj))
    thisSubjRow <- cbind(data.frame(name = subj), thisRow)
    indiv_fits <- rbind(indiv_fits, thisSubjRow)
  }
  
  colnames(indiv_fits) <- column_names
  return(indiv_fits)
}


############################# Plotting Functions ###############################


# Create theme for all plots
my_theme <- theme(aspect.ratio = 1, 
                  axis.title = element_text(size = 20), 
                  axis.text = element_text(size = 16),
                  strip.text.x = element_text(size = 14), 
                  axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
                  legend.position = "none")

# set jitter for Bayesian parameter plots
myJitter <- position_jitternudge(jitter.width = .35, jitter.height = 0,
                                 nudge.x = -.35, nudge.y = 0, seed = 1)

plot_halfViolin <- function(data, xvar, yvar, model_ci, levels, plotlabels,
                            beginCol, endCol, ylim = c(0,1), ylab = "% following evidence", 
                            jitterWidth = 0, adjust.width = F){
  # Takes data, variable names, DF containing CIs, variable levels and labels
  # Returns plot with half violin next to subject points and CI error bars
  
  # create data for plotting
  plot_data <- data %>%
    group_by(Chimpanzee, !!sym(xvar)) %>%
    summarise(mean.resp = mean(!!sym(yvar)))
  
  # organize data
  plot_data <- plot_data %>%
    group_by(!!sym(xvar), mean.resp) %>%
    mutate(trial.per.cell = row_number()) %>%
    ungroup() %>%
    mutate(x2 = as.numeric(as.factor(!!sym(xvar))) - 0.08 - (trial.per.cell * 0.04))
  
  # move violin to the side if too crowded
  if(adjust.width == T){
    plot_data <- plot_data %>%
      mutate(x2 = as.numeric(as.factor(!!sym(xvar))) - 0.24)
  }
  
  # plot 
  p <- ggplot()+
    # lines between subject points
    geom_line(data = plot_data, color = "gray", lty = 1, alpha = subjLine_alpha,
              aes(x = x2, y = mean.resp, group = Chimpanzee),
              position = position_jitter(width = jitterWidth, height = 0, seed = 1))+
    # half violins
    geom_violinhalf(data = plot_data, position = position_nudge(x = 0.1),alpha = violinAlpha,
                    aes(x = !!sym(xvar), y = mean.resp, fill = !!sym(xvar)))+
    # subject points
    geom_point(data = plot_data, size = dotSize, alpha = dotAlpha, shape = 21, color = "black",
               aes(x = x2, y = mean.resp, fill = !!sym(xvar)),
               position = position_jitter(width = jitterWidth, height = 0, seed = 1))+
    # error bars
    geom_errorbar(data = model_ci,  width = errorbarWidth, size = errorbarSize,
                  aes(x = as.numeric(!!sym(xvar)),y = fitted, ymin = lower.cl, 
                      ymax = upper.cl, color = !!sym(xvar)))+
    geom_point(data = model_ci, shape=23, size = group_dotSize,
               aes(x = as.numeric(!!sym(xvar)), y = fitted, fill = !!sym(xvar)))+
    # theme
    scale_x_discrete(limits = levels, name = xtext_evType, labels = plotlabels)+
    {if (ylim[1] == 0 & ylim[2] == 1) scale_y_continuous(name = ylab, labels = scales::percent)} +
    {if (ylim[1] != 0 | ylim[2] != 1) scale_y_continuous(name = ylab)} +
    coord_cartesian(ylim = ylim, xlim = c(0.95, NA))+
    scale_fill_viridis_d(begin = beginCol, end = endCol, option = "magma")+
    scale_color_viridis_d(begin = beginCol, end = endCol, option = "magma")+
    theme_classic()+ my_theme
  
  return(p)
}

plot_halfViolin <- function(data, xvar, yvar, colvar, model_ci, levels, plotlabels, 
                            beginCol, endCol, ylim = c(0,1), 
                            ylab = "% following evidence", 
                            jitterWidth = 0, adjust.width = F){
  # Takes data, variable names, DF containing CIs, variable levels and labels
  # Returns plot with half violin next to subject points and CI error bars
  
  # create data for plotting
  plot_data <- data %>%
    group_by(Chimpanzee, !!sym(xvar), !!sym(colvar)) %>%
    summarise(mean.resp = mean(!!sym(yvar)))
  
  # organize data
  plot_data <- plot_data %>%
    group_by(!!sym(xvar), !!sym(colvar), mean.resp) %>%
    mutate(trial.per.cell = row_number()) %>%
    ungroup() %>%
    mutate(x2 = as.numeric(as.factor(!!sym(xvar))) - 0.08 - (trial.per.cell * 0.04))
  
  # move violin to the side if too crowded
  if(adjust.width == T){
    plot_data <- plot_data %>%
      mutate(x2 = as.numeric(as.factor(!!sym(xvar))) - 0.24)
  }
  
  
  # plot 
  p <- ggplot(data = plot_data)+
    # lines between subject points
    geom_line(data = plot_data, color = "gray", lty = 1, alpha = subjLine_alpha,
              aes(x = x2, y = mean.resp, group = Chimpanzee),
              position = position_jitter(width = jitterWidth, height = 0, seed = 1))+
    # half violins
    geom_violinhalf(data = plot_data, position = position_nudge(x = 0.1),alpha = violinAlpha,
                    aes(x = !!sym(xvar), y = mean.resp, fill = !!sym(colvar)))+
    # subject points
    geom_point(data = plot_data, size = dotSize, alpha = dotAlpha, shape = 21, color = "black",
               aes(x = x2, y = mean.resp, fill = !!sym(colvar)), 
               position = position_jitter(width = jitterWidth, height = 0, seed = 1))+
    # error bars
    geom_errorbar(data = model_ci,  width = errorbarWidth, size = errorbarSize,
                  aes(x = as.numeric(!!sym(xvar)), y = fitted, ymin = lower.cl, 
                      ymax = upper.cl, color = !!sym(colvar)))+
    geom_point(data = model_ci, shape=23, size = group_dotSize,
               aes(x = as.numeric(!!sym(xvar)), y = fitted, fill = !!sym(colvar)))+
    # theme
    scale_x_discrete(limits = levels, name = xtext_evType, labels = plotlabels)+
    {if (ylim[1] == 0 & ylim[2] == 1) scale_y_continuous(name = ylab, labels = scales::percent)} +
    {if (ylim[1] != 0 | ylim[2] != 1) scale_y_continuous(name = ylab)} +
    coord_cartesian(ylim = ylim, xlim = c(0.95, NA))+
    scale_fill_viridis_d(begin = beginCol, end = endCol, option = "magma")+
    scale_color_viridis_d(begin = beginCol, end = endCol, option = "magma")+
    theme_classic()+ my_theme
  
  return(p)
}


plot_sigmoid <- function(plot_data, bar_data){
  # Takes data and CI data
  # Returns plot with subject points and means on sigmoid
  # Note: Only running on Experiments 1 & 2, so the parameters are fixed to eStrong & eWeak
  
  # generate data for logistic curve
  logitDat <- data.frame(x=seq(-5,5,.01)) %>%
    mutate(y = inv_logit(x))
  
  # Prepare data for plotting
  # calculate belief of eStrong + eWeak
  sigData <- plot_data %>%
    group_by(Chimpanzee) %>%
    mutate(both = sum(value))
  
  # combine eStrong, eWeak, and both
  df1 <- sigData[c(1,3,4)]
  df2 <- data.frame(name = sigData$Chimpanzee,
                    parameter = rep("both",nrow(sigData)),
                    value = sigData$both)
  sigData <- rbind(df1,df2)
  
  # calculate mean of both
  bothMean <- mean(sigData$value[sigData$parameter == "both"])
  sig_bars <- rbind(bar_data, data.frame(parameter = "both", fitted = bothMean,
                                         lower.cl = NA, upper.cl = NA,
                                         parameterF = "both", parameterF2 = "both"))
  
  # calculate choice probability for each belief strength
  sigData$yval <- inv_logit(sigData$value)
  sig_bars$yval <- inv_logit(sig_bars$fitted)
  
  # fix factor order for coloring
  sigData$parameterF <- factor(sigData$parameter, levels = c("eStrong", "both", "eWeak"), labels = c("strong", "strong + weak", "weak"))
  sig_bars$parameterF <- factor(sig_bars$parameter, levels = c("eStrong", "both", "eWeak"), labels = c("strong", "strong + weak", "weak"))
  
  # Plot
  p <- sigData %>%
    arrange(parameterF) %>%
    ggplot(data = ., aes(x = value, y = yval, fill = parameterF))+
    # base sigmoid
    geom_path(inherit.aes = F, data = logitDat, aes(x = x, y = y), 
              linewidth = 1.5, alpha = .5, color = "black")+
    # lines to y-axis from group means
    geom_segment(inherit.aes = F, data = sig_bars, linetype = 2, 
                 aes(x = fitted, xend = 0, y = yval, yend = yval, color = parameterF))+
    # subject points
    geom_point(alpha = dotAlpha*.75, size = dotSize*1.5, shape = 21, color = "black")+
    # mean points
    geom_point(alpha = .9, inherit.aes = F, data = sig_bars, shape=23, size = group_dotSize*1.5,
               aes(x = fitted, y = yval, fill = parameterF))+
    # aesthetics
    theme_classic()+ my_theme + 
    theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
    labs(x = ytext_param,y = ytext_followStrong, 
         color = "type of evidence", fill = "type of evidence")+
    coord_axes_inside(labels_inside = TRUE)+
    scale_y_continuous(labels = scales::percent)+
    scale_colour_viridis_d(begin=beginCol,end=endCol, option="magma", labels = parse_format())+
    scale_fill_viridis_d(begin=beginCol,end=endCol, option="magma", labels = parse_format())
  
  return(p)
}












