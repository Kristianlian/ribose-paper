





# A function for checking model assumptions

# model, a model object fitted with lme4 or nlme
# nested, for nlme::lme models, to get estimates of random effects from a nested model set to TRUE
# lme4, if the model is fitted with lme4 set to TRUE

mod_assume <- function(model, nested = FALSE, lme4 = TRUE) {
  
  df <- data.frame(resid = resid(model, type = "pearson"), 
                   fitted = fitted(model))
  
  qq <- ggplot(df, aes(sample = resid)) + geom_qq(shape = 21, color = "blue", size = 2.5) +
    geom_qq_line(linetype = "dashed") +
    labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_minimal() + coord_flip()
  
  fr <- ggplot(df, aes(fitted, resid)) + geom_point(shape = 21, size = 2.5, color = "blue") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "Fitted values", y = "Standardized residuals") +
    theme_minimal()

  if(lme4 == TRUE) {
    
    random_effects_intervals <- data.frame(lme4::VarCorr(model)) %>%
      mutate(type = if_else(is.na(var2), "sd", "cor"),
             coef = if_else(is.na(var2), var1, paste0(var2, ".", var1)), 
             coef = if_else(grp == "Residual", "sigma", coef), 
             type = if_else(coef == "sigma", "sigma", type)) %>%
      dplyr::select(coef,type, est = sdcor) %>%
      dplyr::inner_join(
        
        data.frame(lme4::confint.merMod(model, method = "boot", oldNames = FALSE, boot.type = "perc", parallel = "snow")) %>%
          
          dplyr::mutate(effect = rownames(.)) %>%
          data.frame(row.names = NULL) %>%
          tidyr::separate(effect, into = c("type", "coef"), sep = "_") %>%
          dplyr::mutate(coef = if_else(!(type %in% c("sd", "cor")), type, coef), 
                 type = if_else(!(type %in% c("sd", "cor", "sigma")), "fixed", type), 
                 coef = gsub("\\|subject", "", coef)) %>%
          dplyr::select(coef, type, lower = X2.5.., upper = X97.5..)) %>%
      
      mutate(coef = fct_reorder(coef, type)) %>%
      
      ggplot(aes(est, coef, fill = type)) + geom_errorbarh(aes(xmin = lower, xmax = upper)) +
      geom_point(shape = 21, color = "blue", size = 2.5) +
      theme_minimal()
      
  } else {
    
    if(nested == TRUE) {
      random_effects_intervals <- data.frame(intervals(model, which = "var-cov")$reStruct) %>%
        pivot_longer(everything()) %>%
        separate(name, into = c("effect", "est")) %>%
        pivot_wider(names_from = est, values_from = value) %>%
        
        ggplot(aes(est, effect)) + geom_point(shape = 21, color = "blue", fill = "black") +
        geom_errorbarh(aes(xmin = lower, xmax = upper)) + 
        labs(x = "Estimate (95% CI)", 
             y = "")+
        theme_minimal()
      
      
    } else {
      random_effects_intervals <- data.frame(intervals(model, which = "var-cov")$reStruct$subject) %>%
        mutate(effect = rownames(.)) %>%
        ggplot(aes(est., effect)) + geom_point(shape = 21, color = "blue", fill = "black") +
        geom_errorbarh(aes(xmin = lower, xmax = upper)) + 
        labs(x = "Estimate (95% CI)", 
             y = "")+
        theme_minimal()
      
    }
    
    
  }
  
  
  cowplot::plot_grid(cowplot::plot_grid(qq, fr, ncol = 2), random_effects_intervals, ncol = 1)
  
}





### A function that returns the model with maximal number of random effects but without singularity issues

return_best <- function(models) {
  # Count the number of non-zero random effects (nranef),
  # Check if singular (singular)
  results <- data.frame(models = names(models), 
                        singular = unlist(lapply(models, isSingular)), 
                        nranef = unlist(lapply(models, function(x) {sum(sapply(coef(x)$subject, var) > 0)})), 
                        row.names = NULL)
  # Filter df based on singular requirement
  filtered <- results[!results$singular, ]
  # Return the model with maximum number of nonzero variance random effects
  return_model <- filtered[which.max(filtered$nranef),]$models
  
  return(models[[return_model]])
  
}




