LL <- function(b1, sigma){
        ## This is the model formula
        Y.pred = b1 * value
        
        ## This is the negative log likelihood that MLE will try to minimize
        -sum(dnorm(response, mean=Y.pred, sd=sigma, log=TRUE))
}

# splits dataframe in half randomly
split_half <- function(d){
        val_length <- round(nrow(d) / 2)
        val_indices <- sample(seq_len(nrow(d)), size = val_length)
        return(val_indices)
}

# calculate BIC total for feel models
feel_BIC <- function(num){
        vector <- double(20)
        model_names <- character(20)
        
        for(i in 1:10){
                model_names[i] <- paste0(paste0("feel_expect_mod", i), ".fit")
        }
        
        for(i in 11:20){
                model_names[i] <- paste0(paste0("feel_experience_mod", i - 10), ".fit")
        }
        
        for(i in 1:num){
                for(j in 1:20){
                        model <- paste0(paste0(paste0(model_names[j], "[["), i), "]]") ## make feel_expect_mod j .fit[[ i ]]
                        model <- eval(parse(text = model))
                        if(is.na(model)){
                                vector[j] <- NA #### If there was an error when fitting, assign NA to indicate that the model was not good
                        } else{
                                vector[j] <- vector[j] + BIC(model)        
                        }
                }
        }
        return(vector)
}

# calculate BIC total for choice models
choice_BIC <- function(num){
        vector <- double(7)
        for(i in 1:10){
                model_names[i] <- paste0(paste0("choice_mod", i), ".fit")
        }
        
        for(i in 1:num){
                for(j in 1:7){
                        model <- paste0(paste0(paste0(model_names[j], "[["), i), "]]")
                        model <- eval(parse(text = model))
                        if(is.na(model)){
                                vector[j] <- NA 
                        } else{
                                vector[j] <- vector[j] + BIC(model)       
                        }
                }
        }
        return(vector)
}


# formulas from the paper
mod1 <- function(b, sigma){
        Y.pred = b * value
        -sum(dnorm(response, mean=Y.pred, sd=sigma, log=T))
}
mod2 <- function(bgain, bloss, sigma){
        Y.pred = ifelse(value > 0, bgain * value, bloss * value)
        -sum(dnorm(response, mean=Y.pred, sd=sigma, log=T))
}
mod3 <- function(b, p, sigma){
        Y.pred = ifelse(value > 0, b * abs(value) ^ p , -b * abs(value) ^ p)
        -sum(dnorm(response, mean=Y.pred, sd=sigma, log=T))
}
mod4 <- function(bgain, bloss, p, sigma){
        Y.pred = ifelse(value > 0, bgain * abs(value) ^ p, -bloss * abs(value) ^ p)
        -sum(dnorm(response, mean=Y.pred, sd=sigma, log=T))
}
mod5 <- function(b, pgain, ploss, sigma){
        Y.pred = ifelse(value > 0, b * abs(value) ^ pgain, -b * abs(value) ^ ploss)
        -sum(dnorm(response, mean=Y.pred, sd=sigma, log=T))
}
mod6 <- function(bgain, bloss, pgain, ploss, sigma){
        Y.pred = ifelse(value > 0, bgain * abs(value) ^ pgain, -bloss * abs(value) ^ ploss) 
        -sum(dnorm(response, mean=Y.pred, sd=sigma, log=T))
}
mod7 <- function(b, e, sigma){
        Y.pred = ifelse(value > 0, b * value + e, b * value - e) 
        -sum(dnorm(response, mean=Y.pred, sd=sigma, log=T))
}
mod8 <- function(bgain, bloss, e, sigma){
        Y.pred = ifelse(value > 0, bgain * value + e, bloss * value - e)   
        -sum(dnorm(response, mean=Y.pred, sd=sigma, log=T))
}
mod9 <- function(b, egain, eloss, sigma){
        Y.pred = ifelse(value > 0, b * value + egain, b * value - eloss)  
        -sum(dnorm(response, mean=Y.pred, sd=sigma, log=T))
}
mod10 <- function(bgain, bloss, egain, eloss, sigma){
        Y.pred = ifelse(value > 0, bgain * value + egain, bloss * value - eloss)
        -sum(dnorm(response, mean=Y.pred, sd=sigma, log=T))
}

## Here begins the worst code that I've ever written in my entire life

# function that fits all the feeling model
feel_fit <- function(num){
        
        ## init empty lists of models
        feel_expect_mod1.fit <- list()
        feel_expect_mod2.fit <- list()
        feel_expect_mod3.fit <- list()
        feel_expect_mod4.fit <- list()
        feel_expect_mod5.fit <- list()
        feel_expect_mod6.fit <- list()
        feel_expect_mod7.fit <- list()
        feel_expect_mod8.fit <- list()
        feel_expect_mod9.fit <- list()
        feel_expect_mod10.fit <- list()
        feel_experience_mod1.fit <- list()
        feel_experience_mod2.fit <- list()
        feel_experience_mod3.fit <- list()
        feel_experience_mod4.fit <- list()
        feel_experience_mod5.fit <- list()
        feel_experience_mod6.fit <- list()
        feel_experience_mod7.fit <- list()
        feel_experience_mod8.fit <- list()
        feel_experience_mod9.fit <- list()
        feel_experience_mod10.fit <- list()
        
        
        ## fit each model to each participant
        for(i in 1:num){
                
                temp_feel_expect <- feel_expect %>% filter(id == i)
                temp_feel_experience <- feel_experience %>% filter(id == i)
                
                #estimation of residual error (upper bounds)
                sigma1 <- sd(temp_feel_expect$response)
                sigma2 <- sd(temp_feel_expect$response)
                
                feel_expect_mod1.fit <- c(feel_expect_mod1.fit, 
                                          list(mle2(mod1, start=list(b=1, sigma=sigma1), data=temp_feel_expect)))
                
                feel_expect_mod2.fit <- c(feel_expect_mod2.fit, 
                                          list(mle2(mod2, start=list(bgain=1, bloss=1, sigma=sigma1), data=temp_feel_expect)))
                
                feel_expect_mod3.fit <- c(feel_expect_mod3.fit, 
                                          list(mle2(mod3, start=list(b=1, p=.5, sigma=sigma1), data=temp_feel_expect)))
                
                feel_expect_mod4.fit <- c(feel_expect_mod4.fit, 
                                          list(mle2(mod4, start=list(bgain=1, bloss=1, p=.5, sigma=sigma1), data=temp_feel_expect)))
                
                feel_expect_mod5.fit <- c(feel_expect_mod5.fit, 
                                          list(mle2(mod5, start=list(b=1, pgain=.5, ploss=.5, sigma=sigma1), data=temp_feel_expect)))
                
                feel_expect_mod6.fit <- c(feel_expect_mod6.fit, 
                                          list(mle2(mod6, start=list(bgain=1, bloss=1, pgain=.5, ploss=.5, sigma=sigma1), data=temp_feel_expect)))
                
                feel_expect_mod7.fit <- c(feel_expect_mod7.fit, 
                                          list(mle2(mod7, start=list(b=1, e=.5, sigma=sigma1), data=temp_feel_expect)))
                
                feel_expect_mod8.fit <- c(feel_expect_mod8.fit, 
                                          list(mle2(mod8, start=list(bgain=1, bloss=1, e=.5, sigma=sigma1), data=temp_feel_expect)))
                
                feel_expect_mod9.fit <- c(feel_expect_mod9.fit, 
                                          list(mle2(mod9, start=list(b=1, egain=.5, eloss=.5, sigma=sigma1), data=temp_feel_expect)))
                
                feel_expect_mod10.fit <- c(feel_expect_mod10.fit, 
                                           list(mle2(mod10, start=list(bgain=1, bloss=1, egain=.5, eloss=.5, sigma=sigma1), data=temp_feel_expect)))
                
                feel_experience_mod1.fit <- c(feel_experience_mod1.fit, 
                                              list(mle2(mod1, start=list(b=1, sigma=sigma2), data=temp_feel_experience)))
                
                feel_experience_mod2.fit <- c(feel_experience_mod2.fit, 
                                              list(mle2(mod2, start=list(bgain=1, bloss=1, sigma=sigma2), data=temp_feel_experience)))
                
                feel_experience_mod3.fit <- c(feel_experience_mod3.fit, 
                                              list(mle2(mod3, start=list(b=1, p=.5, sigma=sigma2), data=temp_feel_experience)))
                
                feel_experience_mod4.fit <- c(feel_experience_mod4.fit, 
                                              list(mle2(mod4, start=list(bgain=1, bloss=1, p=.5, sigma=sigma2), data=temp_feel_experience)))
                
                feel_experience_mod5.fit <- c(feel_experience_mod5.fit, 
                                              list(mle2(mod5, start=list(b=1, pgain=.5, ploss=.5, sigma=sigma2), data=temp_feel_experience)))
                
                feel_experience_mod6.fit <- c(feel_experience_mod6.fit, 
                                              list(mle2(mod6, start=list(bgain=1, bloss=1, pgain=.5, ploss=.5, sigma=sigma2), data=temp_feel_experience)))
                
                feel_experience_mod7.fit <- c(feel_experience_mod7.fit, 
                                              list(mle2(mod7, start=list(b=1, e=.5, sigma=sigma2), data=temp_feel_experience)))
                
                feel_experience_mod8.fit <- c(feel_experience_mod8.fit, 
                                              list(mle2(mod8, start=list(bgain=1, bloss=1, e=.5, sigma=sigma2), data=temp_feel_experience)))
                
                feel_experience_mod9.fit <- c(feel_experience_mod9.fit, 
                                              list(mle2(mod9, start=list(b=1, egain=.5, eloss=.5, sigma=sigma2), data=temp_feel_experience)))
                feel_experience_mod10.fit <- c(feel_experience_mod10.fit, 
                                               list(mle2(mod10, start=list(bgain=1, bloss=1, egain=.5, eloss=.5, sigma=sigma2), data=temp_feel_experience)))
        }
        
        #make these models a global variable
        assign("feel_expect_mod1.fit", feel_expect_mod1.fit, envir = .GlobalEnv)
        assign("feel_expect_mod2.fit", feel_expect_mod2.fit, envir = .GlobalEnv)
        assign("feel_expect_mod3.fit", feel_expect_mod3.fit, envir = .GlobalEnv)
        assign("feel_expect_mod4.fit", feel_expect_mod4.fit, envir = .GlobalEnv)
        assign("feel_expect_mod5.fit", feel_expect_mod5.fit, envir = .GlobalEnv)
        assign("feel_expect_mod6.fit", feel_expect_mod6.fit, envir = .GlobalEnv)
        assign("feel_expect_mod7.fit", feel_expect_mod7.fit, envir = .GlobalEnv)
        assign("feel_expect_mod8.fit", feel_expect_mod8.fit, envir = .GlobalEnv)
        assign("feel_expect_mod9.fit", feel_expect_mod9.fit, envir = .GlobalEnv)
        assign("feel_expect_mod10.fit", feel_expect_mod10.fit, envir = .GlobalEnv)
        assign("feel_experience_mod1.fit", feel_experience_mod1.fit, envir = .GlobalEnv)
        assign("feel_experience_mod2.fit", feel_experience_mod2.fit, envir = .GlobalEnv)
        assign("feel_experience_mod3.fit", feel_experience_mod3.fit, envir = .GlobalEnv)
        assign("feel_experience_mod4.fit", feel_experience_mod4.fit, envir = .GlobalEnv)
        assign("feel_experience_mod5.fit", feel_experience_mod5.fit, envir = .GlobalEnv)
        assign("feel_experience_mod6.fit", feel_experience_mod6.fit, envir = .GlobalEnv)
        assign("feel_experience_mod7.fit", feel_experience_mod7.fit, envir = .GlobalEnv)
        assign("feel_experience_mod8.fit", feel_experience_mod8.fit, envir = .GlobalEnv)
        assign("feel_experience_mod9.fit", feel_experience_mod9.fit, envir = .GlobalEnv)
        assign("feel_experience_mod10.fit", feel_experience_mod10.fit, envir = .GlobalEnv)
        
}

#value transformation function; returns a list or predicted values; win == 1, lost == 2, sure == 3
transform_predict <- function(d, model){
        coef <- coef(model)
        b <- coef[['b']]
        p <- coef[['p']]
        twin <- ifelse(d$win > 0, b * abs(d$win) ^ p , -b * abs(d$win) ^ p)
        tlose <- ifelse(d$lose > 0, b * abs(d$lose) ^ p , -b * abs(d$lose) ^ p)
        tsure <- ifelse(d$sure > 0, b * abs(d$sure) ^ p , -b * abs(d$sure) ^ p)
        d$win <- twin
        d$lose <- tlose
        d$sure <- tsure
        return(as.data.frame(d))
}

# log transformation function for choice model 4
log0 <- function(x){
        
        if(length(x) == 1){
                
                if(x == 0){
                        ans = 0
                } else if(x < 0){
                        ans = -log1p(-x)
                } else{
                        ans = log1p(x)
                }
                return(ans)
        } else{
                return(sapply(x, FUN = log0))
        }
}

transform_log <- function(d){
        d$win <- log0(d$win)
        d$lose <- log0(d$lose)
        d$sure <- log0(d$sure)
        return(as.data.frame(d))
}

# choice model formulas from the paper

#standard logistic regression
choice_mod1 <- function(b1, b2, b3, sigma){
        Y.pred = 1 / (1 + exp(-(b1 * win + b2 * lose + b3 * sure)))
        -sum(log((Y.pred ^ gamble) * (1 - Y.pred) ^ (1 - gamble)))
}

#loss aversion model
choice_mod5 <- function(u, lamda, sigma){
        Y.pred = 1 / (1 + exp(-(u * (0.5 * win + 0.5 * lamda * abs(lose)))))
        suppressWarnings(-sum(log((Y.pred ^ gamble) * (1 - Y.pred) ^ (1 - gamble))))
}

#risk aversion model
choice_mod6 <- function(u, gamma, sigma){
        Y.pred = 1 / (1 + exp(-(u * (0.5 * (abs(win) ^ gamma) - 0.5 * (abs(lose) ^ gamma)))))
        -sum(log((Y.pred ^ gamble) * (1 - Y.pred) ^ (1 - gamble)))
}

#risk and loss aversion model
choice_mod7 <- function(u, lamda, gamma, sigma){
        Y.pred = 1 / (1 + exp(-(u * (0.5 * (abs(win) ^ gamma) - 0.5 * lamda * (abs(lose) ^ gamma)))))
        -sum(log((Y.pred ^ gamble) * (1 - Y.pred) ^ (1 - gamble)))
}

# function that fits all the choice functions
choice_fit <- function(split = TRUE, num){
        
        choice_mod1.fit <- list()
        choice_mod2.fit <- list()
        choice_mod3.fit <- list()
        choice_mod4.fit <- list()
        choice_mod5.fit <- list()
        choice_mod6.fit <- list()
        choice_mod7.fit <- list()
        
        for(i in 1:num){
                temp_gamble_block <- gamble_block %>% filter(id == i)
                if(split == TRUE){
                        ind <- split_half(temp_gamble_block)
                        temp_gamble_block <- temp_gamble_block[ind, ]
                }
                
                sigma = 1
                
                choice_mod1.fit <- c(choice_mod1.fit, list(mle2(choice_mod1, start=list(b1=1, b2=1, b3=1, sigma=sigma), 
                                                                data = transform_predict(temp_gamble_block, feel_expect_mod3.fit[[i]]))))
                
                choice_mod2.fit <- c(choice_mod2.fit, list(mle2(choice_mod1, start=list(b1=1, b2=1, b3=1, sigma=sigma), 
                                                                data = transform_predict(temp_gamble_block, feel_experience_mod3.fit[[i]]))))
                
                choice_mod3.fit <- c(choice_mod3.fit, list(mle2(choice_mod1, start=list(b1=1, b2=1, b3=1, sigma=sigma), 
                                                                data = temp_gamble_block)))
                
                choice_mod4.fit <- c(choice_mod4.fit, list(mle2(choice_mod1, start=list(b1=1, b2=1, b3=1, sigma=sigma), 
                                                                data = transform_log(temp_gamble_block))))
                
                choice_mod5.fit <- c(choice_mod5.fit, list(mle2(choice_mod5, start=list(u=1, lamda=2, sigma=sigma), 
                                                                data = temp_gamble_block)))
                
                choice_mod6.fit <- c(choice_mod6.fit, list(mle2(choice_mod6, start=list(u=1, gamma=1, sigma=sigma), 
                                                                data = temp_gamble_block)))
                
                choice_mod7.fit <- c(choice_mod7.fit, list(mle2(choice_mod7, start=list(u=1, lamda=2, gamma=1, sigma=sigma), 
                                                                data = temp_gamble_block)))
                
                
        }
        
        #make these models a global variable
        assign("choice_mod1.fit", choice_mod1.fit, envir = .GlobalEnv)
        assign("choice_mod2.fit", choice_mod2.fit, envir = .GlobalEnv)
        assign("choice_mod3.fit", choice_mod3.fit, envir = .GlobalEnv)
        assign("choice_mod4.fit", choice_mod4.fit, envir = .GlobalEnv)
        assign("choice_mod5.fit", choice_mod5.fit, envir = .GlobalEnv)
        assign("choice_mod6.fit", choice_mod6.fit, envir = .GlobalEnv)
        assign("choice_mod7.fit", choice_mod7.fit, envir = .GlobalEnv)
}

## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
        library(plyr)
        
        # New version of length which can handle NA's: if na.rm==T, don't count them
        length2 <- function (x, na.rm=FALSE) {
                if (na.rm) sum(!is.na(x))
                else       length(x)
        }
        
        # This does the summary. For each group's data frame, return a vector with
        # N, mean, and sd
        datac <- ddply(data, groupvars, .drop=.drop,
                       .fun = function(xx, col) {
                               c(N    = length2(xx[[col]], na.rm=na.rm),
                                 mean = mean   (xx[[col]], na.rm=na.rm),
                                 sd   = sd     (xx[[col]], na.rm=na.rm)
                               )
                       },
                       measurevar
        )
        
        # Rename the "mean" column    
        datac <- rename(datac, c("mean" = measurevar))
        
        datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
        
        # Confidence interval multiplier for standard error
        # Calculate t-statistic for confidence interval: 
        # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
        ciMult <- qt(conf.interval/2 + .5, datac$N-1)
        datac$ci <- datac$se * ciMult
        
        return(datac)
}