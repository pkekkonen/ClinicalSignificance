library(ggplot2) # For plotting

# This file is a generated template, your changes will not be overwritten

clinsigClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "clinsigClass",
    inherit = clinsigBase,
    private = list(
        .run = function() {
            # formula <- paste(self$options$pre, '~', self$options$post)
            # formula <- as.formula(formula)
            # means  <- aggregate(formula, self$data, mean)[,2]

            col_index_pre <- grep(self$options$pre, colnames(self$data)) #get the index of the pre column
            values_pre <- self$data[,col_index_pre] #get the values of pre
            col_index_post <- grep(self$options$post, colnames(self$data)) #get the index of the post column
            values_post <- self$data[,col_index_post] #get the values of post
            m_pre <- mean(values_pre) #get the mean
            std_pre <- sd(values_pre) # get the standard deviation
            print(std_pre)

            if(self$options$higherBetter) { # Checks if higher score indicates improvement
                results_a <- m_pre+2*std_pre
                improved_or_not <- c(ifelse(values_post >= results_a,"yes","no")) # Checks whether or not patient is above cutoff-point a)
            } else {
                results_a <- m_pre-2*std_pre
                improved_or_not <- c(ifelse(values_post <= results_a,"yes","no")) # Checks whether or not patient is below cutoff-point a)
            }

            score <- c(values_pre) # Patients scores

            df <- data.frame(improved_or_not = improved_or_not, score = score) # Dataframe consisting of if a patient is above cutoff-point and the patients scores

            frequency_df <- as.data.frame(table(df$improved_or_not)) # Frequency dataframe of patient treatments outcomes
            colnames(frequency_df) <- c("improved", "no_of_patients")


            image <- self$results$plot
            image$setState(frequency_df)
            
            # Calculation for RCI
            r_value <- self$options$valueOfR
            std_for_chosen_cutoff_point <- sd(values_pre) # OBS! SHOULD BE SD OF THE DECIDED CUTOFF POINT. SO RIGHT NOW ONLY APPLICEABLE IF CUTOFF POINT A IS USED
            standard_error_of_measurement <- std_for_chosen_cutoff_point*sqrt(1-r_value)
            s_diff <- sqrt(2*(standard_error_of_measurement ^ 2))
            
            # We want to create rci boundary lines (y=kx+m) where x = values_pre, y = values_post, k = 1 and m (the interception point) is the negative and positive value of the following
            interception_point = s_diff*1.96

            self$results$text$setContent(interception_point) # Print r
            
            df_dotplot <- data.frame(values_pre = values_pre, values_post = values_post, results_a = results_a, interception_point = interception_point) # Dataframe consisting of pre and postvalues

            colnames(df_dotplot) <- c("values_pre", "values_post", "results_a", "interception_point")

            image_dot <- self$results$dotplot
            image_dot$setState(df_dotplot)            
            # image_dot$setState(list(df_dotplot, results_a interception_point))            
            

        },
        .plot=function(image, ...) {
            plotData <- image$state
            plot <- ggplot(data=plotData, aes(x=improved, y=no_of_patients)) +
                geom_bar(stat="identity") # Barplot
            print(plot)
            TRUE
        },
        .dotplot=function(image_dot, ...) {
            plotData <- image_dot$state
            results_a <- image_dot$state$results_a
            interception_point <- image_dot$state$interception_point
            # plotData <- image_dot$state[1]
            # results_a <- image_dot$state[2]
            # interception_point <- image_dot$state[3]
            dot_plot <- ggplot(data=plotData, aes(x=values_pre, y = values_post)) +
                geom_point(data = plotData, stat="identity") + geom_hline(yintercept = results_a, color = "red") +
                geom_abline(intercept=interception_point, slope=1, linetype="dashed") + # rci boundary
                geom_abline(intercept=0, slope=1) + # line indicating no change
                geom_abline(intercept=-interception_point, slope=1, linetype="dashed") # rci boundary

            print(dot_plot)
            TRUE
        }
        )
)
