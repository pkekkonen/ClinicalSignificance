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
            m_pre <- mean(values_pre) #get the mean of pre values
            m_post <- mean(values_post) #get the mean of post values
            std_pre <- sd(values_pre) # get the standard deviation of pre values
            std_post <- sd(values_post) # get the standard deviation of post values


            if(self$options$cutoffs == "a") { #check which cut off point
                if(self$options$higherBetter) { # Checks if higher score indicates improvement
                    result_abc <- m_pre+2*std_pre
                } else {
                    result_abc <- m_pre-2*std_pre
                }
            }
            if(self$options$cutoffs == "b") { #check which cut off point
                if(self$options$higherBetter) { # Checks if higher score indicates improvement
                    result_abc <- m_post-2*std_post
                } else {
                    result_abc <- m_post+2*std_post
                }

            }
            if(self$options$cutoffs == "c") { #check which cut off point
                result_abc <- (std_post * m_pre + std_pre * m_post)/(std_post + std_pre)
            }

            if(self$options$higherBetter) {
                improved_or_not <- c(ifelse(values_post >= result_abc,"yes","no")) # Checks whether or not patient is above cutoff-point
            } else {
                improved_or_not <- c(ifelse(values_post <= result_abc,"yes","no")) # Checks whether or not patient is below cutoff-point
            }


            score <- c(values_pre) # Patients scores

            df <- data.frame(improved_or_not = improved_or_not, score = score) # Dataframe consisting of if a patient is above cutoff-point and the patients scores

            frequency_df <- as.data.frame(table(df$improved_or_not)) # Frequency dataframe of patient treatments outcomes
            colnames(frequency_df) <- c("improved", "no_of_patients")


            image <- self$results$plot
            image$setState(frequency_df)

            # Calculation for RCI
            r_value <- self$options$valueOfR
            # std_for_chosen_cutoff_point <- sd(values_pre) # OBS! SHOULD BE SD OF THE DECIDED CUTOFF POINT. SO RIGHT NOW ONLY APPLICEABLE IF CUTOFF POINT A IS USED
            standard_error_of_measurement <- std_pre*sqrt(1-r_value)
            s_diff <- sqrt(2*(standard_error_of_measurement ^ 2))

            # We want to create rci boundary lines (y=kx+m) where x = values_pre, y = values_post, k = 1 and m (the interception point) is the negative and positive value of the following
            interception_point = s_diff*1.96

            self$results$text$setContent(interception_point) # Print r

            df_dotplot <- data.frame(values_pre = values_pre, values_post = values_post, result_abc = result_abc, interception_point = interception_point) # Dataframe consisting of pre and postvalues

            colnames(df_dotplot) <- c("values_pre", "values_post", "result_abc", "interception_point")

            image_dot <- self$results$dotplot
            image_dot$setState(df_dotplot)
            # image_dot$setState(list(df_dotplot, result interception_point))


        },
        .plot=function(image, ...) {
            plotData <- image$state
            plot <- ggplot(data=plotData, aes(x=improved, y=no_of_patients)) +
                geom_bar(stat="identity") # Barplot
            print(plot)
            TRUE
        },
        .dotplot=function(image_dot, ...) {
            plotData <- df <- subset(image_dot$state, select = c(values_pre, values_post))
            result_abc <- image_dot$state$result_abc
            interception_point <- image_dot$state$interception_point
            # plotData <- image_dot$state[1]
            # result <- image_dot$state[2]
            # interception_point <- image_dot$state[3]

            dot_plot <- ggplot(data=plotData, aes(x=values_pre, y = values_post)) +
                geom_point(data = plotData, stat="identity") +
                geom_abline(show.legend = TRUE, aes(intercept = result_abc, slope=0,linetype = "Cutoff point", color="Cutoff point")) +
                geom_abline(show.legend = TRUE, aes(intercept=interception_point, slope=1, linetype="Boundary for reliable change", color="Boundary for reliable change")) + # rci boundary

                geom_abline(show.legend = TRUE, aes(intercept=0, slope=1, linetype = "No change", color = "No change")) + # line indicating no change
                # OBS: När jag lägger linetype i aes dyker legends upp men då blir den faltisla linetypen fel men om jag lägger de utanför dyker inga legends upp :(

                geom_abline(show.legend = TRUE, aes( intercept=-interception_point, slope=1, linetype="Boundary for reliable change", color="Boundary for reliable change")) + # rci boundary
             #   scale_linetype_discrete(name = "Status", labels = c("No change", "RCI boundary"))
                scale_linetype_manual(values=c("Boundary for reliable change"="dashed", "No change"="solid", "Cutoff point"="solid"))+
                scale_color_manual(values=c("Boundary for reliable change"="black", "No change"="black", "Cutoff point"="red"))+
                theme(legend.position = "right") +
                labs(color  = "Line explanations", linetype = "Line explanations") # Used to get legends for both line type and color at the same time


            print(dot_plot)
            TRUE
        }
        )
)

