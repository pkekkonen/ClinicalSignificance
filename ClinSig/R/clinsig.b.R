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
            if(self$options$higherBetter) # Checks if higher score indicates improvement
                results_a <- m_pre+2*std_pre
            else
                results_a <- m_pre-2*std_pre

            improved_or_not <- c(ifelse(values_pre >= results_a,"yes","no")) # Checks whether or not patient is above or equal to cutoff-point a)
            score <- c(values_pre) # Patients scores

            df <- data.frame(improved_or_not = improved_or_not, score = score) # Dataframe consisting of if a patient is above cutoff-point and the patients scores

            frequency_df <- as.data.frame(table(df$improved_or_not)) # Frequency dataframe of patient treatments outcomes
            colnames(frequency_df) <- c("improved", "no_of_patients")

            self$results$text$setContent(frequency_df) # Print the result of the frequency dataframe

            image <- self$results$plot
            image$setState(frequency_df)



            df_dotplot <- data.frame(values_pre = values_pre, values_post = values_post) # Dataframe consisting of if a patient is above cutoff-point and the patients scores

            colnames(df_dotplot) <- c("values_pre", "values_post")

            image_dot <- self$results$dotplot
            image_dot$setState(df_dotplot)


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
            dot_plot <- qplot(values_pre, values_post, data=plotData)
            print(dot_plot)
            TRUE
        }
        )
)
