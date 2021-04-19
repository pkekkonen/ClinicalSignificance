
# This file is a generated template, your changes will not be overwritten

clinsigClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "clinsigClass",
    inherit = clinsigBase,
    private = list(
        .run = function() {

            col_index <- grep(self$options$pre, colnames(self$data)) #get the index of the pre column
            values_pre = self$data[,col_index] #get the values of pre
            m_pre <- mean(values_pre) #get the mean
            std_pre <- sd(values_pre) # get the standard deviation
            print(std_pre)
            if(self$options$higherBetter) # Checks if higher score indicates improvement 
                results_a <- m_pre+2*std_pre
            else 
                results_a <- m_pre-2*std_pre
            self$results$text$setContent(results_a) #print the result

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

        })
)
