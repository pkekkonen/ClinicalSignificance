
# This file is a generated template, your changes will not be overwritten

clinsigClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "clinsigClass",
    inherit = clinsigBase,
    private = list(
        .run = function() {

            col_index <- grep(self$options$pre, colnames(self$data)) #get the index of the pre column 
            m1 <- mean(self$data[,col_index]) #get the mean
            # sd1 <- sd(self$data$len)
            results <- m1
            self$results$text$setContent(results) #print the result

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

        })
)
