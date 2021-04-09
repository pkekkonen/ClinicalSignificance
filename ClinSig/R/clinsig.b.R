
# This file is a generated template, your changes will not be overwritten

clinsigClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "clinsigClass",
    inherit = clinsigBase,
    private = list(
        .run = function() {

            # Hard coded for the moment being
            m1 <- mean(self$data$len)
            sd1 <- sd(self$data$len)
            results <- m1
            self$results$text$setContent(results)

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

        })
)
