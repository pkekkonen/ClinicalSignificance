
# This file is a generated template, your changes will not be overwritten

clinsigClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "clinsigClass",
    inherit = clinsigBase,
    private = list(
        .run = function() {


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


            # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
            #                                                  RCI CALCULATION                                                        #
            # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

            # Calculation for RCI
            r_value <- self$options$valueOfR
            # std_for_chosen_cutoff_point <- sd(values_pre) # OBS! SHOULD BE SD OF THE DECIDED CUTOFF POINT. SO RIGHT NOW ONLY APPLICEABLE IF CUTOFF POINT A IS USED
            standard_error_of_measurement <- std_pre*sqrt(1-r_value)
            s_diff <- sqrt(2*(standard_error_of_measurement ^ 2))

            # We want to create rci boundary lines (y=kx+m) where x = values_pre, y = values_post, k = 1 and m (the interception point) is the negative and positive value of the following
            interception_point = s_diff*1.96


            # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
            #                                             PATIENT STATUS CALCULATION                                                  #
            # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
            if(self$options$higherBetter) {
                patient_status <- c(ifelse(values_post-values_pre >= interception_point, ifelse(values_post >= result_abc,"Recovered","Improved"),ifelse(values_post-values_pre <= -interception_point,"Detoriated","Unchanged"))) # Checks whether or not patient is above cutoff-point
            } else {
                patient_status <- c(ifelse(values_post-values_pre <= -interception_point,ifelse(values_post <= result_abc,"Recovered","Improved"),ifelse(values_post-values_pre >= interception_point,"Detoriated","Unchanged"))) # Checks whether or not patient is above cutoff-point
            }

            # self$results$text$setContent(patient_status) # Print df


            df_dotplot <- data.frame(values_pre = values_pre, values_post = values_post, patient_status = patient_status, result_abc = result_abc, interception_point = interception_point) # Dataframe consisting of pre and postvalues


            colnames(df_dotplot) <- c("values_pre", "values_post", "patient_status", "result_abc", "interception_point")

            image_dot <- self$results$dotplot
            image_dot$setState(df_dotplot)
            # image_dot$setState(list(df_dotplot, result interception_point))


            # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
            #                                                        BAR PLOT                                                         #
            # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

            score <- c(values_pre) # Patients scores

            df <- data.frame(patient_status = patient_status, score = score) # Dataframe consisting of if a patient is above cutoff-point and the patients scores

            frequency_df <- as.data.frame(table(df$patient_status)) # Frequency dataframe of patient treatments outcomes
            colnames(frequency_df) <- c("patient_status", "no_of_patients")


            image <- self$results$plot
            image$setState(frequency_df)
            print(frequency_df)

            # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
            #                                                        TABLE                                                            #
            # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


            self$results$table$setRow(1, values = list(
                patient_status = "Number of patients",
                Detoriated = frequency_df$no_of_patients[1],
                Improved = frequency_df$no_of_patients[2],
                Recovered = frequency_df$no_of_patients[3],
                Unchanged = frequency_df$no_of_patients[4]

            ))

            print(self$results$table)

        },
        .plot=function(image, ...) {
            plotData <- image$state
            plot <- ggplot(data=plotData, aes(x=patient_status, y=no_of_patients, fill = patient_status)) +
                geom_bar(stat="identity") + scale_fill_manual(values=c("Recovered"="green", "Improved"="blue", "Unchanged"="orange", "Detoriated"="red")) +
                labs(x = NULL, y = "Number of patients", fill = "Patient status") # Barplot
            print(plot)
            TRUE
        },
        .dotplot=function(image_dot, ...) {
            plotData <- df <- subset(image_dot$state, select = c(values_pre, values_post, patient_status))
            result_abc <- image_dot$state$result_abc
            interception_point <- image_dot$state$interception_point

            dot_plot <- ggplot(data=plotData, aes(x=values_pre, y = values_post)) +
                geom_point(aes( fill = factor(patient_status)), size=3, shape=21, stroke=0) +
                geom_abline(aes(intercept = result_abc, slope=0,linetype = "Cutoff point", color="Cutoff point")) +
                geom_abline( aes(intercept=interception_point, slope=1, linetype="Boundary for reliable change", color="Boundary for reliable change")) + # rci boundary

                geom_abline(aes(intercept=0, slope=1, linetype = "No change", color = "No change")) + # line indicating no change

                geom_abline(aes( intercept=-interception_point, slope=1, linetype="Boundary for reliable change", color="Boundary for reliable change")) + # rci boundary
                scale_fill_manual(values=c("Recovered"="green", "Improved"="blue", "Unchanged"="orange", "Detoriated"="red"))+

             #   scale_linetype_discrete(name = "Status", labels = c("No change", "RCI boundary"))
                scale_linetype_manual(values=c("Boundary for reliable change"="dashed", "No change"="solid", "Cutoff point"="solid"))+
                scale_color_manual(values=c("Boundary for reliable change"="black", "No change"="black", "Cutoff point"="red"))+
                theme(legend.position = "right") +
                # labs(color  = "Status", linetype = "Line explanations") # Used to get legends for both line type and color at the same time
                labs(x = "Before treatment", y = "After treatment", linetype = "Line explanations", color = "Line explanations", fill= "Status") # Used to get legends for both line type and color at the same time


            print(dot_plot)
            TRUE
        }
        )
)
