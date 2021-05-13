library(ggplot2) # For plotting

clinsigClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "clinsigClass",
    inherit = clinsigBase,
    private = list(
        .run = function() {
            if ((!is.null(self$options$pre)) & (!is.null(self$options$post))) {

                # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
                #                                            DATA EXTRACTION AND PREPERATION                                              #
                # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

                # Extract pre and post values
                col_index_pre <- grep(self$options$pre, colnames(self$data)) #get the index of the pre column
                values_pre <- self$data[,col_index_pre] #get the values of pre
                col_index_post <- grep(self$options$post, colnames(self$data)) #get the index of the post column
                values_post <- self$data[,col_index_post] #get the values of post

                # Extract groups
                if(!is.null(self$options$groupingVar)) { # Check if grouping variable is present
                    col_index_group <- grep(self$options$groupingVar, colnames(self$data)) #get the index of the groups column
                    values_group <- self$data[,col_index_group] #get the values of groups
                } else {
                    values_group <- rep(1, length(values_pre)) # If grouping variable is not present, fill column with 1's
                }

                # Delete rows that has at least one NA
                dataframe_with_na <- data.frame(values_pre = values_pre, values_post = values_post, values_group=values_group) # Dataframe consisting of pre and postvalues and groups
                dataframe_without_na <- na.omit(dataframe_with_na)

                values_post <- dataframe_without_na$values_post
                values_pre <- dataframe_without_na$values_pre
                values_group <- dataframe_without_na$values_group

                # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
                #                                             MEAN AND STANDARD DEVIATION                                                 #
                # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

                if(self$options$dysNorms == "manualValues") {
                    m_pre <- self$options$dys_mean # Get the inputted mean
                    sd_pre <- self$options$dys_sd # Get the inputted standard deviation
                } else {
                    m_pre <- mean(values_pre) # Calculate the mean of pre values
                    sd_pre <- sd(values_pre) # Calculate the standard deviation of pre values
                }

                # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
                #                                                 CUTOFF POINTS                                                           #
                # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

                if(self$options$cutoffs == "a") {
                    if(self$options$higherBetter) { # Checks if higher score indicates improvement
                        result_abc <- m_pre+2*sd_pre
                    } else {
                        result_abc <- m_pre-2*sd_pre
                    }
                }

                if(self$options$cutoffs == "b") {
                    m_post <- self$options$func_mean
                    sd_post <- self$options$func_sd

                    if(self$options$higherBetter) { # Checks if higher score indicates improvement
                        result_abc <- m_post-2*sd_post
                    } else {
                        result_abc <- m_post+2*sd_post
                    }
                }

                if(self$options$cutoffs == "c") {
                    m_post <- self$options$func_mean
                    sd_post <- self$options$func_sd
                    result_abc <- (sd_post * m_pre + sd_pre * m_post)/(sd_post + sd_pre)
                }


                # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
                #                                                  RCI CALCULATION                                                        #
                # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

                # Calculation for RCI
                r_value <- self$options$valueOfR
                standard_error_of_measurement <- sd_pre*sqrt(1-r_value)
                s_diff <- sqrt(2*(standard_error_of_measurement ^ 2))

                # We want to create rci boundary lines (y=kx+m) where x = values_pre, y = values_post, k = 1 and m (the interception point) is the negative and positive value of the following
                interception_point <- s_diff*1.96
                interception_point_minus <- interception_point - (interception_point * 2)


                # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
                #                                             PATIENT STATUS CALCULATION                                                  #
                # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

                if(self$options$higherBetter) {
                    patient_status <- c(ifelse(values_post-values_pre >= interception_point, ifelse(values_post >= result_abc,"Recovered","Improved"),ifelse(values_post-values_pre <= interception_point_minus,"Detoriated","Unchanged")))
                } else {
                    patient_status <- c(ifelse(values_post-values_pre <= interception_point_minus,ifelse(values_post <= result_abc,"Recovered","Improved"),ifelse(values_post-values_pre >= interception_point,"Detoriated","Unchanged")))
                }


                # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
                #                                                SCATTER PLOT PREPERATION                                                 #
                # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

                if(self$options$scatterplot) {
                    # Create dataframe with the relevant values for plotting
                    df_scatterplot <- data.frame(values_pre, values_post, values_group, patient_status, result_abc, interception_point, interception_point_minus) # Dataframe consisting of pre and postvalues
                    colnames(df_scatterplot) <- c("values_pre", "values_post", "values_group", "patient_status", "result_abc", "interception_point", "interception_point_minus")

                    # Save dataframe to image
                    image_scatter <- self$results$scatterplot
                    image_scatter$setState(df_scatterplot)
                } else {
                    self$results$scatterplot$setVisible(FALSE)
                }


                # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
                #                                                   BAR PLOT PREPERATION                                                  #
                # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

                df <- data.frame(patient_status = patient_status, values_pre = values_pre, values_group = values_group) # Dataframe consisting of if a patient is above cutoff-point and the patients scores

                frequency_df <- as.data.frame(table(df$patient_status, df$values_group)) # Frequency dataframe of patient treatments outcomes
                colnames(frequency_df) <- c("patient_status", "values_group", "no_of_patients")

                if(self$options$barplot) {
                    image <- self$results$barplot
                    image$setState(frequency_df)

                } else {
                    self$results$barplot$setVisible(FALSE)
                }

                # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
                #                                              TABLE PREPERATION AND DISPLAY                                              #
                # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

                table_zero <-table(factor(df$patient_status,
                                          levels = c("Detoriated",  "Improved", "Recovered", "Unchanged")),
                                   factor(df$values_group))

                frequency_df_zero <- as.data.frame(table_zero)
                colnames(frequency_df_zero) <- c("patient_status", "values_group", "no_of_patients")

                i <- 0
                for (group in unique(frequency_df$values_group)){
                    self$results$table$addRow(group, values = list(
                        treatment = group,
                        Detoriated = frequency_df_zero$no_of_patients[1 + i],
                        Improved = frequency_df_zero$no_of_patients[2 + i],
                        Recovered = frequency_df_zero$no_of_patients[3 + i],
                        Unchanged = frequency_df_zero$no_of_patients[4 + i]
                    ))
                    i <- i + 4
                }

                if(self$options$table) {
                    print(nrow(self$results$table))
                } else {
                    self$results$table$setVisible(FALSE)
                    return;
                }

            } else {
                self$results$table$setVisible(FALSE)
                return;
            }
            },

        # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
        #                                                       RENDERING                                                         #
        # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

        .barplot=function(image, ...) {
            plotData <- image$state
            if (!is.null(plotData)) {
                barplot <- ggplot(data=plotData, aes(x=values_group, y=no_of_patients, fill = patient_status)) +
                    geom_bar(stat="identity", position = position_dodge()) + scale_fill_manual(values=c("Recovered"="green", "Improved"="blue", "Unchanged"="orange", "Detoriated"="red")) +
                    labs(x = "Treatments", y = "Number of patients", fill = "Patient status") # Barplot
                print(barplot)
                TRUE
            } else {
                self$results$barplot$setVisible(FALSE)
                return;
            }
        },
        .scatterplot=function(image_scatter, ...) {

            plotData <- image_scatter$state
            if (!is.null(plotData)) {
                result_abc <- image_scatter$state$result_abc
                interception_point <- image_scatter$state$interception_point
                interception_point_minus <- image_scatter$state$interception_point_minus
                values_group <- image_scatter$state$values_group
                patient_status <-plotData$patient_status
                groups <- unique(plotData$values_group)
                max_all <- max(max(plotData$values_pre), max(plotData$values_post))
                min_all <- min(min(plotData$values_pre), min(plotData$values_post))


                if(length(groups) > 5 & !(is.null(self$options$groupingVar))) {  # Show error message if more than five different treatments
                    scatter_plot <-  ggplot() +
                        theme_void() +
                        geom_text(aes(0,0,label="Can only display a maximum of five different treatments at once."), color="red", size=5) +
                        xlab(NULL)
                } else {

                    available_filling_shapes <- c(21,22,23,24,25)
                    used_filling_shapes <- available_filling_shapes[1:length(groups)]

                    scatter_plot <- ggplot(data=plotData, aes(x=plotData$values_pre, y = plotData$values_post)) +
                        coord_cartesian(xlim = c(min_all, max_all), ylim = c(min_all, max_all), expand = TRUE) +
                        geom_abline(aes(intercept = result_abc, slope=0,linetype = "Cutoff point", color="Cutoff point")) +
                        geom_abline( aes(intercept=interception_point, slope=1, linetype="Boundary for reliable change", color="Boundary for reliable change")) + # rci boundary
                        geom_abline(aes(intercept=0, slope=1, linetype = "No change", color = "No change")) + # line indicating no change
                        geom_abline(aes(intercept=interception_point_minus, slope=1, linetype="Boundary for reliable change", color="Boundary for reliable change")) + # rci boundary
                        scale_fill_manual(values=c("Recovered"="green", "Improved"="blue", "Unchanged"="orange", "Detoriated"="red")) +
                        scale_linetype_manual(values=c("Boundary for reliable change"="dashed", "No change"="solid", "Cutoff point"="solid")) +
                        scale_color_manual(values=c("Boundary for reliable change"="black", "No change"="black", "Cutoff point"="red")) +
                        theme(legend.position = "right")

                    # Check whether treatment should be with as a variable
                    if (length(groups) == 1) {
                        scatter_plot <- scatter_plot +
                            geom_point(aes(fill = factor(patient_status)), shape = 21,size=3, stroke=0) +
                            labs(x = "Before treatment", y = "After treatment", linetype = "Line explanations", color = "Line explanations", fill= "Status")
                    } else {
                        scatter_plot <- scatter_plot +
                            geom_point(aes(shape = factor(values_group),fill = factor(patient_status)), size=3, stroke=0) +
                            scale_shape_manual(values=used_filling_shapes) +
                            guides(fill = guide_legend(override.aes = list(shape = 22, size=5)))+
                            guides(shape = guide_legend(override.aes = list(fill = "black")))+
                            labs(x = "Before treatment", y = "After treatment", linetype = "Line explanations", color = "Line explanations", fill= "Status", shape = "Treatment")
                    }

                }
                print(scatter_plot)

                TRUE
            } else {
                self$results$scatterplot$setVisible(FALSE)
                return;
            }}
    )
)

