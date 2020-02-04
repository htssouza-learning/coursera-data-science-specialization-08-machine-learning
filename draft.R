

############################################################################
# Debugging constants

############################################################################
# Constants (change may be required for your own environment)


path.expand()




OUTLIER_IQR_FACTOR <- 2

############################################################################
# Environment
setwd(WORKING_DIRECTORY)

############################################################################
# File download & uncompress


############################################################################
# Loading Data


# remove near zero var colums
nzv.indexes <- nearZeroVar(training)
nzv.names <- names(training)[nzv.indexes]
training <- select(training, -nzv.names)

# remove other columns
unwanted.columns <- c("raw_timestamp_part_1", "raw_timestamp_part_2", "cvtd_timestamp", "num_window")
training <- select(training, -unwanted.columns)

b <- names(training)[as.character(sapply(training, function(x) { class(x) })) %in% c("numeric", "integer")]

x <- x[!x %in% boxplot.stats(x, coef = OUTLIER_IQR_FACTOR)$out]


# check histogram to identify outliers
sparse.columns <- c("var_yaw_belt",
                    "max_yaw_dumbbell",
                    "amplitude_roll_dumbbell",
                    "var_accel_dumbbell",
                    "var_roll_dumbbell",
                    "gyros_forearm_y",
                    "gyros_forearm_z")

for(column.name in sparse.columns) {
    x <- select(training, !!column.name)
    x <- filter(x, !is.na(!!column.name))
    x <- x[!is.na(x)]
    print(column.name)
    #print(ggplot(data = data.frame(x = x), aes(x = x)) +
    #    geom_histogram(bins = 100) +
    #    ggtitle(column.name))
    print(ggplot(data = data.frame(x = x), aes(y = x)) +
              geom_boxplot(coef = OUTLIER_IQR_FACTOR,
                           outlier.colour = "red",
                           outlier.shape = 16,
                           outlier.size = 2,
                           notch = FALSE) +
              ggtitle(column.name))
    x <- x[!x %in% boxplot.stats(x, coef = OUTLIER_IQR_FACTOR)$out]
    print(ggplot(data = data.frame(x = x), aes(y = x)) +
              geom_boxplot(coef = OUTLIER_IQR_FACTOR,
                           outlier.colour = "blue",
                           outlier.shape = 16,
                           outlier.size = 2,
                           notch = FALSE) +
              ggtitle(column.name))    
    # print(boxplot(x, ))
    # boxplot(x, )
}




