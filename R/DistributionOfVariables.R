#' @title Providing the Distribution of variables by plotting the graphs
#'
#' @description This package includes function to create plots for the given data and stores the plots in specified directory
#'
#' @param symbol
#'
#' @return plots
#'
#' @examples  DistributionOfVariables(data,dir,featurevector)
#'
#' @export DistributionOfVariables

# The function is for creating graphs for the variables in a data set.
# These graphs can help us to know the distribution of variables using
# bar plots, pie charts, box plots and histograms for categorical and numerical variables
# @params dataset (having numerical and categorical variables
# @params column numbers of variables as a vector, if you need graphs for specific columns, or by default, graphs for all the variables is returned
# @parms the directory name or the path where you want to store the graphs, or by default it will store in the current working directory
# @result is bar plots and pie charts for categorical variables. Boxplots and histograms for numerical variables

DistributionOfVariables <- function(data, dir=getwd(), features=data) #the arugements are for data, directory path, variable numbers as vector
{
  setwd(dir) #setting the given working directory as the location
  if(!is.data.frame(data)) # checking if the data provided is of dataframe class type
    stop("The given object is not a data frame")

  fdata = ifelse(length(features)!=0, length(features), ncol(data)) # checking if any specific vector of column numbers is provided, if not taking all columns
  for(i in 1:fdata)
  {
    if(is.numeric(data[,i])) #checking if the column values are numeric

    {
      png(paste(names(data)[i], ".png", sep="")) #as the graph is to be stored as an image, appending the .png extension to the column name as file name

      par(mfrow=c(1,2)) # dividing the console space into 2 vertical divisions for both graphs
      boxplot(data[,i], main = paste("Boxplot of", names(data)[i]),
              ylab = names(data)[i], col = "maroon", border = "grey5",
              horizontal = T) #plotting boxplot

      hist(data[,i], main = paste("Histogram of", names(data)[i]),
           xlab = names(data)[i], ylab = "No. of Houses", col = "lightgreen", border=F) #plotting histogram

      dev.off()  #this method closes  the plot

    }
    else if(is.factor(data[,i])) # checking if the variable is categorical
    {
      png(paste(names(data)[i], ".png", sep="")) #NOTE this step

      par(mfrow=c(1,2))
      barplot(table(data[,i]), main = paste("Barplot of", names(data)[i]),
              ylab = names(data)[i], col = "maroon", border = "grey5",
              horizontal = T) #plotting bar plot

      pie(table((data[,i])),col = c("red","green","blue")) #plotting pie chart

      dev.off()
    }
  }
}
