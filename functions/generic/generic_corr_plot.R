# generic_corr_plot Function Overview:
# 1. Function creates a matrix of scatter plots for pairwise correlations in a dataset.
# 2. Inputs: Plotting_Dataset (data frame for plotting), Field_Names (names of variables).
# 3. Scales the data (normalization) for comparability.
# 4. Constructs a grid of plots (matrix form) with number of columns and rows equal to the number of variables.
# 5. Special handling:
#    a. Diagonal: Displays variable names instead of plots.
#    b. Upper Quadrant: Leaves plots empty (to avoid redundant mirror plots of lower quadrant).

generic_corr_plot <- function(Plotting_Dataset, Field_Names){
  
  
  #Set up the data set. 
  plt_dataset <- data.frame(scale(Plotting_Dataset))
  
  #Get the hyper-parameters
  num_cols <- ncol(plt_dataset)
  
  #Set the plot-dimensions
  par(mfrow = c(num_cols, num_cols), mar = c(1,1,1,1))

  
  for(cols in 1:num_cols){
    for(rows in 1:num_cols){
      
      #---------------------------------------------#
      #There are the name for the diagnoal values.
      if(cols == rows) {
        plot(1, type="n", xlab="", ylab="", 
             xlim=c(0, 2), ylim=c(0, 2), 
             xaxt='n', yaxt='n')
        text(1, 1, paste0(Field_Names[cols]), cex = 1.75)
      }
      
      
      #---------------------------------------------#
      #Empty Plots Upper Quadrant
      if(cols < rows){
        plot.new()
      }
      
      #---------------------------------------------#
      #The plot columns
      if(cols > rows){
        
        #Estimate the correlation coefficient
        cor_coeff <- cor(plt_dataset[,rows], plt_dataset[,cols])
        cor_coeff <- round(cor_coeff, 2)
        
        # Calculate the linear regression model
        reg_model <- lm(plt_dataset[,cols] ~ plt_dataset[,rows])
        
        plot(plt_dataset[,rows], plt_dataset[,cols], 
             pch = 19, cex = 0.5, xaxt='n', yaxt='n')
        abline(reg_model, col="red")
        text(x = max(plt_dataset[,rows])-1.75, y = max(plt_dataset[,cols])-0.5, 
             paste0("R=", cor_coeff), col ='red', cex = 1.5)
    
        }
      }
  }
  
}




