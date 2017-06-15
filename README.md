# CMS Hospital Compare Star Rating SAS Pack Replica  

-----
### Introduction  
The purpose of this package is to use R to replicate and improve the methodlogy of CMS Hospital Compare Star Rating as posted on https://www.qualitynet.org.

-----
### Installation   
 
> require(devtools);  # Install the package devtools if you didn't do so.  
> devtools::install_github("huangrh/rstarating");  
> devtools::install_github("huangrh/relvm");  
> devtools::install_github("huangrh/rclus");  
> require(rstarating); require(relvm); require(rclus)  

-----
### Tutorial  (use the data from october 2016.)
\# Load the input dataset from October 2016.   
> x       <- cms2016oct_input

\# Data preparation.   
> x <- mstbl(x)   

\# LVM model fitting.    
> fit2       <-   relvm(x)  #fit2_quad       <-   relvm_quad(x)

\# K-means clustering.   
> sr <- rating(fit2$groups$summary_score, iter.max = 110)

\# Save output. Setup the local directory accordingly.      
> op <- out_dir("C:/rhuang/github/rstarating/inst")  

\# Save the parameters  
> write.csv(fit2$groups$pars,  file=file.path(op,"Oct2016_par_truelvm_fit2.csv"))  

\# Save the group scores   
> write.csv(fit2$groups$preds, file=file.path(op,"Oct2016_preds_truelvm_fit2.csv"))   

\# Save the summary scores & stars       
> write.csv(sr$summary_score,  file=file.path(op,"Oct2016_sum_score_truelvm_fit2.csv"))  

