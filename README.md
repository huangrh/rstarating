# CMS Hospital Compare Star Rating SAS Pack Replica  

-----
### Introduction  
The package is to reimplement the [SAS-Pack](http://www.qualitynet.org/dcs/ContentServer?c=Page&pagename=QnetPublic%2FPage%2FQnetTier3&cid=1228775958130) script for the CMS Hospital Compare Star Rating as posted on [https://www.qualitynet.org](http://www.qualitynet.org/dcs/ContentServer?c=Page&pagename=QnetPublic%2FPage%2FQnetTier2&cid=1228775183434). R is widely used open source software in statistic analysis. This process has been speed up tremendous and it 

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
> x <- cms2016oct_input

\# Step 1: Prepare and clean up the dataset.   
> x <- mstbl(x)   

\# Step 2: Fit the LVM model.    
> fit2 <-  relvm_quad(x) # fit2 <-   relvm(x)  

\# Step 3: K-means clustering.   
> sr <- rating(fit2$groups$summary_score, iter.max = 1)

\# Save the output. Setup the local directory accordingly.      
> op <- out_dir("C:/rhuang/github/rstarating/inst")  

\# Save the parameters  
> write.csv(fit2$groups$pars,  file=file.path(op,"Oct2016_par_truelvm_fit2.csv"))  

\# Save the group scores   
> write.csv(fit2$groups$preds, file=file.path(op,"Oct2016_preds_truelvm_fit2.csv"))   

\# Save the summary scores & stars       
> write.csv(sr$summary_score,  file=file.path(op,"Oct2016_sum_score_truelvm_fit2.csv"))  

