# CMS Hospital Compare Star Rating SAS Pack Replica  

-----
### Introduction  
The initial goal is to reimplement the [SAS-Pack](http://www.qualitynet.org/dcs/ContentServer?c=Page&pagename=QnetPublic%2FPage%2FQnetTier3&cid=1228775958130) for the CMS Hospital Compare Overall Star Rating as posted on [https://www.qualitynet.org](http://www.qualitynet.org/dcs/ContentServer?c=Page&pagename=QnetPublic%2FPage%2FQnetTier2&cid=1228775183434). During the reimplementation, two major issues have been found: 

- CMS's SAS Pack, which implements the k-means clustering with ONE iteratrion, failed to converge.  This leads to ~ 1/4 hospitals receiving an incorrect star rating.

- CMS's Latent Variable Model (LVM), which uses a Gaussian quadrature approximation with 30 qpoints, failed to approach the integral of the objective function. This also leads to hundreds of hospitals receiving an incorrect star rating. 

-----
### Installation   
 
> require(devtools);  # Install the package devtools if you didn't do so 
> devtools::install_github("huangrh/rstarating");  
> devtools::install_github("huangrh/relvm");  
> devtools::install_github("huangrh/rclus");  
> require(rstarating); require(relvm); require(rclus)  

-----
### To replicate the original sas pack (use the data from october 2016) 
\# Load the input dataset from October 2016.   
> x <- cms2016oct_input

\# Step 1: Prepare and clean up the dataset.   
> x <- mstbl(x)   

\# Step 2: Fit the LVM model.    
> fit2 <-  relvm_quad(x) # fit2 <-   relvm(x)  

\# Step 3: K-means clustering.   
> sr <- rating(fit2$groups$summary_score, iter.max = 1)

\# Save the output.       
> op <- out_dir("C:/rhuang/github/rstarating/inst")           # Setup the output directory accordingly.   
> write.csv(fit2$groups$pars,  file=file.path(op,"Oct2016_par_truelvm_fit2.csv"))       #the parameters   
> write.csv(fit2$groups$preds, file=file.path(op,"Oct2016_preds_truelvm_fit2.csv"))     #group scores           
> write.csv(sr$summary_score,  file=file.path(op,"Oct2016_sum_score_truelvm_fit2.csv")) #the summary scores & stars    

### Tutorial to run the true latent variable model and the corrected kmeans clustering 

\# Step 1: Prepare and clean up the dataset.   
> x <- mstbl(x)   

\# Step 2: Fit the LVM model.    
> fit2 <-   relvm(x)  

\# Step 3: K-means clustering.   
> sr <- rating(fit2$groups$summary_score, method="kmeans", iter.max = 100)


### License
GPL(3)

### For a Python user:
[hydrus](https://github.com/mark-r-g/hydrus) is developed in parallel. It runs in less than a minute. 

### [To report an issue](https://github.com/huangrh/rstarating/issues/new)
