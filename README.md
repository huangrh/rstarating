# CMS Hospital Compare Star Rating SAS Pack Replica   

-----
### Introduction  
The initial goal is to reimplement the [SAS-Pack](http://www.qualitynet.org/dcs/ContentServer?c=Page&pagename=QnetPublic%2FPage%2FQnetTier3&cid=1228775958130) for the CMS Hospital Compare Overall Star Rating as posted on [https://www.qualitynet.org](http://www.qualitynet.org/dcs/ContentServer?c=Page&pagename=QnetPublic%2FPage%2FQnetTier2&cid=1228775183434) in [R](https://cran.r-project.org/). During the reimplementation, two major issues have been found: 

- CMS's K-means clustering, which runs for ONE iteration, failed to converge.  This leads to ~ 1/4 hospitals receiving an incorrect star rating.

- CMS's Latent Variable Model (LVM), which uses a Gaussian quadrature approximation with 30 qpoints, failed to approach the integral of the objective function. This also leads to hundreds of hospitals receiving an incorrect star rating. 

The issues are fixed in the R package. See the tutorial to replicate the original SAS-Pack and to run the the corrected LVM algorithm and K-means clustering. Click the link at the end of this page to report if you have questions or any other issues. 

-----
### Installation   
 
> require(devtools);  # Install the package devtools if you didn't do so.     
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

### Updates on July 2018 

#### CMS has improved the LVM and the k-means clustering as below:   

1. The k-means clustering is converged.   
2. The non-adaptive LVM is replaced with adaptive LVM model, which generates a similar results to those from our true/analytical LVM.   

#### To replicate the above updates in R: 

Install and load the packages according to the installation instruction as above.    

\# Step 1: Prepare and clean up the dataset.     

> input <- rstarating::cms_star_rating_input_2017dec # The input dataset from Dec. 2017 is attached. 
> x     <- mstbl(input)

\# Step 2: Fit the LVM model.
> fit3 <- relvm(x)

\# Step 3: K-means clustering. 
> sr3  <- rating(x=fit3$groups$summary_score,method="rclus2",score_col="sum_score",iter.max=5000)

### License
GPL(3)

### For a Python user:
[hydrus](https://github.com/mark-r-g/hydrus) is developed in parallel as of July 2017. It runs in less than a minute. 

### [To report an issue (click the link)](https://github.com/huangrh/rstarating/issues/new)
