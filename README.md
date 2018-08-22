# CMS Hospital Compare Star Rating SAS Pack Replica   

-----
### 1. Introduction  
The initial goal is to reimplement the [SAS-Pack](http://www.qualitynet.org/dcs/ContentServer?c=Page&pagename=QnetPublic%2FPage%2FQnetTier3&cid=1228775958130) for the CMS Hospital Compare Overall Star Rating as posted on [https://www.qualitynet.org](http://www.qualitynet.org/dcs/ContentServer?c=Page&pagename=QnetPublic%2FPage%2FQnetTier2&cid=1228775183434) in [R](https://cran.r-project.org/). During the reimplementation, two major issues have been found: 

- CMS's K-means clustering, which runs for ONE iteration, failed to converge.  This leads to ~ 1/4 hospitals receiving an incorrect star rating.

- CMS's Latent Variable Model (LVM), which uses a Gaussian quadrature approximation with 30 qpoints, failed to approach the integral of the objective function. This also leads to hundreds of hospitals receiving an incorrect star rating. 

The issues are fixed in the R package. See the tutorial to replicate the original SAS-Pack and to run the the corrected LVM algorithm and K-means clustering. Click the link at the end of this page to report if you have questions or any other issues. 

-----
### 2. Installation & Load The Packages      
 
> require(devtools);  # Install the package devtools if you didn't do so.     
> devtools::install_github("huangrh/rstarating");     
> devtools::install_github("huangrh/relvm");    
> devtools::install_github("huangrh/rclus");    
> require(rstarating); require(relvm); require(rclus)    

-----
### 3. To replicate the original sas pack (use the data from october 2016) 
\# Load the input dataset from October 2016.   
> x <- cms2016oct_input

\# Step 1: Prepare and clean up the dataset.   
> x <- mstbl(x)   

\# Step 2: Fit the LVM model (non-adaptive).    
> fit_noad <-  relvm_noad(x) # fit2 <-   relvm(x)  

\# Step 3: K-means clustering.   
> sr_noad <- rating(fit_noad$groups$summary_score, method="rclus",score_col="sum_score_win",iter.max = 1)

\# Comparison with cms hospital overall rating published in Oct. 2016.   
> merge(x = sr_noad$star, y = cms_star_hospital_overall_rating2016oct, by = "ccnid",all.x=T) %>%
> with(table(star,hospital_overall_rating))

\# Save the output
> op <- out_dir("C:/rhuang/github/rstarating/inst")           # Setup the output directory accordingly.   
> write.csv(fit_noad$groups$pars,  file=file.path(op,"Oct2016_par_truelvm_fit2.csv"))       #the parameters   
> write.csv(fit_noad$groups$preds, file=file.path(op,"Oct2016_preds_truelvm_fit2.csv"))     #group scores           
> write.csv(sr$summary_score,  file=file.path(op,"Oct2016_sum_score_truelvm_fit2.csv"))     #the summary scores & stars    

### 4. Tutorial to run the true latent variable model and the corrected kmeans clustering 

\# Step 1: Prepare and clean up the dataset.   
> x <- mstbl(x)   

\# Step 2: Fit the LVM model.    
> fit2 <-   relvm(x)  

\# Step 3: K-means clustering.   
> sr <- rating(fit2$groups$summary_score, method="kmeans", iter.max = 100)

### 5. Updates on July 2018 

#### 5.1. CMS has improved the LVM and the k-Means clustering in December 2017:   

1. The k-means clustering is converged.   
2. The non-adaptive LVM is replaced with adaptive LVM model, which generates a similar results to those from our true/analytical LVM.   

#### 5.2. To replicate the CMS updates released in Dec 2017: 

\# Install and load the packages according to the installation instruction above.     
> input <- rstarating::cms_star_rating_input_2017dec # The input dataset from Dec. 2017 is attached.

\# Step 1: Prepare and clean up the dataset.     
> x     <- mstbl(input)

\# Step 2: Fit the LVM model.
> fit3 <- relvm(x)

\# Step 3: K-means clustering. 
> sr3  <- rating(x=fit3$groups$summary_score,method="rclus2",score_col="sum_score",iter.max=5000)

#### 5.3. In this release (Dec. 2017), one issue we found is that the measure factor loading is dominated by one measure in the safety of care group, and all other measures have a very little contribution to the group score. This can be tested with a measure randomization during the data praparation.   
\# For example, randimizing the hai_1 measure will not change the star rating.   
> input <- measure_manipulate(dat=cms_star_rating_input_2017dec,method="randomize",measures="hai_1")  
> x     <- mstbl(input)  
> fit4  <- relvm(x)  
> sr4   <- rating(x=fit4$groups$summary_score,method="rclus2",score_col="sum_score",iter.max=5000)  

#### 5.4. Removing the measure weigts in the LVM model is one way to balance the measure loading. Currently this can be achieved by setting them to one or by setting the measure denominator in the input data frame to one as below.   
> input <- measure_manipulate(dat=cms_star_rating_input_2017dec, method="den_one", measures="hai_1")  


### License
GPL(3)

### For a Python user:
[hydrus](https://github.com/mark-r-g/hydrus) is developed in parallel as of July 2017. It runs in less than a minute. 

### [To report an issue or if you have any questions (click the link)](https://github.com/huangrh/rstarating/issues/new)
