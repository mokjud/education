#### ETO OLFACTORY FMRI PROJECT, DATA CLEANING ####
#### Eniko Kubinyi ####
#### Laura Verónica Cuaya, Attila Andics ####
#### the script was written by Judit Mokos mokjud@gmail.com ####
###################################################

### LIBRARY 
  library(oro.nifti)
  library(neurobase)
  library(fslr) #https://cran.r-project.org/web/packages/fslr/vignettes/fslr.html
  library(stringr)
  library(tidyverse)

# Used R and FLS, using the code (https://jacbel.github.io/virtem_code/signal-to-noise-ratio.html)) 
#written for this paper (Bellmund, J. L. S., Deuker, D., Montijn, N. D., & Doeller, C. F. (2021). Structuring time: The hippocampus constructs sequence memories that generalize temporal relations across experiences. bioRxiv. https://doi.org/10.1101/2021.04.23.440002) as a template. 

### setting up fsl. how to install fsl: https://fsl.fmrib.ox.ac.uk/fsl/fslwiki/FslInstallation/MacOsX; 
      # how to use flsr packages https://cran.r-project.org/web/packages/fslr/vignettes/fslr.html
  options(fsl.path='your path') #giving the path of fsl, change it to your folder
  #checking if fsl is installed
    #have.fsl()
  #checking the fsl verson: 
    if (have.fsl()) {
      print(fsl_version()) #checking the fsl verson:
    }
  
subjects <- list.files(path="") # SPECIFY giving the path for the folder contaning your nifti files. 
n_subs <- length(subjects)
n_image_per_subject <- 2 #specify the image per subjects

### DATA

# create the file names to use (path)
  list_of_nifti_atlas_name <- list.files(path="your path", # SPECIFY giving the path for your atlas, here Czeibert el al (2019)/LabelsNums/ is used (shared by Attila Andics)
                                    pattern = "nii.gz")
  list_of_nifti_atlas_full_path <- list.files(path="your path", # SPECIFY giving the path for your atlas, here Czeibert el al (2019)/LabelsNums/ is used (shared by Attila Andics)
                                              pattern = "nii.gz",
                                              full.name = TRUE)
  list_of_nifti_full_path <- list.files(path="", # SPECIFY giving the path for the folder contaning the nifti files
                                        recursive = TRUE,  # including folders
                                        pattern=".nii", #list only files that contain the pattern 'nii'
                                        full.name = TRUE) #return with the full path of the files
  list_of_nifti_name <- c(rep(NA, length(list_of_nifti_full_path)))
  for(i in 1:length(list_of_nifti_full_path)){
    list_of_nifti_name[i] <- str_split(list_of_nifti_full_path, "/")[[i]][length(str_split(list_of_nifti_full_path, "/")[[i]])]
  }
  rois <- list_of_nifti_atlas_name
  roi_df <- data.frame(rois = rois, #specifies the details of the atlas, here Czeibert el al (2019)/LabelsNums/ , region names
                       roi_number = gsub(".nii.gz", "", rois),
                       region = c("ecephalon", "gyrus frontalis L", "gyrus frontalis R", "gyrus proreus L", "gyrus propreus R",
                                  "gyrus compositus rostralis L", "gyrus compositus rostralis R", "gyrus precruciatus L", "gyrus precruciatus R",
                                  "gyrus postcruciatus L", "gyrus postcruciatus R", "gyrus marginalis L", "gyrus marginalis R", 
                                  "gyrus ectomarginalis L", "gyrus ectomarginalis R", "gyrus occipitalis L", "gyrus occipitalis R",
                                  "gyrus suprasylvius rostralis L", "gyrus suprasylvius rostralis R", "gyrus suprasylvius medius L", "gyrus suprasylvius medius R", 
                                  "gyrus suprasylvius caudalis L", "gyrus suprasylvius caudalis R", "gyrus ectosylvius rostralis L", "gyrus ectosylvius rostralis R", 
                                  "gyrus ectosylvius medius L", "gyrus ectosylvius medius R", "gyrus ectosylvius caudalis L", "gyrus ectosylvius caudalis R", 
                                  "gyrus sylvius rostralis L", "gyrus sylvius rostralis R", "gyrus sylvius caudalis L", "gyrus sylvius caudalis R", 
                                  "gyrus compositus caudalis L", "gyrus compositus caudalis R", "gyrus rectus L", "gyrus rectus R", 
                                  "gyrus genualis L", "gyrus genualis R", "area subcallosa L", "area subcallosa R", "gyrus cinguli L", "gyrus cinguli R", 
                                  "gyrus presplenialis L", "gyrus presplenialis R", 
                                  "gyrus splenialis L", "gyrus splenialis R", "gyrus parahippocampalis L", "gyrus parahippocampalis R", #49
                                  "hippocampus L", "hippocampus R", "lobus piriformis L", "lobus piriformis R", "tuberculum olfactorium L", "tuberculum olfactorium R", 
                                  "gyrus diagonalis L", "gyrus diagonalis R", "gyrus paraterminalis L", "gyrus paraterminalis R", 
                                  "gyrus olfactorius lateralis L", #60
                                  "gyrus olfactorius lateralis R", "thalamus L", "thalamus R", "bulbus olfactorius L", "bulbus olfactorius R", 
                                  "nucleus caudatus L", "nucleus caudatus R", "insular cortex L", "insular cortex R", "hypophysis", #70
                                  "vermis cerebelli", "pons", "medulla oblongata", "medulla spinalis", "mesencephalon", "diencephalon",
                                  "nervus opticus", "hemispherium cerebelli L", "hemispherium cerebelli R", "commissura rostralis", #80
                                  "pedunculus olfactorius L", "pedunculus olfactorius R", "area septalis L", "area septalis R", 
                                  "nucleus et tractus spinalis nervi trigemini L", "nucleus et tractus spinalis nervi trigemini R",
                                  "nucleus ventralis caudalis thalami pars medialis L", "nucleus ventralis caudalis thalami pars medialis R",
                                  "amygdala L", "amygdala R"))
  
  filt_func_fn <- list_of_nifti_full_path
  roi_fn <- list_of_nifti_atlas_full_path
  
   tMean_fn <- file.path("your path/tMean_", # SPECIFY specify the path for a folder to save the tMean files, those are helping files
                        list_of_nifti_name, 
                        fsep ="")
  tStd_fn <- file.path("your path/tSD_", # SPECIFY specify the path for a folder to save the TSD files, those are helping files
                       list_of_nifti_name, 
                       fsep ="")
  tSNR_fn <- file.path("your path/tSNR_", # SPECIFY specify the path for a folder to save the TSNR files, those are helping files
                       list_of_nifti_name, 
                       fsep ="")

#### FUNCTIONS ####
  ## function to calculate tSNR
    calc_tSNR <- function(fourD_fn=NULL, tMean_fn=NULL, tStd_fn=NULL, tSNR_fn=NULL){
      
      # calculate temporal mean for each voxel -- mean
      fslr::fsl_maths(file = fourD_fn, opts = "-Tmean", outfile = tMean_fn,
                      verbose = FALSE, retimg = FALSE)
      
      # calculate temporal mean for each voxel -- std dev
      fslr::fsl_maths(file = fourD_fn, opts = "-Tstd", outfile = tStd_fn,
                      verbose = FALSE, retimg = FALSE)
      
      # calculate temporal mean for each voxel -- mean/stdev
      fslr::fsl_div(file = tMean_fn, file2 = tStd_fn, outfile = tSNR_fn,
                    verbose = FALSE, retimg = FALSE)
    }

  
  # function to calculate the mean tSNR for an ROI
  # To find the average tSNR for a given ROI, we load the tSNR image and the ROI mask before calculating the mean across voxels. Here we define the function that returns the mean tSNR, given the file names of the tSNR image and the ROI masks.
  get_ROI_tSNR <- function(roi_fn, tSNR_fn){
    # load images
    tSNR_nii <- neurobase::readNIfTI2(tSNR_fn)
    roi_nii <- neurobase::readNIfTI2(roi_fn)
    roi_nii_resample = fsl_resample(file = roi_nii, voxel_size = c(2, 2, 2))  #resampling roi for maching dimensions
    # the tSNR values of the ROI voxels
    snr_dat <- tSNR_nii[roi_nii_resample==1]
    # return the average
    return(mean(snr_dat))
  }
  
##### CALCULATIONS for the whole image ####
  # tSNR = meanSNR/stdSNR
  ## calculate helping files to tSNR (mean and sd). 
  invisible(mapply(calc_tSNR, fourD_fn=filt_func_fn, tMean_fn=tMean_fn, tStd_fn=tStd_fn, tSNR_fn=tSNR_fn))
  # if it gives error, but create the figures, don't bother with the error. 
  
  ## calculate helping file to tSNR = meanSNR/stdSNR
    for(i in 1:length(list_of_nifti_name)){
      fslr::fsl_div(file = readnii(tMean_fn[i]), file2 = readnii(tStd_fn[i]), outfile = tSNR_fn[i],
                    verbose = FALSE, retimg = FALSE)
    }
  ## create a datatable with the tSNRs
  df_tSNR <- data.frame(picture=list_of_nifti_name, 
                        tSNR=NA)
  for(i in 1:length(list_of_nifti_name)){
    tSNR_nii <- neurobase::readNIfTI2(tSNR_fn[i])
    df_tSNR$tSNR[i] <- mean(tSNR_nii)
  }
  # write out the data_frame with the tSNRs
  write.csv(df_tSNR, "your path /df_tSNR.csv") # SPECIFY your path to the result table. A .csv will be saved.

  
##### CALCULATIONS for the ROIs image ####

  snr_df <- tibble(subject = rep(list_of_nifti_name, each = length(rois)),
                   roi = rep(roi_df$roi_number, length(list_of_nifti_name)),
                   region = rep(roi_df$region, length(list_of_nifti_name)),
                   tSNR = NA)
  
  start.time <- Sys.time() # to measure the running time
  index <- 0
  for(i in 1:length(list_of_nifti_name)){
    tSNR_fn_i <- tSNR_fn[i]
    for(j in 1:length(rois)){
      index = index+1
      roi_fn_j <- roi_fn[j]
      snr_df$tSNR[index] <- get_ROI_tSNR(roi_fn = roi_fn_j, 
                                         tSNR_fn = tSNR_fn_i)
    }
  }
  end.time <- Sys.time() # to measure the running time
  time.taken <- end.time - start.time #2.92649 hours
  time.taken# to measure the running time
  
  write.csv(snr_df, " your path / df_tSNR_rois.csv") # SPECIFY your path to the result table. A .csv will be saved.
  
  
  
  
  