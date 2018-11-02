# Technical Documentation of Integrated Global Population System Model (IGPSMod)

In this document, we present an intergrated multi-component model of global population growth.  We will outline functions and purposes of the Github repository (files and folders).

We used the ***master*** branch as the latest version of the integrated World Population Model.  The repository contains executable files in the main directory.   

## Model Structure

The model that is introduced here is meant to capture feedbacks between each submodel.  We have created seven distinct submodels: 

* Population (Regional)
* Economy (Regional)
* Health and Education (Regional)
* Food (Global)
* Climate (Global)
* Water (Global)
* Resources (Global)

The primary interactions are listed in the following

![Main Framework](https://github.com/scarygary89/WorldPopulationModel/blob/master/READMEImages/Framework.png)

The main directory contains the files needed to run calibration for each submodel:*ClimateParmEstimation.R*, *EconomyParmEstimation.R*, *FoodParmEstimation.R*, *HealthEducationParmEstimation.R*, *PopulationParmEstimation.R*, *ResourceParmEstimation.R*, and *WaterParmEstimation.R*.  Also in the main directory, we have the file *Main.R*, which runs the integrated model, and a parameter estimation of the entire model.  All executable files are contained in the main directory.

Each file depends on the submodel functions that are contained in the **Submodel** folder.  

The **Calibration** folder contains the files that calibrates and estimates the parameters for each submodel and the entire integrated model.

The **OutputFiles** stores the output plots and data that are produced by the integrated model.

The **READMEImages** folder contains the images that are used for this *README.md*.

The **DataInput** folder contains the data files in .csv format that are needed to parameterize and initialize the model.   

We briefly list the contents of each folder in the following.

## Submodels
The submodels are defined in the **Submodel** folder.  The submodel folder includes seven files that contain the functions which that define the governing equations of each submodel.  All submodel functions are meant to calculate each auxiliary variables and state variables for each time step.

## Calibration
The calibration routines are located in the **Calibration** folder.  The *Calibration* folder has eight files: seven files for each submodel and one file pertains to a global calibration.  There's also a folder for **CalibrationOutput** which includes the output from all calibration functions.    

## DataInput
In this folder, there are three files: 
* *ParameterInput.csv* -- File containing the initial estimates of parameter values for all coefficients and upper/lower bounds. 
* *InitialValueInput.csv* -- File containing the initial estimates of initial values for all state variables
* *CalibrationInput.csv* -- File containing acutal time-series data for each state variable.  These are target values for the parameterization.


## OutputFiles
This folder contains output plots for each state variable and output data files, titled **OutputData.csv**.

## READMEImages
This folder contains the images to this readme document.