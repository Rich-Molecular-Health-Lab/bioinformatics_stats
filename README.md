Bioinformatics and Statistical Analysis
========================================

This repository contains the scripts, tutorials, and templates for the Rich Lab's mainstream bioinformatic workflows.
Before you begin working with any data or samples, you should first make sure you understand the DataInventory and Metadata workflows.

Overview
--------

If you are working with any molecular samples in the lab, you should begin with the following workflows (in order):

1. SampleInventory.Rmd/SampleInventory.html - a script to streamline and standardize the information we maintain about every sample's process of purification and analysis for reliable downstream integration.
2. Metadata.Rmd/Metadata.html - a tutorial that models some best practices for brainstorming, listing, and organizing key potential independent variables for a given study and matching a score for each variable to each sample in one dataframe.
3. Data_Notes.Rmd/Data_Notes.html - some notes, thoughts, and examples on the options we most often use for testing different hypotheses with the types of data we generate.

The raw scripts with chunk of code that you can run directly (assuming you download the entire repository with necessary dependencies) are in .Rmd format. I also use the knitr package to create "prettier" (but read-only) versions of those files, which are easier to read and study on their own.
- To view any of the html files, first download the raw file, and then open it in the browser of your choice (it will automatically open in whichever browser you set as your default).
- To use the .Rmd files, I recommend you pull or clone the repository to your R Studio. You can also download the individual Rmd files and then open them in your R Studio, but keep in mind that you will not have the files referenced in the code.

Once you look through these, you should work on your own Metadata.Rmd script for the hypotheses you are trying to test. You can download the tutorial as a template and edit it from there.

I will add more tutorials and guides later, including details on how to best use github and R Studio to push and pull your own repositories to this site.


Dependencies
------------

- Some of the Rmd files depend on scripts stored as .R files within a subdirectory.
  - I do this to maintain central sources for code that I use in multiple pipelines so that changes applied once will apply everywhere.
  - This also keeps the main workflow modularized, organized, and easier to follow.
- The most up-to-date workflows use the R package config to maintain reproducible file paths.
  - this ensures that the code will still work when moving between computers with different parent directories.
  - the config.yml files in any parent directory for a project contain my settings for this. You can use those as a template and update your own.
- My most recent workflows also refernece a packages.R script, which, when run/sourced, will automatically check whether you have installed all the packages needed for that workflow. If you have not, it should install the packages before activating each for the rest of the script.
  - For this repository you may see setup/global/packages.R
  - The setup folder contains other dependencies sourced in the scripts, including custom functions, parameters for the conflicted package, and custom fonts.

Description of files in parent directory (richlab_main/bioinformatics_stats/)
------------------------------------------------------------------------------

Main R-Markdown (.Rmd) Files to Start From (each also available as a knitted html format):

filename                          |  purpose
----------------------------------|------------------------------------------------------------------------------------
SampleInventory.Rmd               |  Maintain records of all Rich Lab samples and export samplesheets for Dorado.
MetadataSetup.Rmd                 |  Create predictor variables, code them, and then match to samples.
ReadProcessing.Rmd                |  Basecall, demultiplex, filter, clean, and organize raw ONT sequence data.
data_notes.Rmd                    |  Review basic statistical options available for some of our main datasets.
create_dummy_samples.Rmd          |  Export formatted tables with dummy data for samples to test different pipelines.

Scripts not specific to R languages:

filename                          |  description
----------------------------------|------------------------------------------------------------------------------------
README.md                         |  Text file (markdown format) description of the project.
config.yml                        |  directory paths and other parameters to ensure reproducible code pipelines.


Subdirectories:


directory name                    |  purpose
----------------------------------|------------------------------------------------------------------------------------
data/                             |  Intermediate or raw-stage datasets in table form. Subdirectories organized by sampleset.
dataframes/                       |  Data tables produced and used by other Rmd scripts in this repository.
metadata/                         |  Data table files and R scripts to generate tibbles/dataframes with metadata.
microeco/                         |  Datasets and results produced and used directly by the microeco package.
setup/                            |  Modularized .R scripts with all parameters, functions, packages, and other dependencies.



Samplesets Used to Organize and Categorize Data for Current Projects
---------------------------------------------------------------------
Each of these may appear as subdirectories within those listed above to organize the files for each set of projects. Below are the main samplesets in use.

directory name                    |  description
----------------------------------|------------------------------------------------------------------------------------
loris                             |  Pygmy loris genetic, microbial, and behavioral data collected with Henry Doorly Zoo
marmoset                          |  Gut microbiome data gathered from the UNO Research Colony for Shayda Azadmanesh's thesis.
bats                              |  Genetic and gut microbiome data gathered by collaborators at trapping sites across N. America
environmental                     |  Samples gathered from opportunistic environmental sources for Thomas Raad's thesis.
isolates                          |  Purified DNA from bacterial isolates grown in the Ayayeye lab for whole genome sequencing in the Rich Lab.


