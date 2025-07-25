Bioinformatics and Statistical Analysis
========================================

This repository contains the scripts, tutorials, and templates for the Rich Lab's mainstream bioinformatic workflows.
Before you begin working with any data or samples, you should first make sure you understand the DataInventory and Metadata workflows.

>To view the webpage version of this repository, click here: [Rich Lab Bioinformatics/Stats](https://rich-molecular-health-lab.github.io/bioinformatics_stats/)  
>To view the base repository, click here: [Rich Lab Bioinformatics/Stats](https://github.com/Rich-Molecular-Health-Lab/bioinformatics_stats/)  


Overview
--------

If you are working with any molecular samples in the lab, you should begin with the following workflows (in order):

1. [SampleInventory.Rmd](SampleInventory.Rmd) & [SampleInventory.html](https://rich-molecular-health-lab.github.io/read_processing/SampleInventory.html) - a script to streamline and standardize the information we maintain about every sample's process of purification and analysis for reliable downstream integration.
2. [MetadataSetup.Rmd](MetadataSetup.Rmd) & [MetadataSetup.html](https://rich-molecular-health-lab.github.io/bioinformatics_stats/MetadataSetup.html) - a tutorial that models some best practices for brainstorming, listing, and organizing key potential independent variables for a given study and matching a score for each variable to each sample in one dataframe.
3. [MinIONReadProcessing.Rmd](MinIONReadProcessing.Rmd) & [MinIONReadProcessing.html](https://rich-molecular-health-lab.github.io/read_processing/MinIONReadProcessing.html) - a tutorial to use the HCC cluster to process the raw sequencing data (.pod5 files) that we get from the ONT MinION sequencer directly after any sequencing run.
  - *Note: with the new Linux laptop we may be able to shift some of these scripts to local runs rather than using the high performance nodes on the HCC. If this works, I will add a modified script to run the pipeline locally.*
4. [Data_Notes.Rmd](Data_Notes.Rmd) & [Data_Notes.html](https://rich-molecular-health-lab.github.io/bioinformatics_stats/Data_Notes.html) - some notes, thoughts, and examples on the options we most often use for testing different hypotheses with the types of data we generate.

If you are working with microbiome data, then you should follow these tutorials with the MicroEco Setup:

5. [microbiome_new_data.Rmd](microbiome_new_data.Rmd) & [microbiome_new_data.html](https://rich-molecular-health-lab.github.io/bioinformatics_stats/microbiome_new_data.html) - tutorial for microbiome data cleaning and prep that uses the MicroEco package in R for downstream stats and visualization.
  - *Note: the [MicroEco package manual](https://chiliubio.github.io/microeco_tutorial/) is also a helpful reference for this.*
6. [ExploreResults_LorisMicrobiome.Rmd](ExploreResults_LorisMicrobiome.Rmd) & [ExploreResults_LorisMicrobiome.html](https://rich-molecular-health-lab.github.io/bioinformatics_stats/ExploreResults_LorisMicrobiome.html) - in prep tutorial to use microeco for exploratory analysis and summary stats using loris microbiome data. Note: you must follow the MicroEcoDataPrep script to prepare datasets that will work with this pipeline.


The raw scripts with chunk of code that you can run directly (assuming you download the entire repository with necessary dependencies) are in .Rmd format. I also use the knitr package to create "prettier" (but read-only) versions of those files, which are easier to read and study on their own.
- To view any of the html files, first download the raw file, and then open it in the browser of your choice (it will automatically open in whichever browser you set as your default).
- To use the .Rmd files, I recommend you [pull or clone the repository to your R Studio](https://docs.github.com/en/get-started/start-your-journey/downloading-files-from-github). You can also download the individual Rmd files and then open them in your R Studio, but keep in mind that you will not have the files referenced in the code.

Once you look through these, you should work on your own [MetadataSetup](https://rich-molecular-health-lab.github.io/bioinformatics_stats/MetadataSetup.html) script for the hypotheses you are trying to test. You can download the tutorial as a template and edit it from there.

I will add more tutorials and guides later, including details on how to best use github and R Studio to push and pull your own repositories to this site.
- In the meantime see [here](https://happygitwithr.com/) and [here](https://docs.github.com/en/get-started/start-your-journey/git-and-github-learning-resources).


Dependencies
------------

- Some of the Rmd files depend on scripts stored as .R files within a subdirectory.
  - I do this to maintain central sources for code that I use in multiple pipelines so that changes applied once will apply everywhere.
  - This also keeps the main workflow modularized, organized, and easier to follow.
- The most up-to-date workflows use the [R package config](https://rstudio.github.io/config/articles/config.html) to maintain reproducible file paths.
  - this ensures that the code will still work when moving between computers with different parent directories.
  - the [config.yml](config.yml) files in any parent directory for a project contain my settings for this. You can use those as a template and update your own.
- My most recent workflows also refernece a packages.R script, which, when run/sourced, will automatically check whether you have installed all the packages needed for that workflow. If you have not, it should install the packages before activating each for the rest of the script.
  - For this repository you may see [setup/global/packages.R](setup/global/packages.R)
  - The [setup directory](setup/) contains other dependencies sourced in the scripts, including custom functions, parameters for the [conflicted package](https://conflicted.r-lib.org/), and custom fonts.

Description of files in parent directory (richlab_main/bioinformatics_stats/)
------------------------------------------------------------------------------

Main R-Markdown (.Rmd) Files to Start From (each also available as a knitted html format):

R Markdown File                                      |Knitted HTML Link                                      |  purpose
-----------------------------------------------------|-------------------------------------------------------|------------------------------------------------------------------------------------
[SampleInventory.Rmd](SampleInventory.Rmd)           |[SampleInventory.html](https://rich-molecular-health-lab.github.io/read_processing/SampleInventory.html)           |  Maintain records of all Rich Lab samples and export samplesheets for Dorado.
[MetadataSetup.Rmd](MetadataSetup.Rmd)               |[MetadataSetup.html](https://rich-molecular-health-lab.github.io/bioinformatics_stats/MetadataSetup.html)               |  Create predictor variables, code them, and then match to samples.
[MinIONReadProcessing.Rmd](MinIONReadProcessing.Rmd) |[MinIONReadProcessing.html](https://rich-molecular-health-lab.github.io/read_processing/MinIONReadProcessing.html) |  Basecall, demultiplex, filter, clean, and organize raw ONT sequence data.
[microbiome_new_data.Rmd](MicroEcoDataPrep.Rmd)      |[microbiome_new_data.html](https://rich-molecular-health-lab.github.io/bioinformatics_stats/microbiome_new_data.html)         |  Prepare aligned reads and other wf-16s outputs for analysis using MicroEco.
[Data_Notes.Rmd](Data_Notes.Rmd)                     |[Data_Notes.html](https://rich-molecular-health-lab.github.io/bioinformatics_stats/Data_Notes.html)                     |  Review basic statistical options available for some of our main datasets.

Scripts not specific to R languages:

filename                          |  description
----------------------------------|------------------------------------------------------------------------------------
[README.md](README.md)            |  Text file (markdown format) description of the project.
[config.yml](config.yml)          |  directory paths and other parameters to ensure reproducible code pipelines.


Subdirectories:


directory name                    |  purpose
----------------------------------|------------------------------------------------------------------------------------
[data/](data/)                    |  Intermediate or raw-stage datasets in table form. Subdirectories organized by sampleset.
[dataframes/](dataframes/)        |  Data tables produced and used by other Rmd scripts in this repository.
[metadata/](metadata/)            |  Data table files and R scripts to generate tibbles/dataframes with metadata.
[microeco/](microeco/)            |  Datasets and results produced and used directly by the microeco package.
[setup/](setup/)                  |  Modularized .R scripts with all parameters, functions, packages, and other dependencies.



Samplesets Used to Organize and Categorize Data for Current Projects
---------------------------------------------------------------------
Each of these may appear as subdirectories within those listed above to organize the files for each set of projects. Below are the main samplesets in use.

sampleset shorthand               |  description
----------------------------------|------------------------------------------------------------------------------------
loris                             |  Pygmy loris genetic, microbial, and behavioral data collected with Henry Doorly Zoo
marmoset                          |  Gut microbiome data gathered from the UNO Research Colony for Shayda Azadmanesh's thesis.
bats                              |  Genetic and gut microbiome data gathered by collaborators at trapping sites across N. America
environmental                     |  Samples gathered from opportunistic environmental sources for Thomas Raad's thesis.
isolates                          |  Purified DNA from bacterial isolates grown in the Ayayeye lab for whole genome sequencing in the Rich Lab.

Highlighted Summaries or Graphics
---------------------------------------------------------------------
These links take you to full-page summary tables or graphics compiled for some of the in-progress analyses.

- [Summary Table of Loris Metadata](https://rich-molecular-health-lab.github.io/bioinformatics_stats/metadata/loris/loris_metadata_summary.html) 
- [Summary Table of Sequencing Depths for Loris 16S Data](https://rich-molecular-health-lab.github.io/bioinformatics_stats/visuals/loris_depth_summary.html)
- [Histogram of Sequencing Depths for Loris 16S Data](https://rich-molecular-health-lab.github.io/bioinformatics_stats/visuals/loris_depth_hist.html)



Helpful Manuals, Cheatsheets, and Tutorials
---------------------------------------------------------------------
Here are some of the online resources for the packages or platforms that I use the most for different workflows and scripts in the lab.

- [Posit Cheatsheets](https://posit.co/resources/cheatsheets/)
- [R Studio User Guide](https://docs.posit.co/ide/user/)
- [Holland Computing Center Manual](https://hcc.unl.edu/docs/intro/)
- [Happy Git with R](https://happygitwithr.com/)
- [R Markdown Cookbook](https://bookdown.org/yihui/rmarkdown-cookbook/)
- [R Markdown Definitive Guide](https://bookdown.org/yihui/rmarkdown/)
- [MicroEco Tutorial](https://chiliubio.github.io/microeco_tutorial/)
- [Data Analysis in R](https://bookdown.org/steve_midway/DAR/)
- [Tidyverse Packages](https://www.tidyverse.org/)
- [Tools for Microbiome Analysis](https://microsud.github.io/Tools-Microbiome-Analysis/)
- [Unix Commands Cheatsheet](https://www.stationx.net/unix-commands-cheat-sheet/)
- [Bash Scripting Cheatsheet](https://devhints.io/bash)
- [Palateer Discrete R Color Palettes](https://emilhvitfeldt.github.io/r-color-palettes/discrete.html)
- [Unicode Character Reference](https://www.compart.com/en/unicode/category/So)
- [LaTeX Basics](https://en.wikibooks.org/wiki/LaTeX/Basics)
- [HTML Tags](https://www.w3schools.com/tags/)
- [Shiny for R](https://shiny.posit.co/r/getstarted/shiny-basics/lesson1/)
- [Mastering Shiny](https://mastering-shiny.org/)
- [R for Data Science](https://r4ds.had.co.nz/)
- [Tools for Reproducible Research](https://sachsmc.github.io/knit-git-markr-guide/)
- [Reactable Tables](https://glin.github.io/reactable/articles/examples.html)
- [HTML Widgets Gallery](https://gallery.htmlwidgets.org/)
- [Config Package](https://rstudio.github.io/config/articles/config.html)
- [Here Package](https://here.r-lib.org/)
- [Fontawesome Icons](https://fontawesome.com/search?ic=free)
- [Bslib Toolkit](https://rstudio.github.io/bslib/index.html)
- [Getting Started with Git](https://docs.github.com/en/get-started/getting-started-with-git/set-up-git)


