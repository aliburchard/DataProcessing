# DataProcessing
Code for processing Zooniverse Data

This repo contains R code for handling Zooniverse data, especially flattening and aggregating classification data.

## Notebooks
Contains code for notebooks published on www.rpubs.com/aliburchard

## Projects 

### Sample Data
Contains small subsets of data that can be used to try running the R code.

### Point Marking
**Wildebeest:** 
Flattening code for the Serengeti Wildebeest Count (https://www.zooniverse.org/projects/dl-j/serengeti-wildebeest-count)
Note that in this flattened file, every mark gets its own row

### In Development
Contains scripts that are in the works.

This contains scripts and sample data for flattening and aggregating. 

### Survey 

#### Generalized 
This folder contains code that should work on *most* PFE survey tasks.

**flattening-wrapper:** This provides a wrapper around the flattening-script.R, in which you specify your project, identify and subset your dataset to the relevant workflows, and identify your various question types (e.g. your "how many", "yes/no", or "select-all-that-apply" question fields.) This calls the flattening-script.R code and produces a flat file for you to save. Note that if you have shortcut questions, you will need to flatten those separately and integrate into your dataset. See the example script for Kenya Wildlife Watch for examples.

**flattening-wrapper-noninteractive:** If you already know your project specifications (e.g. workflow ID and version, question fields, etc), you can store them in a separate file and call them here. This is possibly useful once you've tested your initial flattening interactively, and now flatten your complete dataset.

**flattening-script:** This includes all of the functions for actually running the flattening, and indeed, a function that actually *runs* the flattening code.

**aggregate-survey:** Aggregation is in less generalizable shape than the flattening code. This script works for Michigan Wildlife Watch, Wildcam Gorongongosa, and basically any project that has any of the standard survey task question types: "how many", "yes/no", "select all that apply." *Note* how many is a special type of a "single choice" question, in that the answers are treated as an ordinal factor and the min/median/max values are reported. This code does not yet handle more standard "single choice" questions, that, say ask the user to select one answer from a variety of answers. 

**aggregate-functions:** This contains all of the functions called in aggregate-survey.R. This could really use a function that takes the median value of an ordinal factor (that would be called in the "how many" extraction.

**getting-started:** This script just introduces you to working with JSON if you want to explore how Hadley Wickham's tidyjson works a bit.

#### Examples
This folder contains code that is adapted for specific projects, sometimes using the top-level scripts but sometimes working through the projects in a more step-by-step matter that illustrates how to interact with JSON.

**michigan-flatten:**
**michigan-aggregate:**

**kenya-flatten:**

**chicago-flatten:**




