# Application components

The Word Vector Interface (WVI) is an R Shiny application. The code which 
defines and runs the application is in a file called [app.R](./app.R), located 
in the same directory as this documentation.

## R code

The Shiny application pulls all the other components together. The application:

* reads the JSON catalog to determine which word embedding models to load;
* loads those models, as well as their titles and descriptions;
* constructs the skeleton web page;
* lays out the form controls for each WVI tab (“Home”, “Compare”, etc.);
* defines what happens when someone changes a setting or types in a query;
* opens up access to the web application at the computer’s port 3939 
([localhost:3939](http://localhost:3939)); and
* performs queries for users on request.

## Word embedding models

The Shiny application decides which models to load by reading the file
[catalog.json](./data/catalog.json). This catalog contains basic information on
models, such as their “short name” (what to call them in the model selector
dropdown) and their location (where the model’s BIN file can be found). 

An entry might look like this:

```
"WWO Corpus": {
    "shortName": "WWO Full Corpus",
    "location": "data/wwo_xquery-nonreg_allTexts.bin",
	    "shortDescription" : "All texts from WWO",
	    "public" : "true",
    "description": "Currently displayed: the entire Women Writers Online corpus of women's writing from 1526 to 1850."
  }
```

Note that models’ file locations should be relative to [app.R](./app.R), *not* 
to the catalog itself.

If an entry is marked as `"public" : "true"`, the corresponding model will be 
loaded into the Shiny application. This repository contains a lot of models,
including some that aren’t available via the Women Writers Vector Toolkit. 

All models are currently stored in the [data folder](./data/), but they could be 
stored anywhere, as long as the filepath listed in the catalog is correct and 
the model is accessible. The Shiny app only needs to know where the catalog can 
be found.

## Web assets


