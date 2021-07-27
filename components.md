# Application components

This file describes the parts of the Word Vector Interface (WVI), an R Shiny
application.

## R code

A file called [app.R](./app.R) consists of R code which sets up and runs the 
WVI. The Shiny application pulls all the other components together. The app:

* reads the JSON catalog to determine which word embedding models to load;
* loads those models, as well as their titles and descriptions;
* constructs the skeleton web page, including any links to web assets that the 
browser should use;
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

Once the Shiny server puts out a complete web page in HTML, more files are 
needed to make the page appear and behave as it does. The WVI makes use of
Cascading Style Sheets (CSS) for styling, and Javascript for interactivity.

* [Shiny Dashboard](http://rstudio.github.io/shinydashboard/index.html) is the R
library that structures the web application. Installing the library also 
installs its pre-made CSS and Javascript, which give the web page its dashboard 
appearance. The R library automatically puts links to the assets in the output 
webpage.
* [Bootstrap](https://getbootstrap.com/) (version 4.1.2) provides CSS for a 
clean, flexible, and customizable foundation. The Bootstrap CSS is linked in the 
Shiny app’s output web page, and is hosted by a [content delivery 
network](https://www.bootstrapcdn.com/), not a local file.
* Local assets consist of CSS, JS, and image files specific to the Shiny app and
the Women Writers Project. The primary files are:
  * [main.css](./www/styles/main.css) adds WVI-specific styles to the CSS 
  library themes described above. R code places a link to the stylesheet in the 
  output webpage.
  * [script.js](./script.js) shows or hides the WVI sidebar on request. The 
  contents of the file are inserted directly into the output webpage by the 
  Shiny app.

