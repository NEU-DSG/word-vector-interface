# HTML Templates for the Word Vector Interface

This directory contains snippets of HTML which are used to construct WVI Shiny app. Rather than building HTML elements through R functions, the structure of each tab is defined in its corresponding template.


## How templating works

Where possible, we use HTML templates to separate the R code from the structure and styling of the web page. The Shiny application ([`app.R`](../app.R)) generates reactive components and fills them into the templates.

For example, the “Home” tab has a [template which lays out the structure of the tab’s content](home_tab_content.html). Placeholders for reactive UI components look like this:

```html
<div class="box-body">
  {{ controls }}
</div>
```

When `app.R` constructs the WVI user interface, it draws on the HTML template, filling in `controls` like so:

```R
home_content <- tabPanel("Home", value=1,
  htmlTemplate("html/home_tab_content.html", 
    # ...
    controls = textInput("basic_word1", "Query term:", width="500px"),
    # ...
  )
)
```

When the webpage is loaded, you will see that Shiny has generated a text input field with the label “Query term”. Behind the scenes, this form control has the HTML identifier `basic_word1`. When you change the value of the field, Shiny uses definitions in `app.R` to determine how the interface reacts:

```R
output$basic_table <- DT::renderDataTable({
  data <- list_models[[input$modelSelect[[1]]]] %>% 
    closest_to(tolower(input$basic_word1), max_terms)
  makeTableForModel(data, session, tableSidebarOpts(input$max_words_home))
})
```

In this case, the WVI app lower-cases the text in the `basic_word1` field, and generates a table of words with the closest cosine similarity. The rendered HTML table is placed in `output$basic_table`. This `basic_table` is used to fill in part of the Home tab UI:

```R
home_content <- tabPanel("Home", value=1,
  htmlTemplate("html/home_tab_content.html", 
    # ...
    controls = textInput("basic_word1", "Query term:", width="500px"),
    results = DT::dataTableOutput("basic_table"))
  )
)
```

The relevant part of the [template](home_tab_content.html) looks like this:

```html
<div class="box-body" id="table-main-1">
  {{ results }}
</div>
```

For more information, see Shiny’s [documentation on HTML templates](https://shiny.posit.co/r/articles/build/templates/).

