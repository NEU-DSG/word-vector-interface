##
##    Word Vector Interface (WVI)
##

## WVI 1. SETUP

# Load libraries.
library(shiny)
library(shinyjs)
library(shinydashboard)
library(magrittr)
library(DT)
library(rjson)
library(wordcloud)
library(ggrepel)
library(tidyverse)
library(wordVectors)

# Maximum number of terms to display per model
max_terms <- 150

# Load JSON catalog of models and information about them.
catalog_filename <- "data/catalog.json"
catalog_json <- fromJSON(file=catalog_filename)

# Load models.
available_models <- c()
list_clustering <- list()
list_models <- list()
list_desc <- list()
vectors <- list()
# The default model is either the first public model in the catalog, or 
# "WWO Full Corpus".
selected_default <- 1
selected_compare_1 <- 1
selected_compare_2 <- 1

# Iterate over models in the catalog, adding them to the lists of models that 
# can be used for various features.
# TODO: First, narrow down to the public models, so they can be counted
for (model in catalog_json) {
  # A model is only acted upon if it's marked as public.
  if (model$public == "true") {
    print(model$shortName)
    print(model$location)
    name <- model$shortName
    # The WWO full corpus and WWO body content models are set as defaults if 
    # they appear.
    if (name == "WWO Full Corpus") {
      selected_default <- name
      # TODO: consider removing selected_compare_1 in favor of selected_default
      selected_compare_1 <- name
    } else if (name == "WWO Body Content") {
      selected_compare_2 <- name
    }
    # TODO: augment the catalog object instead of splitting everything into lists?
    available_models <- append(available_models, name)
    list_models[[name]] <- read.vectors(model$location)
    list_desc[[name]] <- model$description
    list_clustering[[name]] <- kmeans(list_models[[name]], centers=150, iter.max = 40)
    data <- as.matrix(list_models[[name]])
    vectors[[name]] <- stats::predict(stats::prcomp(data))[,1:2]
  }
}
print("Done loading models")

reactive_value_obj <- reactiveValues()
generateClustersData <- function(model) {
  data <- sapply(sample(1:150, 10), function(n) {
    #ls_download_cluster <<- c(ls_download_cluster, n)
    cword <- names(model$cluster[model$cluster==n][1:max_terms])
    #linkToWWO(keyword = cword, session = session)
  })
  reactive_value_obj[['clusters']] <- data %>% 
    as_tibble(.name_repair = "minimal")
  #browser()
}
generateClustersData(list_clustering[[selected_default]])
my_clusters <- c()
ls_download_cluster <- c()


## WVI 2. USER INTERFACE (UI)

# Create a link to search WWO, optionally with a proxy URL.
linkToWWO <- function(keyword, session) {
  url <- paste0("https://wwo.wwp.northeastern.edu/WWO/search?keyword=",keyword)
  requestParams <- parseQueryString(session$clientData$url_search)
  proxy <- requestParams$proxy
  proxy <- ifelse( exists("proxy") && proxy != '', proxy, 'none' )
  # Only use the proxy value if it starts with HTTP or HTTPS protocol
  if ( grepl('^https?://', proxy) ) {
    url <- paste0(proxy,url)
  }
  paste0("<a target='_blank' href='",url,"'>",keyword,"</a>")
}

tableSimpleOpts <- list(lengthMenu = c(10, 20, 100, 150), pageLength=10, searching = TRUE)
tableSidebarOpts <- function(page_len) {
  return(list(dom = 't', pageLength = page_len, searching = FALSE))
}

# Generate a two-column table for a given set of vector data.
makeTableForModel <- function(modelVector, session, opts=list()) {
  data <- modelVector %>% 
    mutate("Link" <- linkToWWO(keyword=.$word, session=session)) %>% 
    .[c(3,2)]
  table <- DT::datatable(data, escape = FALSE,
    colnames = c("Word", "Similarity to word(s)"), 
    options = opts)
  return(table)
}


##  WVI 2a. "HOME" UI

# Create sidebar content for "Home" tab.
home_sidebar <- conditionalPanel(condition="input.tabset1==1",
  selectInput("modelSelect", "Model",
    choices = available_models,
    selected = selected_default),
  br(),
  sliderInput("max_words_home",
    "Number of Words:",
    min = 1,  max = 150,  value = 10)
)
# Create main content for "Home" tab.
home_content <- tabPanel("Home", value=1,
  htmlTemplate("html/home_tab_content.html", 
    model_name = textOutput("model_name_basic"),
    model_desc = uiOutput("model_desc_basic"),
    controls = textInput("basic_word1", "Query term:", width="500px"),
    results = DT::dataTableOutput("basic_table"))
)

##  WVI 2b. "COMPARE" UI

# Create sidebar content for "Compare" tab.
compare_sidebar <- conditionalPanel(condition="input.tabset1==2",
  selectInput("modelSelectc1", "Model 1",
    choices = available_models,
    selected = selected_compare_1),
  selectInput("modelSelectc2", "Model 2",
    choices = available_models,
    selected = selected_compare_2),
  sliderInput("max_words",
    "Number of Words:",
    min = 1,  max = 150,  value = 10)
)
# Create main content for the "Compare" tab.
compare_content <- tabPanel("Compare", value=2,
  id = "compareTab-Id",
  htmlTemplate("html/compare_tab_content.html",
    controls = textInput("basic_word_c", "Query term:", width="500px"),
    model_1_name = textOutput("model_name_compare_1"),
    model_1_desc = uiOutput("model_desc_compare_1"),
    model_1_results = DT::dataTableOutput("basic_table_c1"),
    model_2_name = textOutput("model_name_compare_2"),
    model_2_desc = uiOutput("model_desc_compare_2"),
    model_2_results = DT::dataTableOutput("basic_table_c2"))
)

##  WVI 2c.  "CLUSTERS" UI

# Given some data, create a table of 10 clusters.
renderClusterTable <- function(data, rows) {
  DT::datatable(data, escape = FALSE, 
    colnames = c(paste0("cluster_",1:10)), 
    options = list(dom='ft', 
      lengthMenu = c(10, 20, 100, 150), 
      pageLength = rows, 
      searching = TRUE))
}

# Create a new table of 10 clusters.
generateClusters <- function(model, rows, session) {
  data <- sapply(sample(1:150, 10), function(n) {
    ls_download_cluster <<- c(ls_download_cluster, n)
    cword <- names(model$cluster[model$cluster==n][1:max_terms])
    linkToWWO(keyword = cword, session = session)
  })
  my_clusters <- data %>% as_tibble(.name_repair = "minimal")
  renderClusterTable(my_clusters, rows)
}

# Create sidebar content for "Clusters" tab.
clusters_sidebar <- conditionalPanel(condition="input.tabset1==3",
  selectInput("modelSelect_clusters", "Model",
    choices = available_models,
    selected = selected_default),
  br(),
  column(
    id = "Download_reset_button",
    width = 12,
    actionButton("clustering_reset_input_fullcluster", "Reset clusters", 
      class="clustering-reset-full"),
    downloadButton("downloadData", "Download")
  ),
  br(),
  br(),
  sliderInput("max_words_cluster",
    "Number of Words:",
    min = 1,  max = 150,  value = 10)
)
# Create main content for the "Clusters" tab.
clusters_content <- tabPanel("Clusters", value=3,
  htmlTemplate("html/clusters_tab_content.html",
    controls = actionButton("clustering_reset_input_fullcluster1", 
      "Reset clusters", 
      class="clustering-reset-full"),
    model_name = textOutput("model_name_cluster"),
    model_desc = uiOutput("model_desc_cluster"),
    results = DTOutput('clusters_full'))
)


##  WVI 2d. "OPERATIONS" UI

# Create sidebar content for "Operations" tab.
operations_sidebar <- conditionalPanel(condition="input.tabset1==4",
  selectInput("modelSelect_operations", "Model",
    choices = available_models,
    selected = selected_default),
  selectInput("operator_selector", "Select operator",
    choices = c("Addition", "Subtraction", "Analogies", "Advanced"),
    selected = 1)
)
# Create the contents of the "Addition" operation tab.
controlsPlus <- conditionalPanel(condition="input.operator_selector=='Addition'",
  htmlTemplate("html/operations_addition.html",
    word1 = textInput("addition_word1", "Word 1"),
    operator = icon("plus"),
    word2 = textInput("addition_word2", "Word 2"),
    results = DT::dataTableOutput("addition_table")),
  width = 12)
# Create the contents of the "Subtraction" operation tab.
controlsMinus <- conditionalPanel(condition="input.operator_selector=='Subtraction'",
  htmlTemplate("html/operations_addition.html",
    word1 = textInput("subtraction_word1", "Word 1"),
    operator = icon("minus"),
    word2 = textInput("subtraction_word2", "Word 2"),
    results = DT::dataTableOutput("subtraction_table")),
  width = 12)
# Create the contents of the "Analogies" operation tab.
controlsAnalogy <- conditionalPanel(condition="input.operator_selector=='Analogies'",
  htmlTemplate("html/operations_analogy.html",
    word1 = textInput("analogies_word1", "Word 1"),
    operator = icon("minus"),
    word2 = textInput("analogies_word2", "Word 2"),
    operator2 = icon("plus"),
    word3 = textInput("analogies_word3", "Word 3"),
    results = DT::dataTableOutput("analogies_table")),
  width = 12)
# Create the contents of the "Advanced" operation tab.
operatorsList <- 
  list( "+" = "+", 
        "-" = "-", 
        "*" = "*", 
        "/" = "/")
controlsAdvanced <- conditionalPanel(condition="input.operator_selector=='Advanced'",
  htmlTemplate("html/operations_advanced.html",
    word1 = textInput("advanced_word1", "Word 1"),
    operator1 = selectInput("advanced_math", "Math", choices = operatorsList, 
      selected = 1),
    word2 = textInput("advanced_word2", "Word 2"),
    operator2 = selectInput("advanced_math2", "Math", choices = operatorsList,
      selected = 1),
    word3 = textInput("advanced_word3", "Word 3"),
    results = DT::dataTableOutput("advanced_table")),
  width=12)
# Create main content for the "Operations" tab.
operations_content <- tabPanel("Operations", value=4,
  htmlTemplate("html/operations_tab_content.html",
    model_name = textOutput("model_name_operation"),
    model_desc = uiOutput("model_desc_operation"),
    addition = controlsPlus,
    subtraction = controlsMinus,
    analogies = controlsAnalogy,
    advanced = controlsAdvanced)
)


##  WVI 2e. "VISUALIZATION" UI

# Create sidebar content for "Visualization" tab.
viz_sidebar <- conditionalPanel(condition="input.tabset1==5",
  selectInput("modelSelect_Visualisation_tabs", "Model",
    choices = available_models,
    selected = selected_default),
  selectInput("visualisation_selector","Select visualisation",
    choices = list(
      "Word Cloud" = "wc",
      "Query Term Scatterplot" = "scatter_closest",
      "Cluster Scatterplot" = "scatter"),
    selected = 1),
  # Create sidebar content for word cloud visualization.
  conditionalPanel(condition="input.visualisation_selector=='wc'",
    sliderInput("freq",
      "Similarity",
      step = 5,
      ticks = TRUE,
      min = 0,  max = 100, value = 15),
    sliderInput("max",
      "Maximum Number of Words:",
      min = 0,  max = 150,  value = 100),
    sliderInput("scale",
      "Size of plot:",
      min = 0,  max = 5,  value = 3)
  ),
  # Create sidebar content for query term scatterplot.
  conditionalPanel(condition="input.visualisation_selector=='scatter'",
    selectInput("scatter_cluster", "Cluster",
      choices = list("Cluster 1" = "V1",
        "Cluster 2" = "V2",
        "Cluster 3" = "V3",
        "Cluster 4" = "V4",
        "Cluster 5" = "V5",
        "Cluster 6" = "V6",
        "Cluster 7" = "V7",
        "Cluster 8" = "V8",
        "Cluster 9" = "V9",
        "Cluster 10" = "V10" ),
      selected = 1),
    sliderInput("scatter_number",
      "Number of Words:",
      min = 5,  max = 30,  value = 10),
    actionButton("clustering_reset_input_visualisation", "Reset clusters")
  ),
  # Create sidebar content for cluster scatterplot.
  conditionalPanel(condition="input.visualisation_selector=='scatter_closest'",
    selectInput("scatter_plot_closest_choice", "Cluster",
      choices = list("Top 10",
        "Top 20",
        "Top 40",
        "Top 60",
        "Top 80",
        "Top 150"),
      selected = 1)
  )
)
# Create main content for the "Visualization" tab.
viz_content <- tabPanel("Visualization", value=5,
  fluidRow(
    box(
      tags$h1(textOutput("model_name_visualisation")),
      div(class = "model_desc", p(uiOutput("model_desc_visualisation"))),
      width=12
    ),
    conditionalPanel(condition="input.visualisation_selector=='wc'",
      class = "visualization",
      box( solidHeader = TRUE, 
        textInput("word_cloud_word", "Query term:", width = "500px"), 
        width=12),
      box(
        solidHeader = FALSE,
        box(
          solidHeader = TRUE,
          plotOutput("word_cloud", height="600px"),
          width = 8
        ),
        box(
          div(class = "model_desc", 
            p("The visualizations tab allows you to create a
                              word cloud for the query term you would like to
                              analyze. The word cloud will produce a collage
                              of the most similar words to your query term
                              using the WWO general corpus model. You can
                              adjust the visualization based on the number
                              of words you would like to see appear
                              (top slider bar on the left of this page).
                              These terms are based on their percentage of
                              similarity to the query term. The similarity
                              percentage is also represented in the visualization
                              by the color of each word. See below for the color
                              key. The second slider down from the similarity
                              bar will allow you to adjust the number of words you
                              would like in your word cloud, and the bottom-most
                              slider controls the size of the plot image."),
            div("Similarity Color Key"),
            div("Similarity % -- Color"),
            div("91 – 100 -- gray"),
            div("81 – 90 -- brown"),
            div("71 – 80 -- orange"),
            div("51 – 70 -- green"),
            div("00 – 50 -- pink")
          ),
          width = 4
        ),
        width = 12
      )
    ),
    conditionalPanel(condition="input.visualisation_selector=='scatter'",
      class = "visualization",
      box(
        plotOutput("scatter_plot", height = 600),
        width = 8
      )
    ),
    conditionalPanel(condition="input.visualisation_selector=='scatter_closest'",
      class = "visualization",
      box( solidHeader = TRUE, 
        textInput("scatter_plot_term", "Query term:", width = 500), 
        width=12),
      box(
        plotOutput("scatter_plot_closest", height = 600),
        width = 8
      )
    )
  )
)


##  WVI 2f. FULL APPLICATION UI

# Put together all the pieces of the user interface.
app_ui = dashboardPage(
  title = "Word Vector Interface | Women Writers Vector Toolkit",
  header = tags$header(
    class = "main-header",
    tags$link(rel="stylesheet", type="text/css", 
              href="https://stackpath.bootstrapcdn.com/bootstrap/4.1.2/css/bootstrap.min.css"),
    tags$link(rel="stylesheet", type="text/css", href="styles/main.css"),
    includeScript(path="script.js"),
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"),
    htmlTemplate("html/navbar.html", name="header-component")
  ),
  # Create the sidebar and all content, which is shown or hidden depending on 
  # the current open tab.
  sidebar = dashboardSidebar(home_sidebar, compare_sidebar, clusters_sidebar,
    operations_sidebar, viz_sidebar),
  body = dashboardBody(
    shinyjs::useShinyjs(),
    tabBox(id = "tabset1", width = 12,
      # The id lets us use input$tabset1 on the server to find the current tab
      home_content, compare_content, clusters_content, operations_content, viz_content
    )
  )
)


##  WVI 3. SERVER LOGIC

# Listen for user interactions and handle them. This function runs once per user
# session.
app_server <- function(input, output, session) {
  # Apply settings for this app session.
  #set.seed(122)
  #histdata <- rnorm(500)
  
  # Given a model, generate its description for display.
  renderModelDesc <- function(model) {
    url <- a("[read more]", href="https://wwp.northeastern.edu/lab/wwvt/methodology/")
    renderUI({ tagList(paste(list_desc[[model]], "The text has been regularized."), url) })
  }
  
  
  ## WVI 3a."HOME" REACTIVE COMPONENTS
  
  # Keep the model name and description in sync with the user's choice.
  observeEvent(input$modelSelect, {
    output$model_name_basic <- renderText(input$modelSelect[[1]])
    output$model_desc_basic <- renderModelDesc(input$modelSelect[[1]])
  })
  
  # Generate table for the Home tab.
  output$basic_table <- DT::renderDataTable({
    data <- list_models[[input$modelSelect[[1]]]] %>% 
      closest_to(tolower(input$basic_word1), max_terms)
    makeTableForModel(data, session, tableSidebarOpts(input$max_words_home))
  })
  
  
  ## WVI 3b."COMPARE" REACTIVE COMPONENTS
  
  # Keep the models' names and descriptions in sync with the user's choices.
  observeEvent(input$modelSelectc1, {
    output$model_name_compare_1 <- renderText(input$modelSelectc1[[1]])
    output$model_desc_compare_1 <- renderModelDesc(input$modelSelectc1[[1]])
  })
  observeEvent(input$modelSelectc2, {
    output$model_name_compare_2 <- renderText(input$modelSelectc2[[1]])
    output$model_desc_compare_2 <- renderModelDesc(input$modelSelectc2[[1]])
  })
  
  # Generate 1st table for Compare tab.
  output$basic_table_c1 <- DT::renderDataTable({
    data <- list_models[[input$modelSelectc1[[1]]]] %>% 
      closest_to(tolower(input$basic_word_c), max_terms)
    makeTableForModel(data, session, tableSidebarOpts(input$max_words))
  })
  # Generate 2nd table for Compare tab.
  output$basic_table_c2 <- DT::renderDataTable({
    data <- list_models[[input$modelSelectc2[[1]]]] %>% 
      closest_to(tolower(input$basic_word_c), max_terms)
    makeTableForModel(data, session, tableSidebarOpts(input$max_words))
  })
  
  
  ## WVI 3c."CLUSTERS" REACTIVE COMPONENTS
  
  ## Keep the model name and description in sync with the user's choice.
  observeEvent(input$modelSelect_clusters, {
    output$model_name_cluster <- renderText(input$modelSelect_clusters[[1]])
    output$model_desc_cluster <- renderModelDesc(input$modelSelect_clusters[[1]])
  })
  
  # Generate and render clusters.
  output$clusters_full <- DT::renderDataTable({
    renderClusterTable(reactive_value_obj[['clusters']], input$max_words_cluster)
  })
  # Handle resetting clusters from tab content.
  observeEvent(input$clustering_reset_input_fullcluster, {
    generateClustersData(list_clustering[[input$modelSelect_clusters[[1]]]])
    renderClusterTable(reactive_value_obj[['clusters']], input$max_words_cluster)
  })
  # Handle resetting clusters from sidebar.
  observeEvent(input$clustering_reset_input_fullcluster1, {
    generateClustersData(list_clustering[[input$modelSelect_clusters[[1]]]])
    renderClusterTable(reactive_value_obj[['clusters']], input$max_words_cluster)
  })
  
  # Create a CSV file of clusters when requested.
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste(input$modelSelect_clusters[[1]], ".csv", sep="") 
    },
    content = function(file) {
      data <- reactive_value_obj[['clusters']]
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  
  ## WVI 3d."OPERATIONS" REACTIVE COMPONENTS
  
  # Keep the model name and description in sync with the user's choice.
  observeEvent(input$modelSelect_operations, {
    output$model_name_operation <- renderText(input$modelSelect_operations[[1]])
    output$model_desc_operation <- renderModelDesc(input$modelSelect_operations[[1]])
  })
  
  # Generate table for Addition operation.
  output$addition_table <- DT::renderDataTable({
    validate(need(input$addition_word1 != "" && input$addition_word2 != "", 
      "Enter query term into word 1 and word 2."))
    use_model <- list_models[[input$modelSelect_operations[[1]]]]
    op_vector <- as.VectorSpaceModel(
      use_model[[tolower(input$addition_word1)]] +
      use_model[[tolower(input$addition_word2)]]
    )
    data <- use_model %>% closest_to(op_vector)
    makeTableForModel(data, session, tableSimpleOpts)
  })
  
  # Generate table for Subtraction operation.
  output$subtraction_table <- DT::renderDataTable({
    validate(need(input$subtraction_word1 != "" && input$subtraction_word2 != "", 
      "Enter query term into word 1 and word 2."))
    use_model <- list_models[[input$modelSelect_operations[[1]]]]
    op_vector <- as.VectorSpaceModel(
      use_model[[tolower(input$subtraction_word1)]] -
      use_model[[tolower(input$subtraction_word2)]]
    )
    data <- use_model %>% closest_to(op_vector)
    makeTableForModel(data, session, tableSimpleOpts)
  })
  
  # Generate table for Analogies operation.
  output$analogies_table <- DT::renderDataTable({
    validate(need(input$analogies_word1 != "" && 
                  input$analogies_word2 != "" && 
                  input$analogies_word3 != "", 
      "Enter query term into Word 1, Word 2, and Word 3."))
    use_model <- list_models[[input$modelSelect_operations[[1]]]]
    op_vector <- as.VectorSpaceModel(
        use_model[[tolower(input$analogies_word1)]] -
        use_model[[tolower(input$analogies_word2)]] + 
        use_model[[tolower(input$analogies_word3)]]
      )
    data <- use_model %>% closest_to(op_vector)
    makeTableForModel(data, session, tableSimpleOpts)
  })
  
  # Generate table for Advanced math operation.
  output$advanced_table <- DT::renderDataTable({
    validate(need(input$advanced_word1 != "", "Enter query term into Word 1."))
    use_model <- list_models[[input$modelSelect_operations[[1]]]]
    vector1 <- use_model[[tolower(input$advanced_word1)]]
    # If there's only 1 word, no math needs to be done.
    if (input$advanced_word2 == "" && input$advanced_word3 == "") {
      data <- use_model %>% closest_to(vector1, max_terms)
      # If the 1st and 2nd words were provided...
    } else if (input$advanced_word2 != "" && input$advanced_word3 == "") {
      vector2 <- use_model[[tolower(input$advanced_word2)]]
      if (input$advanced_math == "+") {
        # We have to coerce the result of vector addition into VectorSpaceModel format
        op_vector <- as.VectorSpaceModel(vector1 + vector2)
      } else if (input$advanced_math == "-") {
        op_vector <- vector1 - vector2
      } else if (input$advanced_math == "*") {
        # We have to coerce the result of vector multiplication into VectorSpaceModel format
        op_vector <- as.VectorSpaceModel(vector1 * vector2)
      } else if (input$advanced_math == "/") {
        # We have to coerce the result of vector division into VectorSpaceModel format
        op_vector <- as.VectorSpaceModel(vector1 / vector2)
      }
      data <- use_model %>% closest_to(op_vector, max_terms)
      # If all 3 words have been provided...
    } else if (input$advanced_word2 != "" && input$advanced_word3 != "") {
      vector2 <- use_model[[tolower(input$advanced_word2)]]
      vector3 <- use_model[[tolower(input$advanced_word3)]]
      # When the first operator is + :
      if (input$advanced_math == "+" && input$advanced_math2 == "+") {
        op_vector <- as.VectorSpaceModel(vector1 + vector2 + vector3)
      }
      if (input$advanced_math == "+" && input$advanced_math2 == "-") {
        op_vector <- as.VectorSpaceModel(vector1 + vector2 - vector3)
      }
      if (input$advanced_math == "+" && input$advanced_math2 == "*") {
        op_vector <- as.VectorSpaceModel(vector1 + vector2 * vector3)
      }
      if (input$advanced_math == "+" && input$advanced_math2 == "/") {
        op_vector <- as.VectorSpaceModel(vector1 + vector2 / vector3)
      }
      # When the first operator is - :
      if (input$advanced_math == "-" && input$advanced_math2 == "+") {
        op_vector <- as.VectorSpaceModel(vector1 - vector2 + vector3)
      }
      if (input$advanced_math == "-" && input$advanced_math2 == "-") {
        op_vector <- as.VectorSpaceModel(vector1 - vector2 - vector3)
      }
      if (input$advanced_math == "-" && input$advanced_math2 == "*") {
        op_vector <- as.VectorSpaceModel(vector1 - vector2 * vector3)
      }
      if (input$advanced_math == "-" && input$advanced_math2 == "/") {
        op_vector <- as.VectorSpaceModel(vector1 - vector2 / vector3)
      }
      # When the first operator is * :
      if (input$advanced_math == "*" && input$advanced_math2 == "+") {
        op_vector <- as.VectorSpaceModel(vector1 * vector2 + vector3)
      }
      if (input$advanced_math == "*" && input$advanced_math2 == "-") {
        op_vector <- as.VectorSpaceModel(vector1 * vector2 - vector3)
      }
      if (input$advanced_math == "*" && input$advanced_math2 == "*") {
        op_vector <- as.VectorSpaceModel(vector1 * vector2 * vector3)
      }
      if (input$advanced_math == "*" && input$advanced_math2 == "/") {
        op_vector <- as.VectorSpaceModel(vector1 * vector2 / vector3)
      }
      # When the first operator is / :
      if (input$advanced_math == "/" && input$advanced_math2 == "+") {
        op_vector <- as.VectorSpaceModel(vector1 / vector2 + vector3)
      }
      if (input$advanced_math == "/" && input$advanced_math2 == "-") {
        op_vector <- as.VectorSpaceModel(vector1 / vector2 - vector3)
      }
      if (input$advanced_math == "/" && input$advanced_math2 == "*") {
        op_vector <- as.VectorSpaceModel(vector1 / vector2 * vector3)
      }
      if (input$advanced_math == "/" && input$advanced_math2 == "/") {
        op_vector <- as.VectorSpaceModel(vector1 / vector2 / vector3)
      }
      data <- use_model %>% closest_to(op_vector, max_terms)
    }
    makeTableForModel(data, session, tableSimpleOpts)
  })
  
  
  ## WVI 3e."VISUALIZATION" REACTIVE COMPONENTS
  
  # Keep the model name and description in sync with the user's choice.
  observeEvent(input$modelSelect_Visualisation_tabs, {
    output$model_name_visualisation <- renderText(input$modelSelect_Visualisation_tabs[[1]])
    output$model_desc_visualisation <- renderModelDesc(input$modelSelect_Visualisation_tabs[[1]])
  })
  
  # Generate word cloud.
  output$word_cloud <- renderPlot({
    validate(need(tolower(input$word_cloud_word) != "", 
      "To generate a word cloud, enter a query term in the text field above."))
    data <- list_models[[input$modelSelect_Visualisation_tabs[[1]]]] %>% 
      closest_to(tolower(input$word_cloud_word), max_terms)
    colnames(data) <- c("words", "sims")
    data <- mutate(data, sims = as.integer(sims * 100))
    set.seed(1234)
    wordcloud(words = data$words, freq = data$sims,
              min.freq = input$freq, max.words=input$max,
              random.order=FALSE, rot.per = 0.30, 
              random.color = FALSE, ordered.colors = FALSE,
              colors = brewer.pal(8, "Dark2"), 
              scale = c(input$scale, 0.5),
              use.r.layout = TRUE)
  })
  
  # Generate query term scatterplot.
  dataset <- reactive({
    times <- input$clustering_reset_input_visualisation
    df2 <- sapply(sample(1:150,10), function(n) {
      use_model <- list_clustering[[input$modelSelect_Visualisation_tabs[[1]]]]
      paste0(names(use_model$cluster[use_model$cluster == n][1:150]))
      }) %>% 
      as_tibble(.name_repair = "minimal")
    df2
  })
  datascatter <- reactive({
    df2 <- dataset()
    # print(df2)
    x <- c()
    y <- c()
    names <- c()
    cluster <- c()
    vector <- vectors[[input$modelSelect_Visualisation_tabs[[1]]]]
    for (column in colnames(df2)) {
      for (word in head(df2,input$scatter_number)[column][[1]]) {
        x <- append(x, vector[word, 'PC1'])
        y <- append(y, vector[word, 'PC2'])
        names <- append(names,word)
        cluster <- append(cluster,column)
      }
    }
    df_new <- data.frame(x = x, y = y, names = names, 
                         cluster = as.factor(cluster), 
                         stringsAsFactors = FALSE)
    df_new
  })
  output$scatter_plot <- renderPlot({
    ggplot(datascatter(), aes(x=x, y=y, colour=cluster), 
      height="600px", width="100%") +
    geom_point() +
    geom_text_repel(
      aes(label = ifelse(cluster == input$scatter_cluster, as.character(names), '')), 
      hjust=0.5, vjust=-0.5, max.overlaps = 12
    )
  })
  outputOptions(output, "scatter_plot", suspendWhenHidden = TRUE)
  
  # Generate cluster scatterplot.
  dataset_closet <- reactive({
    use_model <- list_models[[input$modelSelect_Visualisation_tabs[[1]]]]
    data <- as.matrix(use_model)
    vectors <-stats::predict(stats::prcomp(data))[,1:2]
    x <- c()
    y <- c()
    names <- c()
    cluster <-c()
    closeword <- use_model %>% 
      closest_to(tolower(input$scatter_plot_term), max_terms)
    i = 0
    for (word in closeword[[1]]) {
      x <- append(x, vectors[word, 'PC1'])
      y <- append(y, vectors[word, 'PC2'])
      if (i <= 10 ) cluster <- append(cluster, "top 10")
      if (i > 10 & i <= 20 ) cluster <- append(cluster, "top 20")
      if (i > 20 & i <= 40 ) cluster <- append(cluster, "top 40")
      if (i > 40 & i <= 60 ) cluster <- append(cluster, "top 60")
      if (i > 60 & i <= 80 ) cluster <- append(cluster, "top 80")
      if (i > 80 & i <= 100 ) cluster <- append(cluster, "top 100")
      if (i > 100  ) cluster <- append(cluster, "top 150")
      i <- i + 1
      names <- append(names, word)
    }
    df_new <- data.frame(x = x, y = y, names = names, 
                         cluster = as.factor(cluster), 
                         stringsAsFactors = FALSE)
    return(df_new)
  })
  output$scatter_plot_closest <- renderPlot({
    ggplot(dataset_closet(), aes(x=x, y=y, colour=cluster)) +
      geom_point() +
      geom_text_repel(
        aes(label = ifelse(cluster == tolower(input$scatter_plot_closest_choice), 
            as.character(names), '')), 
        hjust=0.5, vjust=-0.5)
  })
  outputOptions(output, "scatter_plot_closest", suspendWhenHidden = TRUE)
}


##  WVI 4. GENERATE THE SHINY APP

# Spin up the Word Vector Interface application.
shinyApp(
  ui = app_ui,
  server = app_server,
  options = list(port = 3939)
)
