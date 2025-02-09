##
##    Word Vector Interface (WVI)
##

##
## WVI 1. SETUP
##

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
# Total number of clusters to generate per model
num_clusters <- 150

# Load JSON catalog of models and information about them.
catalog_filename <- "data/catalog.json"
catalog_json <- fromJSON(file = catalog_filename)

# Reduce the list of models to only the ones marked as public.
is_public_model <- function(model) {
  model$public == "true"
}
catalog_json <- catalog_json[sapply(catalog_json, is_public_model) == TRUE]

# Load models.
available_models <- c()
list_clusters <- list()
list_models <- list()
list_desc <- list()
vectors <- list()
# The default model is either the first public model in the catalog, or 
# "WWO Full Corpus".
selected_default <- 1
selected_compare_1 <- 1
selected_compare_2 <- 1

# Generate clusters for a model.
generateClustersData <- function(model_clusters) {
  data <- sapply( 1:num_clusters, function(n) {
    cword <- names(model_clusters$cluster[model_clusters$cluster == n][1:max_terms])
  }) %>% as_tibble(.name_repair = "minimal")
  data
  #browser()
}
# Choose a random selection of 10 clusters.
getRandomClusters <- function(subset = 10) {
  sample(1:num_clusters, subset)
}

# Iterate over models in the catalog, adding them to the lists of models that 
# can be used for various features.
total_models <- length(catalog_json)
for (i in 1:total_models) {
  model <- catalog_json[[i]]
  # There should now only be public models, but it doesn't hurt to check.
  if ( model$public != "true" ) { next }
  print(paste(c("Loading model", i, "of", total_models), collapse = " "))
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
  # Generate this model's 150 clusters.
  my_centers <- kmeans(list_models[[name]], centers = num_clusters, iter.max = 40)
  list_clusters[[name]] <- list(name = name,
                                centers = my_centers,
                                clusters = generateClustersData(my_centers))
  data <- as.matrix(list_models[[name]])
  vectors[[name]] <- stats::predict(stats::prcomp(data))[,1:2]
}
print("Done loading models.")


##
## WVI 2. USER INTERFACE (UI)
##
## Code that generates HTML goes here. For code that reacts to server
## events, see section 3. Most UI components here have complementary
## subsections in section 3.
##

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

tableSidebarOpts <- function(page_len) {
  return(list(dom = 't',
              pageLength = page_len,
              searching = FALSE))
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
    min = 1,  max = max_terms,  value = 10)
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
    min = 1,  max = max_terms,  value = 10)
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

# Given some kmeans clusters, create a table of 10 clusters.
renderClusterTable <- function(cluster_indexes, clusters, rows, session) {
  #print(cluster_indexes)
  # Add WWO links around words in each cluster.
  data <- sapply(cluster_indexes, function(cluster_num) {
      clusters[[cluster_num]] %>%
        sapply( function(word) {
          linkToWWO(keyword = word, session = session)
        })
    }) %>% as.data.frame(row.names = c(paste0(1:10)))
  DT::datatable(data, escape = FALSE, 
    colnames = c(paste0("cluster ",cluster_indexes)),
    options = list(dom='ft', 
      lengthMenu = c(10, 20, 100, 150), 
      ordering = FALSE,
      pageLength = rows, 
      searching = TRUE))
}

# Create sidebar content for "Clusters" tab.
clusters_sidebar <- conditionalPanel(condition="input.tabset1==3",
  selectInput("modelSelect_clusters", "Model",
    choices = available_models,
    selected = selected_default),
  br(),
  column(
    id = "cluster-button-container",
    width = 12,
    actionButton("clustering_reset_input_fullcluster", "Reset clusters", 
      class="clustering-reset-full"),
    downloadButton("downloadData", "Download")
  ),
  br(),
  br(),
  sliderInput("max_words_cluster",
    "Number of Words:",
    min = 1,  max = max_terms,  value = 10)
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
    selected = 1),
  sliderInput("max_words_operations",
    "Number of Words:",
    min = 1,  max = max_terms,  value = 10)
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
      "Pair Plot" = "pairs"
    ),
    selected = 1),
  # Create sidebar content for word cloud visualization.
  conditionalPanel(condition="input.visualisation_selector=='wc'",
    sliderInput("freq",
      "Minimum similarity",
      step = 0.05,
      ticks = TRUE,
      min = 0.2,  max = 1, value = 0.5),
    sliderInput("max",
      "Maximum Number of Words:",
      min = 0,  max = max_terms,  value = 100),
    sliderInput("scale",
      "Size of plot:",
      min = 1,  max = 5,  value = 1)
  ),
  # Create sidebar content for the pairs plot.
  conditionalPanel(condition="input.visualisation_selector=='pairs'",
    sliderInput("pairs_max",
      "Maximum Number of Words:",
      min = 0,  max = max_terms,  value = 50)
  )
)
# Create main content for the "Visualization" tab.
viz_content <- tabPanel("Visualization", value=5,
  fluidRow(
    htmlTemplate("html/viz_common_tab_content.html",
      model_name = textOutput("model_name_visualisation"),
      model_desc = uiOutput("model_desc_visualisation")
    ),
    conditionalPanel(condition="input.visualisation_selector=='wc'",
      class = "visualization",
      htmlTemplate("html/viz_word-cloud_tab_content.html",
        word = textInput("word_cloud_word", "Query term:", width = "500px"),
        word_cloud = plotOutput("word_cloud", height = "600px")
      )
    ),
    conditionalPanel(condition="input.visualisation_selector=='pairs'",
      class = "visualization",
      htmlTemplate("html/viz_pairs_tab_content.html",
        word1 = textInput("pairs_term1", "Word 1:", width = "500px"),
        word2 = textInput("pairs_term2", "Word 2:", width = "500px"),
        pairs_plot = plotOutput("pairs_plot", height = "600px")
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
    tags$link(rel="stylesheet", type="text/css", href="styles/wvi.css"),
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


##
##  WVI 3. SERVER LOGIC
##
## Code that reacts to user interaction goes here. For code that sets up
## HTML for the app, see section 2. Most server components here have 
## complementary subsections in section 2.
##

# Listen for user interactions and handle them. This function runs once per user
# session.
app_server <- function(input, output, session) {
  # Set up a list of clusters for a single user's session.
  session$userData[['clusters']] <- list()
  # For the default model, choose 10 random indexes out of the 150 clusters. These 
  # will persist until the user resets the clusters, or until the session ends.
  session$userData$clusters[[selected_default]] <- getRandomClusters()
  # Set up a reactive object that will store ONLY the cluster indices that are 
  # currently shown on the Clusters tab. When this object changes, the table will 
  # change to reflect it.
  reactive_obj <- reactiveValues(clusters = session$userData$clusters[[selected_default]])
  
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
  
  # Use the clusters stored in the reactive object to generate a table.
  # The table will update automatically when either `reactive_obj$clusters`
  # or `input$max_words_cluster` changes.
  output$clusters_full <- DT::renderDataTable({
    use_model <- input$modelSelect_clusters[[1]]
    renderClusterTable(reactive_obj[['clusters']], 
      list_clusters[[use_model]]$clusters, input$max_words_cluster, session)
  })
  
  # When a new model is chosen from the list, update the model name, model 
  # description, and what clusters are shown.
  observeEvent(input$modelSelect_clusters, {
    use_model <- input$modelSelect_clusters[[1]]
    output$model_name_cluster <- renderText(use_model)
    output$model_desc_cluster <- renderModelDesc(use_model)
    # If this model has not been accessed before, get the indexes for 10 clusters chosen 
    # at random. Otherwise, the last selected clusters are re-used.
    if ( is.null(session$userData$clusters[[use_model]]) ) {
      session$userData$clusters[[use_model]] <- getRandomClusters()
    }
    # Update the reactive object to reflect the current clusters. This will trigger an 
    # update of the table too.
    reactive_obj[['clusters']] <- session$userData$clusters[[use_model]]
  })
  
  # Handle resetting clusters from tab content.
  observeEvent(input$clustering_reset_input_fullcluster, {
    use_model <- input$modelSelect_clusters[[1]]
    # Store the model's new selection of clusters for persistence.
    session$userData$clusters[[use_model]] <- getRandomClusters()
    # Update the reactive object to reflect the current clusters.
    reactive_obj[['clusters']] <- session$userData$clusters[[use_model]]
  })
  
  # Handle resetting clusters from the sidebar.
  observeEvent(input$clustering_reset_input_fullcluster1, {
    use_model <- input$modelSelect_clusters[[1]]
    # Store the model's new selection of clusters for persistence.
    session$userData$clusters[[use_model]] <- getRandomClusters()
    # Update the reactive object to reflect the current clusters.
    reactive_obj[['clusters']] <- session$userData$clusters[[use_model]]
  })
  
  # Create a CSV file of all clusters (for this model) when requested.
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste(input$modelSelect_clusters[[1]], ".csv", sep="") 
    },
    content = function(file) {
      use_model <- input$modelSelect_clusters[[1]]
      write.table(list_clusters[[use_model]]$clusters, file, row.names = FALSE, 
        col.names = c(paste0('cluster_',1:150)), sep = ","
      )
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
    data <- use_model %>% closest_to(op_vector, max_terms)
    makeTableForModel(data, session, tableSidebarOpts(input$max_words_operations))
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
    data <- use_model %>% closest_to(op_vector, max_terms)
    makeTableForModel(data, session, tableSidebarOpts(input$max_words_operations))
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
    data <- use_model %>% closest_to(op_vector, max_terms)
    makeTableForModel(data, session, tableSidebarOpts(input$max_words_operations))
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
    makeTableForModel(data, session, tableSidebarOpts(input$max_words_operations))
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
              min.freq = as.integer(input$freq * 100), max.words=input$max,
              random.order = FALSE, rot.per = 0.30, 
              random.color = FALSE, ordered.colors = FALSE,
              # We're modifying ColorBrewer palette RdYlBu here:
              colors = c("#FDAE61", "#313695","#D7191C", "#2C7BB6", "#000000"), 
              scale = c(input$scale, 0.5),
              use.r.layout = TRUE)
  })
  
  # Generate the pairs plot.
  output$pairs_plot <- renderPlot({
    validate(need(tolower(input$pairs_term1) != "" && 
                  tolower(input$pairs_term2) != "", 
      "To generate a PAIRS PLOT, enter a query term in the text field above."))
    use_model <- list_models[[input$modelSelect_Visualisation_tabs[[1]]]]
    # TODO: Only generate the plot if each of the terms is in the model
    data <- use_model[[c(tolower(input$pairs_term1), tolower(input$pairs_term2)), average=FALSE]]
    concept_pairs <- use_model[1:3000,] %>% cosineSimilarity(data)
    concept_pairs <- concept_pairs[
      rank(-concept_pairs[,1]) < input$pairs_max |
        rank(-concept_pairs[,2]) < input$pairs_max,
    ]
    plot(concept_pairs, type='n')
    text(concept_pairs, labels=rownames(concept_pairs))
  })
}


##
##  WVI 4. GENERATE THE SHINY APP
##

# Spin up the Word Vector Interface application.
shinyApp(
  ui = app_ui,
  server = app_server,
  options = list(port = 3939)
)
