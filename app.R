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
library("rjson")
library(wordcloud)
library(ggrepel)
library(tidyverse)
library(wordVectors)

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

ls_download_cluster <- c()

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

tableSimpleOpts <- list(lengthMenu = c(10, 20, 100, 150), pageLength = 10, searching = TRUE)
tableSidebarOpts <- function(page_len) {
  return(list(dom = 't', pageLength = page_len, searching = FALSE))
}

getDataForTable <- function(model, vector, session, opts = list()) {
  return(DT::datatable({
    data <- model %>% closest_to(vector) %>%
      mutate("Link" <- linkToWWO(keyword=.$word, session=session)) %>% 
      .[c(3,2)]
  }, escape = FALSE, colnames=c("Word", "Similarity to word(s)"), 
  options = opts))
}

## WVI 2. USER INTERFACE

# TODO: Merge the Dashboard body with the UI structure, with better signposting 
# of components.

# Create the app page's body.
body <- dashboardBody(

  includeScript(path = "script.js"),

  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),

  # Create the contents of the "Home" tab.
    tabBox(
      # The id lets us use input$tabset1 on the server to find the current tab
      id = "tabset1", width = 12,
      tabPanel("Home", value=1,
               htmlTemplate("html/tab_home.html", 
                            model_name = textOutput("model_name_basic"),
                            model_desc = uiOutput("model_desc_basic"),
                            controls = textInput("basic_word1", "Query term:", width="500px"),
                            results = DT::dataTableOutput("basic_table"))),
      
      # Create the contents of the "Compare" tab.
      tabPanel("Compare", value=2,
               id = "compareTab-Id",

               fluidRow(
                 box( solidHeader = TRUE, textInput("basic_word_c", "Query term:", width = "500px"), width=12)
               ),

               fluidRow(
                 box(

                   box(
                     solidHeader = TRUE,
                     class = "model_header",
                     tags$h1(textOutput("model_name_compare_1")),
                     div(class = "model_desc", p(uiOutput("model_desc_compare_1"))),

                     # div(class = "model_desc", p(textOutput("model_desc_compare_1"),
                     #                               "The text has been regularized",
                     #                               a("[read more]", href=paste("https://wwp.northeastern.edu/lab/wwvt/methodology/index.html", sep=""), target="_blank")
                     #                             )
                         # ),
                     width = 12
                   ),
                   box(
                     DT::dataTableOutput("basic_table_c1"),
                     width = 12
                   )
                 ),
                 box(
                   box(
                     solidHeader = TRUE,
                     class = "model_header",
                     tags$h1(textOutput("model_name_compare_2")),
                     div(class = "model_desc", p(uiOutput("model_desc_compare_2"))),

                     # div(class = "model_desc", p(textOutput("model_desc_compare_2"),
                     #                             "The text has been regularized",
                     #                             a("[read more]", href=paste("https://wwp.northeastern.edu/lab/wwvt/methodology/index.html", sep=""), target="_blank")
                     #                             )
                     #     ),
                     width = 12
                   ),
                   box(
                     DT::dataTableOutput("basic_table_c2"),
                     width = 12
                  )
                 )
               )
          ),
      
      # Create the contents of the "Clusters" tab.
      tabPanel("Clusters", value=3,
               fluidRow(
                 box(

                   div(class="home_desc", 
                       p("Clusters are generated based on neighboring words in vector space—words that are used in similar contexts will be clustered together. Each column represents a different cluster, randomly selected from 150 total clusters; the words in the list are those closest to the center of the cluster."),
                       p("Use the dropdown on the left to select which model you want to view. Click the “Download” button to download the set of clusters you are viewing. You can also hit the “reset clusters” button to see a new set of clusters and use the slider to see more terms from each cluster. (Note that adjusting the number of terms per cluster will also reset the clusters.)"),
                       p("If you click on any individual term, a new page will take you to the Women Writers Online interface (subscription required; see ", 
                         tags$a(href="https://wwp.northeastern.edu/wwo/license/", target="_blank", "this page"), 
                         " for information on subscribing and setting up a free trial) to search for your term in the WWO collection.")),

                   tags$h1(textOutput("model_name_cluster")),
                   div(class = "model_desc", p(uiOutput("model_desc_cluster"))),
                   br(),
                   actionButton("clustering_reset_input_fullcluster1", "Reset clusters", class="clustering-reset-full"),

                   # div(class = "model_desc", p(textOutput("model_desc_cluster"),
                   #                             "The text has been regularized",
                   #                             a("[read more]", href=paste("https://wwp.northeastern.edu/lab/wwvt/methodology/index.html", sep=""), target="_blank")
                   #                             )
                   #     ),
                   width=12
                 ),
                 box(
                  # solidHeader = TRUE,
                  DTOutput('clusters_full'), width = 12)
               )
      ),
      
      # Create the contents of the "Operations" tab.
      tabPanel("Operations", value=4,
               fluidRow(
                 box(
                   div(class="home_desc",
                     p("Using the sidebar on the left, you can select from several different operations and choose which model you would like to query.")
                  ),
                  tags$h1(textOutput("model_name_operation")),
                   div(class = "model_desc", p(uiOutput("model_desc_operation"))),
                   width=12
                 )
              ),
              
              # Create the contents of the "Addition" operation tab.
              fluidRow(
                box( width = 7,
                 conditionalPanel(condition="input.operator_selector=='Addition'",
                                  #class = "compare_width",
                                    box(
                                      solidHeader = TRUE,
                                      shinyjs::useShinyjs(),
                                      id = "addition_panel",
                                      column(10,
                                             class = "col-md-5",
                                             textInput("addition_word1", "Word 1")),
                                      column(
                                        2,
                                        br(), tags$label(class = "control-label", icon("plus"))
                                      ),
                                      column(10,
                                             class = "col-md-5",
                                             textInput("addition_word2", "Word 2")
                                      ),
                                      width = 12
                                    ),
                                    box(
                                      DT::dataTableOutput("addition_table"),
                                      width = 12
                                    ),
                    width = 12
                 ),
                 
                 # Create the contents of the "Subtraction" operation tab.
                 conditionalPanel(condition="input.operator_selector=='Subtraction'",
                                  #class = "compare_width",
                                  box(
                                      solidHeader = TRUE,
                                      shinyjs::useShinyjs(),
                                      id = "subtraction_panel",
                                      column(10,
                                             class = "col-md-5",
                                             # Sidebar with a inputs
                                             textInput("subtraction_word1", "Word 1")),
                                      column(
                                        2,
                                        br(), tags$label(class = "control-label", icon("minus"))
                                      ),
                                      column(10,
                                             class = "col-md-5",
                                             textInput("subtraction_word2", "Word 2")
                                      ),
                                      width = 12
                                  ),
                                  box(
                                    DT::dataTableOutput("subtraction_table"),
                                    width = 12
                                  ),
                    width = 12
                 ),
                 
                 # Create the contents of the "Advanced" operation tab.
                 conditionalPanel(condition="input.operator_selector=='Advanced'",
                                  #class = "compare_width",
                                  box(
                                       solidHeader = TRUE,
                                       shinyjs::useShinyjs(),
                                       id = "advanced_panel",
                                       column(10,
                                              # Sidebar with a inputs
                                              class = "col-md-8",
                                              textInput("advanced_word1", "Word 1")),
                                       column(2,
                                              class = "col-md-4 mathCol",
                                              selectInput("advanced_math", "Math",
                                                          choices = list("+" = "+", "-" = "-", "*" = "*", "/" = "/"),
                                                          selected = 1)),
                                       column(10,
                                              class = "col-md-8",
                                              textInput("advanced_word2", "Word 2")),
                                       column(2,
                                              class = "col-md-4 mathCol",
                                              selectInput("advanced_math2", "Math",
                                                          choices = list("+" = "+", "-" = "-", "*" = "*", "/" = "/"),
                                                          selected = 1)),
                                       column(10,
                                              class = "col-md-8",
                                              textInput("advanced_word3", "Word 3")
                                       ),
                                       width = 12
                                    ),
                                    box(
                                      DT::dataTableOutput("advanced_table"),
                                      width = 12
                                    ),
                      width = 12
                 ),
                 
                 # Create the contents of the "Analogies" operation tab.
                 conditionalPanel(condition="input.operator_selector=='Analogies'",
                                  #class = "compare_width",
                                  box(
                                    solidHeader = TRUE,
                                    shinyjs::useShinyjs(),
                                    id = "analogies_panel",
                                    column(10,
                                           class = "col-md-5",
                                           # Sidebar with a inputs
                                           textInput("analogies_word1", "Word 1")),
                                    column(
                                      2, 
                                      br(), tags$label(class = "control-label", icon("minus"))
                                    ),
                                    column(10,
                                           class = "col-md-5",
                                           textInput("analogies_word2", "Word 2")),
                                    column(
                                      2,
                                      br(), tags$label(class = "control-label", icon("plus"))
                                    ),
                                    column(10,
                                           class = "col-md-5",
                                           textInput("analogies_word3", "Word 3")
                                    ),
                                    width = 12
                                  ),
                                  box(
                                    DT::dataTableOutput("analogies_table"),
                                    width = 12
                                  )
                      )
                 ),
                
                 box(
                   p("Addition allows you to add the contexts associated with two terms to each other, while subtraction allows you to subtract the contexts associated with one word from another. To see how these work, try “orange” + “red” and “orange” - “red” and compare the results."),
                   p("The analogies operation allows you to subtract the contexts associated with one term from another, and then add the contexts associated with a third term. For example, you might subtract “man” from “woman” to get a vector associated with the contexts of “woman” as distinct from “man”; then, adding the vector for “king” will bring in its contexts to give you words associated with the distinction between woman and man AND with royalty; in many models, this will be “queen.” Or, put more simply: woman - man + king = queen; woman is to man as queen is to king."),
                   p("The advanced option allows you to create a query of your own using multiple operations."),
                   p("If you click on any individual term, a new page will take you to the Women Writers Online interface (subscription required; see ", 
                     tags$a(href="https://wwp.northeastern.edu/wwo/license/", target="_blank", "this page"), 
                     " for information on subscribing and setting up a free trial) to search for your term in the WWO collection."),
                   width = 5
                 )

               )
      ),
      
      # Create the contents of the "Visualization" tab.
      tabPanel("Visualization", value=5,
               fluidRow(

                 box(
                   tags$h1(textOutput("model_name_visualisation")),
                   div(class = "model_desc", p(uiOutput("model_desc_visualisation"))),
                   width=12
                 ),

                 conditionalPanel(condition="input.visualisation_selector=='wc'",
                    class = "visualization",
                    shinyjs::useShinyjs(),
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
                        #solidHeader = TRUE,
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
                      shinyjs::useShinyjs(),
                      box(
                         plotOutput("scatter_plot", height = 600),
                         width = 8
                      )
                 ),
                 conditionalPanel(condition="input.visualisation_selector=='scatter_closest'",
                      class = "visualization",
                      shinyjs::useShinyjs(),
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
    )
)

sidebar = dashboardSidebar(
  
  # Create sidebar content for "Home" tab.
  conditionalPanel(condition="input.tabset1==1",
                   selectInput("modelSelect", "Model",
                               choices = available_models,
                               selected = selected_default),
                   br(),
                   sliderInput("max_words_home",
                               "Number of Words:",
                               min = 1,  max = 150,  value = 10),
                   br(),
                   actionButton("clustering_reset_input", "Reset clusters")
                   
  ),
  
  # Create sidebar content for "Compare" tab.
  conditionalPanel(condition="input.tabset1==2",
                   selectInput("modelSelectc1", "Model 1",
                               choices = available_models,
                               selected = selected_compare_1),
                   selectInput("modelSelectc2", "Model 2",
                               choices = available_models,
                               selected = selected_compare_2),
                   sliderInput("max_words",
                               "Number of Words:",
                               min = 1,  max = 150,  value = 10)
                   
  ),
  
  # Create sidebar content for "Clusters" tab.
  conditionalPanel(condition="input.tabset1==3",
                   selectInput("modelSelect_clusters", "Model",
                               choices = available_models,
                               selected = selected_default),
                   br(),
                   column(
                     id = "Download_reset_button",
                     width = 12,
                     actionButton("clustering_reset_input_fullcluster", "Reset clusters", class="clustering-reset-full"),
                     downloadButton("downloadData", "Download")
                   ),
                   br(),
                   br(),
                   sliderInput("max_words_cluster",
                               "Number of Words:",
                               min = 1,  max = 150,  value = 10)
                   
  ),
  
  # Create sidebar content for "Operations" tab.
  conditionalPanel(condition="input.tabset1==4",
                   selectInput("modelSelect_analogies_tabs", "Model",
                               choices = available_models,
                               selected = selected_default),
                   
                   selectInput("operator_selector", "Select operator",
                               choices = c("Addition", "Subtraction", "Analogies", "Advanced"),
                               selected = 1)
  ),
  
  # Create sidebar content for "Visualization" tab.
  conditionalPanel(condition="input.tabset1==5",
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
  )

#app_ui_new = htmlTemplate("wvi.html", sidebar = sidebar, body = body)

app_ui = dashboardPage(
  title = "Word Vector Interface | Women Writers Vector Toolkit",
  header = tags$header(
    class = "main-header",
    tags$link(rel = "stylesheet", type = "text/css", 
              href = "https://stackpath.bootstrapcdn.com/bootstrap/4.1.2/css/bootstrap.min.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/main.css"),
    htmlTemplate("html/navbar.html", name = "header-component")
  ),
  # Create the sidebar and all content, which is shown or hidden depending on 
  # the current open tab.
  sidebar = sidebar,
  body = body
)

##  WVI 3. SERVER LOGIC

# Listen for user interactions and handle them.
app_server = function(input, output, session) {
  # The currently selected tab from the first box
  output$tabset1Selected <- renderText({
    input$tabset1
  })
  
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    plot(mtcars$wt, mtcars$mpg)
  })
  
  outputOptions(output, "plot1", suspendWhenHidden = TRUE)
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$modelSelect_clusters[[1]], ".csv", sep = "")
    },
    
    content = function(file) {
      data <- sapply(ls_download_cluster,function(n) {
        paste0(names(list_clustering[[input$modelSelect_clusters[[1]]]]$cluster[list_clustering[[input$modelSelect_clusters[[1]]]]$cluster==n][1:150]))
      }) %>% as_tibble(.name_repair = "minimal")
      
      write.csv(data, file, row.names = FALSE)
    })
  
  observeEvent(input$modelSelect, {
    output$model_name_basic <- renderText(input$modelSelect[[1]])
    # output$model_desc_basic <- renderText({list_desc[[input$modelSelect[[1]]]]})
    
    url <- a("[read more]", href="https://wwp.northeastern.edu/lab/wwvt/methodology/")
    output$model_desc_basic <- renderUI({
      tagList(paste(list_desc[[input$modelSelect[[1]]]], "The text has been regularized."), url)
    })
    
  })
  
  observeEvent(input$modelSelectc1, {
    output$model_name_compare_1 <- renderText(input$modelSelectc1[[1]])
    # output$model_desc_compare_1 <- renderText({list_desc[[input$modelSelectc1[[1]]]]})
    
    url <- a("[read more]", href="https://wwp.northeastern.edu/lab/wwvt/methodology/")
    output$model_desc_compare_1 <- renderUI({
      tagList(paste(list_desc[[input$modelSelectc1[[1]]]], "The text has been regularized."), url)
    })
    
  })
  
  observeEvent(input$modelSelectc2, {
    output$model_name_compare_2 <- renderText(input$modelSelectc2[[1]])
    # output$model_desc_compare_2 <- renderText({list_desc[[input$modelSelectc2[[1]]]]})
    
    url <- a("[read more]", href="https://wwp.northeastern.edu/lab/wwvt/methodology/")
    output$model_desc_compare_2 <- renderUI({
      tagList(paste(list_desc[[input$modelSelectc2[[1]]]], "The text has been regularized."), url)
    })
    
  })
  
  
  observeEvent(input$modelSelect_clusters, {
    output$model_name_cluster <- renderText(input$modelSelect_clusters[[1]])
    # output$model_desc_cluster <- renderText({list_desc[[input$modelSelect_clusters[[1]]]]})
    
    url <- a("[read more]", href="https://wwp.northeastern.edu/lab/wwvt/methodology/")
    output$model_desc_cluster <- renderUI({
      tagList(paste(list_desc[[input$modelSelect_clusters[[1]]]], "The text has been regularized."), url)
    })
    
  })
  
  
  observeEvent(input$modelSelect_analogies_tabs, {
    output$model_name_operation <- renderText(input$modelSelect_analogies_tabs[[1]])
    # output$model_desc_operation <- renderText({list_desc[[input$modelSelect_analogies_tabs[[1]]]]})
    
    url <- a("[read more]", href="https://wwp.northeastern.edu/lab/wwvt/methodology/")
    output$model_desc_operation <- renderUI({
      tagList(paste(list_desc[[input$modelSelect_analogies_tabs[[1]]]], "The text has been regularized."), url)
    })
    
  })
  
  
  observeEvent(input$modelSelect_Visualisation_tabs, {
    output$model_name_visualisation <- renderText(input$modelSelect_Visualisation_tabs[[1]])
    # output$model_desc_visualisation <- renderText({paste(list_desc[[input$modelSelect_Visualisation_tabs[[1]]]], "The text has been regularized")})
    
    url <- a("[read more]", href="https://wwp.northeastern.edu/lab/wwvt/methodology/")
    output$model_desc_visualisation <- renderUI({
      tagList(paste(list_desc[[input$modelSelect_Visualisation_tabs[[1]]]], "The text has been regularized."), url)
    })
    
  })
  
  
  output$word_cloud <- renderPlot({
    validate(
      need(tolower(input$word_cloud_word) != "", 
           "To generate a word cloud, enter a query term in the text field above."))
    data <- list_models[[input$modelSelect_Visualisation_tabs[[1]]]] %>% closest_to(tolower(input$word_cloud_word), 150)
    colnames(data) <- c("words", "sims")
    data <- mutate(data, sims = as.integer(sims * 100))
    
    set.seed(1234)
    wordcloud(words = data$words, freq = data$sims,
              min.freq = input$freq, max.words=input$max,
              random.order=FALSE, random.color = FALSE, rot.per = 0.30, ordered.colors = FALSE,
              colors = brewer.pal(8,"Dark2"), scale= c(input$scale,0.5),
              use.r.layout = TRUE)
  })
  
  # rv <- reactiveValues()
  # rv$setupComplete <- FALSE
  
  
  dataset <- reactive({
    
    times <- input$clustering_reset_input_visualisation
    
    df2 <- sapply(sample(1:150,10),function(n) {
      paste0(names(list_clustering[[input$modelSelect_Visualisation_tabs[[1]]]]$cluster[list_clustering[[input$modelSelect_Visualisation_tabs[[1]]]]$cluster==n][1:150]))
    }) %>% as_tibble(.name_repair = "minimal")
    
    df2
    # rv$setupComplete <- TRUE
    
  })
  
  datascatter <- reactive({
    
    df2 <- dataset()
    # print(df2)
    
    x <- c()
    y <- c()
    names <- c()
    cluster <- c()
    
    vector <- vectors[[input$modelSelect_Visualisation_tabs[[1]]]]
    for (column in colnames(df2))
    {
      for (word in head(df2,input$scatter_number)[column][[1]]){
        x <- append(x, vector[word, 'PC1'])
        y <- append(y, vector[word, 'PC2'])
        names <- append(names,word)
        cluster <- append(cluster,column)
      }
    }
    
    df_new <- data.frame(x = x, y = y, names = names, cluster = as.factor(cluster), stringsAsFactors = FALSE)
    df_new
    
  })
  
  # output$setupComplete <- reactive({
  #   return(rv$setupComplete)
  # })
  
  # outputOptions(output, 'setupComplete', suspendWhenHidden=FALSE)
  
  
  output$scatter_plot <- renderPlot({
    ggplot(datascatter(), aes(x=x, y=y, colour=cluster), height="600px", width="100%") +
      geom_point() +
      geom_text_repel(
        aes(label = ifelse(cluster == input$scatter_cluster, as.character(names),'')), 
        hjust=0.5, vjust=-0.5, max.overlaps = 12)
  })
  
  outputOptions(output, "scatter_plot", suspendWhenHidden = TRUE)
  
  
  dataset_closet <- reactive({
    
    data <- as.matrix(list_models[['WWO Full Corpus']])
    vectors <-stats::predict(stats::prcomp(data))[,1:2]
    
    x <- c()
    y <- c()
    names <- c()
    cluster <-c()
    
    closeword <- list_models[['WWO Full Corpus']] %>% closest_to(tolower(input$scatter_plot_term), 150)
    
    i = 0
    for(word in closeword[[1]])
    {
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
      names <- append(names,word)
    }
    df_new <- data.frame(x = x, y = y, names = names, cluster = as.factor(cluster), stringsAsFactors = FALSE)
    df_new
    
  })
  
  output$scatter_plot_closest <- renderPlot({
    ggplot(dataset_closet(), aes(x=x, y=y, colour=cluster)) +
      geom_point() +
      geom_text_repel(aes(label=ifelse(cluster == tolower(input$scatter_plot_closest_choice) ,as.character(names),'')), hjust=0.5,vjust=-0.5)
  })
  
  outputOptions(output, "scatter_plot_closest", suspendWhenHidden = TRUE)
  
  # Generate table for Addition operation
  output$addition_table <- DT::renderDataTable({
    validate(need(input$addition_word1 != "" && input$addition_word2 != "", 
                  "Enter query term into word 1 and word 2."))
    use_model <- list_models[[input$modelSelect_analogies_tabs[[1]]]]
    getDataForTable(use_model, 
                    as.VectorSpaceModel(use_model[[tolower(input$addition_word1)]] +
                                          use_model[[tolower(input$addition_word2)]]), 
                    session,
                    tableSimpleOpts)
  })
  
  # Generate table for Subtraction operation
  output$subtraction_table <- DT::renderDataTable({
    validate(need(input$subtraction_word1 != "" && input$subtraction_word2 != "", 
                  "Enter query term into word 1 and word 2."))
    use_model <- list_models[[input$modelSelect_analogies_tabs[[1]]]]
    getDataForTable(use_model,
                    use_model[[tolower(input$subtraction_word1)]] -
                      use_model[[tolower(input$subtraction_word2)]], 
                    session,
                    tableSimpleOpts)
  })
  
  # Generate table for Analogies operation
  output$analogies_table <- DT::renderDataTable({
    validate(need(input$analogies_word1 != "" && input$analogies_word2 != "" && input$analogies_word3 != "", "Enter query term into Word 1, Word 2, and Word 3."))
    use_model <- list_models[[input$modelSelect_analogies_tabs[[1]]]]
    getDataForTable(use_model, as.VectorSpaceModel(
      use_model[[tolower(input$analogies_word1)]] -
        use_model[[tolower(input$analogies_word2)]] + 
        use_model[[tolower(input$analogies_word3)]]),
      session,
      tableSimpleOpts)
  })
  
  # Generate table for Advanced math operation
  output$advanced_table <- DT::renderDataTable(DT::datatable({
    validate(need(input$advanced_word1 != "", "Enter query term into Word 1."))
    use_model <- list_models[[input$modelSelect_analogies_tabs[[1]]]]
    vector1 <- use_model[[tolower(input$advanced_word1)]]
    # If there's only 1 word, no math needs to be done.
    if (input$advanced_word2 == "" && input$advanced_word3 == "") {
      data <- use_model %>% closest_to(vector1) %>%
        mutate("Link" <- linkToWWO(keyword=.$word, session=session)) %>% .[c(3,2)]
      # If the 1st and 2nd words were provided...
    } else if (input$advanced_word2 != "" && input$advanced_word3 == "") {
      vector2 <- use_model[[tolower(input$advanced_word2)]]
      if (input$advanced_math == "+") {
        # We have to coerce the result of vector addition into VectorSpaceModel format
        data <- use_model %>% closest_to(as.VectorSpaceModel(vector1 + vector2), 150)
      } else if (input$advanced_math == "-") {
        data <- use_model %>% closest_to(vector1 - vector2, 150)
      } else if (input$advanced_math == "*") {
        # We have to coerce the result of vector multiplication into VectorSpaceModel format
        data <- use_model %>% closest_to(as.VectorSpaceModel(vector1 * vector2), 150)
      } else if (input$advanced_math == "/") {
        # We have to coerce the result of vector division into VectorSpaceModel format
        data <- use_model %>% closest_to(as.VectorSpaceModel(vector1 / vector2), 150)
      }
      data <- data %>%
        mutate("Link" <- linkToWWO(keyword=.$word, session=session)) %>% .[c(3,2)]
      # If all 3 words have been provided...
    } else if (input$advanced_word2 != "" && input$advanced_word3 != "") {
      vector2 <- use_model[[tolower(input$advanced_word2)]]
      vector3 <- use_model[[tolower(input$advanced_word3)]]
      # When the first operator is +
      if (input$advanced_math == "+" && input$advanced_math2 == "+") {
        data <- use_model %>% 
          closest_to(as.VectorSpaceModel(vector1 + vector2 + vector3), 150)
      }
      if (input$advanced_math == "+" && input$advanced_math2 == "-") {
        data <- use_model %>% 
          closest_to(as.VectorSpaceModel(vector1 + vector2 - vector3), 150)
      }
      if (input$advanced_math == "+" && input$advanced_math2 == "*") {
        data <- use_model %>% 
          closest_to(as.VectorSpaceModel(vector1 + vector2 * vector3), 150)
      }
      if (input$advanced_math == "+" && input$advanced_math2 == "/") {
        data <- use_model %>% 
          closest_to(as.VectorSpaceModel(vector1 + vector2 / vector3), 150)
      }
      # When the first operator is -
      if (input$advanced_math == "-" && input$advanced_math2 == "+") {
        data <- use_model %>% 
          closest_to(as.VectorSpaceModel(vector1 - vector2 + vector3), 150)
      }
      if (input$advanced_math == "-" && input$advanced_math2 == "-") {
        data <- use_model %>% 
          closest_to(as.VectorSpaceModel(vector1 - vector2 - vector3), 150)
      }
      if (input$advanced_math == "-" && input$advanced_math2 == "*") {
        data <- use_model %>% 
          closest_to(as.VectorSpaceModel(vector1 - vector2 * vector3), 150)
      }
      if (input$advanced_math == "-" && input$advanced_math2 == "/") {
        data <- use_model %>% 
          closest_to(as.VectorSpaceModel(vector1 - vector2 / vector3), 150)
      }
      # When the first operator is *
      if (input$advanced_math == "*" && input$advanced_math2 == "+") {
        data <- use_model %>% 
          closest_to(as.VectorSpaceModel(vector1 * vector2 + vector3), 150)
      }
      if (input$advanced_math == "*" && input$advanced_math2 == "-") {
        data <- use_model %>% 
          closest_to(as.VectorSpaceModel(vector1 * vector2 - vector3), 150)
      }
      if (input$advanced_math == "*" && input$advanced_math2 == "*") {
        data <- use_model %>% 
          closest_to(as.VectorSpaceModel(vector1 * vector2 * vector3), 150)
      }
      if (input$advanced_math == "*" && input$advanced_math2 == "/") {
        data <- use_model %>% 
          closest_to(as.VectorSpaceModel(vector1 * vector2 / vector3), 150)
      }
      # When the first operator is /
      if (input$advanced_math == "/" && input$advanced_math2 == "+") {
        data <- use_model %>% 
          closest_to(as.VectorSpaceModel(vector1 / vector2 + vector3), 150)
      }
      if (input$advanced_math == "/" && input$advanced_math2 == "-") {
        data <- use_model %>% 
          closest_to(as.VectorSpaceModel(vector1 / vector2 - vector3), 150)
      }
      if (input$advanced_math == "/" && input$advanced_math2 == "*") {
        data <- use_model %>% 
          closest_to(as.VectorSpaceModel(vector1 / vector2 * vector3), 150)
      }
      if (input$advanced_math == "/" && input$advanced_math2 == "/") {
        data <- use_model %>% 
          closest_to(as.VectorSpaceModel(vector1 / vector2 / vector3), 150)
      }
      data <- data %>%
        mutate("Link" <- linkToWWO(keyword=.$word, session=session)) %>% .[c(3,2)]
    }
    data
  }, escape = FALSE, colnames=c("Word", "Similarity to word(s)"), options = tableSimpleOpts))
  
  # Generate table for the Home tab
  output$basic_table <- DT::renderDataTable(DT::datatable({
    data <- list_models[[input$modelSelect[[1]]]] %>% 
      closest_to(tolower(input$basic_word1), 150) %>% 
      mutate("Link" <- linkToWWO(keyword=.$word, session=session)) %>% 
      .[c(3,2)]
  }, escape = FALSE, colnames=c("Word", "Similarity to word(s)"), 
  options = tableSidebarOpts(input$max_words_home)))
  
  # Generate 1st table for Compare tab
  output$basic_table_c1 <- DT::renderDataTable(DT::datatable({
    data <- list_models[[input$modelSelectc1[[1]]]] %>% 
      closest_to(tolower(input$basic_word_c), 150) %>% 
      mutate("Link" <- linkToWWO(keyword=.$word, session=session)) %>% 
      .[c(3,2)]
  }, escape = FALSE, colnames=c("Word", "Similarity to word(s)"), 
  options = tableSidebarOpts(input$max_words)))
  
  # Generate 2nd table for Compare tab
  output$basic_table_c2 <- DT::renderDataTable(DT::datatable({
    data <- list_models[[input$modelSelectc2[[1]]]] %>% 
      closest_to(tolower(input$basic_word_c), 150) %>% 
      mutate("Link" <- linkToWWO(keyword=.$word, session=session)) %>% 
      .[c(3,2)]
  }, escape = FALSE, colnames=c("Word", "Similarity to word(s)"), 
  options = tableSidebarOpts(input$max_words)))
  
  
  output$tbl <- DT::renderDataTable(DT::datatable({
    data <- sapply(sample(1:150,4),function(n) {
      cword <- names(list_clustering[[input$modelSelect[[1]]]]$cluster[list_clustering[[input$modelSelect[[1]]]]$cluster==n][1:150])
      linkToWWO(keyword = cword, session = session)
    }) %>% as_tibble(.name_repair = "minimal")
  }, escape = FALSE, colnames=c(paste0("cluster_",1:4)), options = list(dom = 't', pageLength = input$max_words_home, searching = FALSE)))
  
  # Handle resetting clusters in the Home tab.
  # TODO: reduce duplication, create function to generate and render table of clusters
  observeEvent(input$clustering_reset_input, {
    output$tbl <- DT::renderDataTable(DT::datatable({
      data <- sapply(sample(1:150,4),function(n) {
        cword <- names(list_clustering[[input$modelSelect[[1]]]]$cluster[list_clustering[[input$modelSelect[[1]]]]$cluster==n][1:150])
        linkToWWO(keyword = cword, session = session)
      }) %>% as_tibble(.name_repair = "minimal")
    }, escape = FALSE, colnames=c(paste0("cluster_",1:4)),options = list(dom = 't', pageLength = input$max_words_home, searching = FALSE)))
  })
  
  # Generate and render clusters.
  output$clusters_full <- DT::renderDataTable(DT::datatable({
    data <- sapply(sample(1:150,10),function(n) {
      ls_download_cluster <<- c(ls_download_cluster,n)
      cword <- names(list_clustering[[input$modelSelect_clusters[[1]]]]$cluster[list_clustering[[input$modelSelect_clusters[[1]]]]$cluster==n][1:150])
      linkToWWO(keyword = cword, session = session)
    }) %>% as_tibble(.name_repair = "minimal")
    
  }, escape = FALSE, colnames=c(paste0("cluster_",1:10)), options = list(dom = 'ft', lengthMenu = c(10, 20, 100, 150), pageLength = input$max_words_cluster, searching = TRUE)))
  
  
  # Handle resetting clusters from tab content.
  observeEvent(input$clustering_reset_input_fullcluster, {
    ls_download_cluster <<- c()
    output$clusters_full <- DT::renderDataTable(DT::datatable({
      data <- sapply(sample(1:150,10),function(n) {
        ls_download_cluster <<- c(ls_download_cluster,n)
        cword <- names(list_clustering[[input$modelSelect_clusters[[1]]]]$cluster[list_clustering[[input$modelSelect_clusters[[1]]]]$cluster==n][1:150])
        linkToWWO(keyword = cword, session = session)
      }) %>% as_tibble(.name_repair = "minimal")
    }, escape = FALSE, colnames=c(paste0("cluster_",1:10)), options = list(dom = 'ft', lengthMenu = c(10, 20, 100, 150), pageLength = input$max_words_cluster, searching = TRUE)))
  })
  
  # Handle resetting clusters from sidebar.
  # TODO: reduce duplication
  observeEvent(input$clustering_reset_input_fullcluster1, {
    ls_download_cluster <<- c()
    output$clusters_full <- DT::renderDataTable(DT::datatable({
      data <- sapply(sample(1:150,10),function(n) {
        ls_download_cluster <<- c(ls_download_cluster,n)
        cword <- names(list_clustering[[input$modelSelect_clusters[[1]]]]$cluster[list_clustering[[input$modelSelect_clusters[[1]]]]$cluster==n][1:150])
        linkToWWO(keyword = cword, session = session)
      }) %>% as_tibble(.name_repair = "minimal")
    }, escape = FALSE, colnames=c(paste0("cluster_",1:10)), options = list(dom = 'ft', lengthMenu = c(10, 20, 100, 150), pageLength = input$max_words_cluster, searching = TRUE)))
  })
  
}

##  WVI 4. GENERATE THE SHINY APP

# Create an HTML wrapper around the Shiny app content.
shinyApp(
  ui = app_ui,
  server = app_server,
  options = list(port = 3939)
)
