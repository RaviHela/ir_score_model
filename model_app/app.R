#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Load Libraries
library(shiny)
library(shinythemes)
library(tidyverse)
library(readxl)
library(tidyquant)
library(rhandsontable)
library(plotly)
library(viridis)
library(RColorBrewer)

#ir_data <- read_excel("/cloud/project/ir_data.xlsx", sheet = "ir_data")
ir_data <-
    read_excel("C:/Users/Rabi/Documents/Data_Science/ir_score_model/ir_data.xlsx",
               sheet = "ir_data")

numerical_features <-
    read_excel("C:/Users/Rabi/Documents/Data_Science/ir_score_model/ir_data.xlsx",
               sheet = "numerical_features")

cat_features <-
    read_excel("C:/Users/Rabi/Documents/Data_Science/ir_score_model/ir_data.xlsx",
               sheet = "cat_features")


#socre numerical Features
numerical_feature_scoring <- ir_data %>%
    
    #processing numerical features
    select(ir_id, unique(numerical_features$feature)) %>%
    pivot_longer(-ir_id, names_to = "feature", values_to = "value") %>%
    
    #tag ratings
    left_join(numerical_features) %>%  rowwise() %>%
    
    #estimate and pull relevant ratings
    mutate(check_limit =  (value > l_limit & value <= u_limit)) %>%
    filter(check_limit == TRUE) %>%
    
    #reshape to longitudanal data
    select(-l_limit,-u_limit,-value,-check_limit) %>%
    pivot_wider(names_from = feature, values_from = score) %>%
    
    #sum up scores
    mutate(
        total_numerical_score = ttl_cnt_pli + ttl_cnt_acc,
        num_numerical_features = length(unique(cat_features$feature))
    )


#socre categorical Features
categorical_feature_scoring <- ir_data %>%
    
    #processing numerical features
    select(ir_id, unique(cat_features$feature)) %>%
    pivot_longer(-ir_id, names_to = "feature", values_to = "value") %>%
    
    #tag ratings
    left_join(cat_features) %>%
    
    #estimate and pull relevant ratings
    # mutate(check_limit =  (value > l_limit & value <= u_limit)) %>%
    # filter(check_limit == TRUE) %>%
    
    #reshape to longitudanal data
    select(-value) %>%
    pivot_wider(names_from = feature, values_from = score) %>%
    
    #sum up scores
    mutate(total_cat_score = region + Industry + flag,
           num_cat_features = length(unique(cat_features$feature)))


#combine numerical and categorical
x <- numerical_feature_scoring %>%
    inner_join(categorical_feature_scoring) %>%
    mutate(total_score = total_numerical_score + total_cat_score,
           score_on_5 = round(total_score / (
               num_cat_features + num_numerical_features
           ))) %>%
    mutate(ir_id = as.factor(ir_id)) %>%
    mutate(ir_id = fct_reorder(ir_id, score_on_5)) %>%
    select(-total_cat_score, -total_numerical_score,-num_numerical_features,-num_cat_features, -total_score) %>%
    pivot_longer(-ir_id, names_to = "attributes", values_to = "value") %>%
    mutate(attributes = factor(attributes, levels=c('score_on_5','ttl_cnt_acc','ttl_cnt_pli','Industry','region', 'flag'))) %>%
    
    ggplot(aes(x = ir_id, y = value, fill = (attributes == "score_on_5"))) +
    geom_col() + 
    facet_grid(. ~ attributes, scales = "free_y") +  coord_flip() +
    scale_fill_brewer(palette = "Dark2") + guides(fill = FALSE) +  theme_light()



# ---- 1.0 UI ----
ui <- navbarPage(
    title = "Scoring Model",
    collapsible = TRUE,
    inverse     = TRUE,
    theme       = shinytheme("paper"),
    
    shiny::tabPanel(title = "Score Distribution",
                    fluidPage(fluidRow(
                        column(4,
                               tabsetPanel(
                                   tabPanel(
                                       "Numeric",
                                       rHandsontableOutput('metric_tbl_num'),
                                       
                                       #shiny::actionButton(inputId = "submit_num", "Update Rules", class = "btn-primary")
                                   ),
                                   tabPanel(
                                       "Categories",
                                       
                                       rHandsontableOutput('metric_tbl_cat'),
                                       #shiny::actionButton(inputId = "submit_cat", "Update Rules", class = "btn-primary")
                                       
                                   ),
                                   tabPanel(
                                       "Filters",
                                       shiny::textInput(
                                           inputId = "select_ir",
                                           label = "IR ID",
                                           value = "ALL"
                                       ),
                                       shiny::textInput(
                                           inputId = "select_team",
                                           label = "Team",
                                           value = "ALL"
                                       ),
                                       shiny::textInput(
                                           inputId = "select_industry",
                                           label = "Industry",
                                           value = "ALL"
                                       ),
                                       shiny::textInput(
                                           inputId = "select_sales",
                                           label = "Sales Size",
                                           value = "ALL"
                                       ),
                                       br(),
                                       br(),
                                       shiny::sliderInput("DatesMerge",
                                                   "Dates:",
                                                   min = as.Date("2016-01-01","%Y-%m-%d"),
                                                   max = as.Date("2016-12-01","%Y-%m-%d"),
                                                   value=c(as.Date("2016-01-01"),as.Date("2016-12-01")),
                                                   timeFormat="%b-%C"
                                                   ),
                                       
                                       
                                       
                                   )
                                   
                               ),
                               br(),
                               br(),
                               fluidRow(shiny::actionButton(inputId = "submit_num", "Update Rules", class = "btn-primary"))),
                        
                        # Show a plot of the generated distribution
                        column(8, fluidRow(wellPanel(
                            div(style = 'height:400px; overflow: scroll', plotlyOutput(outputId = "main_plot"))
                        )),
                        fluidRow(
                            div(DT::dataTableOutput(outputId = "irTbl"), style = "font-size:100%")
                        ))
                    ))),
    shiny::tabPanel(title = "Other")
)




# ---- 2.0 SERVER ----
server <- function(session, input, output) {
    # 2.1 Setup Reactive Values ----
    rv <- reactiveValues()
    num_rv <- reactiveValues(data = numerical_features)
    cat_rv <- reactiveValues(data = cat_features)
    
    
    observeEvent(input$metric_tbl_num$changes$changes, # observe if any changes to the cells of the rhandontable
                 {
                     xi = input$metric_tbl_num$changes$changes[[1]][[1]] # capture the row which is changed
                     num_rv$data <-
                         hot_to_r(input$metric_tbl_num) # convert the rhandontable to R data frame object so manupilation / calculations could be done
                     
                     # Calculating the cell value of column C using cell values in column a and b
                     # 1 is added to row index because change event row and column indices starts with zero vs R index which starts with 1
                     
                 })
    
    observeEvent(input$metric_tbl_cat$changes$changes, # observe if any changes to the cells of the rhandontable
                 {
                     xi = input$metric_tbl_cat$changes$changes[[1]][[1]] # capture the row which is changed
                     cat_rv$data <-
                         hot_to_r(input$metric_tbl_cat) # convert the rhandontable to R data frame object so manupilation / calculations could be done
                     
                     # Calculating the cell value of column C using cell values in column a and b
                     # 1 is added to row index because change event row and column indices starts with zero vs R index which starts with 1
                     
                 })
    
    
    
    observeEvent(input$submit_num, {
        
        # Process data
        print(num_rv$data)
        print(cat_rv$data)
        
        if (input$select_industry == "ALL") {
            industry_filter <- unique(ir_data$Industry)
        } else{
            industry_filter <- strsplit(input$select_industry, split = ",") %>% unlist
        }
        
        
        #print(industry_filter)
        
        
        #socre numerical Features
        rv$numerical_feature_scoring <- ir_data %>%
            filter(Industry %in% industry_filter) %>%
            
            #processing numerical features
            select(ir_id, unique(num_rv$data$feature)) %>%
            pivot_longer(-ir_id, names_to = "feature", values_to = "value") %>%
            
            #tag ratings
            left_join(num_rv$data) %>%  rowwise() %>%
            
            #estimate and pull relevant ratings
            mutate(check_limit =  (value > l_limit &
                                       value <= u_limit)) %>%
            filter(check_limit == TRUE) %>%
            
            #reshape to longitudanal data
            select(-l_limit,-u_limit,-value,-check_limit) %>%
            pivot_wider(names_from = feature, values_from = score) %>%
            
            #sum up scores
            mutate(
                total_numerical_score = ttl_cnt_pli + ttl_cnt_acc,
                num_numerical_features = length(unique(num_rv$data$feature))
            )
        
        #socre categorical Features
        rv$categorical_feature_scoring <- ir_data %>%
            filter(Industry %in% industry_filter) %>%
            
            #processing numerical features
            select(ir_id, unique(cat_rv$data$feature)) %>%
            pivot_longer(-ir_id, names_to = "feature", values_to = "value") %>%
            
            #tag ratings
            left_join(cat_rv$data) %>%
            
            
            
            #reshape to longitudanal data
            select(-value) %>%
            pivot_wider(names_from = feature, values_from = score) %>%
            
            #sum up scores
            mutate(
                total_cat_score = region + Industry + flag,
                num_cat_features = length(unique(cat_rv$data$feature))
            )
        
        #combine numerical and categorical
        rv$scored_df <- rv$numerical_feature_scoring %>%
            inner_join(rv$categorical_feature_scoring) %>%
            mutate(
                total_score = total_numerical_score + total_cat_score,
                score_on_5 = round(total_score / (
                    num_cat_features + num_numerical_features
                ))
            ) %>%
            mutate(ir_id = as.factor(ir_id)) %>%
            mutate(ir_id = fct_reorder(ir_id, score_on_5)) %>%
            select(
                -total_cat_score,-total_numerical_score,-num_numerical_features,-num_cat_features,-total_score
            ) %>%
            pivot_longer(-ir_id, names_to = "attributes", values_to = "value") %>%
            mutate(attributes = factor(
                attributes,
                levels = c(
                    'score_on_5',
                    'ttl_cnt_acc',
                    'ttl_cnt_pli',
                    'Industry',
                    'region',
                    'flag'
                )
            ))
        # print(rv$scored_df)
        
        
        # Score Distribution plot
        output$main_plot <- renderPlotly({
            req(rv$scored_df)
            
            
            g <-
                rv$scored_df %>%
                ggplot(aes(
                    x = ir_id,
                    y = value,
                    fill = (attributes == "score_on_5")
                )) +
                geom_col(show.legend = FALSE) +
                facet_grid(. ~ attributes, scales = "free_y") +  coord_flip() +
                scale_fill_brewer(palette = "Dark2") + guides(fill = FALSE) + theme_light()
            
            
            # g <-
            #     rv$scored_df %>%
            #     ggplot(aes(
            #         x = 1,
            #         y = value,
            #         fill = (attributes == "score_on_5")
            #     )) +
            #     geom_jitter() +
            #     facet_grid(. ~ attributes, scales = "free_y") +  coord_flip() +
            #     scale_fill_brewer(palette = "Dark2") + guides(fill = FALSE)
            
            
            ggplotly(g) %>% layout(showlegend = FALSE)
            
        })
        
        
        # metric definition table numeric
        output$metric_tbl_num <- renderRHandsontable({
            rhandsontable(num_rv$data)
            
        })
        
        # metric definition table numeric
        output$metric_tbl_cat <- renderRHandsontable({
            rhandsontable(cat_rv$data)
        })
        
        
    }, ignoreNULL = FALSE)
    
    # Watching any changes made to table cells in column variables a or b and then update column c based on formula
    
    
    
    
    
    
    output$irTbl <- DT::renderDataTable({
        ir_data
    }, options = list(scrollX = TRUE, scrollY = "500px"), class = 'cell-border stripe',
    extensions = c('FixedColumns', "FixedHeader"))
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
