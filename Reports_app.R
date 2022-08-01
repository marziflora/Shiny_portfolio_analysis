source("Functions_report.R", local=FALSE)
import_packages_report()

cases_transformed <- bind_rows(tp$cases,bm$cases)  %>% 
  select(where(~n_distinct(.) > 1)) %>% 
  select(-matches("_m_pmts|_m_avg|_m_n_pmts|seg_type|origination_dt|data_available_until_dt|cutoff_dt|debt_age_y") ) %>%  
  select_if(not_all_na) %>% 
  remove_non_duplicated_columns() 

col_dates <- cases_transformed %>% select(where(is.date)) %>%  colnames()
col_factors <- cases_transformed %>%  select(where(~n_distinct(.) > 1 & n_distinct(.) <7), -matches("source|dt"))  %>% colnames() 

cases_transformed %>% skimr::skim()
tp_bm <- bind_portfolios(tp, bm)

ui <- fluidPage(
  titlePanel("TP+BM Report"),
  
  sidebarLayout(
    sidebarPanel(width = 3,
      checkboxGroupInput(inputId= "source", label="Source",  #h3("Source"), 
                         choices = c("TP","BM"),
                         selected = c("TP","BM")),
      sliderInput(inputId = "default_dt",
                  label = "default year",
                  min = pmax(pmin(tp$cases$default_dt %>%year() %>% min(),bm$cases$default_dt %>% year() %>% min()),2000), 
                  max = max(tp$cases$default_dt %>%year() %>% max(),bm$cases$default_dt %>% year() %>% max()),
                  value = c(pmax(pmin(tp$cases$default_dt %>%year() %>% min(),bm$cases$default_dt %>% year() %>% min()),2000),
                            max(tp$cases$default_dt %>%year() %>% max(),bm$cases$default_dt %>% year() %>% max())),
                            sep = "", step = 1),
      sliderInput(inputId = "reg_dt",
                  label = "reg year",
                  min = pmax(pmin(tp$cases$reg_date %>%year() %>% min(),bm$cases$reg_date %>% year() %>% min()),2000), 
                  max = max(tp$cases$reg_date %>%year() %>% max(),bm$cases$reg_date %>% year() %>% max()),
                  value = c(pmax(pmin(tp$cases$reg_date %>%year() %>% min(),bm$cases$reg_date %>% year() %>% min()),2000),
                            max(tp$cases$reg_date %>%year() %>% max(),bm$cases$reg_date %>% year() %>% max())),
                  sep = "", step = 1),
      # checkboxGroupInput(inputId = "filter",
      #                    label = "Filter", 
      #                    choices = col_factors,
      #                    selected = col_factors ),
    checkboxGroupInput(inputId = "split",
                       label = "Split", 
                       choices = col_factors,
                       selected = NULL),
    actionButton("reset_split", label="Reset split") ),
    
    mainPanel(width = 7,
              tabsetPanel(
                tabPanel("Basic stats", 
                         tableOutput(outputId = "basic_stats_tp"),
                         tableOutput(outputId = "basic_stats_bm")),
                tabPanel("Dates dist", 
                         
                         textOutput("default_dt_text"),
                         tableOutput(outputId = "distributions_dates_tp_df"),
                         tableOutput(outputId = "distributions_dates_bm_df"),
                         
                         textOutput("reg_dt_text"),
                         tableOutput(outputId = "distributions_dates_tp_reg"), 
                         tableOutput(outputId = "distributions_dates_bm_reg")),
                tabPanel("FV%",
                         textOutput("default_dt_text2"),
                         tableOutput(outputId = "distributions_fv_tp_df"),
                         tableOutput(outputId = "distributions_fv_bm_df"),
              
                         textOutput("reg_dt_text2"),
                         tableOutput(outputId = "distributions_fv_tp_reg"),
                         tableOutput(outputId = "distributions_fv_bm_reg")),
                tabPanel("FV",
                         textOutput("default_dt_text3"),
                         plotOutput(outputId = "fv_chart_tp_df"),
                         plotOutput(outputId = "fv_chart_bm_df"),
                         
                         textOutput("reg_dt_text3"),
                         plotOutput(outputId = "fv_chart_tp_reg"),
                         plotOutput(outputId = "fv_chart_bm_reg")  ),
                tabPanel("FV dist",
                         checkboxInput("log", label = "log", value = FALSE),
                         plotlyOutput(outputId = "fv_chart_distribution")),
          tabPanel("Debt Age", plotlyOutput(outputId = "debt_age")),
          tabPanel("Liq",
                   checkboxInput("cumulative", label = "Cumulative", value = FALSE),
                   plotlyOutput(outputId = "liquidation")  ),
          tabPanel("Liq from default",
                   checkboxInput("cumulative2", label = "Cumulative", value = FALSE),
                   plotlyOutput(outputId = "liquidation_from_default")  ),
          tabPanel("pmts",
                   plotOutput(outputId = "pmts_analysis_bm"),
                   plotOutput(outputId = "pmts_analysis_tp")),
          tabPanel("-pmts",
                   tableOutput(outputId = "negative_pmts_t"),
                   tableOutput(outputId = "negative_pmts_c")),
                   # tabPanel("Negative pmts", plotlyOutput(outputId = "basic_stats_tp")),
          # tabPanel("Non-Payers", plotlyOutput(outputId = "basic_stats_tp"))
      )
      
    )
  ),
  textOutput("txt")
)

server <- function(input, output, session) {

  output$txt <- renderText({
    # paste(input[['log']])
  })
  
  output$default_dt_text <- renderText({ paste("By default year:") })
  output$default_dt_text2 <- renderText({ paste("By default year:") })
  output$default_dt_text3 <- renderText({ paste("By default year:") })
  
  output$reg_dt_text <- renderText({ paste("By reg year:") })
  output$reg_dt_text2 <- renderText({ paste("By reg year:") })
  output$reg_dt_text3 <- renderText({ paste("By reg year:") })
  
  # output$distribution_bm_text <- renderText({ paste("BM:") })
  # output$distribution_tp_text <- renderText({ paste("BM:") })
  
  
  current_data <- reactive({
    list(tp =  tp %>%
      filter(between(year(default_dt), input[["default_dt"]][1],input[["default_dt"]][2])) %>%
      filter(between(year(reg_date), input[["reg_dt"]][1],input[["reg_dt"]][2])),
      bm =  bm %>%
        filter(between(year(default_dt), input[["default_dt"]][1],input[["default_dt"]][2])) %>%
        filter(between(year(reg_date), input[["reg_dt"]][1],input[["reg_dt"]][2])) )  })
  
  observe({ if (input$reset_split > 0) { updateCheckboxGroupInput(session=session, inputId="split", choices=col_factors, selected=NULL)   } })

  output[["basic_stats_tp"]] <- renderTable({
    basic_stats_tp_table <-  current_data() %>% .$tp %>% npl_summary()
    if (!is.null(input[['split']])) { basic_stats_tp_table <- bind_rows( current_data() %>% .$tp %>% npl_summary(group_vector = input[['split']]),
                                                                       basic_stats_tp_table) }
    
    basic_stats_tp_table }, caption="TP",  caption.placement = "top")
  output[["basic_stats_bm"]] <- renderTable({ 
    basic_stats_bm_table <-  current_data() %>% .$bm %>% npl_summary()
    if (!is.null(input[['split']])) { basic_stats_bm_table <- bind_rows(current_data() %>% .$bm %>% npl_summary(group_vector = input[['split']]),
                                                                        basic_stats_bm_table) }
    
    basic_stats_bm_table }, caption="BM",  caption.placement = "top")

  #Dates ----
  output[["distributions_dates_bm_df"]] <- renderTable({ 
    current_data() %>% .$bm %>% cases_distribution(col_variable ='default_dt', group_vector = input[['split']], output_variable='n_cases')  },  caption="BM",  caption.placement = "top")
  
  output[["distributions_dates_tp_df"]] <- renderTable({ 
    current_data() %>% .$tp %>% cases_distribution(col_variable ='default_dt', group_vector = input[['split']], output_variable='n_cases')  },  caption="TP",  caption.placement = "top")
  
  output[["distributions_dates_bm_reg"]] <- renderTable({ 
    current_data() %>% .$bm %>% cases_distribution(col_variable ='reg_date', group_vector = input[['split']], output_variable='n_cases')  },  caption="BM",  caption.placement = "top")
  
  output[["distributions_dates_tp_reg"]] <- renderTable({ 
    current_data() %>% .$tp %>% cases_distribution(col_variable ='reg_date', group_vector = input[['split']], output_variable='n_cases')  },  caption="TP",  caption.placement = "top")
  
  # FV
  output[["distributions_fv_bm_df"]] <- renderTable({ 
    current_data() %>% .$bm %>% cases_distribution(col_variable ='default_dt', group_vector = input[['split']], output_variable='perc')  },  caption="BM",  caption.placement = "top")
  
  output[["distributions_fv_tp_df"]] <- renderTable({ 
    current_data() %>% .$tp %>% cases_distribution(col_variable ='default_dt', group_vector = input[['split']], output_variable='perc')  },  caption="TP",  caption.placement = "top")
  
  output[["distributions_fv_bm_reg"]] <- renderTable({ 
    current_data() %>% .$bm %>% cases_distribution(col_variable ='reg_date', group_vector = input[['split']], output_variable='perc')  },  caption="BM",  caption.placement = "top")
  
  output[["distributions_fv_tp_reg"]] <- renderTable({ 
    current_data() %>% .$tp %>% cases_distribution(col_variable ='reg_date', group_vector = input[['split']], output_variable='perc')  },  caption="TP",  caption.placement = "top")
  
  # charts per year:
  output[["fv_chart_bm_df"]] <- renderPlot({   fv_distribution_chart_per_year(current_data() %>% .$bm, col_variable = 'default_dt')   })
  
  output[["fv_chart_tp_df"]] <- renderPlot({   fv_distribution_chart_per_year(current_data() %>% .$tp, col_variable = 'default_dt')   })
  
  output[["fv_chart_bm_reg"]] <- renderPlot({  fv_distribution_chart_per_year(current_data() %>% .$bm, col_variable = 'reg_date')   })
  
  output[["fv_chart_tp_reg"]] <- renderPlot({  fv_distribution_chart_per_year(current_data() %>% .$tp, col_variable = 'reg_date')   })
  
  #FV charts distribution:
  output[["fv_chart_distribution"]] <- renderPlotly({    
    if (input$log) { fv_distribution_chart(
    bind_portfolios(current_data() %>% .$bm, 
                    current_data() %>% .$tp), groups_vector = input[['split']], scale='log') 
  } else { fv_distribution_chart(
    bind_portfolios(current_data() %>% .$bm, 
                    current_data() %>% .$tp), groups_vector = input[['split']], scale='identity') } })
  
  # Debt Age distribution
  output[["debt_age"]] <- renderPlotly({  
    debt_age_distribution_chart( 
    bind_portfolios(current_data() %>% .$tp,
                    current_data() %>% .$bm),   groups_vector = input[['split']] ) })
                      
  output[["liquidation"]] <- renderPlotly({
    plot_data <- return_liq_rate_grouped(
      bind_portfolios(current_data() %>% .$tp,
                      current_data() %>% .$bm),   groups_vector = input[['split']] )
    
    ggplot_plot_1 <- if (input$cumulative) {
      ggplot(plot_data, aes(x = age, y = liq_ratec, color = source))   
    } else {     ggplot(plot_data, aes(x = age, y = liq_rate, color = source))  }
    

    (ggplot_plot_1 + geom_point(size=0.2) + geom_line() + facet_wrap(~group_col, ncol=1, scale='free_y') +
      theme(legend.position = 'none',  # plot.margin = unit(c(-1, 0.03, -1.2, -1.5), "cm"),
            legend.title.align=1)) %>%  
      ggplotly() %>% 
      layout(height = length(unique(plot_data$group_col))*220, width = '100%',
             legend = list(yanchor="top",    xanchor="left",   font = list(size = 8)) ) 
    })
  
  
  output[["liquidation_from_default"]] <- renderPlotly({
    plot_data <- return_liq_rate_from_default_grouped(
      bind_portfolios(current_data() %>% .$tp,
                      current_data() %>% .$bm),   groups_vector = input[['split']] )
    
    ggplot_plot_1 <- if (input$cumulative2) {
      ggplot(plot_data, aes(x = age, y = liq_ratec, color = source))   
    } else {     ggplot(plot_data, aes(x = age, y = liq_rate, color = source))  }
    
    
    ggplotly(ggplot_plot_1 + geom_point(size=0.2) + geom_line() + facet_wrap(~group_col, ncol=1, scale='free_y') +
        theme(legend.position = 'none',  
              legend.title.align=1)) %>% layout(height = length(unique(plot_data$group_col))*220, width = '100%',
             legend = list(yanchor="top",    xanchor="left",   font = list(size = 8)) )  })
  
  output[["pmts_analysis_tp"]] <- renderPlot({   plot_pmt_analysis(current_data() %>% .$tp %>% pull(pmts), title = "TP")}, height=800 )
  output[["pmts_analysis_bm"]] <- renderPlot({   plot_pmt_analysis(current_data() %>% .$bm %>% pull(pmts), title = 'BM')}, height='auto' )
  
  output[["negative_pmts_t"]] <- renderTable({   bind_portfolios(current_data() %>% .$bm,current_data() %>% .$tp) %>% 
      negative_pmts_table_total( groups_vector = input[['split']] ) })

  output[["negative_pmts_c"]] <- renderTable({   bind_portfolios(current_data() %>% .$bm, current_data() %>% .$tp) %>% 
      negative_pmts_table_per_case() })
  
  
  }

shinyApp(ui = ui, server = server)






