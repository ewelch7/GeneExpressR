library(shiny)
library(shinyjs)
library(shinythemes)
library(readxl)
library(dplyr)
library(DT)
library(openxlsx)

ui <- fluidPage(
  shinyjs::useShinyjs(),
  navbarPage(
    theme = shinytheme("cosmo"),
    title = tags$b("Gene Expression Calculator"),
    tabPanel("Data Upload",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file", "Choose Excel File", accept = c(".xlsx", ".xls")),
                 numericInput("num_rows", "Number of Rows to Display", value = 10, min = 1)
               ),
               mainPanel(
                 tableOutput("data_table")
               )
             )
    ),
    tabPanel("Calculation",
             tabsetPanel(
               tabPanel("Delta Ct",
                        mainPanel(
                          DTOutput("delta_ct_table")
                        )
               ),
               tabPanel("Gene_1",
                        mainPanel(
                          actionButton("calculate_average_gene_1_button", "Calculate Average"),
                          DTOutput("gene_1_table"),
                          DTOutput("rel_ex_1_table"),
                          numericInput("input_gene_1", "Enter Numeric Value for Gene_1:", value = 1),
                          downloadButton("download_gene_1_results", "Download Results")
                        ),
                        sidebarPanel(
                          verbatimTextOutput("gene_1_calculation_output")
                        )
               ),
               tabPanel("Gene_2",
                        mainPanel(
                          actionButton("calculate_average_gene_2_button", "Calculate Average"),
                          DTOutput("gene_2_table"),
                          DTOutput("rel_ex_2_table"),
                          numericInput("input_gene_2", "Enter Numeric Value for Gene_2:", value = 1),
                          downloadButton("download_gene_2_results", "Download Results")
                        ),
                        sidebarPanel(
                          verbatimTextOutput("gene_2_calculation_output")
                        )
               ),
               selected = "Delta Ct"
             )
    )
  )
)
server <- function(input, output, session) {
  
  gene_1_table <- reactiveVal(list(data = data.frame(), selected_rows = integer(0)))
  gene_2_table <- reactiveVal(list(data = data.frame(), selected_rows = integer(0)))
  rel_ex_1_table_data <- reactiveVal(data.frame())
  rel_ex_2_table_data <- reactiveVal(data.frame())
  
  read_excel_file <- function(file_path) {
    readxl::read_excel(file_path)
  }
  
  output$data_table <- renderTable({
    req(input$file)
    file_path <- input$file$datapath
    df <- read_excel_file(file_path)
    head(df, n = input$num_rows)
  })
  
  output$delta_ct_table <- renderDataTable({
    req(input$file)
    file_path <- input$file$datapath
    df <- read_excel_file(file_path)
    df$Delta_Ct_1 <- round(df$Hex - df$Fam, 4)
    df$Delta_Ct_2 <- round(df$Cy5 - df$Fam, 4)
    datatable(
      df,
      options = list(
        searching = TRUE,
        paging = TRUE,
        rownames = FALSE,
        selection = 'none'
      )
    )
  })
  
  output$gene_1_table <- renderDataTable({
    req(input$file)
    file_path <- input$file$datapath
    df <- read_excel_file(file_path)
    df$Delta_Ct_1 <- round(df$Hex - df$Fam, 4)
    gene_1_table_data <- df %>%
      group_by(Animal) %>%
      summarise(
        Sex = first(Sex),
        Treatment = first(Treatment),
        Delta_Ct_1_mean = mean(Delta_Ct_1, na.rm = TRUE),
        Two_Neg_Delt_1 = 2^(-Delta_Ct_1_mean)
      )
    gene_1_table(list(data = gene_1_table_data, selected_rows = integer(0)))
    datatable(
      gene_1_table_data,
      options = list(
        searching = TRUE,
        paging = FALSE,
        rownames = FALSE,
        selection = list(target = 'row', selected = gene_1_table()$selected_rows)
      )
    )
  })
  
  output$rel_ex_1_table <- renderDataTable({
    req(input$file)
    file_path <- input$file$datapath
    df <- read_excel_file(file_path)
    df$Delta_Ct_1 <- round(df$Hex - df$Fam, 4)
    rel_ex_1 <- df %>%
      group_by(Animal) %>%
      summarise(
        Sex = first(Sex),
        Treatment = first(Treatment),
        Two_Neg_Delt_1 = 2^(-mean(Delta_Ct_1, na.rm = TRUE)),
        Relative_Expression_1 = Two_Neg_Delt_1 / input$input_gene_1
      )
    rel_ex_1_table_data(rel_ex_1)
    datatable(rel_ex_1, options = list(searching = TRUE, paging = FALSE, rownames = FALSE))
  })
  
  output$gene_2_table <- renderDataTable({
    req(input$file)
    file_path <- input$file$datapath
    df <- read_excel_file(file_path)
    df$Delta_Ct_2 <- round(df$Cy5 - df$Fam, 4)
    gene_2_table_data <- df %>%
      group_by(Animal) %>%
      summarise(
        Sex = first(Sex),
        Treatment = first(Treatment),
        Delta_Ct_2_mean = mean(Delta_Ct_2, na.rm = TRUE),
        Two_Neg_Delt_2 = 2^(-Delta_Ct_2_mean)
      )
    gene_2_table(list(data = gene_2_table_data, selected_rows = integer(0)))
    datatable(
      gene_2_table_data,
      options = list(
        searching = TRUE,
        paging = FALSE,
        rownames = FALSE,
        selection = list(target = 'row', selected = gene_2_table()$selected_rows)
      )
    )
  })
  
  output$rel_ex_2_table <- renderDataTable({
    req(input$file)
    file_path <- input$file$datapath
    df <- read_excel_file(file_path)
    df$Delta_Ct_2 <- round(df$Cy5 - df$Fam, 4)
    rel_ex_2 <- df %>%
      group_by(Animal) %>%
      summarise(
        Sex = first(Sex),
        Treatment = first(Treatment),
        Two_Neg_Delt_2 = 2^(-mean(Delta_Ct_2, na.rm = TRUE)),
        Relative_Expression_2 = Two_Neg_Delt_2 / input$input_gene_2
      )
    rel_ex_2_table_data(rel_ex_2)
    datatable(rel_ex_2, options = list(searching = TRUE, paging = FALSE, rownames = FALSE))
  })
  
  observeEvent(input$calculate_average_gene_1_button, {
    selected_rows <- input$gene_1_table_rows_selected
    if (length(selected_rows) > 0) {
      selected_data <- gene_1_table()$data[selected_rows, ]
      avg_ex_1_calc <- mean(selected_data$Two_Neg_Delt_1)
      output$gene_1_calculation_output <- renderText({
        paste("Average Two_Neg_Delt_1 for selected rows: ", round(avg_ex_1_calc, 4))
      })
    } else {
      output$gene_1_calculation_output <- renderText("No rows selected.")
    }
  })
  
  observeEvent(input$calculate_average_gene_2_button, {
    selected_rows <- input$gene_2_table_rows_selected
    if (length(selected_rows) > 0) {
      selected_data <- gene_2_table()$data[selected_rows, ]
      avg_ex_2_calc <- mean(selected_data$Two_Neg_Delt_2)
      output$gene_2_calculation_output <- renderText({
        paste("Average Two_Neg_Delt_2 for selected rows: ", round(avg_ex_2_calc, 4))
      })
    } else {
      output$gene_2_calculation_output <- renderText("No rows selected.")
    }
  })
  
  output$download_gene_1_results <- downloadHandler(
    filename = function() {
      paste("rel_ex_1_results_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      rel_ex_1_data <- rel_ex_1_table_data()
      write.xlsx(rel_ex_1_data, file, rowNames = FALSE)
    }
  )
  
  output$download_gene_2_results <- downloadHandler(
    filename = function() {
      paste("rel_ex_2_results_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      rel_ex_2_data <- rel_ex_2_table_data()
      write.xlsx(rel_ex_2_data, file, rowNames = FALSE)
    }
  )
}

shinyApp(ui, server)

