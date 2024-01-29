### Install the following packages, if not currently installed
# install.packages('shiny')
# install.packages('shinyWidgets')
# install.packages('shinydashboard')
# install.packages('dplyr')
# install.packages('ggplot2')
# install.packages('DT')
# install.packages('reshape2')
# install.packages('formattable')
# install.packages('stringr')

### Loads Relevant packages
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(reshape2)
library(formattable)
library(stringr)

### Essentially, working directory sits in the script folder. All data sits in adjacent folder 
### and the desired directory needs to be set to the data folder as a result
base_dir <- gsub('scripts$', 'data', getwd())

### Loads relevant files provided. The names of the files are the same as the files from the 
### zipfile provided, so as long as the same files are used with the same names, and this script
### lies in the same directory as those named files
productCategory <- read.csv(paste0(base_dir, '/ProductCategory.csv'), stringsAsFactors = F)
productCategory <- productCategory %>% select(ProductCategoryKey, EnglishProductCategoryName)

productSubCategory <- read.csv(paste0(base_dir, '/ProductSubcategory.csv'), stringsAsFactors = F)
productSubCategory <- productSubCategory %>% select(ProductSubcategoryKey, ProductCategoryKey, EnglishProductSubcategoryName)

### Gets the association between primary category and subCategory, so we can filter out 'Bikes' from
### the subCategory Selects
categorySubcategory <- left_join(productCategory, productSubCategory, by = 'ProductCategoryKey') %>% 
  select(ProductCategoryKey, EnglishProductCategoryName, ProductSubcategoryKey, EnglishProductSubcategoryName)

### A more aesthetically pleasing action button
primaryActionButton <- function(inputId, label) tags$button(id = inputId, type = "button", class = "btn btn-primary action-button btn-md", label)

ui <- dashboardPage(

  dashboardHeader(title = 'Regional Dashboards'),
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem('SubCategories', tabName = 'subCategoriesPage', selected = T)
      
    )
   
  ), 
  
  dashboardBody(
    
    tabItems(
      
      tabItem(tabName = 'subCategoriesPage',
              
              h1('Regional Comparison of Revenues for Sub Category by Month'),
              
              helpText('Select any number of SubCategories with any number of Countries. You must select a SubCategory, or you will
                       get a warning message, and not choosing a Country will result in comparing to all countries. Since this a request
                       for the Canadia Region, the Canadian Revenues will be highlighted orange, while all other regions will be blue.
                       Regions will still be distinct by their respective shape, as decribed in the legend'),
              
              
              fluidRow(
                column(3, h5('Select Sub Category / Sub Categories:')),
                column(3, h5('Select Region(s):'))
                ),
              
              ### Outputs the Selection Bars
              fluidRow(
                uiOutput('selectizeOptions')
                ),
              
              ### Outputs the plotting bars
              fluidRow(
                plotOutput('categoryPlot')
                ),
              
              ### Outputs the raw information:
              fluidRow(
                dataTableOutput('categoryTable')
                )
              
              )
        )
    )

)

### Loads the server of the app
server <- function(input, output, session) {
  
  ### Loads date associations and gets relevant date metrics
  date <- read.csv(paste0(base_dir, '/Date.csv'), stringsAsFactors = F)
  date <- date %>% select(DateKey, MonthNumberOfYear, CalendarYear)
  
  ### Loads Product sales and isolates relevant information
  factInternetSales <- read.csv(paste0(base_dir, '/FactInternetSales.csv'), stringsAsFactors = F)
  factInternetSales <- factInternetSales %>% select(ProductKey,SalesTerritoryKey, SalesAmount, OrderDateKey, CustomerKey) 
    
  ### Loads Geography association and isolates relevant information
  geography <- read.csv(paste0(base_dir, '/Geography.csv'), stringsAsFactors = F)
  geography <- geography %>% select(SalesTerritoryKey, EnglishCountryRegionName)
  
  ### Loads product association and isolates relevant information
  product <- read.csv(paste0(base_dir, '/Product.csv'), stringsAsFactors = F)
  product <- product %>% select(ProductKey, ProductSubcategoryKey)
  
  ### Joins Sales with date, then filters current and prior year data
  salesDate <- left_join(factInternetSales, date, by = c('OrderDateKey' = 'DateKey'))
  salesDate <- salesDate %>% 
    filter(CalendarYear %in% c(2007, 2008)) %>% 
    filter(MonthNumberOfYear %in% 1:6)
  
  ### Gets the distinct keys and region names
  territoryKeys <- geography %>% 
    select(SalesTerritoryKey, EnglishCountryRegionName) %>% 
    unique()
  
  ### Joins the sales and Date data with relevant geography data
  geoSales <- left_join(salesDate, territoryKeys, by = 'SalesTerritoryKey')
  
  ### Joins all relevant productcategory and product subcategory data,
  ### filters out the Bike product category, and selects relevant sales metrics,
  ### renames and mutates columns for easier plotting
  geoSalesCategoryRegion <- geoSales %>% 
    select(MonthNumberOfYear, CalendarYear, EnglishCountryRegionName, ProductKey, SalesAmount) %>% 
    left_join(product, by = 'ProductKey') %>% 
    left_join(categorySubcategory, by = 'ProductSubcategoryKey') %>% 
    filter(EnglishProductCategoryName != 'Bikes') %>% 
    select(EnglishCountryRegionName, MonthNumberOfYear, CalendarYear, EnglishProductSubcategoryName, SalesAmount) %>% 
    arrange(EnglishProductSubcategoryName) %>% 
    rename(Month = MonthNumberOfYear, Country = EnglishCountryRegionName, Revenue = SalesAmount) %>% 
    mutate(Month = factor(month.abb[Month], levels = month.abb))
  
  ### Outputs relevant selecting options
  output$selectizeOptions <- renderUI(
    tagList(
      fluidRow(
        ### Shows subcategory selects
        column(3, selectizeInput(inputId = 'subcategory',
                                 label = NULL,
                                 choices = c('', unique(geoSalesCategoryRegion$EnglishProductSubcategoryName)),
                                 selected = '',
                                 multiple = T)),
        ### Shows country selects
        column(3, selectizeInput(inputId = 'country',
                                 label = NULL,
                                 choices = c('All', unique(geoSalesCategoryRegion$Country)),
                                 selected = 'All', 
                                 multiple = T)),
        ### Shows button to confirm the process
        column(3, primaryActionButton(inputId = 'makePlot', label = 'Get Comparison'))
        )
      )
    )
  
  ### Label for ggplot revenues
  revenueLabel <- function(x) paste0('$', comma(x, digits = 0))
  
  ### Fires off after the button is clicked
  observeEvent(input$makePlot, {
    
    ### Renames and reassigns values for ease of use
    subCategory <- input$subcategory
    
    ### If no country is selected, all are assumed
    country <- ifelse(is.na(input$country) | is.null(input$country), 
                      'All', 
                      input$country)
    
    ### Selects all available countries if "All" is in the selection space 
    if('All' %in% country) country <- unique(geoSalesCategoryRegion$Country)
    
    ### Fires if no subCategory is collected
    if(subCategory == '' || is.na(subCategory) || is.null(subCategory)) {
      
      ### Renders the plot empty
      output$categoryPlot <- renderPlot(NULL)
      output$categoryTable <- renderDataTable(NULL)
      
      ### Warns the user that they need to select a category
      return(sendSweetAlert(session = session, 
                     title = "Please Select a Category!",
                     type = 'warning'))
      
    } 
    
    ### Filters the data to the selected metrics, then gets the sum of the revenues
    ### by Country, by month. Year, for this application, is not necessary, since
    ### Bikes was the only category in 2007.
    facetPlotData <- geoSalesCategoryRegion %>% 
      filter(grepl(paste0(subCategory, collapse = '|'), EnglishProductSubcategoryName)) %>% 
      filter(grepl(paste0(country, collapse = '|'), Country)) %>% 
      group_by(Country, Month) %>% 
      summarise(Revenue = sum(Revenue)) %>% 
      ungroup()
    
    ### Shouldn't happen, but as a security measure, will fire if there is no data
    if(nrow(facetPlotData) == 0) {
      
      ### Renders the plot Empty
      output$categoryPlot <- renderPlot(NULL)
      output$categoryTable <- renderDataTable(NULL)
      
      ### Returns a warning message
      return(sendSweetAlert(session = session, 
                            title = paste0('No Data for ', paste0(subCategory, sep = ', ')),
                            text = 'Please select another subcategory.',
                            type = 'warning'))
      
    } 
    
    ### Creates the plot colors. Since Canada is the focus, every region will be shown to be 
    ### blue, except for the Canada dealer, which will be orange
    plotColors <- rep('blue', length(unique(facetPlotData$Country)))
    names(plotColors) <- unique(facetPlotData$Country)
    plotColors[names(plotColors) == 'Canada'] <- 'orange'
    
    ### Plots the Revenue By month for each country. Different countries are marked with
    ### different shapes, and Canada is highlighed in orange
    subCategoryFacetPlot <- ggplot(facetPlotData, aes(x = Month,
                                                      y = Revenue,
                                                      group = Country,
                                                      color = Country, 
                                                      shape = Country)) +
      geom_line(size = 1) +
      geom_point(size = 4) +
      ggtitle(paste0('Monthly Revenues for ', paste0(subCategory, collapse = '; '), ' By Country')) +
      ylab('Profit (US Dollars)') +
      scale_y_continuous(labels = revenueLabel) +
      scale_color_manual(values = plotColors) +
      theme(panel.background = element_rect(fill = NULL, color = 'black'))
    
    ### Renders the plot
    output$categoryPlot <- renderPlot(subCategoryFacetPlot)
    
    ### Reshapes the data to be more legible
    monthlyRevenueCast <- facetPlotData %>% 
      dcast(Country ~ Month, value.var = 'Revenue') %>% 
      mutate(Total = rowSums(.[,2:ncol(.)], na.rm = T))
    
    ### Fills in months that have no data to be 0
    monthlyRevenueCast[is.na(monthlyRevenueCast)] <- 0
    
    ### Formats the table to show clean dollar amounts
    monthlyRevenueCast[, 2:ncol(monthlyRevenueCast)] <- lapply(monthlyRevenueCast[, 2:ncol(monthlyRevenueCast)],
                                                               FUN = function(x) paste0('$', comma(x, digits = 2)))
    
    ### Gets the row of the Canada Region, if it exists.
    canadaRow <- ifelse('Canada' %in% monthlyRevenueCast$Country, which(monthlyRevenueCast$Country == 'Canada'), 0)

    ### Formats the data frame so that the canada row is highlighted, if it exists. Otherwise, if Canada is not listed,
    ### the table will have the 0th row highlighted, which doesn't exist.
    monthlyRevenueCast <- monthlyRevenueCast %>% 
      datatable() %>% 
      formatStyle(backgroundColor = styleEqual(canadaRow, 'yellow'), target = 'row', columns = 1, valueColumns = 0)
    
    ### Outputs Data Table
    output$categoryTable <- renderDataTable(monthlyRevenueCast)
    
  })
  
}

### Runs the Shiny Application
shinyApp(ui = ui, server = server)