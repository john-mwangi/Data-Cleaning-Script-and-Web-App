library(shiny)
library(readxl)
library(lubridate)
library(tidyverse)
    
ui <- fluidPage(
    titlePanel("Company X"),
    sidebarLayout(
        sidebarPanel(
            fileInput(inputId = "sales",
                      label = "Upload sales data",
                      multiple = TRUE,
                      accept = ".xlsx")
        ),
        mainPanel(
            HTML("<p>
                <b>INSTRUCTIONS</b></br>
                <ol>
                <li> Copy and paste sales onto Excel in the first column </li>
                <li> Save the Excel file with month and year as the file name in the format: Oct2020.xlsx </li>
                <li> A sales chart can be displayed if more than 1 month is uploaded </li>
                </ol>
                </p><p>
                
                </br>
                
                <b>SALES BREAKDOWN</b></p>"),
            #checkboxInput(inputId = "disp_chart", label = "Display sales chart?", value = TRUE),
            tabsetPanel(type = "pill",
                        tabPanel(title = "Revenue", tableOutput("table")),
                        tabPanel(title = "Revenue Breakdown", tableOutput("brk_table")),
                        tabPanel(title = "Sales Trend", plotOutput("chart")))
        )
    )
)

server <- function(input, output) {
        
    fetch_sales <- reactive({
    
    #This prevents the reactive from initialising unless a file is uploaded
    req(input$sales)
    
    ext <- tools::file_ext(input$sales$datapath)
    validate(need(ext == "xlsx", "Please upload an Excel (.xlsx) file"))
            
    #Obtain filepaths
    filepaths <- input$sales$datapath
    filenames <- input$sales$name
    
    sales_data <- filepaths %>% 
        map_dfr(~read_excel(path = ., col_names = FALSE)) %>% 
        rename(data = ...1) %>% 
        mutate(data = str_trim(str_to_lower(data)))
    
    #Add filenames    
    filerows <- filepaths %>% 
        map(~read_excel(path = .,col_names = FALSE)) %>% 
        map(nrow) %>% 
        unlist()
    
    filenames_all <- c()
    for(f in seq_along(filenames)){
        filenames_all <- append(filenames_all, rep(filenames[f], times = filerows[f]))
    }
    
    sales_data$filename <- filenames_all
    
    #Extract year and month. Create date column.
    sales_data <- sales_data %>% 
        extract(col = filename, 
                into = c("month","year"), 
                regex = "([[:alpha:]]+)([[:digit:]]+)")
    
    sales_data <- sales_data %>%
        mutate(day = str_extract(string = data, pattern = "^\\d\\d?\\w\\w")) %>%
        fill(day, .direction = "down") %>% 
        mutate(day = parse_number(day),
               date = paste0(day,month,year),
               date = dmy(date),
               date = as.character(date)) %>% 
        select(sales = data, date)
    
    #Extract product details and location
    sales_data <- sales_data %>% 
        separate(col = sales,
                 into = c("product","price_1","location"), 
                 sep = "-", 
                 convert = TRUE, 
                 remove = FALSE) %>% 
        separate(col = price_1, into = c("price","extras"), sep = "/", remove = FALSE, convert = TRUE)
    
    #Filter out unnecessary rows.
    sales_data <- sales_data %>% 
        filter(!is.na(price) | !str_detect(sales, "[:digit:]")) %>% 
        replace_na(list(price = 0, extras = 0)) %>% 
        select(-price_1)
    
    #Include product categories
    sales_data <- sales_data %>% 
        mutate(category = case_when(str_detect(product,"sausage") ~ "sausage",
                                    str_detect(product,"^chicken$") ~ "chicken",
                                    str_detect(product,"sandwich") ~ "sandwich",
                                    str_detect(product,"fries|fry|poussin|masala|cheesy|poisson|^spicy$|spicy f.*") ~ "fries",
                                    str_detect(product,"burger|/|bacon") ~ "burger",
                                    str_detect(product,"sales") ~ "no sales",
                                    TRUE ~ "pizza")) %>% 
        mutate(category = na_if(category,"NA"))
    
    #Corrected spellings
    bce_regex <- "b/e/c|b/c/e|e/c/b|c/e/b|b, c and e|bacon.*egg"
    bc_regex <- "b/c burger|bacon and cheese|cheese and bacon|b and c|c and b|bacon cheese|bacon burger and cheese"
    md_regex <- "^deluxe$|meat deluxe|meaty deluxe"
    
    sales_data <- sales_data %>% 
        mutate(product_name = case_when(category=="sausage" ~ "sausage",
                                        category=="chicken" ~ "chicken",
                                        TRUE ~ product)) %>% 
        mutate(product_name = case_when(category=="fries" & str_detect(product,"cheesy") ~ "cheesy fries",
                                        category=="fries" & str_detect(product,"masala") ~ "masala fries",
                                        category=="fries" & str_detect(product,"poussin|poisson") ~ "poussin fries",
                                        category=="fries" & str_detect(product,"spicy") ~  "spicy fries",
                                        category=="fries" & str_detect(product,"^fry$|^fries$|salted") ~ "regular fries",
                                        TRUE ~ product_name)) %>% 
        mutate(product_name = case_when(category=="sandwich" & str_detect(product,"chicken") ~ "chicken sandwich",
                                        category=="sandwich" & str_detect(product,"beef") ~ "beef sandwich",
                                        category=="sandwich" & str_detect(product,"egg") ~ "egg sandwich",
                                        TRUE ~ product_name)) %>% 
        mutate(product_name = case_when(category=="pizza" & str_detect(product,"periperi") ~ "chicken periperi",
                                        category=="pizza" & str_detect(product,"bbq") ~ "bbq beef",
                                        category=="pizza" & str_detect(product,"sweet|s and") ~ "sweet and sour chicken",
                                        category=="pizza" & str_detect(product,"margharita|margarita") ~ "margherita",
                                        category=="pizza" & str_detect(product,"hawaian") ~ "hawaiian",
                                        category=="pizza" & str_detect(product,md_regex) ~ "meat deluxe",
                                        category=="pizza" & str_detect(product,"^pepperoni$") ~ "beef pepperoni",
                                        TRUE ~ product_name)) %>%
        mutate(product_name = case_when(category=="burger" & str_detect(product,"^burgers?$|original") ~ "original burger",
                                        category=="burger" & str_detect(product,bce_regex) ~ "bacon,cheese,egg burger",
                                        category=="burger" & str_detect(product,bc_regex) ~ "bacon and cheese burger",
                                        category=="burger" & str_detect(product,"bacon burger") ~ "bacon burger",
                                        category=="burger" & str_detect(product,"cheese burger") ~ "cheese burger",
                                        TRUE ~ product_name)) %>% 
        mutate(category = ifelse(product_name=="hawaiian/pepperoni","pizza",category))
    
    #Determining product sizes for chicken and pizza
    # Pizza price lists
    pizza_large <- c(850,900,950,1000)
    pizza_medium <- c(650,700,750,800)
    pizza_small <- c(450,500,550,600)
    
    # Modulo function.
    mod_price <- function(x, price_list){
        mod <- c()
        for(p in seq_along(price_list)){
            mod[p] <- x %% price_list[p]
        }
        return(mod)
    }
    
    # Applying the custom function
    sales_data <- sales_data %>% 
        mutate(mod_pizza_large = map(.x = price, .f = mod_price, price_list = pizza_large),
               mod_pizza_medium = map(.x = price, .f = mod_price, price_list = pizza_medium),
               mod_pizza_small = map(.x = price, .f = mod_price, price_list = pizza_small)) %>% 
        mutate(mod_pizza_large = map_dbl(.x = mod_pizza_large, .f = min),
               mod_pizza_medium = map_dbl(.x = mod_pizza_medium, .f = min),
               mod_pizza_small = map_dbl(.x = mod_pizza_small, .f = min)) %>% 
        mutate(size = case_when(mod_pizza_large==0 ~ "large",
                                mod_pizza_medium==0 ~ "medium",
                                mod_pizza_small==0 ~ "small",
                                TRUE ~ "Unknown")) %>% 
        mutate(size = case_when(size=="Unknown" & category=="pizza" & price>=850 ~ "large",
                                size=="Unknown" & category=="pizza" & price>450 & price<850 ~ "medium",
                                size=="Unknown" & category=="pizza" & price<=450 ~ "small",
                                TRUE ~ size)) %>% 
        select(-starts_with("mod")) 
    
    #Adding chicken sizes
    chicken_price <- c(200)
    
    sales_data <- sales_data %>% 
        mutate(size = ifelse(test = category=="chicken", 
                             yes = paste0((price/chicken_price)*0.25,"kg"), 
                             no = size))
    
    #Everything else the size is same as category.
    sales_data <- sales_data %>% 
        mutate(size = ifelse(!category %in% c("pizza","chicken"), category, size))
    
    
    #Final renaming, rearranging, selection of columns
    colnames(sales_data) <- str_to_title(colnames(sales_data))
    
    sales_data <- sales_data %>%
        select(-Sales,-Product) %>% 
        rename(Pizza = Product_name) %>% 
        mutate(Revenue = Price + Extras) %>% 
        select(Date,Pizza,Size,Price,Extras,Revenue,Location,Category) %>% 
        mutate_if(.predicate = is.character, .funs = str_to_title)
    
    #Simple summaries for sales.
    sales_summary <- sales_data %>% 
        group_by(Year = year(Date), Month = month(Date)) %>% 
        summarise(revenue = sum(Revenue)) %>% 
        inner_join(sales_data %>%
                       group_by(Year = year(Date)) %>% 
                       summarise(revenue_yr = sum(Revenue)), by = "Year") %>% 
        ungroup()
    
    #Formatting for display
    sales_summary_display <- sales_summary %>% 
        mutate(Year = as.character(Year),
               Month = as.character(Month),
               revenue = scales::comma(revenue, accuracy = 0.01),
               revenue_yr = scales::comma(revenue_yr, accuracy = 0.01))
    
    #Display the Sales chart
    if(nrow(sales_summary)>1) {
    
    sales_summary_chart <- sales_summary %>%
        mutate(mon_abr = month(x = paste0(Year,"-",Month,"-01"), label = TRUE, abbr = TRUE)) %>% 
        mutate(year_mon = paste0(mon_abr," ",Year)) %>% 
        mutate(period = row_number()) %>% 
        mutate(year_mon = fct_reorder(year_mon, period))
    
    breaks_x <- sales_summary_chart$period
    labels_x <- sales_summary_chart$year_mon
    
    trend <- lm(formula = revenue ~ period, data = sales_summary_chart) %>%
        broom::tidy() %>%
        mutate(sign = sign(estimate),
               sign = ifelse(sign==1,"increasing","decreasing"),
               estimate = round(estimate),
               estimate = scales::comma(estimate)) %>%
        filter(term == "period") %>%
        select(estimate, sign)

    sales_chart <- sales_summary_chart %>%
        ggplot(aes(x = period, y = revenue)) +
        geom_line() +
        geom_smooth(method = "lm", se = FALSE) +
        scale_x_continuous(breaks = breaks_x, labels = labels_x) +
        scale_y_continuous(labels = scales::dollar_format(prefix = "KES ")) +
        theme(axis.text.x = element_text(angle = 90)) +
        labs(title = "Sales Chart and Trend",
             subtitle = glue::glue("On average, revenue has been {trend$sign} at a rate of KES {trend$estimate} per month."),
             x = "",
             y = "")
    } else {
        sales_chart <- NULL
    }
    
    #Monthly breakdown of product sales
    monthly_breakdown <- sales_data %>% 
        mutate(month = month(Date),
               year = year(Date)) %>% 
        group_by(Category,year,month) %>% 
        summarise(revenue = sum(Revenue),
                  n = n()) %>%
        summarise(monthly_revenue = round(mean(revenue)),
                  units_sold = round(mean(n))) %>% 
        arrange(year,desc(monthly_revenue))
    
    monthly_breakdown_disp <- monthly_breakdown %>% 
        relocate(year, .before = everything()) %>% 
        mutate(monthly_revenue = scales::comma(monthly_revenue, accuracy = 0.01),
               year = as.character(year),
               units_sold = as.character(units_sold))

    return(list(result=filenames, 
                table=sales_summary_display, 
                chart=sales_chart, 
                brk_table=monthly_breakdown_disp))
    
    })

        
    # output$result <- renderText(fetch_sales()$result)
    output$table <- renderTable(fetch_sales()$table, align = "r")
    output$chart <- renderPlot(fetch_sales()$chart)
    output$brk_table <- renderTable(fetch_sales()$brk_table, align = c("rlrr"))
}

shinyApp(ui = ui, server = server)