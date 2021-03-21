## ----setup, include=FALSE------------------------
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------
rm(list = ls())


## ------------------------------------------------
if(!require(pacman)){install.packages("pacman")}
pacman::p_load(readxl, writexl, scales, lubridate, tidyverse)


## ------------------------------------------------
sales_path <- "../Sales/"
expenses_path <- "../Expenses/"
main_path <- "../../Main/"


## ------------------------------------------------
filepaths <- dir(path = sales_path, all.files = FALSE, pattern = ".xlsx$", full.names = TRUE)

sales_data <- filepaths %>% 
  map(~read_excel(path = ., col_names = FALSE)) %>% 
  do.call(bind_rows, .) %>% 
  rename(data = ...1) %>% 
  mutate(data = str_trim(str_to_lower(data)))

dim(sales_data)
head(sales_data)


## ------------------------------------------------
filerows <- filepaths %>% 
  map(~read_excel(path = .,col_names = FALSE)) %>% 
  map(nrow) %>% 
  unlist()

filepaths_all <- c()
for(f in seq_along(filepaths)){
  filepaths_all <- append(filepaths_all, rep(filepaths[f], times = filerows[f]))
}

sales_data$filepath <- filepaths_all

dim(sales_data)
head(sales_data)


## ------------------------------------------------
sales_data <- sales_data %>% 
  extract(col = filepath, 
          into = c("month","year"), 
          regex = ".*/([[:alpha:]]+)([[:digit:]]+)")

sales_data <- sales_data %>% 
  mutate(day = str_extract(string = data, pattern = "^\\d\\d?\\w\\w")) %>% 
  fill(day, .direction = "down") %>% 
  mutate(day = parse_number(day),
         date = paste0(day,month,year),
         date = dmy(date)) %>% 
  select(sales = data, date)

dim(sales_data)
head(sales_data)


## ------------------------------------------------
sales_data <- sales_data %>% 
  separate(col = sales,
           into = c("product","price_1","location"), 
           sep = "-", 
           convert = TRUE, 
           remove = FALSE) %>% 
  separate(col = price_1, into = c("price","extras"), sep = "/", remove = FALSE, convert = TRUE)

dim(sales_data)
head(sales_data)


## ------------------------------------------------
sales_data <- sales_data %>% 
  filter(!is.na(price) | !str_detect(sales, "[:digit:]")) %>% 
  replace_na(list(price = 0, extras = 0)) %>% 
  select(-price_1)

dim(sales_data)
head(sales_data)


## ------------------------------------------------
sales_data <- sales_data %>% 
  mutate(category = case_when(str_detect(product,"sausage") ~ "sausage",
                              str_detect(product,"^chicken$") ~ "chicken",
                              str_detect(product,"sandwich") ~ "sandwich",
                              str_detect(product,"fries|fry|poussin|masala|cheesy|poisson|^spicy$|spicy f.*") ~ "fries",
                              str_detect(product,"burger|/|bacon") ~ "burger",
                              str_detect(product,"sales") ~ "no sales",
                              TRUE ~ "pizza")) %>% 
  mutate(category = na_if(category,"NA"))


dim(sales_data)
head(sales_data)


## ------------------------------------------------
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

dim(sales_data)
head(sales_data)


## ------------------------------------------------
# Pizza price lists
pizza_large <- c(850,900,950,1000)
pizza_medium <- c(650,700,750,800)
pizza_small <- c(450,500,550,600)

# Modulo function. We'll use apply instead of a second for loop over the price vector.
# The idea is to give it a price and return a vector of results then take that and do apply()
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

dim(sales_data)
head(sales_data)

# Alternative approach
# Ref: https://www.youtube.com/watch?v=5lJX4IgmqRA
# mod_pizza_large <- lapply(X = price, FUN = mod_price, price_list = pizza_large)
# mod_pizza <- sapply(X = mod_pizza_large, FUN = function(x) min(unlist(x)))


## ------------------------------------------------
chicken_price <- c(200)

sales_data <- sales_data %>% 
  mutate(size = ifelse(test = category=="chicken", 
                       yes = paste0((price/chicken_price)*0.25,"kg"), 
                       no = size))

dim(sales_data)
head(sales_data)


## ------------------------------------------------
sales_data <- sales_data %>% 
  mutate(size = ifelse(!category %in% c("pizza","chicken"), category, size))

dim(sales_data)
head(sales_data)


## ------------------------------------------------
colnames(sales_data) <- str_to_title(colnames(sales_data))

sales_data <- sales_data %>%
  select(-Sales,-Product) %>% 
  rename(Pizza = Product_name) %>% 
  mutate(Revenue = Price + Extras) %>% 
  select(Date,Pizza,Size,Price,Extras,Revenue,Location,Category) %>% 
  mutate_if(.predicate = is.character, .funs = str_to_title)

dim(sales_data)
head(sales_data)


## ------------------------------------------------
sales_summary <- sales_data %>% 
  group_by(Year = year(Date), Month = month(Date)) %>% 
  summarise(revenue = sum(Revenue)) %>% 
  inner_join(sales_data %>%
              group_by(Year = year(Date)) %>% 
              summarise(revenue_yr = sum(Revenue)), by = "Year") %>% 
  ungroup()

(sales_summary)

# TABLE FOR INNER JOIN - SUMMARISE REVENUE BY YEAR
# sales_data %>%
#   group_by(Year = year(Date)) %>% 
#   summarise(revenue_yr = sum(Revenue))

## ------------------------------------------------
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
         estimate = round(estimate)) %>% 
  filter(term == "period") %>%
  select(estimate, sign)
    
sales_chart <- sales_summary_chart %>%
  ggplot(aes(x = period, y = revenue)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(breaks = breaks_x, labels = labels_x) + 
  scale_y_continuous(labels = dollar_format(prefix = "KES ")) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Sales Chart and Trend",
       subtitle = glue::glue("On average, growth has been {trend$sign} at a rate of KES {trend$estimate} per month"),
       x = "Period (Year.Month)",
       y = "Revenue")

sales_chart


## ------------------------------------------------
sales_summary %>% 
  mutate(Year = as.character(Year),
         Month = as.character(Month),
         revenue = scales::comma(revenue, accuracy = 0.01),
         revenue_yr = scales::comma(revenue_yr, accuracy = 0.01))


## ----message=FALSE-------------------------------
# Read all transaction files
expense_files <- list.files(path = expenses_path, pattern = "^Transactions.*", all.files = FALSE, full.names = FALSE, ignore.case = TRUE)

# Merge all files into list
expense_sets <- list()
for (e in seq_along(expense_files)){
  expense_sets[[e]] <- read_csv(reader::find.file(fn = expense_files[e], dir = expenses_path, dirs = getwd()))
}

# Bind all dataframes into one
expenses_df <- data.frame(Date = as.character(),
                          Category = as.character(),
                          Amount = as.numeric(),
                          Note = as.logical(),
                          Account = as.character())

for (e in seq_along(expense_files)){
  expenses_df <- rbind(expenses_df,expense_sets[[e]])
}

expenses_df$Date <- as.Date(expenses_df$Date, "%d/%m/%Y")


## ------------------------------------------------
# Read salaries and rent
fixed_costs <-  read_csv(file = paste0(expenses_path,"fixed_costs.csv"))

fixed_costs <- fixed_costs %>% 
  mutate(Datex = paste0("01-",Month,"-",Year),
         Date = as.Date(x = Datex,format = "%d-%B-%Y")) %>% 
  select(-(Datex))

# Bind fixed and variable costs
all_expenses <- bind_rows(expenses_df, fixed_costs) %>% 
  mutate(Amount = abs(Amount))


## ------------------------------------------------
expenses_summary <- all_expenses %>% 
  group_by(Year = year(Date),Month = month(Date)) %>% 
  summarise(expenses = sum(abs(Amount), na.rm = T)) %>% 
  left_join(all_expenses %>% 
                group_by(Year = year(Date)) %>% 
                summarise(expenses_yr = sum(Amount)), by = "Year")

head(expenses_summary)

# all_expenses %>% 
#   group_by(Year = year(Date)) %>% 
#   summarise(expenses_yr = sum(Amount))


## ------------------------------------------------
profit_summary <- inner_join(x = sales_summary, y = expenses_summary, by = c("Year","Month")) %>% 
  mutate(profit = revenue - expenses)

head(profit_summary)
View(profit_summary)


## ------------------------------------------------
# Export data
exp_file <- paste0("CF Data ",as.character(Sys.time()),".xlsx")
exp_file <- gsub(pattern = ":|-",replacement = "",x = exp_file)

write_xlsx(x = list(Sales = sales_data, Costs = all_expenses), 
           path = paste0(main_path,exp_file))


## ------------------------------------------------
this_sales <- read_excel(paste0(main_path,exp_file), sheet = "Sales") %>% 
  mutate(Date = as.Date(Date))

this_costs <- read_excel(paste0(main_path,exp_file), sheet = "Costs") %>% 
  mutate(Date = as.Date(Date)) %>% 
  select(Date,Category,Amount) %>% 
  rename(Item = Category,
         "Total cost" = Amount)

main_sales <- read_excel(paste0(main_path,"Company X Data.xlsx"), sheet = 'Sales') %>% 
  mutate(Price = as.double(Price),
         Date = as.Date(Date))

main_costs <- read_excel(paste0(main_path,"Company X Data.xlsx"), sheet = 'Costs') %>% 
  mutate(Date = as.Date(Date))

all_sales <- bind_rows(main_sales,this_sales) %>% 
  arrange(Date)

all_costs <- bind_rows(main_costs,this_costs) %>% 
  arrange(Date)

main_coords <- read_excel(paste0(main_path,"Company X Data.xlsx"), sheet = 'Coordinates') 


## ------------------------------------------------
now <- Sys.time()
now <- gsub(pattern = ":|-", replacement = "", x = now)
now_file <- paste0("Company X Data ",now,".xlsx")

write_xlsx(x = list(Sales = all_sales, Costs = all_costs, Coordinates = main_coords), 
           path = paste0(main_path,now_file))


## ------------------------------------------------
sales_data %>% 
  mutate(month = month(Date),
         year = year(Date)) %>% 
  group_by(Category,year,month) %>% 
  summarise(revenue = sum(Revenue),
            n = n()) %>%
  summarise(revenue = mean(revenue),
            units_pm = mean(n)) %>% 
  filter(year != 2021)

