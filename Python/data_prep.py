# %% [markdown]
# # Objective
# Prepare a monthly summary of sales, expenses, and profits.

# %%
import pandas as pd
from glob import glob
import os
from pprint import pprint
import re
from config import SALES_PATH, EXPENSES_PATH, DESKTOP_PATH

# %%
pd.options.display.max_columns = None
# pd.options.display.max_rows = None

# %%
# Price lists
pizza_large = (850, 900, 950, 1000)
pizza_medium = (650, 700, 750, 800)
pizza_small = (450, 500, 550, 600)

# %% [markdown]
# # Revenue
# ## File paths

# %%
sales_files = glob(pathname=os.path.join(SALES_PATH, "*.xlsx"))
expense_files = glob(pathname=os.path.join(EXPENSES_PATH, "*.csv"))

# %%
print(sales_files[0])
print(sales_files[12])
sales_files[:10]

# %% [markdown]
# ## Read worksheets

# %%
# All Excel sheets have either Sheet1 or KK/KB.

sales_df = pd.DataFrame()

for path in sales_files:
    try:
        temp_dict = pd.read_excel(path, sheet_name=["Sheet1"], names=["data"])
        temp_df = temp_dict.get("Sheet1")
        temp_df["branch"] = "KK"
    except:
        temp_dict = pd.read_excel(
            path, sheet_name=["KK", "KB"], names=["data"]
        )
        temp_kb = temp_dict.get("KB")
        temp_kb["branch"] = "KB"
        temp_kk = temp_dict.get("KK")
        temp_kk["branch"] = "KK"
        temp_df = pd.concat(objs=[temp_kb, temp_kk], axis=0)

    sales_df = pd.concat(objs=[sales_df, temp_df], axis=0)

sales_df.shape

# %%
def read_unnamed_sheet(path: str) -> pd.DataFrame:
    """Reads unnamed worksheets i.e. Sheet1."""
    temp_dict = pd.read_excel(
        path, sheet_name=["Sheet1"], names=["data"], header=None
    )
    temp_df = temp_dict.get("Sheet1")
    temp_df["branch"] = "KK"
    temp_df["file_name"] = path
    return temp_df


def read_named_sheets(path: str) -> pd.DataFrame:
    """Reads the named worksheets i.e. either KB or KK."""
    temp_dict = pd.read_excel(
        path, sheet_name=["KK", "KB"], names=["data"], header=None
    )
    temp_kb = temp_dict.get("KB")
    temp_kb["branch"] = "KB"
    temp_kb["file_name"] = path
    temp_kk = temp_dict.get("KK")
    temp_kk["branch"] = "KK"
    temp_kk["file_name"] = path
    temp_df = pd.concat(objs=[temp_kb, temp_kk], axis=0)
    return temp_df


# %%
sales_df = pd.DataFrame()

for path in sales_files:
    try:
        temp_df = read_unnamed_sheet(path)
    except:
        temp_df = read_named_sheets(path)

    sales_df = pd.concat(objs=[sales_df, temp_df], axis=0)

sales_df.shape

# %%
sales_df.head(10)

# %% [markdown]
# ## Extract prices

# %%
price_rgx = r"\w+-(\d+)"
revenue = sales_df["data"].str.extract(pat=price_rgx).astype(float)
sales_df["revenue"] = revenue
sales_df.head(10)

# %% [markdown]
# ## Extract dates

# %%
months = sales_df["file_name"].str.extract(".*?(\w+\d+.xlsx)")
sales_df["months"] = months
sales_df["months"] = sales_df.months.str.replace(".xlsx", "", regex=False)
sales_df.head(10)

# %%
days = sales_df.data.str.extract("^(\d+)")
sales_df["days"] = days
sales_df["days"] = sales_df.days.fillna(method="ffill")
sales_df.head(10)

# %%
sales_df["sales_day"] = sales_df.days + sales_df.months
sales_df.head(10)

# %%
sales_df["date"] = pd.to_datetime(sales_df.sales_day, format="%d%b%Y")
sales_df["year_mon"] = sales_df.date.dt.strftime("%Y%m")
sales_df

# %% [markdown]
# ## Sales summary

# %%
sales_summary = sales_df.groupby(
    by=["branch", "year_mon"], as_index=False
).aggregate(sales=("revenue", "sum"))

sales_summary.head(10)

# %% [markdown]
# # Expenses
# ## File paths

# %%
expense_files = glob(pathname=os.path.join(EXPENSES_PATH, "*.csv"))
kabete_files = [path for path in expense_files if "KB" in path]
kikuyu_files = [path for path in expense_files if "KB" not in path]

# %% [markdown]
# ## Read files

# %%
kabete_df = pd.DataFrame()
for path in kabete_files:
    temp_df = pd.read_csv(path)
    kabete_df = pd.concat(objs=[kabete_df, temp_df], axis=0)
    kabete_df["branch"] = "KB"

kikuyu_df = pd.DataFrame()
for path in kikuyu_files:
    temp_df = pd.read_csv(path)
    kikuyu_df = pd.concat(objs=[kikuyu_df, temp_df], axis=0)
    kikuyu_df["branch"] = "KK"

expenses_df = pd.concat(objs=[kabete_df, kikuyu_df], axis=0)
expenses_df = expenses_df.loc[:, :"branch"]
expenses_df.columns = expenses_df.columns.str.strip()
expenses_df["Amount"] = expenses_df.Amount.abs()
expenses_df

# %%
pd.to_datetime(expenses_df.Date, format="%d/%m/%Y")
expenses_df["Date"] = pd.to_datetime(expenses_df.Date, format="%d/%m/%Y")
expenses_df["year_mon"] = expenses_df.Date.dt.strftime("%Y%m")
expenses_df

# %% [markdown]
# ## Expenses summary

# %%
expenses_summary = expenses_df.groupby(
    by=["branch", "year_mon"], as_index=False
).aggregate(costs=("Amount", "sum"))

expenses_summary.head(10)

# %% [markdown]
# # Profit summary
# ## By branch

# %%
profit_summary = pd.merge(
    left=sales_summary,
    right=expenses_summary,
    on=["branch", "year_mon"],
    how="left",
)
profit_summary["profit"] = profit_summary.sales - profit_summary.costs
profit_summary.head(10)

# %% [markdown]
# ## All branches

# %%
overall_performance = profit_summary.groupby(
    by="year_mon", as_index=False
).aggregate(profit=("profit", "sum"))

overall_performance.head(10)

# %% [markdown]
# ## By year

# %%
profit_summary.head(10)
profit_summary["year"] = list(map(lambda x: x[:4], profit_summary.year_mon))
profit_summary.head(10)

# %%
yearly_profit = profit_summary.groupby(by="year", as_index=False).aggregate(
    yearly_profit=("profit", "sum")
)
yearly_profit

# %% [markdown]
# # Product info
# ## Product categories

# %%
products = sales_df.data.str.split("-", expand=True).iloc[:, :1]
products.columns = ["sale"]
products["sale"] = products["sale"].str.strip().str.lower()
products = pd.concat(objs=[products, sales_df.revenue], axis=1)
products

# %%
def str_detect(pattern: str, word: str, category: str) -> bool:
    word = str(word).strip().lower()
    if re.search(
        pattern=pattern, string=word
    ):  # re.search works better than re.match
        return category
    return word


products.sale.apply(
    lambda x: str_detect(pattern="fries", word=x, category="Fries")
)[:10]

# %%
fries_rgx = r"fr|asala|poisson|poussin|bhaj|cheesy|chilli|^spicy$"
burger_rgx = r"burg|bacon|original|egg|\w/\w/\w|bce|^cheese$"
sandwich_rgx = r"sand|grilled|and cheese|beef|chicken chicken"
pizza_rgx = r"meat|haw|delux|peri|bbq|sweet|steak|pep|spicy chicken|marg|magh|fest|s and s"

products["category"] = products.sale.apply(
    lambda x: str_detect(pattern="^\d", word=x, category=pd.NA)
)
products["category"] = products.category.apply(
    lambda x: str_detect(pattern=fries_rgx, word=x, category="fries")
)
products["category"] = products.category.apply(
    lambda x: str_detect(pattern=burger_rgx, word=x, category="burgers")
)
products["category"] = products.category.apply(
    lambda x: str_detect(pattern=sandwich_rgx, word=x, category="sandwiches")
)
products["category"] = products.category.apply(
    lambda x: str_detect(pattern="dog", word=x, category="hot dogs")
)
products["category"] = products.category.apply(
    lambda x: str_detect(pattern="^chicken$", word=x, category="chicken")
)
products["category"] = products.category.apply(
    lambda x: str_detect(pattern="melon", word=x, category="juices")
)
products["category"] = products.category.apply(
    lambda x: str_detect(pattern="sales", word=x, category="no sales")
)
products["category"] = products.category.apply(
    lambda x: str_detect(pattern="sausage", word=x, category="sausages")
)
products["category"] = products.category.apply(
    lambda x: str_detect(pattern=pizza_rgx, word=x, category="pizza")
)
products["category"] = products.category.replace("nan", pd.NA)

products.groupby(by="category", as_index=False).aggregate(
    avg_price=("revenue", "mean"), count=("category", "count")
)

# %% [markdown]
# ## Pizza sizes
# ### Modulo function

# %%
1000 % 400  # Modulo
1000 // 400  # Floor division

"""
Pseudo code:
- Provide a list of pizza prices for large
- Loop through the products and find the modulo
- Store each modulo in a list for each product (in a column)
- Check if any of the results are 0
- If so, that pizza is large
"""


def mod_price(x: float, price_list: tuple, size: str) -> str:
    mod_list = list()
    for price in price_list:
        mod = x % price
        mod_list.append(mod)

    if 0 in mod_list:
        return size
    return False


mod_price(600, pizza_large, size="Large")
products.revenue.apply(
    lambda x: mod_price(x, price_list=pizza_large, size="Large")
).value_counts()

# %%
products = products.assign(
    mod_large=products.revenue.apply(
        lambda x: mod_price(x, price_list=pizza_large, size="large")
    ),
    mod_medium=products.revenue.apply(
        lambda x: mod_price(x, price_list=pizza_medium, size="medium")
    ),
    mod_small=products.revenue.apply(
        lambda x: mod_price(x, price_list=pizza_small, size="small")
    ),
)

# %% [markdown]
# ### Refine #1

# %%
"""
- If all sizes are False, return TBC
- If any of the size is large, return Large
- If may of the size if medium but large is not there, return Medium
- Else, return Small
"""

pizza_sizes = []
size_results = zip(products.mod_large, products.mod_medium, products.mod_small)
for size in size_results:
    if any(size) == False:
        pizza_sizes.append(False)
    elif "large" in size:
        pizza_sizes.append("large")
    elif "medium" in size and "large" not in size:
        pizza_sizes.append("medium")
    else:
        pizza_sizes.append("small")

pizza_sizes[:10]

# %%
products = products.assign(pizza_sizes=pizza_sizes)

products.head(10)

# %% [markdown]
# ### Refine #2

# %%
products.query("category=='pizza' & pizza_sizes==False").revenue.value_counts()
# products.query("category=='pizza' & pizza_sizes==False & revenue<1000")

# %%
"""
- if the category is not pizza, return category
- if category is pizza and size is False:
    - If price >= min large, then Large
    - If price < min large but >= min medium, then medium
    - Else, small
- otherwise return size
"""

pizza_sizes2 = []
for size, price, category in zip(
    products.pizza_sizes, products.revenue, products.category
):
    if category is not "pizza":
        pizza_sizes2.append(category)
    elif (category is "pizza") and (size is False):
        if price >= min(pizza_large):
            pizza_sizes2.append("large")
        elif price < min(pizza_large) and price >= min(pizza_medium):
            pizza_sizes2.append("medium")
        else:
            pizza_sizes2.append("small")
    else:
        pizza_sizes2.append(size)

pizza_sizes2[:10]

# %%
products["size"] = pizza_sizes2
products.head(10)

# %% [markdown]
# ### Drop columns

# %%
products.columns.str.startswith(("mod", "pizza"))
products.columns[products.columns.str.startswith(("mod", "pizza"))]
products_final = products.drop(
    columns=products.columns[
        products.columns.str.startswith(("mod", "pizza", "revenue"))
    ]
)
products_final.head(10)

# %%
pd.concat(objs=[sales_df, products_final], axis=1).head(10)

# %% [markdown]
# # Export

# %%
# with pd.ExcelWriter(path=os.path.join(DESKTOP_PATH, "CF_Summary.xlsx")) as writer:
#     profit_summary.to_excel(excel_writer=writer, sheet_name="profit_summary", index=False)
#     overall_performance.to_excel(excel_writer=writer, sheet_name="overall_performance", index=False)
#     yearly_profit.to_excel(excel_writer=writer, sheet_name="yearly_profit", index=False)
