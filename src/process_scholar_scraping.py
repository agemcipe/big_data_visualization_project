# %%
import pandas as pd
import pathlib

# %%
columns = [
    "container_type",
    "filled",
    "source",
    "scholar_id",
    "url_picture",
    "name",
    "affiliation",
    "email_domain",
    "interests",
    "citedby",
    "search_term",
]

# %%
intermediate_dir = pathlib.Path(__file__).absolute().parent / "intermediate"
scholar_scrape_dir = intermediate_dir / "scholar_scraping"

# %%
df = pd.DataFrame(columns = columns)
for csv_file in scholar_scrape_dir.glob("*.csv"):
    print(f"{csv_file.name}")
    df = df.append(pd.read_csv(csv_file, names = columns, header=None))

# %%
df[["search_term", "name", "affiliation"]].sample(5)

# %%
df["affiliation"].nunique() # very sparse...
# %%
df["affiliation"].value_counts().to_frame().head(20).plot(kind="barh")
# %%
df[~df["affiliation"].isnull()]["affiliation"].sample(20)
# %%

## join results to author df

author_df = pd.read_csv(intermediate_dir / "authors.csv")
# %%
joined = author_df.merge(df, left_on="author_name", right_on="search_term")
# joined = joined.drop("search_term", axis=1)
# %%
joined.to_csv( intermediate_dir/ "authors_joined.csv", header=True, index=False)
# %%
