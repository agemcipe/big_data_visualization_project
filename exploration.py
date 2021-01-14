# %%
import pandas as pd
import numpy as np
import pathlib
import json
import tqdm

# %%
print(f"Script {__file__} started")
arxiv_json_filepath = "input/arxiv-metadata-oai-snapshot.json"

data = []
num_lines_in_file = sum(1 for _ in open(arxiv_json_filepath))
num_lines_to_read = num_lines_in_file
num_lines_to_read = 10_000

assert 0 < num_lines_to_read <= num_lines_in_file

with open(arxiv_json_filepath) as json_file:
    for i in tqdm.tqdm(range(num_lines_to_read)):
        _dict = json.loads(json_file.readline())
        data.append([_dict.get("id"), _dict.get("authors_parsed")])
# %%
df = pd.DataFrame(data, columns=["id", "authors_parsed"])


# %%
df = df.explode("authors_parsed")
df = df.reset_index(drop=True)

df["author_name"] = df["authors_parsed"].map(lambda l: " ".join(l))

# factorize
df["author_id"] = pd.factorize(df["author_name"])[0]

authors_df = df[["author_name", "author_id"]].drop_duplicates()

# %%
# TODO: investigate "cleanness" of author_name
a_df = authors_df.copy()

a_df["author_name_len"] = a_df["author_name"].str.len()
a_df.sort_values("author_name_len").head()

a_df["author_name_len"].hist(bins=list(range(40)))


# %%
# can we filter out authors that are not relevant
ac_df = pd.DataFrame(df.groupby("author_name")["id"].count()).reset_index()

ac_df["id"].hist(bins=list(range(10)))  # authors that have never worked with anybody?

# TODO: papers with only one contributor

# %%
combined_df = (
    pd.merge(df, df, how="inner", on="id")  # join df onto itself
    .pipe(lambda m_df: m_df.loc[m_df.author_id_x < m_df.author_id_y])
    .groupby(["author_id_x", "author_id_y"])[["id"]]
    .count()
    .rename({"id": "cnt_publications"}, axis=1)
    .sort_values(by="cnt_publications", ascending=False)
    .reset_index()
)

# %%
_cols = ["author_id", "num_collaborators"]
x_df = pd.DataFrame(
    combined_df.groupby("author_id_x")["author_id_y"].nunique()
).reset_index()
y_df = pd.DataFrame(
    combined_df.groupby("author_id_y")["author_id_x"].nunique()
).reset_index()

x_df.columns = y_df.columns = _cols

z_df = x_df.append(y_df)

z_df.groupby("author_id")["num_collaborators"].sum().hist(
    bins=list(range(20))
)  # number of collaborators...

# %%
# write processed output
output_dir = pathlib.Path(__file__).absolute().parent / "output"
output_dir.mkdir(exist_ok=True)

print(f"Writing output to {output_dir}")
combined_df.to_csv(output_dir / "preprocessed_data.csv", header=True, index=False)
authors_df.to_csv(output_dir / "authors.csv", header=True, index=False)
