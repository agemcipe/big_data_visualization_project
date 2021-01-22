# %%
import re
import pandas as pd

# %%

df = pd.read_csv("intermediate/author_terms_count.csv")
#%%
clean_name = lambda s: re.sub("[^0-9a-zA-Z]+", "_", s)

# %%
for name in df["author_name"].unique()[:3]:
    sub_df = df[df.author_name == name]
    cl_name = clean_name(name)[:20]
    print(cl_name)
    output_file = f"intermediate/author_terms_count/{cl_name}.csv"
    sub_df[["term", "count"]].to_csv(output_file, index=False)
# %%
