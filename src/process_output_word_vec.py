import re
import pandas as pd

# %%

df = pd.read_csv("intermediate/author_terms_count.csv")
#%%
clean_name = lambda s: re.sub("[^0-9a-zA-Z]+", "_", s)

# %%
for name in df["author_name"].unique()[9:]:
    sub_df = df[df.author_name == name].sort_values(by="count", ascending=False)
    cl_name = clean_name(name.strip())
    output_file = f"intermediate/author_terms_count/{cl_name}.csv"
    try:
        sub_df[["term", "count"]].to_csv(output_file, index=False)
    except:
        print(name)
        print(cl_name)
        print("--------------------")
