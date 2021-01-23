import pandas as pd
import pathlib
import json
import tqdm


FIELDS_TO_EXTRACT = ["id", "authors_parsed", "categories"]


def load_arxiv_json_to_df(filepath, nrows, offset=0) -> pd.DataFrame:

    data = []

    nrows_in_file = sum(1 for _ in open(filepath))

    assert (
        offset < nrows_in_file
    ), "Cannot start reading from line {offset} in file with only {nrows_in_file} rows"

    with open(filepath) as json_file:
        # skip first offset rows
        if offset > 0:
            for j in range(offset - 1):
                next(json_file)

        for i in tqdm.tqdm(range(min(nrows, nrows_in_file))):
            _dict = json.loads(json_file.readline())
            data.append([_dict.get(field_name) for field_name in FIELDS_TO_EXTRACT])

    # turn list of authors into individual rows
    df = (
        pd.DataFrame(data, columns=FIELDS_TO_EXTRACT)
        .explode("authors_parsed")
        .reset_index(drop=True)
    )

    # create raw author name
    df["author_name"] = df["authors_parsed"].map(lambda l: " ".join(l).strip())

    df = df.drop(columns="authors_parsed")

    # filter unclean author names
    df = df[(2 < df["author_name"].str.len()) & (df["author_name"].str.len() < 40)]

    # assign author id
    # df["author_id"] = pd.factorize(df["author_name"])[0]
    return df


def create_author_df(df):

    author_df = (
        df.assign(categories=lambda df: df["categories"].str.split(" "))
        .explode("categories")
        .rename(columns={"categories": "category_specific"})
    )

    author_df["category_main"] = (
        author_df["category_specific"].str.split(".").map(lambda x: x[0])
    )

    # total number of publications per author
    author_df = (
        author_df.groupby(["author_name", "category_specific", "category_main"])["id"]
        .nunique()
        .reset_index()
        .rename(columns={"id": "cnt_publications"})
    )

    author_df = author_df[
        ["author_name", "category_main", "category_specific", "cnt_publications"]
    ]

    return author_df


def create_link_df(df):
    # df = df[["id", "author_id"]]
    df = df[["id", "author_name"]]
    link_df = (
        pd.merge(df, df, how="inner", on="id")  # join df onto itself
        # .pipe(lambda m_df: m_df.loc[m_df["author_id_x"] < m_df["author_id_y"]])
        # .groupby(["author_id_x", "author_id_y"])[["id"]]
        .pipe(lambda m_df: m_df.loc[m_df["author_name_x"] < m_df["author_name_y"]])
        .groupby(["author_name_x", "author_name_y"])[["id"]]
        .count()
        .rename({"id": "cnt_publications"}, axis=1)
        .sort_values(by="cnt_publications", ascending=False)
        .reset_index()
    )

    return link_df

