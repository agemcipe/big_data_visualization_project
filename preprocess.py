import pandas as pd
import pathlib
import json
import tqdm


if __name__ == "__main__":

    print(f"Script {__file__} started")
    arxiv_json_filepath = "arxiv-metadata-oai-snapshot.json"

    data = []
    num_lines_in_file = sum(1 for _ in open(arxiv_json_filepath))
    num_lines_to_read = num_lines_in_file
    num_lines_to_read = 100_000

    assert 0 < num_lines_to_read <= num_lines_in_file

    with open(arxiv_json_filepath) as json_file:
        for i in tqdm.tqdm(range(num_lines_to_read)):
            _dict = json.loads(json_file.readline())
            data.append([_dict.get("id"), _dict.get("authors_parsed")])

    df = pd.DataFrame(data, columns=["id", "authors_parsed"])

    df = df.explode("authors_parsed")

    df["author_id"] = (
        df["authors_parsed"].map(lambda l: " ".join(l)).map(hash)
    )  # TODO: use pd.factorize instead
    df["author_name"] = df["authors_parsed"].map(lambda l: ", ".join(l))

    authors_df = df[["author_name", "author_id"]].drop_duplicates()

    df_combined = (
        pd.merge(df, df, how="inner", on="id")  # join df onto itself
        .pipe(lambda m_df: m_df.loc[m_df.author_id_x < m_df.author_id_y])
        .groupby(["author_id_x", "author_id_y"])[["id"]]
        .count()
        .rename({"id": "cnt_publications"}, axis=1)
        .sort_values(by="cnt_publications", ascending=False)
        .reset_index()
    )

    # write processed output
    output_dir = pathlib.Path(__file__).absolute().parent / "output"
    output_dir.mkdir(exist_ok=True)

    print(f"Writing output to {output_dir}")
    df_combined.to_csv(output_dir / "preprocessed_data.csv", header=True, index=False)
    authors_df.to_csv(output_dir / "authors.csv", header=True, index=False)
