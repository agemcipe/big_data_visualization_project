import pandas as pd
import pathlib
import json
import tqdm
import shutil

import utils

HERE = pathlib.Path(__file__).absolute().parent
ROOT = HERE.parent

nrows_per_batch = 20_000
num_batches = 10
starting_offset = 0
save_intermediate_files = False

if __name__ == "__main__":

    print(f"Script {__file__} started")

    arxiv_json_filepath = (
        ROOT / "data" / "raw_input" / "arxiv-metadata-oai-snapshot.json"
    )
    assert arxiv_json_filepath.exists()

    base_output_dir = ROOT / "data" / "intermediate" / "arxiv_parsing"
    intermediate_output_dir = base_output_dir / "intermediate"

    if not base_output_dir.exists():
        print("Creating base output directory", base_output_dir)
        base_output_dir.mkdir(parents=True)

    if save_intermediate_files:

        intermediate_output_dir.mkdir(exist_ok=True)
        existing_sub_dirs = list(intermediate_output_dir.glob("*"))

        if existing_sub_dirs:
            print(
                f"Warning! Subdirectories are already existing in {intermediate_output_dir}"
            )
            overwrite_choice = ""
            while overwrite_choice not in ["Y", "N"]:
                overwrite_choice = input("Do you want to remove them? 'Y' or 'N' ")
                if overwrite_choice == "Y":
                    print("Removing subdirectories")
                    for d in existing_sub_dirs:
                        shutil.rmtree(d)

    authors_df = None
    links_df = None
    offset = starting_offset
    for i in range(num_batches):

        print(
            f"Parsing rows {offset} - {offset + nrows_per_batch} from file to DataFrame"
        )
        try:
            df = utils.load_arxiv_json_to_df(
                arxiv_json_filepath, nrows_per_batch, offset
            )
        except AssertionError:
            # reached offset greater than rows in file
            break
        print("Creating author DataFrame")
        _author_df = utils.create_author_df(df)
        print("Number of unique authors:", len(_author_df))
        if authors_df is None:
            authors_df = _author_df.copy()
        else:
            authors_df = authors_df.append(_author_df)

        print("Creating link DataFrame")
        _link_df = utils.create_link_df(df)
        print("Number of links:", len(_link_df))
        if links_df is None:
            links_df = _link_df.copy()
        else:
            links_df = links_df.append(_link_df)

        if save_intermediate_files:
            output_dir = (
                intermediate_output_dir / f"{offset}_{offset + nrows_per_batch}"
            )
            output_dir.mkdir(exist_ok=True)
            _author_df.to_csv(output_dir / "authors.csv", header=True, index=False)
            _link_df.to_csv(output_dir / "links.csv", header=True, index=False)

        # increase offset
        offset += nrows_per_batch

        print("---------------------------------------")

    print("Done Batch Processing")
    print("Aggregating dataframes")

    file_name_ext = f"{starting_offset}_{offset}"

    authors_df = (
        authors_df.groupby(["author_name", "category_main", "category_specific"])
        .agg({"cnt_publications": "sum"})
        .reset_index()
    )
    authors_df.to_csv(
        base_output_dir / f"authors_not_aggregated_{file_name_ext}.csv",
        header=True,
        index=False,
    )

    # aggregate author df
    authors_df["total_cnt_publications"] = authors_df.groupby(["author_name"])[
        "cnt_publications"
    ].transform(lambda x: x.sum())

    authors_df = (
        authors_df.groupby(["author_name", "total_cnt_publications"])[
            ["category_main", "category_specific"]
        ]
        .apply(pd.DataFrame.mode)
        .reset_index()
    )

    authors_df = authors_df[
        ["author_name", "category_main", "category_specific", "total_cnt_publications"]
    ]

    # we might still have duplicates since mode can return multiple rows per group
    authors_df = authors_df.drop_duplicates(subset=["author_name"])

    authors_df.to_csv(
        base_output_dir / f"authors_starting_{file_name_ext}.csv",
        header=True,
        index=False,
    )
    print("Total number of unique authors:", len(authors_df))

    links_df = (
        links_df.groupby(["author_name_x", "author_name_y"])["cnt_publications"]
        .sum()
        .reset_index()
    )

    links_df.to_csv(
        base_output_dir / f"links_{file_name_ext}.csv", header=True, index=False
    )
    print("Total number of links:", len(links_df))
