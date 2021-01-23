import pandas as pd
import pathlib
from scholarly import scholarly, ProxyGenerator
import random
import multiprocessing


def fail_safe_search_author(search_term, verbose=False):
    empty_search = {
        "container_type": None,
        "filled": None,
        "source": None,
        "scholar_id": None,
        "url_picture": None,
        "name": None,
        "affiliation": None,
        "email_domain": None,
        "interests": None,
        "citedby": None,
    }
    res = empty_search
    if verbose:
        print(search_term)
    try:
        res = next(scholarly.search_author(search_term))
    except StopIteration:
        pass

    res["search_term"] = search_term
    return res


def append_dict_to_file(dict, output_file):
    pd.DataFrame(dict).to_csv(
        intermediate_dir / "scholar_scraping" / output_file, mode="a", header=False
    )


def process_chunk(author_name_ls):
    pg = ProxyGenerator()
    pg.Tor_External(
        tor_sock_port=9050, tor_control_port=9051, tor_password="scholarly_password"
    )
    scholarly.use_proxy(pg)

    try:
        scholar_data = [
            fail_safe_search_author(name, verbose=True) for name in author_name_ls
        ]
        append_dict_to_file(
            scholar_data, f"output_{random.randint(100_000, 999_999)}.csv"
        )
    except Exception as e:
        print(str(e))
        print(f"Author list was: {', '.join(author_name_ls)}")


if __name__ == "__main__":
    print("Script Started")
    intermediate_dir = pathlib.Path(__file__).absolute().parent / "intermediate"
    author_df = pd.read_csv(intermediate_dir / "authors.csv")

    assert intermediate_dir.exists()

    chunk_size = 100
    print("Chunking size:", chunk_size)
    author_ls = author_df["author_name"].values
    author_chunks = [
        author_ls[x : x + chunk_size] for x in range(0, len(author_ls), chunk_size)
    ]

    print("Starting Multiprocessing")
    with multiprocessing.Pool(20) as p:
        p.map(func=process_chunk, iterable=author_chunks)

