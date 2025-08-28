import re
import pandas as pd
import numpy as np
import glob

def find_incorrect_symbols(text, alphabet):
    return list(re.finditer(f"[^{''.join(alphabet)} ]", text, flags=re.I))
    #\.,-–—=\?…«»“”\(\)\[\]<>

def convert_to_ortho(text : str, ortho : pd.DataFrame, correct_symbols=None, segment=False):
    if correct_symbols is None:
        correct_symbols = []
    for _, row in ortho[ortho["replace"] == "yes"].iterrows():
        text = re.sub(row["letter"], row["replace_to"], text, flags=re.I)

    correct_symbols = correct_symbols + sorted(ortho.replace_to)
    inc = find_incorrect_symbols(text, correct_symbols)

    text = re.sub(" ", " \t ", text)

    if segment:
        alphabet = "|".join(list(ortho.replace_to) + ["\t"])
        text = " ".join(re.findall(alphabet, text))
        return re.sub(" \t ", "  ", text)
    else:
        return re.sub(" \t ", " ", text)
      
unification_scheme = pd.read_csv("scripts/rutul_ortho_correspondencies.csv")
UNIFIED_ALPHABET = set("".join(unification_scheme.replace_to))

dataframe = pd.read_csv("data/database.csv")

dataframe["answer"] = dataframe["answer"].astype(str).apply(
        convert_to_ortho, ortho=unification_scheme, correct_symbols = ["-", "—"])
dataframe["value"] = dataframe["value"].astype(str).apply(
        convert_to_ortho, ortho=unification_scheme, correct_symbols = ["-", "—"])
 
dataframe.to_csv("data/database.csv", index=False)       
dataframe.to_excel("data/database.xlsx", index=False)

