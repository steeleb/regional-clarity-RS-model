"""Spatial holdout CV harness matching 03_split_data.Rmd / 04_make_models.Rmd:
5 HUC4-grouped partitions, leave-one-partition-out CV across partitions
1-4, partition 5 held out entirely as the untouched test set. No row ever
crosses from train to val/test within a fold because partitions are whole
HUC4 groups, not random rows - this is what keeps the spatial holdout
honest (no nearby-site leakage between train and validation)."""
from dataclasses import dataclass

import pandas as pd

TARGET = "harmonized_value"
TEST_PART = 5
CV_PARTS = [1, 2, 3, 4]


@dataclass
class Fold:
    part: int
    train: pd.DataFrame
    val: pd.DataFrame


def load(path: str) -> pd.DataFrame:
    df = pd.read_parquet(path)
    assert set(df["part"].unique()) >= set(CV_PARTS + [TEST_PART]), \
        f"expected parts 1-5, got {sorted(df['part'].unique())}"
    return df


def train_val_test_split(df: pd.DataFrame):
    train_val = df[df["part"] != TEST_PART].reset_index(drop=True)
    test = df[df["part"] == TEST_PART].reset_index(drop=True)
    return train_val, test


def build_folds(train_val: pd.DataFrame) -> list:
    """Leave-one-partition-out folds over partitions 1-4, mirroring
    04_make_models.Rmd's train1/val1 ... train4/val4."""
    folds = []
    for held_out in CV_PARTS:
        train = train_val[train_val["part"] != held_out].reset_index(drop=True)
        val = train_val[train_val["part"] == held_out].reset_index(drop=True)
        folds.append(Fold(part=held_out, train=train, val=val))
    return folds
