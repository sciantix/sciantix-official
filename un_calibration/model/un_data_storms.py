"""
Fission Gas Release — Table 1 dataset extraction.

Source: Table 1 "Summary of fission gas release parameters and calculated fuel
temperatures" (UN and U-Pu-N nitride fuels), 134 numbered rows in the original
paper.

Columns
-------
    id                 : original row number from the paper (1..134; see note)
    designation        : experiment / rod identifier (asterisk stripped)
    material           : "UN" (rows 1-95) or "U-Pu-N" (rows 96-134)
    release            : fission gas release [%]
    burnup             : burnup [% FIMA — to be confirmed against paper text]
    TD                 : fraction of theoretical density [%]
    T_Ross             : calculated fuel temperature [K], Ross model (ref. [3])
    T_Baars            : calculated fuel temperature [K], Baars model (ref. [1])
    T_Thomas           : calculated fuel temperature [K], Thomas model (ref. [2])
    excluded_from_fit  : True if the original table marked the row with (*),
                         meaning "Points not used to obtain eq. (7)" per the
                         footnote on the third page.

⚠  MISSING ROW 51
    The first table page in the screenshots ends at id=50 (PW-610-W-27 T*) and
    the second page resumes at id=52 (PW-610-W-28 M*). Row 51 falls on the
    page break and is NOT in this extraction. Fill it in once recovered.

OCR-correction notes (verify against original PDF if possible):
  - id   8: "PBRF 503I"      [OCR read "5031"]
  - id  34: "PW 26-241-136"  [OCR read "PW 36-241-136"]
  - id  65: "ORNL-57-656-T*" [missing dash in OCR]
  - id 107: "BMI B-3-6"      [OCR read "BMI D-3-6"]
  - id 126: "BMI-50-8 B28"   [missing dashes in OCR]
  - ids 111 & 112 are both labelled "EBR-K-4" with different burnup/TD
    values — preserved verbatim; check the source for a missing suffix.
"""

from __future__ import annotations

from pathlib import Path
from typing import Optional
import pandas as pd


# ----------------------------------------------------------------------------
# Raw records: (id, designation_with_asterisk_marker, release, burnup, TD,
#               T_Ross, T_Baars, T_Thomas)
# - "id" is the row number printed in the original paper.
# - Asterisk "*" suffix on the designation == flagged in source as "(*)".
# - id=51 is intentionally absent (see module docstring).
# ----------------------------------------------------------------------------
RAW_RECORDS: list[tuple[int, str, float, float, float,
                        Optional[int], Optional[int], Optional[int]]] = [
    # --- UN section ----------------------------------------------------------
    (  1, "PBRF 504B*",       0.02,  0.47,  94.0,  None, None, 1324),
    (  2, "PBRF 503B*",       0.05,  0.89,  94.0,  None, None, 1265),
    (  3, "PBRF 503C*",       0.02,  0.92,  94.0,  None, None, 1256),
    (  4, "PBRF 501F*",       0.10,  1.36,  94.0,  None, None, 1313),
    (  5, "PBRF 503D*",       0.09,  1.46,  94.0,  None, None, 1272),
    (  6, "PBRF 503E*",       0.09,  1.46,  94.0,  None, None, 1297),
    (  7, "PBRF 502A*",       0.03,  0.40,  94.0,  None, None, 1270),
    (  8, "PBRF 503I",        1.29,  2.16,  94.0,  None, None, 1334),
    (  9, "PBRF 503G",        0.18,  2.34,  94.0,  None, None, 1353),
    ( 10, "PBRF 505E",        0.22,  1.85,  94.0,  None, None, 1322),
    ( 11, "PBRF 505F",        1.91,  1.85,  94.0,  None, None, 1357),
    ( 12, "PBRF 507D",        0.22,  1.94,  94.0,  None, None, 1309),
    ( 13, "PBRF 509A*",       0.09,  1.88,  94.0,  None, None, 1345),
    ( 14, "PBRF 509B",        0.13,  1.88,  94.0,  None, None, 1277),
    ( 15, "PBRF 505A",        0.15,  0.22,  94.0,  None, None, 1170),
    ( 16, "PW 26-201-40",     3.50,  0.57,  90.8,  1533, None, 1564),
    ( 17, "PW 26-201-41",     5.10,  0.72,  90.3,  1533, None, 1575),
    ( 18, "PW 26-210-108",    3.00,  0.42,  96.1,  1575, None, 1537),
    ( 19, "PW 26-210-109",    2.50,  0.42,  96.1,  1630, None, 1597),
    ( 20, "PW 26-210-110",    2.50,  0.40,  96.2,  1630, None, 1588),
    ( 21, "PW 26-220-118",    4.90,  0.95,  94.1,  1561, 1618, 1505),
    ( 22, "PW 26-220-119",   12.00,  0.95,  94.3,  1714, 1766, 1659),
    ( 23, "PW 26-220-121",    4.30,  0.96,  94.0,  1589, 1646, 1508),
    ( 24, "PW 26-230-123",    0.53,  0.58,  96.4,  1727, None, 1710),
    ( 25, "PW 26-230-124",    0.88,  0.63,  96.0,  1825, None, 1800),
    ( 26, "PW 26-230-125",    0.68,  0.61,  95.7,  1714, None, 1697),
    ( 27, "PW 26-231-126",   10.50,  1.94,  96.0,  1714, 1795, 1688),
    ( 28, "PW 26-231-127*",  30.80,  1.91,  96.1,  1839, 1936, 1807),
    ( 29, "PW 26-231-128",   11.70,  2.02,  96.2,  1769, 1857, 1737),
    ( 30, "PW 26-240-132*",  18.90,  1.51,  95.0,  1700, 1779, 1660),
    ( 31, "PW 26-240-133*",  36.90,  1.61,  94.9,  1783, 1863, 1736),
    ( 32, "PW 26-240-144*",  21.20,  1.58,  94.4,  1700, 1800, 1696),
    ( 33, "PW 26-241-135",    5.80,  0.78,  94.9,  1797, 1914, 1765),
    ( 34, "PW 26-241-136",    3.90,  0.74,  94.7,  1839, None, 1806),
    ( 35, "PW 26-241-137",    4.00,  0.72,  94.3,  1755, None, 1735),
    ( 36, "PW 200-42*",       0.08,  0.06,  90.4,  None, None, 1211),
    ( 37, "PW 200-43*",       0.09,  0.06,  91.0,  None, None, 1312),
    ( 38, "PW-600-W-11 T*",   0.10,  0.19,  97.8,  1380, None, 1390),
    ( 39, "PW-600-W-12 M*",   0.10,  0.18,  96.2,  1505, None, 1515),
    ( 40, "PW-600-W-13 B*",   0.10,  0.18,  95.5,  1519, None, 1528),
    ( 41, "PW-601-W-14 T*",   0.10,  0.39,  96.7,  1436, None, 1420),
    ( 42, "PW-601-W-15 M",    0.20,  0.39,  96.2,  1491, None, 1492),
    ( 43, "PW-601-W-16 B",    0.20,  0.40,  95.6,  1491, None, 1478),
    ( 44, "PW-602-W-17 T*",   0.02,  0.97,  95.4,  None, 1312, 1300),
    ( 45, "PW-602-W-18 M",    0.20,  0.91,  95.6,  None, 1454, 1444),
    ( 46, "PW 602-W-19 B*",   0.08,  0.86,  94.8,  None, None, 1414),
    ( 47, "PW-603-W-20 T*",   0.05,  0.74,  96.2,  1297, None, 1288),
    ( 48, "PW-603-W-21 M*",   0.10,  0.73,  95.8,  1450, 1492, 1439),
    ( 49, "PW-603-W-22 B*",   0.06,  0.71,  95.5,  None, None, 1410),
    ( 50, "PW-610-W-27 T*",   0.005, 0.16,  96.7,  None, None, 1377),
    # --- id 51 missing — page break between the two screenshots ---
    ( 52, "PW-610-W-28 M*",   0.005, 0.16,  95.9,  None, None, 1472),
    ( 53, "PW-610-W-29 B*",   0.005, 0.17,  95.4,  None, None, 1463),
    ( 54, "PW-26-613-W-47*",  0.04,  0.54,  96.4,  1394, None, 1421),
    ( 55, "PW-26-613-W-44*",  0.06,  0.53,  96.0,  1505, 1515, 1529),
    ( 56, "PW-26-613-W-46*",  0.03,  0.58,  95.9,  1408, None, 1446),
    ( 57, "PW-26-640-WL-25*", 0.04,  0.42,  94.3,  None, None, 1527),
    ( 58, "PW-26-630-WL-2*",  0.06,  0.25,  95.0,  None, None, 1510),
    ( 59, "ORNL-57-612-T*",   0.05,  2.54,  95.0,  1338, None, 1298),
    ( 60, "ORNL-57-612-M",    0.37,  2.54,  95.0,  1453, None, 1400),
    ( 61, "ORNL-57-612-B*",   0.05,  2.54,  95.0,  1383, None, 1341),
    ( 62, "ORNL-57-642",      0.34,  1.18,  95.0,  1498, 1502, 1493),
    ( 63, "ORNL-57-649*",     0.04,  1.43,  95.0,  1488, 1492, 1479),
    ( 64, "ORNL-57-652-M*",   0.09,  0.70,  95.0,  None, None, 1516),
    ( 65, "ORNL-57-656-T*",   0.03,  0.60,  95.0,  None, None, 1504),
    ( 66, "ORNL-57-656-M",    0.11,  0.60,  95.0,  None, None, 1504),
    ( 67, "ORNL-57-656-B",    0.13,  0.60,  95.0,  None, None, 1504),
    ( 68, "ORNL-57-658-M",    0.14,  1.08,  95.0,  1503, None, 1495),
    ( 69, "ORNL-57-658-B*",   0.09,  1.08,  95.0,  1483, 1504, 1475),
    ( 70, "ORNL-57-660-M*",   0.05,  0.70,  95.0,  None, None, 1546),
    ( 71, "ORNL-57-662-T*",   0.01,  1.16,  95.0,  1388, None, 1380),
    ( 72, "ORNL-57-662-M*",   0.08,  1.16,  95.0,  1523, 1522, 1516),
    ( 73, "ORNL-57-662-B*",   0.04,  1.16,  95.0,  1513, None, 1506),
    ( 74, "ORNL-57-664-T*",   0.01,  0.58,  95.0,  1418, None, 1414),
    ( 75, "ORNL-57-664-M*",   0.01,  0.58,  95.0,  1463, None, 1454),
    ( 76, "ORNL-57-664-B*",   0.01,  0.58,  95.0,  1478, None, 1469),
    ( 77, "ORNL-57-665-T",    1.36,  4.58,  95.0,  1323, 1337, 1286),
    ( 78, "ORNL-57-665-M",    5.96,  4.58,  95.0,  None, None, 1388),
    ( 79, "ORNL-57-665-B",    1.78,  4.58,  95.0,  1373, None, 1330),
    ( 80, "ORNL-57-667-T*",   0.01,  0.78,  95.0,  1438, None, 1429),
    ( 81, "ORNL-57-667-M*",   0.02,  0.78,  95.0,  1498, None, 1490),
    ( 82, "ORNL-57-667-B*",   0.03,  0.78,  95.0,  1503, None, 1495),
    ( 83, "ORNL-57-669-T*",   0.05,  2.72,  95.0,  1213, None, 1196),
    ( 84, "ORNL-57-669-M",    0.13,  2.72,  95.0,  1348, 1365, 1338),
    ( 85, "ORNL-57-669-B*",   0.02,  2.72,  95.0,  1308, None, 1289),
    ( 86, "ORNL-57-002-T*",   0.02,  1.12,  95.0,  1443, None, 1435),
    ( 87, "ORNL-57-002-2T*",  0.02,  1.12,  95.0,  1503, None, 1495),
    ( 88, "ORNL-57-002-3T*",  0.06,  1.12,  95.0,  1518, 1522, 1514),
    ( 89, "ORNL-57-003-T",    6.00,  4.17,  95.0,  1513, 1515, 1485),
    ( 90, "ORNL-57-003-2T",  10.30,  4.17,  95.0,  1573, 1576, 1547),
    ( 91, "ORNL-57-003-3T",  11.50,  4.17,  95.0,  1598, 1600, 1572),
    ( 92, "ORNL-57-003-B",   12.60,  4.17,  95.0,  1573, 1578, 1547),
    ( 93, "ORR-UN-3 T*",      0.10,  1.45,  95.0,  None, None, 1805),
    ( 94, "ORR-UN-3 B",       7.10,  1.75,  95.0,  None, None, 1812),
    ( 95, "ORR-UN-4 M11",     4.30,  2.76,  84.6,  None, None, 1588),

    # --- U-Pu-N section ------------------------------------------------------
    ( 96, "BMI B-1-3",       13.40,  3.60,  81.40, None, None, 1365),
    ( 97, "BMI B-1-4",        2.10,  5.85,  94.00, None, None, 1187),
    ( 98, "BMI B-2-2",        5.00,  5.48,  94.00, None, None, 1281),
    ( 99, "BMI B-2-3",        3.70,  8.22,  94.00, None, None, 1294),
    (100, "BMI B-2-6",        2.70,  5.60,  94.00, None, None, 1284),
    (101, "BMI B-2-7",        2.40,  5.60,  94.00, None, None, 1287),
    (102, "BMI B-2-8",        0.30,  2.90,  94.00, None, None, 1293),
    (103, "BMI B-3-2",        7.00,  2.78,  88.40, None, None, 1365),
    (104, "BMI B-3-3",        7.40,  2.77,  91.30, None, None, 1368),
    (105, "BMI B-3-4",       11.00,  2.70,  93.90, None, None, 1336),
    (106, "BMI B-3-5",        7.00,  2.74,  90.40, None, None, 1356),
    (107, "BMI B-3-6",        7.50,  8.39,  94.80, None, None, 1873),
    (108, "BMI B-3-7*",      21.70,  5.49,  88.50, None, None, 1907),
    (109, "BMI B-3-8",       12.90,  8.06,  89.80, None, None, 1798),
    (110, "EBR-C-5",          1.56,  8.10,  94.10, None, None, 1139),
    (111, "EBR-K-4",          0.80,  9.12,  97.00, None, None, 1119),
    (112, "EBR-K-4",          0.81,  7.27,  96.60, None, None, 1122),  # duplicate label — verify
    (113, "BMI-50-4 A3*",     0.09,  4.23,  93.80, None, None,  928),
    (114, "BMI-50-4 B3",      0.26,  5.44,  96.40, None, None, 1081),
    (115, "BMI-50-4 B4",      1.55,  7.65,  84.20, None, None, 1162),
    (116, "BMI-50-4 C1",      0.34,  6.20,  95.70, None, None, 1127),
    (117, "BMI-50-4 C2",      0.26,  4.87,  96.50, None, None, 1031),
    (118, "BMI-50-5 B5",     13.50, 13.09,  80.20, None, None,  914),
    (119, "BMI-50-5 B6",      0.80,  8.58,  95.60, None, None,  833),
    (120, "BMI-50-5 C7",      4.80,  9.98,  97.20, None, None,  903),
    (121, "BMI-50-6 B2*",    27.00, 18.71,  83.90, None, None, 1275),
    (122, "BMI-50-7 B22",     0.40,  9.32,  95.50, None, None, 1089),
    (123, "BMI-50-7 B24",     9.20, 11.76,  82.50, None, None, 1110),
    (124, "BMI-50-7 C22",     0.40,  9.48,  96.00, None, None, 1117),
    (125, "BMI-50-8 B25",    10.10, 18.32,  83.50, None, None, 1208),
    (126, "BMI-50-8 B28",     5.92, 12.67,  95.50, None, None, 1109),
    (127, "BMI-50-8 C25",     7.94, 13.89,  95.00, None, None, 1192),
    (128, "BMI-50-8 C26",     8.49, 15.78,  96.30, None, None, 1388),
    (129, "BMI-50-11 A43",    6.00,  6.74,  82.70, None, None, 1433),
    (130, "BMI-50-11 A44",    5.90,  5.54,  94.70, None, None, 1422),
    (131, "BMI-50-11 B43",    7.40,  7.07,  82.70, None, None, 1485),
    (132, "BMI-50-11 B54",    8.30,  5.83,  94.40, None, None, 1469),
    (133, "BMI-50-11 C43*",  28.00,  6.10,  94.30, None, None, 1521),
    (134, "BMI-50-11 C44*",  47.00,  5.78,  94.30, None, None, 1460),
]

COLUMNS = ["id", "designation_raw", "release", "burnup", "TD",
           "T_Ross", "T_Baars", "T_Thomas"]

UN_LAST_ROW_ID = 95         # rows 1..95 = UN ; rows 96..134 = U-Pu-N
EXPECTED_TOTAL = 134        # paper has 134 numbered rows
KNOWN_MISSING_IDS = {51}    # documented above; not yet recovered


def build_dataframe() -> pd.DataFrame:
    """Build the cleaned dataframe from RAW_RECORDS, preserving paper IDs."""
    df = pd.DataFrame(RAW_RECORDS, columns=COLUMNS)

    # Flag rows marked with "(*)" in the source — footnote on page 3:
    #   "(*) Points not used to obtain eq. (7)."
    df["excluded_from_fit"] = df["designation_raw"].str.endswith("*")

    # Clean designation
    df["designation"] = df["designation_raw"].str.rstrip("*").str.strip()

    # Material: UN vs U-Pu-N (by table section header)
    df["material"] = df["id"].apply(
        lambda i: "UN" if i <= UN_LAST_ROW_ID else "U-Pu-N"
    )

    df = df[[
        "id", "designation", "material",
        "release", "burnup", "TD",
        "T_Ross", "T_Baars", "T_Thomas",
        "excluded_from_fit",
    ]]
    return df.sort_values("id").reset_index(drop=True)


def integrity_check(df: pd.DataFrame) -> None:
    """Confirm expected count and detect surprise gaps / duplicates."""
    present = set(df["id"].tolist())
    expected = set(range(1, EXPECTED_TOTAL + 1))
    missing = expected - present - KNOWN_MISSING_IDS
    extra = present - expected
    dup_counts = df["id"].value_counts()
    duplicates = dup_counts[dup_counts > 1]

    print("--- Integrity check ---")
    print(f"Rows in dataframe         : {len(df)}")
    print(f"Expected (paper)          : {EXPECTED_TOTAL}")
    print(f"Known-missing (page break): {sorted(KNOWN_MISSING_IDS)}")
    if missing:
        print(f"!! Unexpected missing ids : {sorted(missing)}")
    if extra:
        print(f"!! Unexpected extra ids   : {sorted(extra)}")
    if len(duplicates):
        print(f"!! Duplicate ids          : {duplicates.to_dict()}")
    if not missing and not extra and not len(duplicates):
        print("OK — all accounted for (modulo the documented page-break gap).")
    print()


def summary(df: pd.DataFrame) -> None:
    print("--- Summary ---")
    print(f"Total rows                : {len(df)}")
    print(f"  UN                      : {(df['material'] == 'UN').sum()}")
    print(f"  U-Pu-N                  : {(df['material'] == 'U-Pu-N').sum()}")
    print(f"Excluded from eq. (7) fit : {df['excluded_from_fit'].sum()}")
    print(f"Used in eq. (7) fit       : {(~df['excluded_from_fit']).sum()}")
    print()
    print("Missing values per column:")
    print(df.isna().sum().to_string())
    print()
    print("Range checks:")
    for col in ["release", "burnup", "TD", "T_Ross", "T_Baars", "T_Thomas"]:
        s = df[col].dropna()
        if len(s):
            print(f"  {col:10s}: min={s.min():>8.3f}  max={s.max():>8.3f}  "
                  f"mean={s.mean():>8.3f}  n={len(s)}")
    print()


def main(out_dir: Path = Path(__file__).resolve().parents[1]
                                / "reports" / "storms1988") -> pd.DataFrame:
    df = build_dataframe()
    out_dir.mkdir(parents=True, exist_ok=True)

    csv_path = out_dir / "fgr_table1.csv"
    df.to_csv(csv_path, index=False)
    print(f"Saved CSV : {csv_path.resolve()}")

    # Optional parquet — only if pyarrow / fastparquet is available
    try:
        parquet_path = out_dir / "fgr_table1.parquet"
        df.to_parquet(parquet_path, index=False)
        print(f"Saved parq: {parquet_path.resolve()}")
    except (ImportError, ValueError) as e:
        print(f"[info] parquet skipped (no engine): {str(e)[:80]}")
    print()

    integrity_check(df)
    summary(df)
    return df


if __name__ == "__main__":
    df = main()