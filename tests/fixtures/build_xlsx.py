"""Rebuild XLSX fixtures from the test harness's deterministic CSV files."""
import csv
import sys
from pathlib import Path
from openpyxl import Workbook

folder = Path(sys.argv[1])
for stem in ("counts", "samples"):
    source = folder / f"{stem}.csv"
    wb = Workbook()
    sheet = wb.active
    with source.open(encoding="utf-8-sig", newline="") as handle:
        for i, row in enumerate(csv.reader(handle)):
            if stem == "counts" and i:
                row = [row[0]] + [int(value) for value in row[1:]]
            sheet.append(row)
    wb.save(folder / f"{stem}.xlsx")
print("XLSX fixtures generated from deterministic CSV sources")
