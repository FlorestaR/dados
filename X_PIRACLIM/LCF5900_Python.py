# ─────────────────────────────────────────────────────────────────────────────
# LCF5900_CLIMA.py
# Author:  Luiz Carlos Estraviz Rodriguez
# Updated: 31/Mai/2026
# ─────────────────────────────────────────────────────────────────────────────
# ── 0. Install dependencies (run once in terminal)
# pip install pandas openpyxl requests matplotlib imageio[ffmpeg] pillow

import os
import requests
import io
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.ticker as mticker
import imageio.v2 as imageio

# ── 1. Working directory 
os.makedirs("/content/PiraClima", exist_ok=True)
os.chdir("/content/PiraClima")                          # equivalent to setwd()

# ── 2. Build the GitHub URL
url_1    = "https://github.com/FlorestaR/dados/blob/main/X_PIRACLIM/"
xls_2    = "DadosClima_Piracicaba.xlsx"
prm_3    = "?raw=true"
git_file = url_1 + xls_2 + prm_3

#── 3.  Download the Excel file
response = requests.get(git_file)
response.raise_for_status()                    # raises error if download fails
excel_bytes = io.BytesIO(response.content)

# ── 4. Import the sheet, set column types, convert first 8 cols to "category"
#       (Python's equivalent of R's "factor")
sheet_name   = "DadosClima_Piracicaba"
n_text_cols  = 8
n_num_cols   = 16

# Read all columns as strings first, then convert
df_raw = pd.read_excel(excel_bytes, sheet_name=sheet_name, dtype=str)

# Columns 0–7 → category (factor); columns 8–23 → numeric
cat_cols = df_raw.columns[:n_text_cols].tolist()
num_cols = df_raw.columns[n_text_cols:n_text_cols + n_num_cols].tolist()

df = df_raw.copy()
df[cat_cols] = df[cat_cols].astype("category")
# coerce = as.numeric safely
df[num_cols] = df[num_cols].apply(pd.to_numeric, errors="coerce")

# Equivalent to colnames(df) and str(df)
print("Column names:\n", df.columns.tolist())
print("\nData types:\n", df.dtypes)
print("\nShape:", df.shape)

print("\nDataframe Info:")
df.info()
print("\nDataframe Head:")
df.head()

# ── 5. Extract TMAX for years 2022–2025 ──────────────────────────────────────
# NOTE: Ano is a category column, so compare against strings
years  = ["2022", "2023", "2024", "2025"]
t_max  = df[df["Ano"].isin(years)]["TMAX"].dropna().values

# ── 6. Histogram as relative frequency (%) ───────────────────────────────────
breaks      = [0, 5, 10, 15, 20, 25, 30, 35, 40, 45]
counts, _   = np.histogram(t_max, bins=breaks)
percentages = counts / counts.sum() * 100             # relative frequency in %

# Build class labels: "0–5", "5–10", …
labels = [f"{breaks[i]}–{breaks[i+1]}" for i in range(len(breaks) - 1)]

fig, ax = plt.subplots(figsize=(9, 5))
x_pos = np.arange(len(labels))
ax.bar(x_pos, percentages, color="grey", edgecolor="black", width=0.8)
ax.set_xticks(x_pos)
ax.set_xticklabels(labels, rotation=0)
ax.set_title("Temperaturas Máximas 2022-2025 - Piracicaba-SP")
ax.set_xlabel("Temperatura (°C)")
ax.set_ylabel("Frequência relativa (%)")
ax.yaxis.set_major_formatter(mticker.FormatStrFormatter("%.1f"))
plt.tight_layout()
plt.savefig("hist_tmax.png", dpi=150)
plt.show()

# ── 7. Create new_df: select columns, drop NAs, filter temps < 50 ────────────
cols   = ["Ano", "Mes", "TMED", "TMIN", "TMAX", "Chuva"]
new_df = (df[cols]
          .dropna()                                                 # drop_na()
          .loc[lambda d: (d["TMED"] < 50) &                      # if_all(< 50)
                         (d["TMIN"] < 50) &
                         (d["TMAX"] < 50)]
          .reset_index(drop=True))

print("\nnew_df dtypes:\n", new_df.dtypes)
print("new_df shape:", new_df.shape)

# ── 8. Summary statistics ─────────────────────────────────────────────────────
summary = new_df[["TMED", "TMIN", "TMAX", "Chuva"]].mean().rename({
    "TMED":  "m_TMED",
    "TMIN":  "m_TMIN",
    "TMAX":  "m_TMAX",
    "Chuva": "m_Chuva"
})
print("\nMeans:\n", summary)

# ── 9. Animated GIF: mean monthly TMED per year ───────────────────────────────
month_labels = ["Jan","Fev","Mar","Abr","Mai","Jun",
                "Jul","Ago","Set","Out","Nov","Dez"]
# 9a. Summarise
med_mes = (new_df
           .assign(Ano=new_df["Ano"].astype(int),
                   Mes=new_df["Mes"].astype(int))
           .groupby(["Ano", "Mes"], observed=True)["TMED"]
           .mean()
           .reset_index()
           .rename(columns={"TMED": "tmedMes"}))

y_min = new_df["TMED"].astype(float).min() - 2
y_max = new_df["TMED"].astype(float).max() + 2
anos  = sorted(med_mes["Ano"].unique())                    # sorted numerically
frames = []

for ano in anos:
    subset = med_mes[med_mes["Ano"] == ano].copy()

    # Spine merge now works: both sides are int
    spine  = pd.DataFrame({"Mes": range(1, 13)})
    subset = spine.merge(subset, on="Mes", how="left")

    fig, ax = plt.subplots(figsize=(7.5, 4.5))
    ax.plot(subset["Mes"], subset["tmedMes"],
            marker="o", color="steelblue", linewidth=2, markersize=6)
    ax.set_xticks(range(1, 13))
    ax.set_xticklabels(month_labels)
    ax.set_title(f"Temperatura Média Mensal — Ano: {ano}", fontsize=13)
    ax.set_xlabel("Mês")
    ax.set_ylabel("Temperatura Média (°C)")
    ax.set_ylim(y_min, y_max)
    ax.grid(axis="y", linestyle="--", alpha=0.4)
    plt.tight_layout()

    buf = io.BytesIO()
    plt.savefig(buf, format="png", dpi=120)
    plt.close(fig)
    buf.seek(0)
    frames.append(imageio.imread(buf))
# 9c. Save as GIF (duration in seconds per frame → fps = 1/duration)
imageio.mimsave("grafGIF.gif", frames, duration=1.5, loop=0)
print("GIF saved as grafGIF.gif")

# 9d. Display in Jupyter / VS Code (optional)
from IPython.display import Image, display
display(Image(filename="grafGIF.gif"))