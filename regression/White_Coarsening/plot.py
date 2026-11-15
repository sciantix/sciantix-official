"""#COARSENING: White data, original SCIANTIX and published Barani endpoints."""

from pathlib import Path
import json
import numpy as np
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
import run as campaign

HERE = campaign.HERE
ROOT = campaign.ROOT
NEW, LEGACY, BARANI, MEASURED = "#2379b5", "#888888", "#c44e52", "#222222"
MODEL_LABELS = {
    "coarsening": "Nicodemo 2026",
    "legacy": "SCIANTIX legacy",
    "Barani_table4": "Barani 2020",
}
POOL_COLORS = ("#4c78a8", "#f2cf5b", "#e45756", "#64b5b4", "#80579b")
POOL_LABELS = (
    "Solution",
    "Bulk bubbles",
    "Dislocation bubbles",
    "Grain boundary",
    "Released",
)
plt.rcParams.update(
    {
        "font.size": 10,
        "axes.labelsize": 10,
        "axes.titlesize": 10,
        "xtick.labelsize": 9,
        "ytick.labelsize": 9,
        "legend.fontsize": 9,
    }
)


def save(fig, path):
    path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(path.with_suffix(".png"), dpi=300, bbox_inches="tight")
    plt.close(fig)


def history(result, observation, folder, full=False):
    """Published experimental endpoints have no invented measurement timestamp."""
    sample = result["sample"]
    t = campaign.output(HERE / result["trace"])
    onset = result["onset_h"]
    x = t["Time (h)"] - onset
    end = float(x[-1])
    zoom = end > 8 and not full
    right = min(1.5, end) if zoom else end * 1.02
    left = -min(0.05, 0.1 * right)
    mask = (x >= left) & (x <= right)
    alive = t["Coarsening dislocation concentration (bub/m3)"] > 0
    fig = plt.figure(figsize=(8.3, 6.3))
    gs = fig.add_gridspec(
        3,
        2,
        width_ratios=(5.2, 1.15),
        height_ratios=(0.55, 1.15, 1.15),
        hspace=0.18,
        wspace=0.48,
    )
    aT, aP, aR = [fig.add_subplot(gs[i, 0]) for i in range(3)]
    nAxis = aP.twinx()
    sAxis = aR.twinx()
    h = np.loadtxt(HERE / "inputs" / sample / "input_history.txt")
    aT.plot(h[:, 0] - onset, h[:, 1], color=NEW, lw=1.3)
    aP.plot(
        x[mask],
        np.where(alive, t["Coarsening pressure (Pa)"] / 1e6, np.nan)[mask],
        c=NEW,
        lw=1.4,
    )
    aP.plot(
        x[mask],
        np.where(alive, t["Coarsening equilibrium pressure (Pa)"] / 1e6, np.nan)[mask],
        c=NEW,
        lw=0.9,
        ls=":",
    )
    nAxis.plot(
        x[mask],
        np.where(alive, t["Coarsening vacancies per bubble (vac/bub)"] / 1e7, np.nan)[
            mask
        ],
        c=NEW,
        lw=1.3,
        ls="--",
    )
    aR.plot(
        x[mask],
        np.where(alive, t["Coarsening dislocation radius (m)"] * 1e9, np.nan)[mask],
        c=NEW,
        lw=1.4,
    )
    aR.plot(
        x[mask],
        t["Intragranular bubble radius (m)"][mask] * 1e9,
        c=".5",
        ls=":",
        lw=0.9,
    )
    sAxis.plot(
        x[mask],
        100 * t["Intragranular gas bubble swelling (/)"][mask],
        c=NEW,
        lw=1.3,
        ls="--",
    )
    sAxis.plot(
        x[mask],
        100 * t["Coarsening dislocation swelling (/)"][mask],
        c=NEW,
        lw=0.9,
        ls=":",
    )
    for ax in (aT, aP, aR):
        ax.axvspan(left, 0, color=".92", zorder=-2)
        ax.axvline(0, c=".6", ls=":", lw=0.7)
        ax.grid(alpha=0.16)
        ax.set_xlim(left, right)
    for ax in (aT, aP):
        ax.tick_params(axis="x", labelbottom=False)
    aT.set_ylabel("T (K)")
    aP.set_ylabel("Pressure (MPa), solid")
    nAxis.set_ylabel(r"$n_v$ ($10^7$ vacancies/bubble), dashed", fontsize=9)
    aR.set_ylabel("Radius (nm), solid")
    sAxis.set_ylabel("Total swelling (%), dashed", fontsize=9)
    for ax in (aP, nAxis, aR, sAxis):
        ax.set_ylim(bottom=0)
    aR.set_xlabel(
        "Time from first ramp onset (h)" + (" — early-ramp zoom" if zoom else "")
    )
    final = result["states"]["end"]
    pre = result["states"]["pre"]
    rr = [final["R"] * 1e9, float(observation["radius_exp_nm"])]
    ss = [final["S"], float(observation["swelling_exp_pct"])]
    aR.set_ylim(
        0, max(float(np.max(t["Coarsening dislocation radius (m)"] * 1e9)), *rr) * 1.16
    )
    sAxis.set_ylim(
        0,
        max(float(np.max(100 * t["Intragranular gas bubble swelling (/)"])), *ss)
        * 1.16,
    )
    point = fig.add_subplot(gs[2, 1])
    pointS = point.twinx()
    for i, (color, marker) in enumerate([(NEW, "o"), (MEASURED, "s")]):
        point.scatter(-0.12 + i * 0.24, rr[i], c=color, marker=marker, s=24)
        pointS.scatter(0.88 + i * 0.24, ss[i], c=color, marker=marker, s=24)
    point.set(
        xlim=(-0.5, 1.5),
        ylim=aR.get_ylim(),
        xticks=(0, 1),
        xticklabels=("R", "S"),
        title="Post-test",
    )
    pointS.set_ylim(sAxis.get_ylim())
    point.tick_params(axis="y", left=False, labelleft=False)
    pointS.set_ylabel("Swelling (%)", fontsize=9)
    point.set_xlabel("Final observations", fontsize=9)
    note = fig.add_subplot(gs[:2, 1])
    note.axis("off")
    note.text(
        0,
        0.95,
        f'PRERAMP\nRb = {pre["Rb"]*1e9:.3f} nm\nNd = 0\nP, nv undefined\n\n'
        f'FINAL STATE\nt = {end:.3f} h\nNd = {final["N"]:.2e} m⁻³\n\n'
        f'Dislocation gas\n{pre["dislocations"]:.1f}% → {final["dislocations"]:.1f}%',
        transform=note.transAxes,
        va="top",
        fontsize=9,
    )
    title = sample + (
        " — early ramps; observations refer to the full history" if zoom else ""
    )
    fig.suptitle(title, y=1.02, fontsize=11)
    fig.legend(
        handles=[
            Line2D([], [], c=NEW, label=MODEL_LABELS["coarsening"]),
            Line2D([], [], ls="", marker="s", c=MEASURED, label="White measurements"),
        ],
        loc="upper center",
        bbox_to_anchor=(0.5, 0.995),
        ncol=2,
        frameon=False,
    )
    fig.subplots_adjust(left=0.09, right=0.91, bottom=0.13, top=0.91)
    note = "Dotted: Peq, bulk radius and large-bubble swelling. Grey: preramp. Final references have no assigned timestamp."
    if sample.startswith("4163"):
        note += " Local input omits the41 h hold of Barani Table3."
    fig.text(0.5, 0.012, note, ha="center", fontsize=7.5, wrap=True)
    save(fig, folder / (sample + ("_full" if full else "")))
    return zoom


def plot_all(results, observations):
    figdir = HERE / "figures"
    figdir.mkdir(exist_ok=True)
    samples = [r["sample"] for r in observations]
    measured = {r["sample"]: r for r in observations}
    pub = {
        r["sample"]: r for r in campaign.read_csv(HERE / "data/barani_2020_table4.csv")
    }
    by = {
        m: {r["sample"]: r for r in results if r["model"] == m}
        for m in ("coarsening", "legacy")
    }
    metrics = {m: {} for m in ("coarsening", "legacy", "Barani_table4")}
    config = [
        ("R", "radius_exp_nm", "radius_published_nm", 1e9, "Bubble radius (nm)"),
        (
            "N",
            "density_exp_m3",
            "density_published_m3",
            1.0,
            r"Bubble number (m$^{-3}$)",
        ),
        (
            "S",
            "swelling_exp_pct",
            "swelling_published_pct",
            1.0,
            "Intragranular swelling (%)",
        ),
        ("Sgb", "gb_swelling_exp_pct", None, 1.0, "Intergranular swelling (%)"),
    ]

    def panel(ax, c):
        key, obs, published, scale, title = c
        x = np.array([float(measured[s][obs]) for s in samples])
        all_y = []
        for model, color, label in [
            ("legacy", LEGACY, "SCIANTIX legacy"),
            ("coarsening", NEW, MODEL_LABELS["coarsening"]),
        ]:
            y = np.array([by[model][s]["states"]["end"][key] * scale for s in samples])
            all_y.extend(y)
            ax.scatter(x, y, c=color, s=19, alpha=0.75, label=label)
            ratio = y / x
            metrics[model][key] = dict(
                MAPE_pct=float(100 * np.mean(abs(ratio - 1))),
                RMSE_log10=float(np.sqrt(np.mean(np.log10(ratio) ** 2))),
                within2=int(np.sum((ratio >= 0.5) & (ratio <= 2))),
            )
        if published:
            y = np.array([float(pub[s][published]) for s in samples])
            all_y.extend(y)
            ax.scatter(
                x, y, c=BARANI, marker="x", s=22, label=MODEL_LABELS["Barani_table4"]
            )
            ratio = y / x
            metrics["Barani_table4"][key] = dict(
                MAPE_pct=float(100 * np.mean(abs(ratio - 1))),
                RMSE_log10=float(np.sqrt(np.mean(np.log10(ratio) ** 2))),
                within2=int(np.sum((ratio >= 0.5) & (ratio <= 2))),
            )
        lo = min(min(all_y), x.min()) / 1.5
        hi = max(max(all_y), x.max()) * 1.5
        for f, style in [(1, "--"), (0.5, ":"), (2, ":")]:
            ax.plot([lo, hi], [f * lo, f * hi], c=".55", ls=style, lw=0.9)
        ax.set(
            xscale="log",
            yscale="log",
            xlim=(lo, hi),
            ylim=(lo, hi),
            xlabel="Measured",
            ylabel="Predicted",
            title=title,
        )
        ax.grid(which="both", alpha=0.14)

    fig, axes = plt.subplots(2, 2, figsize=(7.1, 6.6), layout="constrained")
    for ax, c in zip(axes.flat, config):
        panel(ax, c)
    fig.legend(
        *axes[0, 0].get_legend_handles_labels(),
        loc="outside upper center",
        ncol=3,
        frameon=False,
    )
    save(fig, figdir / "00_merit_all")
    for i, c in enumerate(config, 1):
        fig, ax = plt.subplots(figsize=(4.6, 4.5), layout="constrained")
        panel(ax, c)
        ax.legend(loc="best", fontsize=8)
        save(fig, figdir / f"0{i}_merit_{c[0]}")
    campaign.save(HERE / "metrics.json", metrics)
    # Percent produced locally, not an integral rod release fraction.
    inventory = []
    fig, axes = plt.subplots(
        2, 1, figsize=(10.2, 5.1), sharex=True, layout="constrained"
    )
    means = {}
    for ax, phase, title in zip(
        axes, ("pre", "end"), ("Before the first ramp", "End of the available history")
    ):
        bottom = np.zeros(38)
        means[phase] = {}
        for pool, label, color in zip(campaign.POOLS, POOL_LABELS, POOL_COLORS):
            y = np.array([by["coarsening"][s]["states"][phase][pool] for s in samples])
            ax.bar(range(38), y, bottom=bottom, width=0.86, color=color, label=label)
            bottom += y
            means[phase][pool] = float(y.mean())
        for s in samples:
            inventory.append(
                dict(sample=s, phase=phase, **by["coarsening"][s]["states"][phase])
            )
        ax.set(ylim=(0, 100), ylabel="Gas / local produced (%)", title=title)
        ax.grid(axis="y", alpha=0.15)
        ax.set_axisbelow(True)
    axes[-1].set_xticks(range(38), samples, rotation=65, ha="right", fontsize=8)
    fig.legend(
        *axes[0].get_legend_handles_labels(),
        loc="outside upper center",
        ncol=5,
        frameon=False,
    )
    save(fig, figdir / "05_gas_before_after")
    campaign.write_csv(HERE / "inventories.csv", inventory)
    campaign.save(HERE / "mean_inventories.json", means)
    # 4070 is a separately labelled proxy control, not the ramped4005 specimen.
    obs = campaign.read_csv(HERE / "data/white_4070_tem.csv")
    controls = sorted(s for s in by["coarsening"] if s.startswith("4070_"))
    fig, axes = plt.subplots(1, 2, figsize=(7.1, 3.1), layout="constrained")
    axes[0].scatter(
        [float(r["T_K"]) for r in obs],
        [float(r["radius_nm"]) for r in obs],
        marker="s",
        c=MEASURED,
        label="White4070 TEM",
    )
    axes[1].scatter(
        [float(r["T_K"]) for r in obs],
        [float(r["number_m3"]) for r in obs],
        marker="s",
        c=MEASURED,
    )
    for model, color, marker in [("legacy", LEGACY, "o"), ("coarsening", NEW, "^")]:
        values = [by[model][s]["states"]["end"] for s in controls]
        axes[0].scatter(
            [r["T"] for r in values],
            [r["Rb"] * 1e9 for r in values],
            c=color,
            marker=marker,
            label=MODEL_LABELS[model] + " proxy",
        )
        axes[1].scatter(
            [r["T"] for r in values], [r["Nb"] for r in values], c=color, marker=marker
        )
    axes[0].set(xlabel="Temperature (K)", ylabel="Small-bubble radius (nm)")
    axes[1].set(
        xlabel="Temperature (K)", ylabel=r"Small-bubble number (m$^{-3}$)", yscale="log"
    )
    for ax in axes:
        ax.grid(alpha=0.2)
    fig.legend(
        *axes[0].get_legend_handles_labels(),
        loc="outside upper center",
        ncol=3,
        frameon=False,
    )
    save(fig, figdir / "06_preramp_4070")
    zoomed = []
    for sample in samples:
        r = by["coarsening"][sample]
        if history(r, measured[sample], figdir / "histories"):
            zoomed.append(sample)
            history(r, measured[sample], figdir / "histories", full=True)
    lines = [
        "# White coarsening regression — Nicodemo 2026",
        "",
        "38 ramp histories; six4070 unramped proxies. Same fixed inputs and physical yield for the new model and legacy controls. "
        "Red crosses are published Barani Table4 predictions, not measurements or simulated Barani configurations. "
        "No published Barani intergranular values are available in this table.",
        "",
        "![Merit](figures/00_merit_all.png)",
        "",
        "|Model|R MAPE|N MAPE|IG swelling MAPE|GB swelling MAPE|",
        "|---|---:|---:|---:|---:|",
    ]
    for model in metrics:
        lines.append(
            "|"
            + MODEL_LABELS[model]
            + "|"
            + "|".join(
                (
                    f'{metrics[model][k]["MAPE_pct"]:.2f}%'
                    if k in metrics[model]
                    else "not tabulated"
                )
                for k in ("R", "N", "S", "Sgb")
            )
            + "|"
        )
    lines += [
        "",
        "IG swelling includes both populations; R and N describe the large population in the new model. "
        "White SEM primarily resolves the large population. All gas fractions are normalized to local stable Xe+Kr production; "
        "means across38 locations are arithmetic diagnostics, not integral rod FGR.",
        "",
        "![Inventories](figures/05_gas_before_after.png)",
        "",
        "![Preramp](figures/06_preramp_4070.png)",
        "",
        "The4070 inputs are constant-temperature dose proxies. The small-radius scale is retained but the native density defect persists. "
        "Two of40 ramp histories are unavailable; local4163 histories omit the41 h hold listed in Barani Table3.",
        "",
        "## Individual histories",
        "",
        "Pressure and vacancies are undefined until the large population exists. Experimental post-test points are separated from the "
        "time axis, without assigning a measurement timestamp. No Barani time curves are included. Long histories have an early-ramp "
        "zoom and an additional full-history figure; both show the actual final references.",
        "",
    ]
    for s in samples:
        line = f"- [{s}](figures/histories/{s}.png)"
        if s in zoomed:
            line += f" ([full history](figures/histories/{s}_full.png))"
        lines.append(line)
    lines += [
        "",
        "## Run and verify",
        "",
        "```sh",
        "python3 regression/White_Coarsening/run.py --binary build/sciantix.x",
        "python3 regression/White_Coarsening/run.py --binary build/sciantix.x --check --verify --no-plots",
        "```",
        "",
        "Frozen endpoint references are changed only with the explicit `--write-reference` option. Raw traces stay in ignored `runs/`. "
        "Figures are generated only as 300 dpi PNG files. "
        "Equations, parameter provenance and limits are summarized in [context](../../context/COARSENING.md).",
    ]
    (HERE / "README.md").write_text("\n".join(lines) + "\n")
    sources = [
        ROOT / p
        for p in (
            "src/models/IntraGranularBubbleBehavior.C",
            "src/models/IntraGranularBubbleCoarsening.C",
            "src/models/DislocationDensity.C",
            "include/models/IntraGranularBubbleCoarsening.h",
            "include/models/DislocationDensity.h",
        )
    ]
    campaign.save(
        HERE / "manifest.json",
        dict(
            official_base="2344575b7d246d02205f503b541b74f914f5f6b3",
            physical_model="Nicodemo2026, Kmax3e6,1800–1950K, Hart b=.385nm, Dv3bulk, y=.27",
            source_sha256={str(p.relative_to(ROOT)): campaign.sha(p) for p in sources},
            input_sha256={
                str(p.relative_to(HERE)): campaign.sha(p)
                for p in sorted((HERE / "inputs").rglob("*"))
                if p.is_file()
            },
            figures={
                str(p.relative_to(HERE)): campaign.sha(p)
                for p in sorted(figdir.rglob("*"))
                if p.is_file() and p.suffix == ".png"
            },
        ),
    )
