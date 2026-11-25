"""#COARSENING: compare the selected configuration with legacy and published results."""

import numpy as np
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import run as campaign

HERE = campaign.HERE
ROOT = campaign.ROOT
NEW, LEGACY, BARANI, VACANCIES = "#2379b5", "#888888", "#c44e52", "#c44e52"
MODEL_LABELS = {
    "coarsening": "Nicodemo 2026",
    "legacy": "SCIANTIX single-size",
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
        "text.color": "black",
        "axes.labelcolor": "black",
        "axes.titlecolor": "black",
        "xtick.color": "black",
        "ytick.color": "black",
        "legend.labelcolor": "black",
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


def figure7_history(result, observation, folder, zoom=False):
    """Physical axes with post-test observations in the radius/swelling panel."""
    from matplotlib.ticker import ScalarFormatter, MaxNLocator

    def top(default, *values):
        finite = np.concatenate([np.asarray(v).ravel() for v in values])
        actual = (
            float(np.max(finite[np.isfinite(finite)]))
            if np.any(np.isfinite(finite))
            else 0.0
        )
        if actual <= default:
            return default
        scale = 10.0 ** np.floor(np.log10(actual))
        return np.ceil(1.05 * actual / scale * 2.0) / 2.0 * scale

    def scientific(ax, exponent):
        formatter = ScalarFormatter(useMathText=True)
        formatter.set_powerlimits((exponent, exponent))
        formatter.set_useOffset(False)
        ax.yaxis.set_major_formatter(formatter)
        ax.yaxis.set_major_locator(MaxNLocator(nbins=5))

    sample = result["sample"]
    trace = campaign.output(HERE / result["trace"])
    time = trace["Time (h)"] - result["onset_h"]
    end = float(time[-1])
    right = min(1.5, end) if zoom else end
    left = -min(0.05, 0.1 * right)
    first = max(0, int(np.searchsorted(time, left)) - 1)
    selected = slice(first, int(np.searchsorted(time, right, side="right")))
    x = time[selected]
    radius = trace["Coarsening dislocation radius (m)"][selected]
    alive = radius > 0.0
    pressure = np.where(alive, trace["Coarsening pressure (Pa)"][selected], np.nan)
    equilibrium = np.where(
        alive, trace["Coarsening equilibrium pressure (Pa)"][selected], np.nan
    )
    vacancies = np.where(
        alive, trace["Coarsening vacancies per bubble (vac/bub)"][selected], np.nan
    )
    swelling = 100.0 * trace["Intragranular gas bubble swelling (/)"][selected]
    measured_radius = float(observation["radius_exp_nm"]) * 1e-9
    measured_swelling = float(observation["swelling_exp_pct"])

    fig = plt.figure(figsize=(8.6, 7.5), layout="constrained")
    gs = fig.add_gridspec(3, 1, height_ratios=(0.5, 1.45, 1.45), hspace=0.08)
    aT = fig.add_subplot(gs[0, :])
    aP = fig.add_subplot(gs[1, :])
    aR = fig.add_subplot(gs[2, 0])
    nAxis, sAxis = aP.twinx(), aR.twinx()

    aT.plot(x, trace["Temperature (K)"][selected], color="black", ls="--", lw=1.1)
    aT.set_ylabel("Temperature (K)")
    aP.plot(x, pressure, color=NEW, lw=1.5, label="Bubble at dislocation pressure")
    aP.plot(x, equilibrium, color=".35", lw=1.1, ls=":", label="Equilibrium pressure")
    nAxis.plot(
        x,
        vacancies,
        color=VACANCIES,
        lw=1.5,
        ls="--",
        label="Vacancies per bubble at dislocations",
    )
    aP.set_ylabel("Pressure (Pa)")
    nAxis.set_ylabel("Vacancies per bubble")
    aP.set_ylim(0.0, top(1e8, pressure, equilibrium))
    nAxis.set_ylim(0.0, top(2.5e7, vacancies))
    scientific(aP, 8)
    scientific(nAxis, 7)
    handles, labels = aP.get_legend_handles_labels()
    other_h, other_l = nAxis.get_legend_handles_labels()
    aP.legend(
        handles + other_h,
        labels + other_l,
        loc="lower left",
        bbox_to_anchor=(0.0, 1.09),
        ncol=2,
        frameon=False,
        fontsize=8,
    )

    aR.plot(x, radius, color=NEW, lw=1.5, label="Calculated bubble radius")
    sAxis.plot(
        x,
        swelling,
        color=VACANCIES,
        lw=1.5,
        ls="--",
        label="Calculated gaseous swelling",
    )
    reference_time = min(end, right)
    aR.scatter(
        reference_time,
        measured_radius,
        marker="s",
        color=NEW,
        zorder=5,
        s=28,
        label="Experimental bubble radius",
    )
    sAxis.scatter(
        reference_time,
        measured_swelling,
        marker="o",
        color=VACANCIES,
        zorder=5,
        s=28,
        label="Experimental gaseous swelling",
    )
    radius_top = top(8e-8, radius, measured_radius)
    swelling_top = top(10.0, swelling, measured_swelling)
    aR.set(ylim=(0.0, radius_top), ylabel="Bubble radius (m)")
    sAxis.set(ylim=(0.0, swelling_top), ylabel="Gaseous swelling (%)")
    if zoom and end > right:
        aR.text(
            0.98,
            0.03,
            "Post-test references at right edge",
            transform=aR.transAxes,
            ha="right",
            va="bottom",
            fontsize=7,
        )
    scientific(aR, -8)
    handles, labels = aR.get_legend_handles_labels()
    for ax in (sAxis,):
        h, l = ax.get_legend_handles_labels()
        handles += h
        labels += l
    aR.legend(
        handles,
        labels,
        loc="lower left",
        bbox_to_anchor=(0.0, 1.09),
        ncol=2,
        frameon=False,
        fontsize=8,
    )
    for ax in (aT, aP, aR):
        ax.set_xlim(left, right * 1.015)
        ax.axvspan(left, 0.0, color=".94", zorder=-2)
        ax.axvline(0.0, color=".5", ls=":", lw=0.6)
        ax.grid(alpha=0.15)
    for ax in (aT, aP):
        ax.tick_params(axis="x", labelbottom=False)
    aR.set_xlabel("Time from first ramp onset (h)")
    fig.suptitle(sample, fontsize=11)
    filename = sample + ("_ramp" if zoom else "")
    save(fig, folder / filename)


def plot_radii(figdir, by, measured, published, samples):
    fig, axes = plt.subplots(1, 2, figsize=(10.8, 3.8), layout="constrained")
    for ax, phase, title in zip(
        axes, ("pre", "end"), ("Before the first ramp", "End of history")
    ):
        radii = [
            [1e9 * by[model][s]["states"][phase][key] for s in samples]
            for model, key in (
                ("legacy", "R"),
                ("coarsening", "Rb"),
                ("coarsening", "R"),
            )
        ]
        labels = ["Legacy", "Nicodemo 2026\nbulk", "Nicodemo 2026\ndislocations"]
        if phase == "end":
            radii.append([float(published[s]["radius_published_nm"]) for s in samples])
            radii.append([float(measured[s]["radius_exp_nm"]) for s in samples])
            labels.extend(["Barani 2020", "White SEM"])
        ax.boxplot(radii, whis=(0, 100))
        ax.set(
            xticks=range(1, len(labels) + 1),
            xticklabels=labels,
            ylim=(0, None),
            ylabel="Mean radius per case (nm)",
            title=title,
        )
        ax.grid(axis="y", alpha=0.15)
    save(fig, figdir / "8_radii_before_after")


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
        for model, color, marker in (
            ("legacy", LEGACY, "o"),
            ("Barani_table4", BARANI, "x"),
            ("coarsening", NEW, "o"),
        ):
            if model == "Barani_table4":
                if not published:
                    continue
                y = np.array([float(pub[s][published]) for s in samples])
            else:
                y = np.array(
                    [by[model][s]["states"]["end"][key] * scale for s in samples]
                )
            all_y.extend(y)
            ax.scatter(
                x,
                y,
                c=color,
                marker=marker,
                s=20,
                alpha=0.75,
                label=MODEL_LABELS[model],
            )
            ratio = y / x
            metrics[model][key] = dict(
                MAPE_pct=float(100 * np.mean(abs(ratio - 1))),
                RMSE_log10=float(np.sqrt(np.mean(np.log10(ratio) ** 2))),
                within2=int(np.sum((ratio >= 0.5) & (ratio <= 2))),
            )
        lo = min(min(all_y), x.min()) / 1.5
        hi = max(max(all_y), x.max()) * 1.5
        for f, style in [(1, "--"), (0.5, ":"), (2, ":")]:
            label = {1: "Parity", 0.5: "Factor of two"}.get(f)
            ax.plot([lo, hi], [f * lo, f * hi], c=".55", ls=style, lw=0.9, label=label)
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
    save(fig, figdir / "0_merit_all")
    fig, axes = plt.subplots(1, 3, figsize=(10.2, 3.8), layout="constrained")
    for ax, c in zip(axes, config[:3]):
        panel(ax, c)
    fig.legend(
        *axes[0].get_legend_handles_labels(),
        loc="outside upper center",
        ncol=3,
        frameon=False,
    )
    save(fig, figdir / "1_merit_three")
    for i, c in enumerate(config, 1):
        fig, ax = plt.subplots(figsize=(4.6, 4.5), layout="constrained")
        panel(ax, c)
        ax.legend(loc="best", fontsize=8)
        save(fig, figdir / f"{i + 1}_merit_{c[0]}")
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
    save(fig, figdir / "6_gas_before_after")
    campaign.write_csv(HERE / "inventories.csv", inventory)
    campaign.save(HERE / "mean_inventories.json", means)
    # End-of-history inventories for every sample; no averaging across samples.
    fig, axes = plt.subplots(
        2, 1, figsize=(13.8, 6.0), sharex=True, layout="constrained"
    )
    end_inventory = []
    for ax, model in zip(axes, ("coarsening", "legacy")):
        bottom = np.zeros(len(samples))
        for pool, label, color in zip(campaign.POOLS, POOL_LABELS, POOL_COLORS):
            y = np.array([by[model][s]["states"]["end"][pool] for s in samples])
            ax.bar(
                range(len(samples)),
                y,
                bottom=bottom,
                width=0.86,
                color=color,
                label=label,
            )
            bottom += y
        assert np.max(abs(bottom - 100.0)) < 1e-3
        for sample in samples:
            state = by[model][sample]["states"]["end"]
            end_inventory.append(
                dict(
                    sample=sample,
                    model=MODEL_LABELS[model],
                    time_h=state["time_h"],
                    produced_atoms_m3=state["produced"],
                    **{p: state[p] for p in campaign.POOLS},
                )
            )
        ax.set(
            ylim=(0, 100), ylabel="Gas / local produced (%)", title=MODEL_LABELS[model]
        )
        ax.grid(axis="y", alpha=0.15)
        ax.set_axisbelow(True)
    axes[-1].set_xticks(
        range(len(samples)), samples, rotation=65, ha="right", fontsize=9
    )
    fig.legend(
        *axes[0].get_legend_handles_labels(),
        loc="outside upper center",
        ncol=5,
        frameon=False,
    )
    save(fig, figdir / "7_gas_end_all_cases")
    campaign.write_csv(HERE / "inventories_end_all_cases.csv", end_inventory)
    plot_radii(figdir, by, measured, pub, samples)
    zoomed = []
    for sample in samples:
        r = by["coarsening"][sample]
        figure7_history(r, measured[sample], figdir / "Detailed_ramps")
        if r["states"]["end"]["time_h"] - r["onset_h"] > 8.0:
            zoomed.append(sample)
            figure7_history(r, measured[sample], figdir / "Detailed_ramps", zoom=True)
    lines = [
        "# White coarsening regression — Nicodemo 2026",
        "",
        "The merit figures compare Nicodemo 2026, SCIANTIX single-size and the published Barani 2020 predictions against White measurements for 38 ramp histories. "
        "Barani 2020 supplies radius, density and intragranular swelling endpoints. The table does not supply intergranular swelling or gas inventories.",
        "",
        "The regression template sets `iCoarseningLineCapture=1` for the Nicodemo cylindrical cell. "
        "The verification also exercises `0`, the Barani Eq. (6) line coefficient within the same coarsening solver.",
        "",
        "The selected Nicodemo 2026 model uses Carnahan–Starling pressure for dislocation bubbles, "
        "with the same pressure driving vacancy uptake. The volume is V=n*omega(T)+nv*Omega, "
        "where omega(T)=2*pi*dHS(T)^3/3 is the additive hard-sphere gas co-volume; "
        "K=1.6e+06 m^-1 is constant, with nu_d=K*positive full density generation during irradiation, excluding cooling rebound. "
        "Fresh Nd starts at zero. A stationary cylindrical cell supplies the line capture, with near-core and bulk "
        "resistances in series. Direct trapping uses the bulk diffusivity and the capture radius R_d + 0.21 nm. "
        "Both paths remain active throughout the history. The model retains three-dimensional coalescence. "
        "Atomic re-solution has no phi; effective number disappearance and coalescence reduce Nd. "
        "Vacancy uptake requires T>=1500 K and positive CS overpressure; Dv is 30 times the bulk vacancy diffusivity, with an explicitly calibrated effective prefactor. "
        "The density uses the frozen Veshchunov-White refit documented in context/dislocation_density. Parameters are globally selected closures, not independently measured constants. "
        "The effective hard-sphere diameter is that of Xe for the combined stable Xe/Kr inventory.",
        "The fast region extends to 50b (19.25 nm) as a model assumption. The vacancy mobility uses the calibrated effective "
        "prefactor. Pre-ramp agreement and gas partitions are diagnostics.",
        "",
        "The temperature dependence of this reference co-volume also enters geometric overlap and coalescence. "
        "It increases on cooling without creating gas or vacancies; mechanical equilibrium is not imposed.",
        "",
        "![Merit](figures/0_merit_all.png)",
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
        "means across 38 locations are arithmetic diagnostics, not integral rod FGR.",
        "",
        "![Inventories](figures/6_gas_before_after.png)",
        "",
        "[Radii before and after](figures/8_radii_before_after.png)",
        "",
        "![Current 4005-A history](figures/Detailed_ramps/4005-A.png)",
        "",
        "Detailed_ramps contains a full-history figure for every sample. Pressure is in Pa, radius in m, "
        "vacancies are counted per dislocation bubble, and swelling is total intragranular swelling in percent. "
        "All text is black. Axis maxima start at 1e8 Pa, 2.5e7 vacancies/bubble, 8e-8 m and10% and expand "
        "when a calculated or experimental value exceeds them. The temperature is shown separately.",
        "",
        "The dislocation-density correlation was refitted to White 2006 data that cannot be shown. "
        "A comparison with pre-ramp data was also performed; those data and the corresponding figure are not distributed.",
        "",
        "Two of 40 ramp histories are unavailable; local 4163 histories omit the 41 h hold listed in Barani Table 3.",
        "",
        "## Individual histories",
        "",
        "Pressure uses the Carnahan–Starling EOS. Pressure is undefined for initially empty sites. Experimental post-test points are plotted at the end of the "
        "history in the radius/swelling panel. In early-ramp zooms, final observations appear at the right edge as references, not as measurements at that time. No Barani time curves are included. Long histories have an early-ramp "
        "zoom in addition to the full-history figure; both show the actual final references.",
        "",
    ]
    for s in samples:
        line = f"- [{s}](figures/Detailed_ramps/{s}.png)"
        if s in zoomed:
            line += f" ([early-ramp zoom](figures/Detailed_ramps/{s}_ramp.png))"
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
            "regression/White_Coarsening/run.py",
            "regression/White_Coarsening/plot.py",
            "regression/White_Coarsening/verify.py",
        )
    ]
    campaign.save(
        HERE / "manifest.json",
        dict(
            official_base="2344575b7d246d02205f503b541b74f914f5f6b3",
            physical_model="Nicodemo 2026 with density-driven nucleation, cylindrical radial line capture, Carnahan–Starling and gas co-volume, and vacancy uptake above 1500 K",
            site_closure=dict(
                K_per_m=1600000,
                initial="Nd=0 for fresh full histories; no automatic seed",
                source="K*positive density generation during irradiation, excluding cooling rebound",
                number_loss="coalescence and alpha_d*phi_d*Nd; phi only in number loss, proportional vacancy return",
                capture=dict(
                    line="Stationary cylindrical diffusion cell with two radial resistances in series",
                    core_radius_m=1.925e-9,
                    fast_region_radius_b=50,
                    fast_region_radius_m=19.25e-9,
                    direct="4*pi*Dbulk*(Rd + 0.21 nm)*Nd",
                ),
            ),
            pressure_eos=dict(
                name="Carnahan-Starling",
                effective_species="Xe for combined stable Xe/Kr",
                packing_domain="0 <= eta < 1; no clipping",
                physical_inventory_bound="0<=eta<=1/4 for V=n*u+nv*Omega and nonnegative stocks",
            ),
            volume_closure=dict(
                dislocation_bubbles="n*omega(T)+nv*Omega; omega(T)=2*pi*dHS(T)^3/3",
                bulk_bubbles="n*Omega",
                overlap_growth="positive Delta[omega(T)*Md+Omega*Qv] per bubble, excluding number-only dilution",
            ),
            binary_sha256=sorted({r["binary_sha256"] for r in results}),
            outer_steps=sorted({r["steps"] for r in results}),
            relative_tolerance=sorted({r["tolerance"] for r in results}),
            template_sha256={
                name: campaign.sha(HERE / name)
                for name in ("input_settings.txt", "input_scaling_factors.txt")
            },
            source_sha256={str(p.relative_to(ROOT)): campaign.sha(p) for p in sources},
            input_sha256={
                str(p.relative_to(HERE)): campaign.sha(p)
                for sample in sorted(samples)
                for p in sorted((HERE / "inputs" / sample).iterdir())
                if p.is_file()
            },
            figures={
                str(p.relative_to(HERE)): campaign.sha(p)
                for p in sorted(figdir.rglob("*"))
                if p.is_file() and p.suffix == ".png"
            },
        ),
    )
