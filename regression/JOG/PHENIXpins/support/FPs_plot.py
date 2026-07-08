#!/usr/bin/env python3
"""Draw a book-style bubble illustration of Phenix fission-product yields."""
from __future__ import annotations

import math
import os
import sys
from pathlib import Path

os.environ.setdefault("MPLCONFIGDIR", "/tmp/matplotlib")

import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
from matplotlib.patches import Circle
plt.style.use("seaborn-v0_8-whitegrid")
plt.rcParams.update({
    "figure.figsize": (10, 7),
    "font.family": "serif",
    "font.serif": ["Times New Roman", "Times", "Nimbus Roman", "DejaVu Serif"],
    "mathtext.fontset": "dejavuserif",
    "font.size": 20,
    "axes.labelsize": 20,
    "axes.titlesize": 20,
    "xtick.labelsize": 20,
    "ytick.labelsize": 20,
    "legend.fontsize": 20,
    "figure.dpi": 300,
    "axes.grid": True,
    "grid.alpha": 0.5,
    "grid.linestyle": "--",
    "lines.linewidth": 3,
    "lines.markersize": 6,
    "legend.frameon": False,
})

SCRIPT_DIR = Path(__file__).resolve().parent
PROJECT_ROOT = SCRIPT_DIR.parents[1]
WSL_ROOT = SCRIPT_DIR.parents[5]
sys.path.insert(0, str(PROJECT_ROOT / "oxired_lib"))

from oxired.fission_yields import (  # noqa: E402
    GASES_AND_VOLATILE_FPS,
    METALLIC_PRECIPITATES,
    OXIDE_PRECIPITATES,
    PHENIX_FISSION_YIELDS,
    SOLID_SOLUTION,
)


OUTPUT_STEM = WSL_ROOT / "OverLeaf/JOGSCIANTIX/Images" / "fission_product_yield_bubbles"
PIE_OUTPUT_STEM = WSL_ROOT / "OverLeaf/JOGSCIANTIX/Images" / "atomic_percent_pie"
ELEMENT_GAP = 0.4
CATEGORY_SPACING = 2.0

DISTINCT_COLOR_VALUES = [
    "#736F3F", "#BFAE56", "#B29DA6", "#D9AF32", "#A66226", "#733426",
    "#737675", "#9D6953", "#363726",  "#785C2D",
]

# Fixed element -> color assignment shared with run_and_plot_JOG.py's
# ELEMENT_COLORS, so the JOG composition pie here uses the same color for an
# element as the SCIANTIX/GERMINAL comparison pies do. Keep both in sync.
ELEMENT_COLORS = {
    "Cs": "#BFAE56",
    "O": "#A66226",
    "Mo": "#B29DA6",
    "Ba": "#D9AF32",
    "I": "#733426",
    "Rb": "#737675",
    "Zr": "#9D6953",
    "Fe": "#363726",
    "Co": "#785C2D",
    "Pd": "#736F3F",
    "Rh": "#4F6D5A",
    "Ru": "#5B7A99",
    "Tc": "#8A4F7D",
    "Te": "#C1666B",
}


CATEGORY_STYLE = {
    SOLID_SOLUTION: {
        "label": "Solid solution",
        "heading": "Solid\nsolution",
        "color": DISTINCT_COLOR_VALUES[4],
        "edge": DISTINCT_COLOR_VALUES[4],
    },
    OXIDE_PRECIPITATES: {
        "label": "Oxide precipitates",
        "heading": "Oxide\nprecipitates",
        "color": DISTINCT_COLOR_VALUES[1],
        "edge": DISTINCT_COLOR_VALUES[1],
    },
    METALLIC_PRECIPITATES: {
        "label": "Metallic precipitates",
        "heading": "Metallic\nprecipitates",
        "color": DISTINCT_COLOR_VALUES[2],
        "edge": DISTINCT_COLOR_VALUES[2],
    },
    GASES_AND_VOLATILE_FPS: {
        "label": "Gases and volatile FPs",
        "heading": "Gases and\nvolatile FPs",
        "color": DISTINCT_COLOR_VALUES[3],
        "edge": DISTINCT_COLOR_VALUES[3],
    },
}

CATEGORY_ORDER = (
    SOLID_SOLUTION,
    OXIDE_PRECIPITATES,
    METALLIC_PRECIPITATES,
    GASES_AND_VOLATILE_FPS,
)

ATOMIC_PERCENT_COMPOSITION = (
    ("O", 36.01),
    ("Cs", 27.26),
    ("Mo", 19.19),
    ("Ba", 10.31),
    ("I", 3.35),
    ("Rb", 1.20),
    ("Zr", 1.16),
    ("Fe", 1.15),
    ("Co", 0.35),
)


def radius_from_yield(yield_percent: float) -> float:
    """Scale radius so bubble area, not diameter, tracks fission yield."""
    if yield_percent <= 0.0:
        return 0.0
    return 0.35 + 0.13 * math.sqrt(yield_percent)


def grouped_yields():
    return {
        group: [entry for entry in PHENIX_FISSION_YIELDS if entry.group == group]
        for group in CATEGORY_ORDER
    }


def packed_x_positions(entries) -> list[float]:
    radii = [
        radius_from_yield(entry.yield_percent_fp_per_fission)
        for entry in entries
    ]
    positions: list[float] = []
    cursor = 0.0
    for index, radius in enumerate(radii):
        if index == 0:
            cursor = radius
        else:
            cursor += radii[index - 1] + radius + ELEMENT_GAP
        positions.append(cursor)
    return positions


def draw_bubble_plot() -> None:
    groups = grouped_yields()
    visible_groups = {
        group: [
            entry
            for entry in entries
            if entry.yield_percent_fp_per_fission > 0.0
        ]
        for group, entries in groups.items()
    }
    group_x_positions = {
        group: packed_x_positions(entries)
        for group, entries in visible_groups.items()
    }
    row_widths = [
        positions[-1]
        + radius_from_yield(visible_groups[group][-1].yield_percent_fp_per_fission)
        if positions
        else 0.0
        for group, positions in group_x_positions.items()
    ]
    max_row_width = max(row_widths)

    fig_width = max(10.5, max_row_width)
    fig_height = 7

    fig, axis = plt.subplots(figsize=(fig_width, fig_height), constrained_layout=True)
    y_positions = {
        group: (len(CATEGORY_ORDER) - 1 - row) * CATEGORY_SPACING
        for row, group in enumerate(CATEGORY_ORDER)
    }
    label_x = -1.1

    for group, entries in visible_groups.items():
        style = CATEGORY_STYLE[group]
        y = y_positions[group]

        axis.text(
            label_x,
            y,
            style["label"],
            ha="right",
            va="center",
            fontweight="bold",
            color=style["edge"],
        )

        for x, entry in zip(group_x_positions[group], entries):
            radius = radius_from_yield(entry.yield_percent_fp_per_fission)


            alpha = 0.1
            if entry.element in ["Kr", "Xe", "Cs", "Mo", "Tc", "Ru", "Rh", "Pd", "Ba"]:
                alpha = 0.7

            bubble = Circle(
                (x, y),
                radius,
                facecolor=style["color"],
                edgecolor=style["edge"],
                alpha=alpha,
                zorder=2,
            )
            axis.add_patch(bubble)

            axis.text(
                x,
                y + 0.045,
                entry.element,
                ha="center",
                va="center",
                fontweight="bold",
                fontsize=20,
                color="#171717",
                zorder=4,
            )

            yield_label = f"{entry.yield_percent_fp_per_fission:g}%"
            if radius >= 0:
                axis.text(
                    x,
                    y - 0.23,
                    yield_label,
                    fontsize=16,
                    ha="center",
                    va="center",
                    zorder=4,
                )

    axis.set_title(
        "Fission-product yields in MOX fuel", #\nData extracted from K. Samuelsson et al. / Journal of Nuclear Materials 532 (2020) 151969.",
        fontweight="bold",
        y =1.02,
        loc="center"
    )

    axis.set_xlim(label_x - 0.15, max_row_width + 0.35)
    axis.set_ylim(-1.2, (len(CATEGORY_ORDER) - 1) * CATEGORY_SPACING + 1.0)
    axis.set_aspect("equal", adjustable="box")
    axis.axis("off")

    fig.savefig(f"{OUTPUT_STEM}.png", facecolor=fig.get_facecolor())
    plt.close(fig)


def place_pie_wedge_labels(
    axis: plt.Axes,
    wedges: list,
    elements: list[str],
    values: list[float],
    value_labels: list[str],
    pie_radius: float,
    *,
    element_fontsize: float = 16,
    value_fontsize: float = 14,
    small_value_threshold: float = 2.0,
) -> None:
    """Label every wedge outside the pie with a leader line.

    ``elements``/``values``/``value_labels`` must be sorted value-descending
    (matching the order the wedges were drawn in), so wedges below
    ``small_value_threshold`` end up consecutive at the end of the sequence:
    they are all narrow and close together in angle, so instead of placing
    them purely by angle (which makes them overlap) they are stacked as an
    evenly spaced label column.
    """
    n_wedges = len(wedges)
    cluster_start = n_wedges
    for index in range(n_wedges - 1, -1, -1):
        if values[index] < small_value_threshold:
            cluster_start = index
        else:
            break

    thetas = [math.radians((wedge.theta1 + wedge.theta2) / 2.0) for wedge in wedges]
    cluster_size = n_wedges - cluster_start
    cluster_sign = 1.0
    cluster_center_y = 0.0
    line_spacing = 0.24
    if cluster_size > 0:
        cluster_sign = 1.0 if sum(math.cos(t) for t in thetas[cluster_start:]) >= 0.0 else -1.0
        cluster_center_y = sum(math.sin(t) for t in thetas[cluster_start:]) / cluster_size

    for index, (wedge, element, value, value_label, theta) in enumerate(
        zip(wedges, elements, values, value_labels, thetas)
    ):
        x, y = math.cos(theta), math.sin(theta)

        if index >= cluster_start:
            cluster_index = index - cluster_start
            text_x = pie_radius * 1.62 * cluster_sign
            text_y = (
                cluster_center_y
                + line_spacing * (cluster_size - 1) / 2.0
                - line_spacing * cluster_index
            )
        else:
            sign = 1.0 if x >= 0.0 else -1.0
            anchor_radius = pie_radius * (1.32 if index % 2 == 0 else 1.58)
            text_x = anchor_radius * sign
            text_y = anchor_radius * y

        ha = "left" if text_x >= 0.0 else "right"
        axis.annotate(
            "",
            xy=(pie_radius * x, pie_radius * y),
            xytext=(text_x, text_y),
            arrowprops=dict(
                arrowstyle="-",
                color="#8a8a83",
                lw=1.0,
                connectionstyle=f"angle,angleA=0,angleB={math.degrees(theta):.1f}",
            ),
            zorder=3,
        )
        axis.text(
            text_x, text_y + 0.065, element,
            ha=ha, va="center", fontsize=element_fontsize, fontweight="bold", color="#171717",
        )
        axis.text(
            text_x, text_y - 0.065, value_label,
            ha=ha, va="center", fontsize=value_fontsize, color="#171717",
        )


def draw_atomic_percent_pie() -> None:
    filtered_composition = [(element, value) for element, value in ATOMIC_PERCENT_COMPOSITION if value >= 1.0]
    elements = [element for element, _ in filtered_composition]
    values = [value for _, value in filtered_composition]
    colors = [ELEMENT_COLORS.get(element, "#999999") for element in elements]
    outside_labels = ["" for _ in filtered_composition]

    pie_radius = 1.0
    fig, axis = plt.subplots(figsize=(8.5, 7.0), constrained_layout=True)
    wedges, labels = axis.pie(
        values,
        labels=outside_labels,
        colors=colors,
        startangle=20,
        counterclock=False,
        wedgeprops={
            "alpha": 0.7,
        },
        radius=pie_radius,
    )

    place_pie_wedge_labels(
        axis, wedges, elements, values, [f"{value:.0f}%" for value in values], pie_radius,
        element_fontsize=15, value_fontsize=13,
    )

    axis.set_xlim(-2.1 * pie_radius, 2.1 * pie_radius)
    axis.set_ylim(-1.75 * pie_radius, 1.75 * pie_radius)
    axis.set_aspect("equal")
    fig.tight_layout()

    fig.savefig(f"{PIE_OUTPUT_STEM}.png", facecolor=fig.get_facecolor())
    plt.close(fig)


if __name__ == "__main__":
    draw_bubble_plot()
    draw_atomic_percent_pie()
