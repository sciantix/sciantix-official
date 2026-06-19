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
    "font.size": 20,
    "axes.labelsize": 20,
    "axes.labelweight": "bold",
    "axes.titlesize": 20,
    "axes.titleweight": "bold",
    "xtick.labelsize": 20,
    "ytick.labelsize": 20,
    "legend.fontsize": 20,
    "figure.dpi": 300,
    "axes.grid": True,
    "grid.alpha": 0.5,
    "grid.linestyle": "--",
    "lines.linewidth": 2,
    "lines.markersize": 6,
    "legend.frameon": False,
})

SCRIPT_DIR = Path(__file__).resolve().parent
PROJECT_ROOT = SCRIPT_DIR.parents[1]
sys.path.insert(0, str(PROJECT_ROOT / "oxired_lib"))

from oxired.fission_yields import (  # noqa: E402
    GASES_AND_VOLATILE_FPS,
    METALLIC_PRECIPITATES,
    OXIDE_PRECIPITATES,
    PHENIX_FISSION_YIELDS,
    SOLID_SOLUTION,
)


OUTPUT_STEM = SCRIPT_DIR / "fission_product_yield_bubbles"
PIE_OUTPUT_STEM = SCRIPT_DIR / "atomic_percent_pie"
ELEMENT_GAP = 0.4
CATEGORY_SPACING = 2.0

DISTINCT_COLOR_VALUES = [
    "#6baed6", "#fd8d3c", "#74c476", "#9e9ac8", "#e377c2", "#8c564b",
    "#17becf", "#bcbd22", "#c7a9d9", "#fdae6b", "#a1d99b", "#bcbddc",
    "#fdd0a2", "#9ecae1", "#c994c7", "#bdbdbd", "#66c2a5", "#fc8d62",
    "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f", "#e5c494", "#b3b3b3",
]


CATEGORY_STYLE = {
    SOLID_SOLUTION: {
        "label": "Solid solution",
        "heading": "Solid\nsolution",
        "color": DISTINCT_COLOR_VALUES[0],
        "edge": DISTINCT_COLOR_VALUES[0],
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

    fig_width = max(10.5, max_row_width + 3.2)
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
                fontsize=18,
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
        "Elemental fission-product yields in Phenix MOX fuel\nData: Samuelsson et al., Journal of Nuclear Materials 532 (2020) 151969.",
        fontweight="bold",
        y =1.02,
        loc="center"
    )

    axis.set_xlim(label_x - 0.15, max_row_width + 0.35)
    axis.set_ylim(-1.2, (len(CATEGORY_ORDER) - 1) * CATEGORY_SPACING + 1.0)
    axis.set_aspect("equal", adjustable="box")
    axis.axis("off")

    fig.savefig(f"{OUTPUT_STEM}.svg", facecolor=fig.get_facecolor())
    plt.close(fig)


def draw_atomic_percent_pie() -> None:
    elements = [element for element, _ in ATOMIC_PERCENT_COMPOSITION]
    values = [value for _, value in ATOMIC_PERCENT_COMPOSITION]
    colors = DISTINCT_COLOR_VALUES[: len(values)]
    outside_labels = ["" for _ in ATOMIC_PERCENT_COMPOSITION]

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
        textprops={
            "fontweight": "bold",
            "color": "#171717",
        },
    )

    external_labels = []
    for wedge, element, value in zip(wedges, elements, values):
        theta = math.radians((wedge.theta1 + wedge.theta2) / 2.0)
        if value <= 2.0:
            external_labels.append(
                {
                    "element": element,
                    "x": math.cos(theta),
                    "y": math.sin(theta),
                }
            )
            continue

        radius = 0.75
        axis.text(
            radius * math.cos(theta),
            radius * math.sin(theta)+0.05,
            f"{element}",
            ha="center",
            va="center",
            fontweight="bold",
            color="#171717",
        )
        axis.text(
            radius * math.cos(theta),
            radius * math.sin(theta)-0.05,
            f"{value:.0f}%",
            ha="center",
            va="center",
            color="#171717",
        )

    for label in external_labels:
        radius = 1.08
        offset = 0.0
        if label["element"] == "Co":
            offset = -0.03
        axis.text(
            radius * label["x"],
            radius * label["y"] + offset,
            label["element"],
            ha="center",
            va="center",
            fontweight="bold",
            fontsize=16,
            color="#171717",
        )

    axis.set_title(
        "JOG atomic composition",
        fontweight="bold",
        y=0.94,
        loc="center",
    )
    axis.set_aspect("equal")
    fig.tight_layout()

    fig.savefig(f"{PIE_OUTPUT_STEM}.svg", facecolor=fig.get_facecolor())
    plt.close(fig)


if __name__ == "__main__":
    draw_bubble_plot()
    draw_atomic_percent_pie()
