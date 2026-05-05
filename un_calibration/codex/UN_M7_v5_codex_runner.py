#!/usr/bin/env python3
"""Codex-owned structured runner for UN M7 v5 calibration blocks.

This wrapper does not modify the v5 calibration script.  It imports v5 scoring,
patches only the Optuna sampling ranges for named blocks, and writes each study
to a separate output directory under UN_M7_v5_codex_results/.
"""

from __future__ import annotations

import argparse
import math
import os
from dataclasses import dataclass
from typing import Dict, Tuple

import UN_M7_optuna_calibration as base
import UN_M7_optuna_calibration_v5 as v5


ROOT = "UN_M7_v5_codex_results"


@dataclass(frozen=True)
class BlockConfig:
    name: str
    trials: int
    ranges: Dict[str, Tuple[float, float, bool]]
    dv_min: float = 0.2
    dv_max: float = 2.0


DEFAULT_RANGES: Dict[str, Tuple[float, float, bool]] = {
    "f_n": (1.0e-10, 3.0e-6, True),
    "K_d": (1.0e5, 1.5e6, True),
    "rho_d": (5.0e12, 8.0e13, True),
    "fission_rate": (4.0e19, 8.0e19, False),
    "Dv_scale": (0.2, 2.0, True),
    "Dg_scale": (0.15, 4.0, True),
    "D2_xe_scale": (0.05, 20.0, True),
    "b_scale": (0.05, 8.0, True),
    "gb_scale": (0.05, 8.0, True),
    "gd_scale": (0.05, 10.0, True),
    "coalescence_d_scale": (0.1, 20.0, True),
    "capture_scale": (0.01, 8.0, True),
}


def merged_ranges(**updates: Tuple[float, float, bool]) -> Dict[str, Tuple[float, float, bool]]:
    ranges = dict(DEFAULT_RANGES)
    ranges.update(updates)
    return ranges


def overlay(base_ranges: Dict[str, Tuple[float, float, bool]], **updates: Tuple[float, float, bool]) -> Dict[str, Tuple[float, float, bool]]:
    ranges = dict(base_ranges)
    ranges.update(updates)
    return ranges


NEAR = {
    "Dv_scale": (0.5, 1.5, True),
    "Dg_scale": (0.5, 2.0, True),
    "D2_xe_scale": (0.3, 3.0, True),
    "b_scale": (0.3, 3.0, True),
    "gb_scale": (0.3, 3.0, True),
    "gd_scale": (0.5, 3.0, True),
    "coalescence_d_scale": (0.5, 5.0, True),
    "capture_scale": (0.05, 1.5, True),
    "f_n": (1.0e-9, 1.0e-6, True),
    "K_d": (2.0e5, 8.0e5, True),
    "rho_d": (1.0e13, 6.0e13, True),
    "fission_rate": (4.0e19, 8.0e19, False),
}

MODERATE = {
    "Dv_scale": (0.4, 1.6, True),
    "Dg_scale": (0.4, 2.5, True),
    "D2_xe_scale": (0.2, 5.0, True),
    "b_scale": (0.2, 4.0, True),
    "gb_scale": (0.2, 4.0, True),
    "gd_scale": (0.3, 4.0, True),
    "coalescence_d_scale": (0.4, 8.0, True),
    "capture_scale": (0.03, 2.0, True),
    "f_n": (3.0e-9, 1.0e-6, True),
    "K_d": (2.0e5, 1.0e6, True),
    "rho_d": (1.0e13, 6.0e13, True),
    "fission_rate": (4.0e19, 8.0e19, False),
}


BLOCKS: Dict[str, BlockConfig] = {
    "A_nominal_near": BlockConfig("A_nominal_near", 300, merged_ranges(**NEAR), 0.5, 1.5),
    "B_standard_wide": BlockConfig("B_standard_wide", 700, dict(DEFAULT_RANGES), 0.2, 2.0),
    "C1_diffusion_escape": BlockConfig(
        "C1_diffusion_escape", 200,
        overlay(MODERATE, Dv_scale=(0.05, 3.0, True), Dg_scale=(0.08, 6.0, True), D2_xe_scale=(0.02, 30.0, True)),
        0.05, 3.0,
    ),
    "C2_resolution_escape": BlockConfig(
        "C2_resolution_escape", 200,
        overlay(MODERATE, b_scale=(0.02, 12.0, True), D2_xe_scale=(0.1, 8.0, True)),
        0.4, 1.6,
    ),
    "C3_trapping_escape": BlockConfig(
        "C3_trapping_escape", 200,
        overlay(MODERATE, gb_scale=(0.03, 12.0, True), gd_scale=(0.03, 15.0, True)),
        0.4, 1.6,
    ),
    "C4_microstructure_escape": BlockConfig(
        "C4_microstructure_escape", 200,
        overlay(MODERATE, f_n=(1.0e-10, 3.0e-6, True), K_d=(8.0e4, 1.5e6, True), rho_d=(5.0e12, 8.0e13, True)),
        0.4, 1.6,
    ),
    "C5_coalescence_capture_escape": BlockConfig(
        "C5_coalescence_capture_escape", 200,
        overlay(MODERATE, coalescence_d_scale=(0.08, 20.0, True), capture_scale=(0.005, 8.0, True)),
        0.4, 1.6,
    ),
    "D1_b_gd_escape": BlockConfig(
        "D1_b_gd_escape", 250,
        overlay(MODERATE, b_scale=(0.02, 12.0, True), gd_scale=(0.03, 15.0, True)),
        0.4, 1.6,
    ),
    "D2_Dv_coalescence_escape": BlockConfig(
        "D2_Dv_coalescence_escape", 250,
        overlay(MODERATE, Dv_scale=(0.05, 3.0, True), coalescence_d_scale=(0.08, 20.0, True)),
        0.05, 3.0,
    ),
    "D3_Kd_rhod_coalescence_escape": BlockConfig(
        "D3_Kd_rhod_coalescence_escape", 250,
        overlay(MODERATE, K_d=(8.0e4, 1.5e6, True), rho_d=(5.0e12, 8.0e13, True), coalescence_d_scale=(0.08, 20.0, True)),
        0.4, 1.6,
    ),
    "D4_capture_gd_escape": BlockConfig(
        "D4_capture_gd_escape", 250,
        overlay(MODERATE, capture_scale=(0.005, 8.0, True), gd_scale=(0.03, 15.0, True)),
        0.4, 1.6,
    ),
    "E_all_wide_exploratory": BlockConfig("E_all_wide_exploratory", 500, dict(DEFAULT_RANGES), 0.2, 2.0),
}


def in_range(seed: Dict[str, float], ranges: Dict[str, Tuple[float, float, bool]]) -> bool:
    for key, (lo, hi, _) in ranges.items():
        if key in seed and not (lo <= float(seed[key]) <= hi):
            return False
    return True


def make_candidate_sampler(ranges: Dict[str, Tuple[float, float, bool]]):
    def optuna_candidate_from_trial(trial, family: str):
        vals = {}
        for key, (lo, hi, log) in ranges.items():
            vals[key] = trial.suggest_float(key, lo, hi, log=log)
        if family == "baseline":
            vals["capture_scale"] = 0.0
        return base.Candidate(label=f"{family}_v5_codex_trial_{trial.number:05d}", **vals)
    return optuna_candidate_from_trial


def make_seed_enqueue(ranges: Dict[str, Tuple[float, float, bool]]):
    def enqueue(study):
        seeds = []
        for source in (getattr(base, "enqueue_known_good_trials", None),):
            if source is None:
                continue
        for seed in getattr(v5, "CODEX_CAPTURE_ONLY_SEEDS", []):
            if in_range(seed, ranges):
                seeds.append(seed)
        # Include base known-good trials indirectly by relying on v5's list only for strict range safety.
        for seed in seeds:
            try:
                study.enqueue_trial(seed, skip_if_exists=True)
            except TypeError:
                study.enqueue_trial(seed)
    return enqueue


def append_worklog(text: str) -> None:
    os.makedirs(ROOT, exist_ok=True)
    with open(os.path.join(ROOT, "WORKLOG.md"), "a", encoding="utf-8") as f:
        f.write(text.rstrip() + "\n")


def run_block(config: BlockConfig, trials_override: int | None, final: int, plots: bool) -> None:
    trials = config.trials if trials_override is None else trials_override
    out_dir = os.path.join(ROOT, config.name)
    os.makedirs(out_dir, exist_ok=True)

    append_worklog(
        f"\n## Starting {config.name}\n\n"
        f"- Trials requested: {trials}\n"
        f"- Output dir: `{out_dir}`\n"
        f"- Final reruns requested now: {final}\n"
        f"- Plots enabled: {plots}\n"
        f"- Range changes are limited to this Codex wrapper; v5 scoring/equations are reused."
    )

    v5.DV_FIXED = None
    v5.DV_MIN = config.dv_min
    v5.DV_MAX = config.dv_max
    v5.W_RIZK_PRIOR = 0.20
    v5.W_PARTITION = 0.80
    v5.W_QGB = 0.70
    v5.W_RADIUS_GUARD = 0.55
    v5.W_RADIUS_SATURATION = 0.75
    v5.RD2000_MAX_NM = 800.0
    v5.RD_RATIO_MAX = 2.8
    v5.RD1800_SOFT_MAX_NM = 600.0
    v5.RD1900_SOFT_MAX_NM = 700.0
    v5.RD_POST1800_DELTA_MAX_NM = 350.0
    v5.RD_LAST_INCREMENT_FACTOR_MAX = 0.75
    v5.RD_1900_2000_RATIO_MAX = 1.35
    base.W_PRESSURE = 0.70
    base.PRESSURE_FREE_FACTOR = 2.0

    base.optuna_candidate_from_trial = make_candidate_sampler(config.ranges)
    base.score_candidate = v5.score_candidate_v5
    base.enqueue_known_good_trials = make_seed_enqueue(config.ranges)

    base.run_optuna_calibration(
        family="capture_only",
        n_trials=trials,
        output_dir=out_dir,
        fast_dt_h=12.0,
        fast_n_modes=22,
        final_dt_h=1.0,
        final_n_modes=40,
        n_top_final=final,
        use_full_exp_fast=False,
        make_plots=plots,
    )

    append_worklog(
        f"\n## Completed {config.name}\n\n"
        f"- Fast Optuna run finished.\n"
        f"- Next step: run block analysis and decide whether to continue or change strategy."
    )


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--block", choices=sorted(BLOCKS), required=True)
    parser.add_argument("--n-trials", type=int, default=None)
    parser.add_argument("--n-top-final", type=int, default=0)
    parser.add_argument("--plots", action="store_true")
    args = parser.parse_args()
    run_block(BLOCKS[args.block], args.n_trials, args.n_top_final, args.plots)


if __name__ == "__main__":
    main()
