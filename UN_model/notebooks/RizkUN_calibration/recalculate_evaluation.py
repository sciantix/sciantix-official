"""Current stage: load saved OAT assessment or reevaluate saved K_d refinement.
No solver calls, parameter combinations or new proposals are generated.
The previous OAT evaluator is archived in evaluation_archive/before_Kd_refinement/.
"""
from pathlib import Path
import importlib.util
import pandas as pd

def load_saved_evaluation(output_dir):
    out=Path(output_dir)
    summary=pd.read_csv(out/'candidate_summary.csv')
    comparison=pd.read_csv(out/'pareto_comparison.csv')
    directions=pd.read_csv(out/'evaluation_directions.csv')
    assert len(summary)==len(comparison)==13
    return summary,comparison,directions

def recalculate_evaluation(output_dir):
    """Recompute the five-value refinement comparison from CSVs; load the original OAT assessment."""
    out=Path(output_dir)
    source=out/'Kd_refinement'/'evaluate_refinement.py'
    spec=importlib.util.spec_from_file_location('rizk_saved_kd_refinement_evaluation',source)
    module=importlib.util.module_from_spec(spec);spec.loader.exec_module(module)
    module.evaluate_refinement(source.parent)
    return load_saved_evaluation(out)

if __name__=='__main__':recalculate_evaluation(Path(__file__).resolve().parent)
