import json, pathlib, sys
sys.path.insert(0, sys.argv.pop(1))
import regression.runner as runner
original_report = runner.generate_html_report
def capture(results, output_dir):
    pathlib.Path(output_dir).parent.joinpath("baseline_case_results.json").write_text(json.dumps(results, indent=2)+"\n")
    return original_report(results, output_dir)
runner.generate_html_report = capture
runner.main()
