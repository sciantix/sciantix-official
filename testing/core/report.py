"""
sciantix testing suite
author: Giovanni Zullo, Elisa Cappellari
"""

import os
import datetime

def generate_html_report(results, output_dir, oc_summary=None):
    """
    Generate an HTML report for test results.

    Args:
        results: list of tuples (test_name, ok, message), ok in
            {True, False, None} (None = skipped)
        output_dir: directory to save report.html
        oc_summary: optional string (one note per line) describing groups
            that were degraded or skipped for lack of OpenCalphad/a required
            database, surfaced in its own report section
    """

    passed = len([r for r in results if r[1] is True])
    failed = len([r for r in results if r[1] is False])
    skipped = len([r for r in results if r[1] is None])

    oc_summary_html = ""
    if oc_summary:
        lines = "".join(f"<li>{line}</li>" for line in oc_summary.splitlines() if line.strip())
        oc_summary_html = f"""
        <div class="oc-summary">
            <h2>OpenCalphad availability notes</h2>
            <ul>{lines}</ul>
        </div>
        """

    html = f"""
    <!DOCTYPE html>
    <html>
    <head>
        <title>SCIANTIX Testing Report</title>
        <style>
            body {{ font-family: sans-serif; margin: 20px; }}
            h1 {{ color: #333; }}
            .summary {{ margin-bottom: 20px; padding: 10px; background-color: #f0f0f0; border-radius: 5px; }}
            .oc-summary {{ margin-bottom: 20px; padding: 10px; background-color: #fff8e1; border-radius: 5px; }}
            table {{ border-collapse: collapse; width: 100%; }}
            th, td {{ border: 1px solid #ddd; padding: 8px; text-align: left; }}
            th {{ background-color: #f2f2f2; }}
            tr:nth-child(even) {{ background-color: #f9f9f9; }}
            .pass {{ color: green; font-weight: bold; }}
            .fail {{ color: red; font-weight: bold; }}
            .skip {{ color: #b8860b; font-weight: bold; }}
        </style>
    </head>
    <body>
        <h1>SCIANTIX Testing Report</h1>

        <div class="summary">
            <p><strong>Date:</strong> {datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")}</p>
            <p><strong>Total Tests:</strong> {len(results)}</p>
            <p><strong>Passed:</strong> <span class="pass">{passed}</span></p>
            <p><strong>Failed:</strong> <span class="fail">{failed}</span></p>
            <p><strong>Skipped:</strong> <span class="skip">{skipped}</span></p>
        </div>
        {oc_summary_html}
        <table>
            <thead>
                <tr>
                    <th>Test Case</th>
                    <th>Status</th>
                    <th>Message</th>
                </tr>
            </thead>
            <tbody>
    """

    for name, ok, msg in results:
        status_class = "pass" if ok is True else ("skip" if ok is None else "fail")
        status_text = "PASS" if ok is True else ("SKIP" if ok is None else "FAIL")
        message = msg if msg else ""

        html += f"""
                <tr>
                    <td>{name}</td>
                    <td class="{status_class}">{status_text}</td>
                    <td>{message}</td>
                </tr>
        """

    html += """
            </tbody>
        </table>
    </body>
    </html>
    """

    report_path = os.path.join(output_dir, "report.html")
    with open(report_path, "w") as f:
        f.write(html)

    print(f"\nReport generated: {report_path}")
