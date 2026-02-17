"""
sciantix regression suite
author: Giovanni Zullo
"""

import os
import datetime

def generate_html_report(results, output_dir):
    """
    Generate an HTML report for regression results.
    
    Args:
        results: list of tuples (test_name, ok, message)
        output_dir: directory to save index.html
    """
    
    passed = len([r for r in results if r[1]])
    failed = len(results) - passed
    
    html = f"""
    <!DOCTYPE html>
    <html>
    <head>
        <title>SCIANTIX Regression Report</title>
        <style>
            body {{ font-family: sans-serif; margin: 20px; }}
            h1 {{ color: #333; }}
            .summary {{ margin-bottom: 20px; padding: 10px; background-color: #f0f0f0; border-radius: 5px; }}
            table {{ border-collapse: collapse; width: 100%; }}
            th, td {{ border: 1px solid #ddd; padding: 8px; text-align: left; }}
            th {{ background-color: #f2f2f2; }}
            tr:nth-child(even) {{ background-color: #f9f9f9; }}
            .pass {{ color: green; font-weight: bold; }}
            .fail {{ color: red; font-weight: bold; }}
        </style>
    </head>
    <body>
        <h1>SCIANTIX Regression Report</h1>
        
        <div class="summary">
            <p><strong>Date:</strong> {datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")}</p>
            <p><strong>Total Tests:</strong> {len(results)}</p>
            <p><strong>Passed:</strong> <span class="pass">{passed}</span></p>
            <p><strong>Failed:</strong> <span class="fail">{failed}</span></p>
        </div>

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
        status_class = "pass" if ok else "fail"
        status_text = "PASS" if ok else "FAIL"
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
