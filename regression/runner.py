import os
from regression.baker.baker import run_baker

def run_all_baker(base_path: str = None, mode_gold: int = 0):
    if base_path is None:
        base_path = os.path.join(os.path.dirname(__file__), "baker")

    results = []
    for name in sorted(os.listdir(base_path)):
        if "Baker" in name:
            case_dir = os.path.join(base_path, name)
            if os.path.isdir(case_dir):
                print(f"Running Baker case: {name}")
                ok = run_baker(case_dir, mode_gold)
                results.append((name, ok))

    return results

if __name__ == "__main__":
    results = run_all_baker()
    for name, ok in results:
        print(f"{name}: {'PASS' if ok else 'FAIL'}")
