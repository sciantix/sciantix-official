import os

GREEN = "\033[92m"
RED   = "\033[91m"
RESET = "\033[0m"


def color(text: str, ok: bool):
    """
    Color helper for printing PASS/FAIL results.
    """
    return f"{GREEN}{text}{RESET}" if ok else f"{RED}{text}{RESET}"


def list_tests(base_path: str, prefix: str):
    """
    Yield test directories whose name starts with `prefix`.

    Args:
        base_path: directory containing test folders
        prefix: matching prefix (e.g. "test_Baker")

    Yields:
        directory names matching the prefix
    """
    for name in sorted(os.listdir(base_path)):
        full = os.path.join(base_path, name)
        if name.startswith(prefix) and os.path.isdir(full):
            yield name
