"""Characters of the abstract as MRS counts them: title and body together, with spaces (limit 4000).
author: E.Cappellari, POLIMI-CEA 2026

usage: python3 count_characters.py [abstract.tex]
"""

import os
import re
import sys

LIMIT = 4000


def plain(text):
    """LaTeX source to the plain text typed in the MRS form."""
    text = "\n".join(line for line in text.split("\n") if not line.lstrip().startswith("%"))
    for tex, char in ((r"\textdegree ", "°"), (r"\textmu ", "µ"), (r"\%", "%"), ("--", "–"), ("~", " "), ("$_2$", "2")):
        text = text.replace(tex, char)
    return " ".join(text.split())


def main():
    path = sys.argv[1] if len(sys.argv) > 1 else os.path.join(os.path.dirname(os.path.abspath(__file__)), "abstract.tex")
    source = open(path, encoding="utf-8").read()
    title = plain(re.search(r"\\title\{\\textbf\{(.*?)\}\}", source, re.S).group(1))
    body = plain(source.split(r"\section*{Abstract}")[1].split(r"\medskip")[0])
    left = [m for m in re.findall(r"\\[a-zA-Z]+|[{}$]", body)]
    print(f"title: {len(title)} characters\n  {title}")
    total = len(title) + len(body)
    print(f"body:  {len(body)} characters with spaces, {len(body.split())} words")
    print(f"total: {total} characters, title + body (limit {LIMIT}, {LIMIT - total} left)")
    if left:
        print("LaTeX left in the body, not counted correctly:", sorted(set(left)))
    return 0 if total <= LIMIT and not left else 1


if __name__ == "__main__":
    sys.exit(main())
