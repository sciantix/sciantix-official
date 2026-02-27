project = 'SCIANTIX'
copyright = '2025, giozu'
author = 'giozu'
release = '2.1'
templates_path = ['_templates']
exclude_patterns = []
html_static_path = []

import os
import subprocess

extensions = [
    "breathe",
    "exhale",
    "sphinx.ext.autodoc",
    "sphinx.ext.mathjax",
    "sphinx.ext.viewcode",
    "sphinx_design",
    "sphinx_copybutton",
    "myst_parser",
]

html_theme = "furo"
html_logo = "_static/logo.png"
html_static_path = ["_static"]

html_theme_options = {
    "source_repository": "https://github.com/sciantix/sciantix-official/",
    "source_branch": "main",
    "source_directory": "docs/source/",
    "sidebar_hide_name": False,
    "navigation_with_keys": True,
    "light_css_variables": {
        "color-brand-primary": "#003366", # Deep navy
        "color-brand-content": "#006699",
    },
    "dark_css_variables": {
        "color-brand-primary": "#33CCCC", # Vibrant cyan
        "color-brand-content": "#66E6E6",
    },
}

# -- Run Doxygen so that Breathe/Exhale can see the XML -----------------
# Root della repo: docs/source -> .. (docs) -> .. (root)
root_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
doxyfile = os.path.join(root_dir, "Doxyfile")

if os.path.exists(doxyfile):
    try:
        subprocess.check_call(["doxygen", "Doxyfile"], cwd=root_dir)
    except subprocess.CalledProcessError as e:
        raise RuntimeError(f"Doxygen failed with exit code {e.returncode}")
else:
    raise RuntimeError(f"Doxyfile not found at {doxyfile}")

# Breathe config
breathe_projects = {
    "SCIANTIX": "../doxygen/xml"
}
breathe_default_project = "SCIANTIX"

exhale_args = {
    "containmentFolder": "./api",
    "rootFileName": "index_api.rst",
    "rootFileTitle": "SCIANTIX API reference",
    "doxygenStripFromPath": "../..",
    "createTreeView": True,
    "contentsDirectives": False,
}
