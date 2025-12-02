project = 'SCIANTIX'
copyright = '2025, giozu'
author = 'giozu'
release = '2.1'
extensions = []
templates_path = ['_templates']
exclude_patterns = []
html_theme = 'alabaster'
html_static_path = ['_static']

import os
import subprocess

extensions = [
    "breathe",
    "exhale",
    "sphinx.ext.autodoc",
    "sphinx.ext.mathjax",
    "sphinx.ext.viewcode",
]

html_theme = "sphinx_rtd_theme"

# Breathe config
breathe_projects = {
    "SCIANTIX": "../doxygen/xml"
}
breathe_default_project = "SCIANTIX"

# Exhale config
exhale_args = {
    "containmentFolder": "./api",
    "rootFileName": "index_api.rst",
    "rootFileTitle": "SCIANTIX API reference",
    "doxygenStripFromPath": "../..",
    "createTreeView": True,
}

def run_doxygen(app):
    if app.builder.name != "html":
        return
    root_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
    subprocess.check_call(["doxygen", "Doxyfile"], cwd=root_dir)

def setup(app):
    app.connect("builder-inited", run_doxygen)
