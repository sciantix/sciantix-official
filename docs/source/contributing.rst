Contributing & development workflow
===================================

Follow the repository `CONTRIBUTING.md` guidance when contributing changes
to SCIANTIX. A concise development workflow is provided here for convenience:

1. **Fork the repository** on GitHub and clone your fork locally.

2. **Create a feature branch** for your changes:

.. code-block:: bash

    git checkout -b my-feature-branch

3. **Make small, focused commits** describing the changes you implemented.

4. **Run regression tests** before opening a Pull Request to ensure no
   regressions are introduced:

.. code-block:: bash

    python3 regression/regression.py

5. **Build the documentation locally** to verify formatting and links:

.. code-block:: bash

    cd docs
    make html

6. **Push the branch** to your fork and open a Pull Request against the
   `main` branch of the upstream repository:

.. code-block:: bash

    git push -u origin my-feature-branch

7. **Pull Request checklist** (recommended):

- Ensure the code compiles on the supported platforms (C++17).
- Ensure regression tests pass (or failures are justified and documented).
- Document new models, input options, and variables in Doxygen and the
  `docs/` directory.
- Use clear commit messages and reference any related issue numbers.

If you are unsure about the scope of a change, open an issue first to discuss
the proposal with the maintainers.
