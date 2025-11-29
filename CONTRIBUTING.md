# Contributing to SCIANTIX

We welcome contributions to SCIANTIX.  
To ensure a smooth workflow, please follow these guidelines.

## Reporting issues
Use the GitHub issue tracker.  
Please include:
- a clear description of the problem,
- steps to reproduce,
- input files or minimal examples,
- expected and actual behaviour.

## Development workflow
1. Fork the repository.
2. Create a new branch:
```

git checkout -b new_name

```
3. Make your changes with clear commits.
4. Ensure that:
- the code compiles,
- regression tests pass or new behaviour is covered by tests.
5. Open a Pull Request against `main`.

## Coding style
- Use modern C++ (C++17 or later).
- Follow the structure already present in `src/` and `include/`.
- Document new classes and methods using Doxygen comments.

## Testing
SCIANTIX includes a regression test suite.  
Before submitting a PR, run:

```

python regression/regression.py

```

## Documentation
Any new model, variable or option must be documented in:
- Doxygen comments,
- the user manual (`docs/`),
- example input files when relevant.
```