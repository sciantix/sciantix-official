# Basic guide

## Instructions for Committing New Code

Follow these steps to commit new code to the SCIANTIX repository:

1. Open your terminal and execute the following commands:

   ```bash
   gh repo clone sciantix/sciantix-official
   cd sciantix
   ```

2. Create an issue on GitHub.

3. Proceed with the following commands:

   ```bash
   git checkout -b <BranchName_#IssueNumber>
   git add <Files>
   git commit -m "<CommitMessageEndingIn#IssueNumber>"
   git pull --rebase upstream version1.0
   git push origin <BranchName_#IssueNumber>
   ```

4. Create a merge request on GitHub.

### Check Your Work Status

At any time, you can check the status of your work by running:

```bash
git status
```

---

## Building SCIANTIX on macOS

Follow these steps to build SCIANTIX on macOS:

1. **Install Homebrew and Git**:
   - **Homebrew**: Install by running the following command in Terminal:
```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```
   - **Git**: Install via Homebrew:
```bash
brew install git
```

2. **Install Dependencies**:
   - Install CMake and g++ using Homebrew:
```bash
brew install cmake g++
```

3. **Clone the Repository**:
   - Clone the repository:
```bash
gh repo clone sciantix/sciantix-official
```

4. **Build SCIANTIX**:
   - Create a build directory and navigate into it:
```bash
mkdir build && cd build
```
  - Configure the build with CMake:
```bash
cmake ..
```
   - Compile the code (use `-j` to speed up the process):
```bash
make -j
```

5. **Locate the Executable**:
   - Find the `sciantix.x` executable in the `build` directory.

---

## Sciantix Syntax Guidelines

To maintain consistency in the Sciantix codebase, please adhere to the following guidelines:

- **Use extensive names** for functions and variables. Long names are encouraged for clarity.
- **Variable names** should be in lowercase with words separated by underscores (`_`).
  - Example: `diffusion_coefficient`
- **Global variables** should be in uppercase with words separated by underscores (`_`).
  - Example: `GAS_PRODUCED`
- **Input integer variables** (used for model selection) should start with an `i`.
  - Example: `iGasDiffusivity`
- **Scaling factor variables** should start with `sf_`.
  - Example: `sf_diffusion_coefficient`
- **Function names** should be in PascalCase with each word capitalized and no separation signs.
  - Example: `HeliumDiffusionCoefficient`
- **File names** should match the name of the primary function they contain.
- **Documentation comments** in the code with `/**` are meant for inclusion in the Doxygen documentation and should be placed at the beginning of every function.
- **Regular comments** using `//` should be added liberally throughout the code.
- **Scientific references** should be added as comments in the code wherever applicable (e.g., parameter values, equations).

Thank you for adhering to these guidelines!

---

## Useful Git Commands

Below are some essential Git commands for effectively managing your local and remote repositories:

### How to Create a New Pull Request

1. **Create and Switch to a New Branch:**
   ```bash
   git checkout -b new_repo
   ```

2. **Make Changes and Commit Them:**
   - Stage all changes:
     ```bash
     git add .
     ```
   - Commit the changes:
     ```bash
     git commit -m "commit message"
     ```
   - Push the new branch to the remote repository:
     ```bash
     git push origin new_repo
     ```

3. **Create a New Pull Request:**
   - Using GitHub CLI:
     ```bash
     gh pr create --base main --head new_repo --title "Title of the pull request" --body "Brief description of the pull request"
     ```

4. **Check for Reviews or Approvals:**
   - Ensure the pull request meets the necessary criteria. If the pull request is approved and ready to be merged, update your main branch:
     ```bash
     git pull origin main
     ```

5. **Switch Back to the Main Branch:**
   ```bash
   git checkout main
   ```

6. **Merge the New Branch into Main:**
   ```bash
   git merge new_repo
   ```

7. **Push the Updated Main Branch to Complete the Pull Request:**
   ```bash
   git push -u origin main
   ```

- **Delete old remote Git branches**:
  
  ```bash
  git remote prune origin
  ```

  [Source](https://git-scm.com/docs/git-remote#Documentation/git-remote.txt-empruneem)

- **Delete local Git branches that were deleted on the remote repository**:

  ```bash
  git branch -vv | grep ': gone]' | grep -v "\*" | awk '{ print $1; }' | xargs -r git branch -D
  ```

  Alternative command:

  ```bash
  git fetch -p; git branch -r | awk '{print $1}' | egrep -v -f /dev/fd/0 <(git branch -vv | grep origin) | awk '{print $1}' | xargs git branch -d
  ```

  [Source](https://medium.com/@kcmueller/delete-local-git-branches-that-were-deleted-on-remote-repository-b596b71b530c)

- **Delete the most recent local commit (not pushed to origin)**:

  ```bash
  git reset HEAD~1
  ```

- **Clear Git cache**:

  ```bash
  git rm -r --cached .
  ```

---

We hope this guide helps simplify the process of committing new code and building SCIANTIX on macOS, while also ensuring consistency in the source code through our syntax guidelines. If you encounter any issues or have suggestions, please feel free to contact the developers.