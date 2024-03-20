# Instructions for committing new code in Sciantix

Open your terminal and type in the following instructions
```
git clone git@gitlab.com:<UserName>/sciantix.git
cd sciantix
git remote add upstream git@gitlab.com:poliminrg/sciantix.git
git pull –-rebase upstream version1.0
```
Create issue on gitlab.com, then proceed with the following instructions
```
git checkout –b <BranchName_#IssueNumber>
git add <Files>
git commit –m "<CommitMessageEndingIn#IssueNumber>"
git pull –-rebase upstream version1.0
git push origin <BranchName_#IssueNumber>
```
Create merge request gitlab.com.

In every moment, to check the status of your work type `git status`

# Instruction for building SCIANTIX on Macintosh

- Install Git from: https://sourceforge.net/projects/git-osx-installer/files/;
- Install Home brew from: https://brew.sh;
- Clone Sciantix using git (git clone);
- Search g++ in Homebrew directory (e.g., /opt/homebrew/bin/g++-12)
- Put pwd of g++ into Makefile of Sciantix (e.g., CC := /opt/homebrew/bin/g++-12)
- Install the command line developer tool to 'make'(usually a windows (usually opens a window when you 'make).

# Syntax guidelines

In Sciantix we use some basic guidelines to help keeping an uniform syntaxing style among different contributions.

- Use **extensive names** for functions and variables. Do not be afraid of long names.
- Variables names are in lower cases, with words separated by "_" (e.g., `helium_diffusion_coefficient`).
- Global variables are in Upper cases, with words separated by "_" (e.g., `Helium_produced`).
- Input integer variables (e.g., used for model selection) start with an "i" (e.g., `iverification`).
- Flag integer variables start with an "f" (e.g., `fannealing`).
- Scaling factors variables start with "sf_" (e.g., `sf_helium_diffusion_coefficient`).
- Functions names are in Upper cases, with each word in Upper cases and no separation signs (e.g., `HeliumDiffusionCoefficient`).
- Files should have the same name as the function they contain.
- Comments in the code with "///" are meant for inclusion in the Manual and should be collected at the beginning of every function.
- Normal comments "//" should be used as much as possible.
- References to scientic literature should be added as comment in the code at **every** occurrence (e.g., values of parameters, equations...).

Thank for sticking as much as possible to these guidelines. We appreciate! :)

# Short list of useful GIT commands

- Delete old remote GIT branches
`git remote prune origin`

(source: https://git-scm.com/docs/git-remote#Documentation/git-remote.txt-empruneem)

- Delete local GIT branches that were deleted on remote repository
`git branch -vv | grep ': gone]'|  grep -v "\*" | awk '{ print $1; }' | xargs -r git branch -D`
'git fetch -p ; git branch -r | awk '{print $1}' | egrep -v -f /dev/fd/0 <(git branch -vv | grep origin) | awk '{print $1}' | xargs git branch -d'

(source: https://medium.com/@kcmueller/delete-local-git-branches-that-were-deleted-on-remote-repository-b596b71b530c)

- Delete local commits (not pushed to origin):
`git reset HEAD~1`

- Clear git cache
`git rm -r --cached .`



