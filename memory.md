# OpenPHIGS Bindings Refactor (PR #51) - Memory & Context

## Context
This file serves as a memory checkpoint for the work done on the `feat/bindings-refactor-v2` branch. The maintainer advised against trying to fix or rebase the messy git history of the original `feat/bindings-refactor` branch. Instead, they suggested applying the splitting scripts directly onto the latest updated `main` branch.

## What We Have Done So Far
1. **Fresh Start from `main`**:
   - Checked out the latest upstream `main` branch.
   - Created a clean branch: `feat/bindings-refactor-v2`.

2. **Automated File Splitting**:
   - Ran `split_bindings.py`, `restructure.py`, and `fix_globals.py`.
   - These scripts automatically split the monolithic C and Fortran binding files (like `cb_text.c`, `cb_ar.c`) into hundreds of individual `.c` files placed in `c_binding/` and `f_binding/`.

3. **Handling Static Symbols**:
   - In the old branch, we had to strip `static` keywords from private helper functions (e.g., `valid_ar_fname`) to avoid "missing symbols" during testing.
   - *Discovery*: The maintainer had already addressed this upstream in `main`. The `static` keywords were already stripped from the monolithic files, so the split files were generated correctly without them.

4. **Cherry-Picked Missing Includes (Commit `a7fcdb7`)**:
   - The newly split C and Fortran files were missing standard library includes (`<string.h>`, `"util.h"`) and internal declarations (`cb_internal.h`) that were previously at the top of the monolithic files.
   - We cherry-picked the specific commit from the old branch that resolved these implicit function declarations.

5. **Fixed CMake Configuration & CI Error**:
   - The CI pipeline was failing immediately (within 20-50 seconds) on the new branch.
   - *Root Cause*: The `bindings_src.cmake` file was out of sync. It contained outdated filenames (e.g., looking for `pset_text_align.c` instead of `pset_anno_align.c` because the maintainer fixed a typo in the upstream documentation headers).
   - *Fix*: We used a python script to completely regenerate and alphabetically sort `bindings_src.cmake` so it exactly matches the actual `.c` files on disk.

6. **Pushed to GitHub**:
   - Committed the changes and pushed `feat/bindings-refactor-v2` to the user's fork.
   - The git history is now perfectly clean, and the codebase exactly matches the maintainer's expectations.

## Current Status
- Maintainer (`schwicke`) reviewed PR #51 locally and provided a patch (`0001-fix-remove-duplicates-and-fix-remaining-issues-e.g.-.patch`) to fix remaining issues.
- The patch was successfully applied to the local `feat/bindings-refactor-v2` branch and pushed to the fork.
- **Success:** The patch solved the issues and all CI tests which were previously failing have now passed!

## Next Steps
- The PR is fully green and ready for the maintainer to merge.
- Left a thank you comment on the PR acknowledging the fix.
