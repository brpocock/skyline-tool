# Request: Add Intellivision Palette Generation Support

## Problem Statement
The Intellivision port requires a sophisticated 16‑color palette system that clips intermediate colours to the nearest available hardware palette when fading between lighting modes (white, black, red, cyan). The current SkylineTool asset pipeline does not expose any facilities to generate or map these palettes, and the existing Lisp scripts only support 7800‑type colour generation.

## Proposal
1. **New Lisp Module** – `src/intv-palette-gen.lisp`
   * Implements a `:intv-palette-generator` function that takes target RGB values and produces a sequence of INTV palette indices reflecting ¼‑step fades.
   * Internally uses the fixed 16‑color Intellivision palette table and performs Euclidean distance matching for nearest‑colour selection.
   * Exposes a **JSON** interface (via `byteArrayToJSON`) so Skyline‑Tool can request the list of palette indices in a language‑agnostic way.

2. **CLI Hook** – Add a `--intv-palette` option to `skyline-tool` that accepts an input file containing a list of target RGB values and outputs a `.pal` file with the computed palette indices.

3. **Test Suite** – `SkylineTool/tests/intv-palette-tests.lisp` verifying:
   * Correct mapping of pure white, black, red, cyan.
   * Proper intermediate values are rounded to the nearest fixed colour.
   * Generates exactly 5 indices (0, ¼, ½, ¾, 1 for each fade step). 

4. **Documentation** – Update `SkylineTool/READMEs.md` with usage example:
   ```bash
   bin/skyline-tool --intv-palette --input target-colors.json --output IntvPalette.pal
   ```

## Acceptance Criteria
* Running `make test` inside `SkylineTool/` should pass the new IntV palette tests.
* The `SkylineTool` CLI should expose the `--intv-palette` flag with no side‑effects on other commands.
* Generated `.pal` file must be consumable by the 5200/Intv build pipelines.

---
Please approve, and I will add the Lisp module, CLI hook, tests, and documentation.
