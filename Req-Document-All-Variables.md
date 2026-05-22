# Request: Complete TextInfo (`.texf`) for All Variables

## Goal
Ensure every global variable, constant, and register mapping used by the game across all ports has a TexInfo fragment documenting:
-**Definition** (address, size, encoding)
-**Unit** (bytes, bits, fixed‑point format, sound frequency, color palette, etc.)
-**Usage** (which ports, subsystems, functions manipulate it)
-**Side‑effects** (who changes it, when it is reset, etc.)

## Actions
1. Scan each source tree for symbols defined in a `.s` file or `*.cob`.
2. For each symbol not already referenced in a `.texf`, generate a TexInfo block using the template from `Source/Documentation/7800/7800.texf`.
3. Add an index entry (`@cindex`) for each symbol.
4. Commit the new `.texf` files and run `make doc`.

## Acceptance
- All variables appear in the generated TEXINFO index.
- No `*.texf` contains an `@unknown` tag.
- `make check-docs` passes without missing placeholders.

---
<p>Please approve this memo and assign to the documentation team.</p>