# insitu Progress Report

## Done
- Added `insitu` as a supported glue modifier in `GlueParser`.
- Added `isInsitu` / `setInsitu` to `MeaningConstructor`.
- Kept `noscope` support intact.
- Enforced `noscope` and `insitu` as mutually exclusive.
- Updated the parser output path so `insitu` is carried through to downstream processing.

## Verification
- Added regression coverage in `WorkbenchMainTest` for:
  - parsing `|| insitu`
  - rejecting `|| noscope, insitu`
- Ran `mvn -q -Dtest=WorkbenchMainTest test` successfully.

## Notes
- The change is intentionally small: `insitu` follows the same parser/data-model shape as `noscope`.
- If needed later, the next step is to teach any UI or export layer that prints glue modifiers to surface `insitu` explicitly.
