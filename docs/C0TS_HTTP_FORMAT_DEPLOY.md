# C0TSWS HTTP `format` deploy note

The routines **`trunk/p/C0TSWS.m`** and **`trunk/p/C0TSWSU.m`** in this branch implement `format=json|xml|csv|mumps` for the `/bsts/*` web handlers, strip the spurious `^(0)` line count after HTML `ADDTO` output (`RMCNT0^C0TSWSU`), and stream large **codelist** JSON in multiple lines to avoid GT.M string limits.

## Deploy to VistA

1. Copy `C0TSWS.m` and `C0TSWSU.m` to the instance **`p` routine directory** (e.g. `/home/osehra/p`).
2. In GT.M: `ZL "C0TSWSU"` then `ZL "C0TSWS"`.
3. Ensure **`addService^%webutils`** registers `bsts/*` (see your site’s `KBAIWS` or `SYNWEBRG` equivalent). **`go^%webreq`** (or your listener) after routine load.
4. Smoke: `curl -sS 'http://host:port/bsts/codeset?format=json' | head -c 200`

Mirror copies may also live in **VistA-FHIR-Server-Codex** `src/` for CI and image builds.

## Branch

**`feature/c0ts-format-bsts-integration`** — C0TS format integration with Codex; merge after review.
