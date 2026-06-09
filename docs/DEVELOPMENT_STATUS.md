# Development Status and Gap Analysis — bsts-vista

Status date: 2026-06-09
Branch at time of writing: `feature/c0ts-format-bsts-integration`

Part of the VistA-on-FHIR workspace. Ecosystem-level context lives in
`VistA-FHIR-Server-Codex/docs/PROJECT_OVERVIEW.md`; the cross-repo roadmap is
`VistA-FHIR-Server-Codex/docs/PATH_FORWARD.md`.

## Role of this repository

Canonical source for the IHS BSTS (Standard Terminology Service) package for
VistA/RPMS plus the C0TS HTTP layer over it. BSTS maintains a local FileMan
cache (`^BSTS`, files `9002318*`) of SNOMED CT, RxNorm, ICD, and IHS subsets,
optionally synchronized from an external DTS server. C0TS
(`trunk/p/C0TSWS*.m`) exposes that data as `/bsts/*` HTTP endpoints with
`format=html|json|xml|csv|mumps`.

In the stack: terminology content provider behind the Codex FHIR server
(which keeps `C0TSWS`/`C0TSWSU` copies in sync) and behind the
C0T-terminology-gateway (which vendors `C0TSFM`, `C0TSWSD`, `C0TSUTL`).

## What is working today

- `/bsts/codeset`, `/bsts/codelist|codes`, `/bsts/code`, `/bsts/concept`,
  `/bsts/subset` HTTP endpoints with multi-format serialization, streaming
  JSON for GT.M string limits, and HTML cleanup (`RMCNT0^C0TSWSU`).
- SCT→ICD9/ICD10 mapping via `^BSTS(9002318.4,"CODE",...)` (`C0TSUTL.m`).
- Layered M API (`BSTSAPI` → `BSTSAPIA`–`F`): SEARCH, CODESETS, SUBSET,
  VALTERM, associations, drug lookups.
- DTS sync routines (`BSTSWSV*`, `BSTSDTS0`–`3`, `BSTSCMCL`) for sites with a
  live terminology server.
- KIDS artifacts under `kids/` (C0TS VistA Terminology Server v1.1).

## Gap analysis

1. **Search is not exposed over HTTP.** `SEARCH^BSTSAPI` exists in-process
   but there is no normalized web search/typeahead endpoint here; interactive
   search went to C0T's Lexicon path instead. Decide deliberately whether
   C0TS ever grows a search endpoint or whether search is C0T's job
   permanently (recommended: the latter, with C0T's planned `C0TBSTS`
   provider calling `SEARCH^BSTSAPI` in-process).
2. **No pagination** on large codelists — only `max` truncation (default
   4000).
3. **Route registration is environment-specific** (site `KBAIWS` config);
   nothing in-tree registers `/bsts`, and the CPRS demo proxy does not list
   it. Option A (direct `/bsts`) is documented but not exercised by current
   UIs.
4. **One-line README.** Real documentation lives in Codex
   (`docs/BSTS_C0TS_FORMAT_WEB_SERVICES.md`, `docs/BSTS_INTEGRATION_PLAN.md`)
   and `docs/C0TS_HTTP_FORMAT_DEPLOY.md` here. A short README pointing at
   those would prevent confusion.
5. **No automated tests** — `BSTSTST.m` is interactive only.
6. **Content dependency.** Without an installed/populated `^BSTS` (and
   SNOMED licensing), the endpoints return empty/errors. Each target
   container needs a content-provisioning step that is currently manual.
7. **Sync risk with Codex copies.** `C0TSWS.m`/`C0TSWSU.m` exist in three
   repos (here, Codex, C0T). Until ownership is consolidated, every change
   needs a three-way sync. See the consolidation recommendation in
   `VistA-FHIR-Server-Codex/docs/PATH_FORWARD.md`.

## Integration points

| Repo | Relationship |
|---|---|
| VistA-FHIR-Server-Codex | Mirrors C0TS routines; install matrix smoke-tests `/bsts/*` |
| C0T-terminology-gateway | Vendors C0TS helpers; future `C0TBSTS` provider will call `BSTSAPI` |
| rehmp | Option A direct `/bsts` documented (`rehmp/docs/BSTS_TERMINOLOGY_OPTION_A.md`) but not active |
| RPMS | BSTS is native to RPMS; key asset for the dual-stack plan |
