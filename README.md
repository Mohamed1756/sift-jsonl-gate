# Sift: Deterministic Release Gate for JSONL Batches

> **Clean what is safe, hold what is not.**  
> Sift is a command-line release gate for JSONL batches. It decides whether a batch is safe to release, and if not, it refuses.

Sift classifies records against schema rules, isolates bad data into a quarantine, checks release thresholds, and publishes a new release only when the entire batch passes. Consumers read through the pointer file. Direct reads of run directories bypass the gate.

---

## Consumer Contract

The gate relies on four rules:

1. **Downstream consumers read the file path stored in `OUT_DIR/current`.**
2. Reading directly from `runs/...` bypasses the gate and voids all publication guarantees.
3. If a run is blocked, `current` is never created or updated. Previous releases remain active. On a fresh `OUT_DIR` where the first run blocks, `current` does not exist. Downstream consumers must handle initial absence in addition to staleness.
4. Downstream tooling must treat `quarantine.jsonl` as untrusted input because raw bytes from input lines are preserved.

```bash
# How downstream consumers read the active release:
ACTIVE_CLEAN_FILE=$(cat /path/to/out/current)
process_data "/path/to/out/$ACTIVE_CLEAN_FILE"
```

---

## Trust Boundary

- **Environment**: Single-tenant local filesystem owned by the executing user. No hostile local actors.
- **Filesystem Support**: POSIX local filesystems only. Network filesystems (NFS, SMB) and cross-filesystem publication are unsupported.
- **Concurrency**: Mutual exclusion covers cooperating Sift processes only. OS file locks are advisory (`flock`/`fcntl`).
- **Crash Safety**: Durable write barrier via `FileChannel.force(true)` on every file and directory prior to atomic `rename(2)` moves. Publication invariants survive both process termination (`kill -9`) and sudden power loss.

---

## The Demo: Refusal First, Release Second

Run the verification demo:

```bash
./demo.sh
```

The demo runs in three steps:

1. **The Refusal (`exit 1`)**:
   - Evaluates [`demo/input.jsonl`](demo/input.jsonl) containing 9 records (invalid dates such as `2026-02-29`, floats in integer columns, duplicate keys, nulls, and non-object lines) against strict rules (`max_quarantine_ratio: 0.0005`).
   - Sift blocks release, exits with code `1`, leaves `current` uncreated, and isolates the run to `demo_out/runs/<run_id>.blocked-1/`.
2. **The Clean Release (`exit 0`)**:
   - Evaluates [`demo/clean_input.jsonl`](demo/clean_input.jsonl) containing conforming records.
   - Sift releases, exits with code `0`, writes to `demo_out/runs/<run_id>/`, and creates `demo_out/current`.
3. **Cache Reuse at IO Speed**:
   - Re-running the clean batch skips re-classification, revalidates cryptographic output checksums at disk speed, and exits `0`.

---

## CLI Usage

```bash
# Standard run (reuses existing complete run matching run ID, or computes):
sift run --input <FILE.jsonl> --rules <RULES.json> --out <DIR>

# Force recomputation even if a completed run exists:
sift run --input <FILE.jsonl> --rules <RULES.json> --out <DIR> --force-rerun

# Force rerun and permit replacing the active release pointed to by current:
sift run --input <FILE.jsonl> --rules <RULES.json> --out <DIR> --force-rerun --replace-live
```

### Exit Codes (v1.0.1)

| Exit Code | Status | Description |
| :---: | :--- | :--- |
| `0` | **Released** | Batch passed all release rules. `current` points to this release. |
| `1` | **Blocked** | Release blocked due to quarantine ratio breach, critical field missing, or input data syntax errors. Output isolated to `.blocked-<n>/`. `current` remains untouched. |
| `2` | **Fatal Error** | Configuration or preflight failure (invalid rules, input inside out dir, invalid UTF-8/BOM, concurrent lock conflict). |

### Flags
- `--input <FILE>`: Path to input `.jsonl` file. Must not resolve inside `--out`.
- `--rules <FILE>`: Path to JSON rules file.
- `--out <DIR>`: Target directory for published releases and lock state.
- `--force-rerun`: Recomputes even if a complete run with matching run ID exists.
- `--replace-live`: Only meaningful with `--force-rerun`. Permits replacing the live release when the replacement has the same run ID and passes release.

---

## Rules Format

Rules are exact JSON and reject unknown keys:

```json
{
  "schema_version": 1,
  "required": ["order_id", "order_date", "amount_pence", "currency"],
  "string_columns": ["order_id", "currency", "customer_notes"],
  "date_columns": ["order_date"],
  "integer_columns": ["amount_pence"],
  "release": {
    "max_quarantine_ratio": 0.0005,
    "block_if_missing": ["order_id", "amount_pence"]
  }
}
```

- **Required Fields**: Present if key exists, value is not JSON `null`, and value is not `""`.
- **Disjoint Column Types**: `string_columns`, `date_columns`, and `integer_columns` must be pairwise disjoint.
- **Strict Dates**: Evaluated via `java.time.LocalDate` using strict ISO `YYYY-MM-DD`. Leap days (such as `2024-02-29`) are accepted. Invalid calendar dates (such as `2026-02-29`), timestamps, and spaces are quarantined.
- **Safe Integers**: Lexemes must be optional `-` and ASCII digits with no leading zeros except `0` and `-0`. Values bounded to IEEE 754 53-bit safe integers $[-(2^{53}-1), 2^{53}-1]$. Strings, floats, and exponents quarantine as `INVALID_INTEGER`.
- **Single-Pass Duplicate Keys**: Caught at any nesting depth via token streaming (`DUPLICATE_JSON_KEY`). Jackson enforces a default maximum nesting depth of 1,000 via `StreamReadConstraints`. Deeper documents quarantine as `UNPARSEABLE_LINE`.
- **Release Threshold**: $\text{badRatio} = \frac{\text{quarantined} + \text{unparseable}}{\text{records\_read} + \text{unparseable}}$. Release blocks if $\text{badRatio} > \text{max\_quarantine\_ratio}$, if any field in `block_if_missing` was missing, or if any unparseable line exists.

---

## Output Layout

```text
OUT_DIR/
  runs/
    <64-hex>/              # Completed, releasable run
      clean.jsonl          # Clean records with _sift metadata appended
      quarantine.jsonl     # Quarantined records with failure reasons
      receipt.json         # Canonical cryptographic receipt
    <64-hex>.blocked-<n>/  # Blocked run (isolated, never releasable)
    <64-hex>.replaced-<n>/ # Tombstone directory during replace-live transition
  current                  # Text file containing: runs/<64-hex>/clean.jsonl\n
  .sift-lock               # Advisory file lock and crash recovery journal
```

> **Run ID Convention**: In `receipt.json`, all hashes carry the `sha256:` prefix (for example `"run_id": "sha256:a794..."`). In the filesystem, directory paths use the bare 64-character lowercase hex string (`runs/a794.../`), matching the relative target inside `current`.

### Clean Output (`clean.jsonl`)
Original input key order and number lexemes pass through verbatim, with `_sift` appended as the final key:
```json
{"order_id":"ORD-101","order_date":"2026-09-02","amount_pence":1550,"currency":"GBP","_sift":{"source_record_id":"sha256:...","result":"clean"}}
```

### Quarantine Output (`quarantine.jsonl`)
Quarantined records retain original contents with `_sift` metadata appended. Non-objects and unparseable lines are wrapped with a documented 8 KiB cap on raw text:
```json
{"order_id":"ORD-103","order_date":"2026-02-29","amount_pence":2500,"currency":"EUR","_sift":{"source_record_id":"sha256:...","result":"quarantined","reasons":["INVALID_ISO_DATE"]}}
```

### Canonical Receipt (`receipt.json`)
Emitted with sorted keys at all depths, rational arithmetic ratios, zero environmental leakage (no hostnames, timestamps, usernames, or absolute paths), and SHA-256 output checksums:
```json
{
  "bad_ratio": "0.000500",
  "bad_ratio_denominator": 2000,
  "bad_ratio_numerator": 1,
  "clean_records": 1999,
  "ignored_empty_lines": 0,
  "input_sha256": "sha256:...",
  "lines_read": 2000,
  "outputs": {
    "clean": "runs/30f7.../clean.jsonl",
    "clean_sha256": "sha256:...",
    "quarantine": "runs/30f7.../quarantine.jsonl",
    "quarantine_sha256": "sha256:..."
  },
  "quarantine_reasons": {
    "INVALID_ISO_DATE": 1
  },
  "quarantined_records": 1,
  "records_read": 2000,
  "release": "released",
  "release_reasons": [],
  "rules_sha256": "sha256:...",
  "run_id": "sha256:...",
  "schema_version": 1,
  "sift_version": "1.0.0",
  "unparseable_lines": 0
}
```

---

## Integration

Sift runs directly ahead of loaders and databases:

```text
[Raw Webhooks / S3 Dump] ──► [sift run] ──(quarantined rows)──► [quarantine.jsonl]
                                 │
                         (atomic POSIX rename)
                                 ▼
                       [runs/<id>/clean.jsonl] ◄── [current]
                                 │
                                 ▼
                     [Loaders / DuckDB / Airflow]
```

### In Shell and Orchestrators
```bash
if ./bin/sift run --input raw.jsonl --rules rules.json --out /data/warehouse; then
    LOAD_PATH="/data/warehouse/$(cat /data/warehouse/current)"
    load_into_warehouse "$LOAD_PATH"
else
    echo "Batch refused by gate. Inspecting receipt..."
    BLOCKED_RECEIPT=$(ls -1 /data/warehouse/runs/*.blocked-*/receipt.json | head -n 1)
    python3 -m json.tool "$BLOCKED_RECEIPT"
    exit 1
fi
```

---

## Non-Goals (§15)

The following are explicit non-goals for v1:

- CSV, Parquet, Avro, Spark, Iceberg, Delta
- YAML
- Date catalogues beyond strict ISO
- Deduplication
- Source-of-truth backfill
- Cryptographic signatures or KMS
- Formal verification
- Distributed or cross-filesystem publication
- Dashboards, services, queues, actors
- LLM or agent integrations
- General predicates or transformations
- Schema inference
- Downstream enforcement beyond the documented `current` contract

---

## Installation and Build

### Standalone Native Binary (Zero JVM Dependency)
Compile ahead of time to a standalone Mach-O (macOS) or ELF (Linux) binary via GraalVM Native Image:

```bash
scala-cli --power package project.scala src/ --main-class sift.Sift --native-image -o sift --graalvm-args=--no-fallback
```

Execute directly:
```bash
./sift run --input <FILE.jsonl> --rules <RULES.json> --out <DIR>
```

### Self-Contained Executable Assembly
Package into a runnable assembly JAR (requires Java 17+ on host):

```bash
scala-cli --power package project.scala src/ --main-class sift.Sift --assembly -o sift
```

### Run From Source
```bash
./bin/sift run --input <FILE.jsonl> --rules <RULES.json> --out <DIR>
```

---

## Verification

```bash
# Run test suite (Tests 1 to 27 and Adversarial Hardening)
scala-cli run project.scala src/ test/AcceptanceSuite.scala --main-class sift.AcceptanceSuite --server=false

# Run true OS sub-process kill -9 crash recovery test (Test 22)
scala-cli run project.scala src/pub/PublicationEngine.scala src/receipt/ReceiptEngine.scala test/pub/FaultInjectionTest.scala --main-class sift.pub.FaultInjectionTest --server=false

# Run 10 GB scale proof in a 256 MB JVM heap (Test 28)
scala-cli run project.scala src/ test/scale/ScaleProofTest.scala --main-class sift.scale.ScaleProofTest --server=false
```

---

## Codebase Size (§13)

The production codebase is **1,183 lines of Scala** across 7 files (measured against the 1,200 LOC target and 1,500 LOC hard stop):

- `InputModel.scala` (117 LOC)
- `JsonStreamParser.scala` (259 LOC)
- `RulesModel.scala` (111 LOC)
- `ClassifierEngine.scala` (152 LOC)
- `ReceiptEngine.scala` (108 LOC)
- `PublicationEngine.scala` (281 LOC)
- `Sift.scala` (155 LOC)

---

## License

Apache License 2.0.
