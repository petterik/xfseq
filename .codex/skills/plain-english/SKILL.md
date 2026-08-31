---
name: plain-english
description: Explain an xfseq phase plan, decision, benchmark, review, or implementation in short plain English grounded in the actual repository evidence.
---

# Plain English

Explain the idea, not the document's wording.

- Read the relevant plan, code, diff, tests, or benchmark results first.
- Start with a one-sentence mental model.
- State the problem, the chosen approach, why it matters, and what remains
  uncertain.
- Translate jargon into ordinary words. Define unavoidable Clojure or JVM terms
  once.
- Use a small example only when it makes the mechanism clearer.
- Distinguish measured facts from expectations.
- Prefer short sentences and concrete nouns and verbs.

For phase workflows, finish each planning or review stage with a `What matters`
section of at most seven bullets. It must let the user understand the current
decision and risks without reading the full technical artifact.
