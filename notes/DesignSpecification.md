# Pantograph: Design Specification

What do we want to achieve with abstractions, and what do we want to keep
atomic?

abstract:
- use pre-expression to derive paths + exprs + expr changes + context changes
- traversing with context (e.g. typing context + context changes) over paths + exprs
  - renaming/substituting in paths + exprs + changes (requires context for handling bindings)

concrete:
- 