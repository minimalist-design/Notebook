# Notebook

A local, single-user notebook. **Articles** are long-form markdown; **snippets** are short side entries (code, quick reference). Notes link to each other with `[[wiki-links]]`; typing `[[` in the editor opens an autocomplete overlay that fuzzy-ranks every note on each keystroke.

Clojure (Ring/Jetty, reitit, SQLite FTS5) + ClojureScript (Reagent, re-frame, CodeMirror 6, markdown-it). Typeset in IBM Plex.

## Requirements

- JDK 21+ (`sdk install java 21-tem`), Clojure CLI, Node 20+

## Run

```sh
npm install                 # once
npm run vendor              # copy Plex fonts, KaTeX, Mermaid into resources/public
npx shadow-cljs watch app   # terminal 1: hot-reloading frontend
clojure -M:run              # terminal 2: server → http://127.0.0.1:8080
```

Options: `clojure -M:run --notes ~/my-notes --port 8080` (or `NOTEBOOK_DIR`, `NOTEBOOK_PORT`). REPL: `clojure -M:dev`, then `(go)` / `(reset)` / `(halt)`.

### Release

```sh
npm run release             # vendor + advanced-compiled JS
clojure -T:build uber       # → target/notebook.jar
java -jar target/notebook.jar --notes ~/my-notes
```

## Notes on disk

```
notes/
  articles/<slug>.md     long-form
  snippets/<slug>.md     side entries
  assets/                pasted / dropped images and files
  .notebook/index.db     SQLite index — disposable, rebuilt from the files
```

Frontmatter: `title`, `tags: [a, b]`, `created`, and `lang` for snippets. Files are the source of truth: edit them with any editor and the app picks the change up live (file watcher → index → SSE to the browser).

## Writing

| Syntax | Meaning |
|---|---|
| `[[slug]]` or `[[Note Title]]` | link (shows the target's title) |
| `[[slug\|text]]` | link with custom text |
| `[[slug#Heading]]` | link to a heading |
| `![[slug]]` | embed a note inline (max 2 levels deep, cycle-safe) |
| `![[image.png]]` | embed an asset |
| `$x^2$`, `$$…$$` | KaTeX math |
| ` ```mermaid ` | Mermaid diagram |

Links to notes that don't exist yet are shown dashed; clicking one creates the note. Renaming a note rewrites every `[[link]]` to it in other files.

## Keys

| | |
|---|---|
| `Ctrl/⌘ K` | palette: jump to a note, create one, full-text search |
| `Ctrl/⌘ E` | toggle read / edit |
| `Ctrl/⌘ S` | save now (autosave runs ~0.8 s after typing stops) |
| `[[` / `![[` | link / embed autocomplete |

## Tests

```sh
clojure -M:test                   # server + shared cljc (kaocha)
npx shadow-cljs compile test      # markdown pipeline (node)
```

## Layout

- `src/clj/notebook/` — server: `notes/store` (files), `notes/index` (SQLite/FTS5), `notes/core` (create/save/rename/delete), `notes/sync` (watcher), `api`, `server`, `sse`
- `src/cljc/notebook/` — shared: `links` (wiki-link parsing, slugs, rename rewriting), `fuzzy` (ranking)
- `src/cljs/notebook/` — client: `md/render` (markdown-it pipeline), `editor/` (CodeMirror + autocomplete), `views/`, `events`, `subs`
- `resources/public/css/` — `tokens`, `prose` (reading typography), `code`, `layout`, `editor`
