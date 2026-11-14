(ns notebook.notes.index
  "SQLite (+ FTS5) index over the notes directory. Disposable: it can always
  be rebuilt from the files."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(def schema-version 1)

(def ^:private opts {:builder-fn rs/as-unqualified-lower-maps})

(defn open [path]
  (.mkdirs (.getParentFile (.getAbsoluteFile (io/file path))))
  (let [ds (jdbc/get-datasource {:jdbcUrl (str "jdbc:sqlite:" path)})
        conn (jdbc/get-connection ds)]
    ;; one long-lived connection; access is serialised with `locking`
    (jdbc/execute! conn ["PRAGMA journal_mode=WAL"])
    (jdbc/execute! conn ["PRAGMA foreign_keys=ON"])
    (let [v (:user_version (jdbc/execute-one! conn ["PRAGMA user_version"] opts))]
      (when (not= v schema-version)
        (doseq [t ["notes_fts" "links" "tags" "notes"]]
          (jdbc/execute! conn [(str "DROP TABLE IF EXISTS " t)]))
        (doseq [sql ["CREATE TABLE notes (
                        slug TEXT PRIMARY KEY, type TEXT NOT NULL, title TEXT NOT NULL,
                        hash TEXT NOT NULL, mtime INTEGER NOT NULL, created TEXT,
                        lang TEXT, excerpt TEXT)"
                     "CREATE TABLE tags (slug TEXT NOT NULL REFERENCES notes(slug) ON DELETE CASCADE,
                        tag TEXT NOT NULL, PRIMARY KEY (slug, tag))"
                     "CREATE INDEX tags_tag ON tags(tag)"
                     "CREATE TABLE links (src TEXT NOT NULL REFERENCES notes(slug) ON DELETE CASCADE,
                        dst TEXT NOT NULL, kind TEXT NOT NULL, alias TEXT)"
                     "CREATE INDEX links_dst ON links(dst)"
                     "CREATE INDEX links_src ON links(src)"
                     "CREATE VIRTUAL TABLE notes_fts USING fts5(
                        slug UNINDEXED, title, body, tags, tokenize='porter unicode61')"
                     (str "PRAGMA user_version=" schema-version)]]
          (jdbc/execute! conn [sql]))))
    conn))

(defn close [conn] (.close ^java.sql.Connection conn))

(defmacro ^:private with-db [[sym conn] & body]
  `(let [~sym ~conn] (locking ~sym ~@body)))

;; ---------------------------------------------------------------------------
;; Writes

(defn delete! [conn slug]
  (with-db [c conn]
    (jdbc/with-transaction [tx c]
      (jdbc/execute! tx ["DELETE FROM notes_fts WHERE slug = ?" slug])
      (jdbc/execute! tx ["DELETE FROM links WHERE src = ?" slug])
      (jdbc/execute! tx ["DELETE FROM tags WHERE slug = ?" slug])
      (jdbc/execute! tx ["DELETE FROM notes WHERE slug = ?" slug]))))

(defn upsert! [conn {:keys [slug type title hash mtime created lang excerpt body tags links]}]
  (with-db [c conn]
    (jdbc/with-transaction [tx c]
      (jdbc/execute! tx ["DELETE FROM notes_fts WHERE slug = ?" slug])
      (jdbc/execute! tx ["DELETE FROM links WHERE src = ?" slug])
      (jdbc/execute! tx ["DELETE FROM tags WHERE slug = ?" slug])
      (jdbc/execute! tx ["INSERT INTO notes (slug,type,title,hash,mtime,created,lang,excerpt)
                          VALUES (?,?,?,?,?,?,?,?)
                          ON CONFLICT(slug) DO UPDATE SET type=excluded.type, title=excluded.title,
                            hash=excluded.hash, mtime=excluded.mtime, created=excluded.created,
                            lang=excluded.lang, excerpt=excluded.excerpt"
                         slug type title hash mtime created lang excerpt])
      (doseq [t tags]
        (jdbc/execute! tx ["INSERT OR IGNORE INTO tags (slug, tag) VALUES (?,?)" slug t]))
      (doseq [{:keys [dst kind alias]} links]
        (jdbc/execute! tx ["INSERT INTO links (src,dst,kind,alias) VALUES (?,?,?,?)" slug dst kind alias]))
      (jdbc/execute! tx ["INSERT INTO notes_fts (slug,title,body,tags) VALUES (?,?,?,?)"
                         slug title body (str/join " " tags)]))))

;; ---------------------------------------------------------------------------
;; Reads

(defn- q [conn sql-params]
  (with-db [c conn] (jdbc/execute! c sql-params opts)))

(defn- q1 [conn sql-params]
  (with-db [c conn] (jdbc/execute-one! c sql-params opts)))

(defn- attach-tags [conn rows]
  (let [tags (->> (q conn ["SELECT slug, tag FROM tags ORDER BY tag"])
                  (group-by :slug))]
    (mapv #(assoc % :tags (mapv :tag (get tags (:slug %)))) rows)))

(defn catalog [conn]
  (attach-tags conn (q conn ["SELECT slug, type, title, mtime, created, lang, excerpt
                              FROM notes ORDER BY mtime DESC"])))

(defn hashes [conn]
  (into {} (map (juxt :slug :hash)) (q conn ["SELECT slug, hash FROM notes"])))

(defn note-meta [conn slug]
  (when-let [row (q1 conn ["SELECT slug, type, title, hash, mtime, created, lang, excerpt
                            FROM notes WHERE slug = ?" slug])]
    (assoc row :tags (mapv :tag (q conn ["SELECT tag FROM tags WHERE slug = ? ORDER BY tag" slug])))))

(defn backlinks [conn slug]
  (q conn ["SELECT n.slug, n.type, n.title, n.excerpt, n.lang,
                   MAX(CASE WHEN l.kind = 'embed' THEN 1 ELSE 0 END) AS embeds
            FROM links l JOIN notes n ON n.slug = l.src
            WHERE l.dst = ? AND l.src <> ?
            GROUP BY n.slug ORDER BY n.title" slug slug]))

(defn outgoing [conn slug]
  (q conn ["SELECT l.dst AS slug, n.type, n.title, n.excerpt, n.lang,
                   MAX(CASE WHEN l.kind = 'embed' THEN 1 ELSE 0 END) AS embeds,
                   MIN(rowid_order) AS ord
            FROM (SELECT dst, kind, rowid AS rowid_order FROM links WHERE src = ?) l
            LEFT JOIN notes n ON n.slug = l.dst
            GROUP BY l.dst ORDER BY ord" slug]))

(defn referencing [conn slug]
  (mapv :src (q conn ["SELECT DISTINCT src FROM links WHERE dst = ?" slug])))

(defn tag-counts [conn]
  (q conn ["SELECT tag, COUNT(*) AS count FROM tags GROUP BY tag ORDER BY count DESC, tag"]))

(defn by-tag [conn tag]
  (attach-tags conn (q conn ["SELECT n.slug, n.type, n.title, n.mtime, n.created, n.lang, n.excerpt
                              FROM notes n JOIN tags t ON t.slug = n.slug
                              WHERE t.tag = ? ORDER BY n.mtime DESC" tag])))

;; ---------------------------------------------------------------------------
;; Full-text search

(defn fts-query
  "User text -> safe FTS5 MATCH expression. Every term is quoted; the last
  one is a prefix query so results update while typing."
  [s]
  (let [terms (->> (str/split (or s "") #"\s+")
                   (map #(str/replace % "\"" ""))
                   (remove str/blank?))]
    (when (seq terms)
      (str/join " " (concat (map #(str "\"" % "\"") (butlast terms))
                            [(str "\"" (last terms) "\"*")])))))

(defn- escape-html [s]
  (-> s (str/replace "&" "&amp;") (str/replace "<" "&lt;") (str/replace ">" "&gt;")
      (str/replace "\"" "&quot;")))

(defn- marks->html [s]
  (-> (escape-html (or s ""))
      (str/replace "\u0001" "<mark>")
      (str/replace "\u0002" "</mark>")
      (str/replace #"\s+" " ")))

(defn search [conn text]
  (if-let [match (fts-query text)]
    (->> (q conn ["SELECT f.slug, n.type, n.title, n.lang,
                          snippet(notes_fts, 1, char(1), char(2), '', 12) AS title_hl,
                          snippet(notes_fts, 2, char(1), char(2), '…', 24) AS snippet,
                          bm25(notes_fts, 0.0, 10.0, 1.0, 4.0) AS rank
                   FROM notes_fts f JOIN notes n ON n.slug = f.slug
                   WHERE notes_fts MATCH ?
                   ORDER BY rank LIMIT 50" match])
         (mapv #(-> %
                    (update :snippet marks->html)
                    (update :title_hl marks->html))))
    []))
