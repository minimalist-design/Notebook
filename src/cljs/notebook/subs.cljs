(ns notebook.subs
  (:require [notebook.fuzzy :as fuzzy]
            [re-frame.core :as rf]))

(rf/reg-sub :route (fn [db] (:route db)))
(rf/reg-sub :catalog (fn [db] (:catalog db)))
(rf/reg-sub :booting? (fn [db] (:booting? db)))
(rf/reg-sub :notes (fn [db] (:notes db)))
(rf/reg-sub :assets (fn [db] (:assets db)))
(rf/reg-sub :tags (fn [db] (:tags db)))
(rf/reg-sub :editor (fn [db] (:editor db)))
(rf/reg-sub :palette (fn [db] (:palette db)))
(rf/reg-sub :search (fn [db] (:search db)))
(rf/reg-sub :tag-view (fn [db] (:tag-view db)))

(rf/reg-sub :note (fn [db [_ slug]] (get-in db [:notes slug])))
(rf/reg-sub :catalog-entry (fn [db [_ slug]] (get-in db [:catalog slug])))

(rf/reg-sub
 :notes-content
 :<- [:notes]
 (fn [notes] (update-vals notes :content)))

(rf/reg-sub
 :by-type
 :<- [:catalog]
 (fn [catalog [_ type]]
   (->> (vals catalog)
        (filter #(= (name type) (:type %)))
        (sort-by #(- (or (:mtime %) 0))))))

(rf/reg-sub
 :palette/results
 :<- [:catalog]
 :<- [:palette]
 (fn [[catalog {:keys [query]}] _]
   (vec (fuzzy/rank query (vals catalog) 40))))
