(ns fluree.db.query.exec
  "Find and format results of queries against database values."
  (:require [clojure.core.async :as async :refer [go]]
            [fluree.db.query.exec.select :as select]
            [fluree.db.query.exec.where :as where]
            [fluree.db.query.exec.group :as group]
            [fluree.db.query.exec.order :as order]
            [fluree.db.query.exec.having :as having]
            [fluree.db.util.log :as log :include-macros true]))

#?(:clj (set! *warn-on-reflection* true))

(defn drop-offset
  "Returns a channel containing the stream of solutions from `solution-ch` after
  the `offset` specified by the supplied query. Returns the original
  `solution-ch` if no offset is specified."
  [{:keys [offset]} solution-ch]
  (if offset
    (async/pipe solution-ch
                (async/chan 2 (drop offset)))
    solution-ch))

(defn take-limit
  "Returns a channel that contains at most the specified `:limit` of the supplied
  query solutions from `solution-ch`, if the supplied query has a limit. Returns
  the original `solution-ch` if the supplied query has no specified limit."
  [{:keys [limit]} solution-ch]
  (if limit
    (async/take limit solution-ch)
    solution-ch))

(defn collect-results
  "Returns a channel that will eventually contain the stream of results from the
  `result-ch` channel collected into a single vector, but handles the special
  case of `:select-one` queries by only returning the first result from
  `result-ch` in the output channel. Note that this behavior is different from
  queries with `:limit` set to 1 as those queries will return a vector
  containing a single result to the output channel instead of the single result
  alone."
  [q result-ch]
  (if (:select-one q)
    (async/take 1 result-ch)
    (async/into [] result-ch)))

(defn query
  "Execute the parsed query `q` against the database value `db`. Returns an async
  channel which will eventually contain a single vector of results, or an
  exception if there was an error."
  [db q]
  (go
   (let [error-ch  (async/chan)
         result-ch (->> (where/search db q error-ch)
                        (group/combine q)
                        (having/filter q error-ch)
                        (order/arrange q)
                        (drop-offset q)
                        (take-limit q)
                        (select/format db q error-ch)
                        (collect-results q))]
     (async/alt!
       error-ch  ([e] e)
       result-ch ([result] result)))))
