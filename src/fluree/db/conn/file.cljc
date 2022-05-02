(ns fluree.db.conn.file
  (:refer-clojure :exclude [exists?])
  (:require [fluree.db.util.core :as util]
            [fluree.db.conn.state-machine :as state-machine]
            [fluree.db.util.log :as log]
            [fluree.db.conn.proto :as conn-proto]
            [fluree.db.storage.core :as storage]
            [fluree.db.index :as index]
            #?(:clj [fluree.db.full-text :as full-text])
            #?(:clj  [clojure.core.async :as async]
               :cljs [cljs.core.async :as async])
            [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            [fluree.crypto :as crypto])
  #?(:clj
     (:import (java.io ByteArrayOutputStream FileNotFoundException File))))


(defn key->unix-path
  "Given an optional base-path and our key, returns the storage path as a
  UNIX-style `/`-separated path."
  ([key] (key->unix-path nil key))
  ([base-path key]
   #?(:clj
      (let [split-key (str/split key #"_")
            file      (apply io/file base-path split-key)]
        (.toString ^File file))
      :cljs
      (throw (ex-info "TODO: implement for node" {})))))


(defn read-file
  "Read bytes from disk at `path`. Returns nil if file does not exist."
  [path]
  #?(:clj
     (try
       (with-open [xin  (io/input-stream path)
                   xout (ByteArrayOutputStream.)]
         (io/copy xin xout)
         (.toByteArray xout))

       (catch FileNotFoundException _
         nil)
       (catch Exception e (throw e)))
     :cljs
     (throw (ex-info "TODO: implement for node" {}))))

(defn storage-read
  "Reads file `key` from `base-path` into memory."
  [base-path key]
  (read-file (key->unix-path base-path key)))

(defn connection-read
  "Closes over base path to create reader."
  [base-path]
  (fn [key]
    (storage-read base-path key)))

(defn write-file
  "Write bytes to disk at the given file path."
  [^bytes val path]
  #?(:clj
     (try
       (println "writing:" path)
       (with-open [out (io/output-stream (io/file path))]
         (.write out val))
       (catch FileNotFoundException _
         (try
           (io/make-parents (io/file path))
           (with-open [out (io/output-stream (io/file path))]
             (.write out val))
           (catch Exception e
             (log/error (str "Unable to create storage directory: " path
                             " with error: " (.getMessage e) "."))
             (log/error (str "Fatal Error, shutting down!"))
             (System/exit 1))))
       (catch Exception e (throw e)))
     :cljs
     (throw (ex-info "TODO: implement for node" {}))))

(defn storage-write
  "Write disk `data` bytes to file in `key` file at `base-path` on disk."
  [base-path key data]
  (write-file data (key->unix-path base-path key)))

(defn connection-write
  "Closes over base-path to create writer."
  [base-path]
  ;; TODO: arg order may be backwards?
  (fn [key data]
    (write-file data (key->unix-path base-path key))))

(defn connection-commit
  [base-path]
  (fn [data]
    (let [bytes (.getBytes data)
          hash  (crypto/sha2-256 bytes :hex)
          path  #?(:clj (str (-> (io/file "") .getAbsolutePath) "/" base-path "/commits/" hash)
                   :cljs (throw (ex-info "TODO: implement for node" {})))]
      (write-file bytes path)
      (str "fluree:file:" path))))

(defn exists?
  [path]
  #?(:clj
     (.exists? (io/file path))
     :cljs
     (throw (ex-info "TODO: implement for node" {}))))

(defn storage-exists?
  [base-path key]
  (exists? (key->unix-path base-path key)))

(defn connection-exists?
  [base-path]
  (fn [key]
    (storage-exists? base-path key)))

(defn connection-push
  "Just write to a different directory?"
  [base-path]
  #?(:clj
     (fn
       ([commit-id]
        (let [p (promise)]
          (future
            (let [path (str (-> (io/file "") .getAbsolutePath) "/" base-path "/HEAD")
                  [_ _ filename] (str/split commit-id #":")]
              (write-file (.getBytes filename) path)
              (deliver p (str "fluree:file:" path))))
          p))
       ([commit-id ledger]
        (let [p (promise)]
          (future
            (let [path (str (-> (io/file "") .getAbsolutePath) "/" base-path "/" ledger "/HEAD")
                  [_ _ filename] (str/split commit-id #":")]
              (write-file (.getBytes filename) path)
              (deliver p (str "fluree:file:" path))))
          p)))
     :cljs
     (throw (ex-info "TODO: implement for node" {}))))

(defn storage-rename
  [base-path old-key new-key]
  #?(:clj
     (.renameTo
       (io/file (key->unix-path base-path old-key))
       (io/file (key->unix-path base-path new-key)))
     :cljs
     (throw (ex-info "TODO: implement for node" {}))))

(defn connection-rename
  [base-path]
  (fn [old-key new-key]
    (storage-rename base-path old-key new-key)))

(defrecord FileConnection [id transactor? memory state
                           context did
                           local-read local-write
                           push commit
                           read write
                           rename exists?
                           parallelism close-fn
                           msg-in-ch msg-out-ch]
  conn-proto/Commit
  (c-read [_ commit-key] (read commit-key))
  (c-write [_ commit-data] (commit commit-data))

  conn-proto/NameService
  (push [this commit-id] (push commit-id))
  (push [this commit-id ledger] (push commit-id ledger))
  (pull [this ledger] (throw (ex-info "Unsupported FileConnection op: pull" {})))
  (subscribe [this ledger] (throw (ex-info "Unsupported FileConnection op: subscribe" {})))

  conn-proto/iConnection
  (close [_] #_(when (fn? close-fn) (close-fn) (swap! state assoc :closed? true)))
  (closed? [_] (boolean (:closed? @state)))
  (method [_] :file)
  (parallelism [_] parallelism)
  (transactor? [_] transactor?)
  (id [_] id)
  (read-only? [_] (not (fn? write)))
  (context [_] context)
  (did [_] did)
  (msg-in [conn msg] (throw (ex-info "Unsupported FileConnection msg-in: pull" {})))
  (msg-out [conn msg] (throw (ex-info "Unsupported FileConnection msg-out: pull" {})))
  (state [_] @state)
  (state [_ ledger] (get @state ledger))

  storage/Store
  ;; I've got this shadowing Commit, is that okay?
  (read [s k] (read k))
  (write [s k data] (write k data))
  (exists? [s k] (exists? k))
  (rename [s old-key new-key] (rename old-key new-key))

  index/Resolver
  (resolve
    [conn node]
    ;; all root index nodes will be empty

    (storage/resolve-empty-leaf node))

  #?@(:clj
      [full-text/IndexConnection
       (open-storage [conn network dbid lang]
         (throw (ex-info "File connection does not support full text operations."
                         {:status 500 :error :db/unexpected-error})))]))


(defn connect
  "Create a new file system connection."
  [{:keys [context did local-read local-write parallelism storage-path publish-path] :as opts}]
  (let [conn-id  (str (util/random-uuid))
        commit   (connection-commit storage-path)
        read     (connection-read storage-path)
        write    (connection-write storage-path)
        exists?  (connection-exists? storage-path)
        rename   (connection-rename storage-path)
        push     (connection-push publish-path)
        state    (state-machine/blank-state)
        close-fn (fn [] (log/info (str "File Connection " conn-id " Closed")))]
    ;; TODO - need to set up monitor loops for async chans
    (map->FileConnection {:id          conn-id
                          :transactor? false
                          :context     context
                          :did         did
                          :local-read  local-read
                          :local-write local-write
                          :read        read
                          :write       write
                          :commit      commit
                          :push        push
                          :exists?     exists?
                          :rename      rename
                          :parallelism parallelism
                          :msg-in-ch   (async/chan)
                          :msg-out-ch  (async/chan)
                          :close       close-fn
                          :memory      true
                          :state       state})))
