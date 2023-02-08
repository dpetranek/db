(ns fluree.publisher.ledger
  (:require
   [fluree.common.identity :as ident]
   [fluree.common.iri :as iri]
   [fluree.common.util :as util]
   [fluree.store.api :as store]))

(defn ledger-path
  [ledger-name]
  (str "ledger/" ledger-name))

(defn create-ledger-address
  [store ledger-name]
  (store/address store "ledger" (ledger-path ledger-name)))

(defn create-ledger-entry
  [prev-ledger db-summary]
  (let [prev-db (-> prev-ledger (get iri/LedgerHead) (get iri/LedgerDbHead))]
    {iri/type               iri/LedgerEntry
     iri/LedgerEntryCreated (util/current-time-iso)
     iri/LedgerDbHead       (or db-summary prev-db)}))

(defn create-ledger
  [store ledger-name {:keys [context tx-address db-address] :as opts}]
  (let [address (create-ledger-address store ledger-name)]
    {iri/type iri/Ledger
     iri/id address
     iri/LedgerName ledger-name
     iri/LedgerAddress address
     iri/LedgerV 0
     iri/LedgerContext context
     iri/LedgerHead (create-ledger-entry nil
                                         {iri/DbBlockAddress db-address
                                          iri/DbBlockTxAddress tx-address})}))

(defn create-ledger-id
  "Create a stable ledger-id"
  [ledger-name]
  (ident/create-id "ledger" ledger-name))
