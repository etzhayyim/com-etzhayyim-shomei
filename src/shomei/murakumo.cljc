(ns shomei.murakumo
  "Pure cljc actor boundary generated from manifest migration scaffold."
  (:require [clojure.string :as str]))

(def actor-did
  "did:web:etzhayyim.com:actor:shomei")

(def common-gates
  [:council-charter-attestation
   :no-platform-held-key-baseline
   :no-probing-baseline
   :murakumo-only-inference-baseline
   :did-primary-baseline
   :append-only-gate-baseline
   :kotoba-only-substrate-baseline])

(defn collection
  [name]
  (str "com.etzhayyim.shomei." name))

(def cell-specs {
  :shomei_challenge {:legacy-cell "shomei-challenge"
     :phase :event
     :murakumo-node "reuben"
     :collections [(collection "shomei_challenge")]
     :required-gates common-gates
     :trigger "manifest cell shomei_challenge"
     :ceiling "Manifest-driven migration scaffold; explicit execution stays in runtime methods"}
  :shomei_verify_claim {:legacy-cell "shomei-verify-claim"
     :phase :event
     :murakumo-node "reuben"
     :collections [(collection "shomei_verify_claim")]
     :required-gates common-gates
     :trigger "manifest cell shomei_verify_claim"
     :ceiling "Manifest-driven migration scaffold; explicit execution stays in runtime methods"}
  :shomei_aggregate {:legacy-cell "shomei-aggregate"
     :phase :event
     :murakumo-node "reuben"
     :collections [(collection "shomei_aggregate")]
     :required-gates common-gates
     :trigger "manifest cell shomei_aggregate"
     :ceiling "Manifest-driven migration scaffold; explicit execution stays in runtime methods"}
  :shomei_revoke {:legacy-cell "shomei-revoke"
     :phase :event
     :murakumo-node "reuben"
     :collections [(collection "shomei_revoke")]
     :required-gates common-gates
     :trigger "manifest cell shomei_revoke"
     :ceiling "Manifest-driven migration scaffold; explicit execution stays in runtime methods"}
  :shomei_gov_attest {:legacy-cell "shomei-gov-attest"
     :phase :event
     :murakumo-node "reuben"
     :collections [(collection "shomei_gov_attest")]
     :required-gates common-gates
     :trigger "manifest cell shomei_gov_attest"
     :ceiling "Manifest-driven migration scaffold; explicit execution stays in runtime methods"}
})

(defn safe-rkey
  [s]
  (let [clean (-> (str s)
                  (str/replace #"^did:web:" "")
                  (str/replace #"[^A-Za-z0-9._~-]" "-"))]
    (if (str/blank? clean) "unknown" clean)))

(defn gate-value
  [attestations gate]
  (or (get attestations gate)
      (get attestations (name gate))
      (when (set? attestations) (attestations gate))
      (when (set? attestations) (attestations (name gate)))))

(defn missing-gates
  [spec attestations]
  (->> (:required-gates spec)
       (remove #(boolean (gate-value attestations %)))
       vec))

(defn put-record-effect
  [collection rkey record]
  {:op :mst/put-record
   :actor actor-did
   :collection collection
   :rkey rkey
   :record record})

(defn records-for
  [spec {:keys [records record computed-at request-id]
         :as input}]
  (let [input-records (cond
                        (map? records) records
                        (some? record) {0 record}
                        :else {})
        base {:actorDid actor-did
              :computedAt computed-at
              :legacyCell (:legacy-cell spec)
              :phase (:phase spec)
              :requestId request-id
              :actorBoundary "cljc-migration-scaffold"
              :scaffold true
              :constitutionalStatus "attested-plan"}]
    (map-indexed
     (fn [idx coll]
       (let [record* (merge {:$type coll}
                            base
                            (or (get input-records coll)
                                (get input-records idx)
                                {}))
             rkey (safe-rkey (or (:rkey record*)
                                 (get record* "rkey")
                                 (:tid record*)
                                 request-id
                                 (str (:legacy-cell spec) "-" idx)))]
         {:collection coll
          :record record*
          :rkey rkey}))
     (:collections spec))))

(defn cell-plan
  [cell-key {:keys [attestations] :as input}]
  (let [spec (get cell-specs cell-key)]
    (when-not spec
      (throw (ex-info "unknown cell" {:cell cell-key})))
    (let [missing (missing-gates spec attestations)]
      (merge
       {:cell cell-key
        :legacy-cell (:legacy-cell spec)
        :actor actor-did
        :phase (:phase spec)
        :murakumo-node (:murakumo-node spec)
        :trigger (:trigger spec)
        :ceiling (:ceiling spec)
        :required-gates (:required-gates spec)
        :missing-gates missing}
       (if (seq missing)
         {:status :blocked
          :effects []}
         (let [planned-records (records-for spec input)]
           {:status :ready
            :records (vec planned-records)
            :effects (mapv (fn [{:keys [collection record rkey]}]
                             (put-record-effect collection rkey record))
                           planned-records)}))))))

(defn all-cell-plans
  [input]
  (into {}
        (map (fn [cell-key] [cell-key (cell-plan cell-key input)]))
        (keys cell-specs)))
