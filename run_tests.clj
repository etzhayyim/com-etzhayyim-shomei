(require '[clojure.test :as test])
(def test-namespaces
  '[shomei.cells.test-cell-scaffolds shomei.methods.test-aggregate
    shomei.methods.test-analyze shomei.methods.test-blake2b
    shomei.methods.test-charter-invariants shomei.methods.test-claims
    shomei.methods.test-factors shomei.methods.test-lexicons
    shomei.methods.test-revoke shomei.methods.test-step-up
    shomei.methods.test-verify shomei.murakumo-test
    shomei.repository-contract-test])
(doseq [namespace test-namespaces] (require namespace))
(let [result (apply test/run-tests test-namespaces)]
  (println "==> shomei:" (select-keys result [:test :pass :fail :error]))
  (when (pos? (+ (:fail result) (:error result))) (System/exit 1)))
