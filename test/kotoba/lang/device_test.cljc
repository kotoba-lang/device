(ns kotoba.lang.device-test
  (:require [clojure.test :refer [deftest is testing]]
            [kotoba.lang.device :as dev]
            [kotoba.lang.wit :as wit]))

(deftest surfaces-are-capability-tokens
  (let [ss (dev/surfaces)]
    (is (= 5 (count ss)))
    (is (some #(= (:wit/capability %) "device:bluetooth") ss))
    (is (some #(= (:wit/capability %) "device:geolocation") ss))))

(deftest surface-cap
  (is (= "device:bluetooth" (dev/surface-cap :bluetooth)))
  (is (= "device:wifi" (dev/surface-cap :wifi))))

(deftest describe-surface-schema
  (let [d (dev/describe :bluetooth)]
    (is (= :bluetooth (:surface d)))
    (is (contains? (:methods d) :scan))
    (is (contains? (:methods d) :connect)))
  (is (nil? (dev/describe :nonexistent))))

(deftest discover-returns-only-granted
  (let [pol (-> (wit/policy) (wit/grant "device:bluetooth") (wit/grant "device:geolocation"))
        mgr (dev/make-device-manager pol {} {})]
    (is (= [:bluetooth :geolocation] (dev/discover mgr)))))

(deftest discover-empty-policy-returns-nothing
  (let [mgr (dev/make-device-manager (wit/policy) {} {})]
    (is (empty? (dev/discover mgr)))))

(deftest call-deny-by-default
  (let [mgr (dev/make-device-manager (wit/policy) {:bluetooth (dev/mock-device)} {})]
    (is (= ::dev/denied (dev/call mgr :bluetooth :scan {})))))

(deftest call-gated-scan
  (let [pol (-> (wit/policy) (wit/grant "device:bluetooth"))
        dev0 (dev/mock-device (atom {"device-A" {:rssi -40}}))
        mgr (dev/make-device-manager pol {:bluetooth dev0} {})]
    (is (= ["device-A"] (dev/call mgr :bluetooth :scan {})))))

(deftest call-gated-read-and-write
  (let [pol (-> (wit/policy) (wit/grant "device:bluetooth"))
        state (atom {"d1" "hello"})
        mgr (dev/make-device-manager pol {:bluetooth (dev/mock-device state)} {})]
    (is (= "hello" (dev/call mgr :bluetooth :read {:handle "d1"})))
    (dev/call mgr :bluetooth :write {:handle "d1" :data "world"})
    (is (= "world" (dev/call mgr :bluetooth :read {:handle "d1"})))))

(deftest call-ungranted-surface-denied
  (let [pol (-> (wit/policy) (wit/grant "device:bluetooth"))
        mgr (dev/make-device-manager pol {:wifi (dev/mock-device)} {})]
    ;; wifi driver present but policy grants bluetooth only -> wifi denied
    (is (= ::dev/denied (dev/call mgr :wifi :scan {})))))

(deftest call-rejects-a-method-not-in-the-surfaces-effects
  ;; Granting a surface only authorizes the methods THAT surface's schema
  ;; declares (surface-effects), not every IDevice method a driver happens
  ;; to implement. geolocation's effects are #{:read} only -- a read-only
  ;; grant must not let a caller drive :write/:subscribe through the same
  ;; driver.
  (let [pol (-> (wit/policy) (wit/grant "device:geolocation"))
        mgr (dev/make-device-manager pol {:geolocation (dev/mock-device)} {})]
    (is (= ::dev/unknown-method
           (dev/call mgr :geolocation :write {:handle "loc" :data "spoofed-coords"})))
    (is (= ::dev/unknown-method
           (dev/call mgr :geolocation :subscribe {:handle "loc" :fn (fn [_])})))
    (is (not= ::dev/unknown-method (dev/call mgr :geolocation :read {}))
        "read IS in geolocation's effects"))
  (testing "wifi's effects are scan/connect/read, not write"
    (let [pol (-> (wit/policy) (wit/grant "device:wifi"))
          mgr (dev/make-device-manager pol {:wifi (dev/mock-device)} {})]
      (is (= ::dev/unknown-method (dev/call mgr :wifi :write {:handle "x" :data "y"})))
      (is (not= ::dev/unknown-method (dev/call mgr :wifi :scan {}))))))

(deftest mock-device-shape
  (let [d (dev/mock-device (atom {"a" 1}))]
    (is (= ["a"] (dev/scan d)))
    (is (= 1 (dev/read-dev d "a")))
    (dev/write-dev d "a" 2)
    (is (= 2 (dev/read-dev d "a")))))
