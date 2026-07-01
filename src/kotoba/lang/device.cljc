(ns kotoba.lang.device
  "Device capability interfaces (bluetooth/wifi/display/geolocation/camera) as
  EDN + a discover protocol, host-injected drivers. The kotoba principle — a
  component touches only what it was granted — applies especially to hardware.
  device does NOT wrap OS APIs; it defines device surfaces as wit capability
  tokens and a discover/describe protocol, so aiueos's :aiueos/device +
  aiueos:host gate can grant/deny each surface. Consumes wit + coll. No
  third-party deps; .cljc (JVM/SCI/CLJS/GraalVM/kotoba-WASM)."
  (:require [kotoba.lang.wit :as w]
            [kotoba.lang.coll :as c]))

;; ---------- device surfaces (capability tokens) ----------

(def ^:private surface-effects
  "Effects per device surface — what a granted surface lets the component do."
  {:bluetooth   #{:scan :connect :read :write}
   :wifi        #{:scan :connect :read}
   :display     #{:write}
   :geolocation #{:read}
   :camera      #{:read}})

(defn- surface->cap [s] (str "device:" (name s)))

(defn surfaces
  "Return the canonical device-surface capability tokens (wit-compatible). Each
  is {:wit/capability \"device:<s>\" :wit/effects #{...}}."
  []
  (for [[s effects] surface-effects]
    {:wit/capability (surface->cap s)
     :wit/effects    effects}))

(defn surface-cap
  "Return the capability string for a device surface keyword (`:bluetooth` etc)."
  [s] (surface->cap s))

;; ---------- surface schemas ----------

(defn- surface-schema
  "Return the method schema for a device surface."
  [s]
  (case s
    :bluetooth   {:methods {:scan     {:params {} :result :seq}
                            :connect  {:params {:id :string} :result :bool}
                            :read     {:params {:handle :string} :result :bytes}
                            :write    {:params {:handle :string :data :bytes} :result :bool}}}
    :wifi        {:methods {:scan    {:params {} :result :seq}
                            :connect {:params {:ssid :string} :result :bool}
                            :read    {:params {:handle :string} :result :bytes}}}
    :display     {:methods {:write {:params {:target :string :data :bytes} :result :bool}}}
    :geolocation {:methods {:read {:params {} :result {:lat :double :lon :double}}}}
    :camera      {:methods {:read {:params {:source :string} :result :bytes}}}
    nil))

(defn describe
  "Return a surface's schema (methods + effects), or nil if unknown."
  [s]
  (when-let [schema (surface-schema s)]
    (c/assoc-some (assoc schema :surface s)
                  :effects (get surface-effects s))))

;; ---------- IDevice protocol (host-injected driver) ----------

(defprotocol IDevice
  (scan      [dev] "Return a seq of discoverable entities (devices/SSIDs/...).")
  (read-dev  [dev handle] "Read from a handle (bytes/coords/frame).")
  (write-dev [dev handle data] "Write `data` to a handle. Returns bool.")
  (subscribe [dev handle fn*] "Subscribe to a handle; `fn*` called on events."))

;; ---------- mock device (tests / OSS standalone) ----------

(defn mock-device
  "An in-memory IDevice for tests. `state` is an atom of {handle value}. scan
  returns the keys; read returns the value; write sets it; subscribe is a no-op
  (records the subscription)."
  ([]
   (mock-device (atom {})))
  ([state]
   (let [subs (atom {})]
     (reify IDevice
       (scan [_] (keys @state))
       (read-dev [_ handle] (get @state handle))
       (write-dev [_ handle data] (swap! state assoc handle data) true)
       (subscribe [_ handle f] (swap! subs update handle (fnil conj []) f) nil)))))

;; ---------- device manager (policy-gated) ----------

(defn make-device-manager
  "Make a device manager. `policy` is a wit policy (set of granted capability
  strings). `drivers` is a map of surface-key → IDevice. `opts` unused for now.
  Every device call is gated: a surface not in the policy returns ::denied."
  [policy drivers opts]
  {:policy policy :drivers drivers :opts opts})

(defn discover
  "Enumerate the device surfaces a manager's policy grants (in canonical
  order). Returns a seq of surface keywords."
  [mgr]
  (let [granted? (fn [s] (w/allows? (:policy mgr) (surface-cap s)))]
    (filter granted? (keys surface-effects))))

(defn- gate
  "Return the driver for `surface` if the policy grants it, else nil."
  [mgr surface]
  (when (w/allows? (:policy mgr) (surface-cap surface))
    (get-in mgr [:drivers surface])))

(def denied ::denied)

(defn call
  "Capability-gated device call. `surface` is a keyword (`:bluetooth`), `method`
  is `:scan`/`:read`/`:write`/`:subscribe`, `args` is a map of method args.
  Returns `::denied` if the surface isn't granted; dispatches to the driver
  otherwise."
  [mgr surface method args]
  (if-let [dev (gate mgr surface)]
    (case method
      :scan      (scan dev)
      :read      (read-dev dev (:handle args))
      :write     (write-dev dev (:handle args) (:data args))
      :subscribe (subscribe dev (:handle args) (:fn args))
      ::unknown-method)
    denied))
