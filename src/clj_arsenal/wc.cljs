(ns clj-arsenal.wc
  (:require
   [clj-arsenal.vdom.browser :refer [node-type-keyword->element-name] :as browser-vdom]
   [clj-arsenal.vdom :as vdom]
   [clj-arsenal.burp :refer [burp]]
   [clj-arsenal.log :refer [log spy]]
   [clj-arsenal.basis :refer [signal]]
   [clj-arsenal.basis.errors :refer [error-fn]]
   [clj-arsenal.basis.protocols.path-watchable :refer [PathWatchable path-watch path-unwatch]]))

(defn- oget
  ^js [^js obj k]
  (js* "~{}[~{}]" obj k))

(defn- oset!
  [^js obj k v]
  (js* "~{}[~{}] = ~{}" obj k v))

(def ^:dynamic ^js/Window *window* (some-> js/globalThis (oget "window")))
(defonce ^:private init-method-prop-name (js/Symbol "cljArsenalWcInit"))
(defonce ^:private reload-method-prop-name (js/Symbol "cljArsenalWcReload"))
(defonce ^:private state-prop-name (js/Symbol "cljArsenalState"))

(def err-invalid-opt
  (error-fn ::invalid-opt
    "invalid component definition option"))

(def err-invalid-hot-reload-opt-change
  (error-fn ::invalid-hot-reload-opt-change
    "this component definition option isn't allowed to change via hot reload"))

(def err-invalid-hot-reload-opt-remove
  (error-fn ::invalid-hot-reload-opt-remove
    "this component definition option can't be removed via hot reload"))

(def err-prop-name-collision
  (error-fn ::prop-name-collision
    "collision between custom properties given in the :props map, and generated properties"))

(def err-element-name-collision
  (error-fn ::element-name-collision
    "element name collision with an existing custom element"))

(def err-invalid-custom-property
  (error-fn ::invalid-custom-property
    "invalid custom property, key must be a keyword, value must be a :get/:set map or a function"))

(def err-invalid-input
  (error-fn ::invalid-input
    "invalid input, key must be a keyword, value must satisfy ComponentInputSource"))

(defn- swap-state!
  [^js/Object o f & args]
  (oset! o state-prop-name (apply f (oget o state-prop-name) args))
  nil)

(defprotocol ComponentInputSource
  (-eis-attach-instance [eis element input-key])
  (-eis-attach-class [eis class input-key]))

(defrecord ^:no-doc PropInputSource [prop-name])
(defrecord ^:no-doc AttrInputSource [attr-name reader])
(defrecord ^:no-doc StateInputSource [path])

(extend-protocol ComponentInputSource
  PropInputSource
  (-eis-attach-class
    [pis class input-key]
    (let [prop-name (or (.-prop-name pis) (name input-key))]
      (js/Object.defineProperty (.-prototype class)
        prop-name
        #js{:get
            (fn []
              (let [!inputs (::inputs (oget (js* "this") state-prop-name))]
                (get @!inputs input-key)))

            :set
            (fn [v]
              (let [!inputs (::inputs (oget (js* "this") state-prop-name))]
                (swap! !inputs assoc input-key v)
                nil))
            
            :configurable true})
      #(js/Object.defineProperty (.-prototype class) prop-name #js{})))
  (-eis-attach-instance
    [pis element input-key]
    (let [prop-name (or (.-prop-name pis) (name input-key))]
      (when (js/Object.hasOwn element prop-name)
        (let [v (oget element prop-name)]
          (js-delete element prop-name)
          (oset! element prop-name v)))
      nil))
  
  AttrInputSource
  (-eis-attach-class
    [ais class input-key]
    (let [attr-name (or (.-attr-name ais) (name input-key))]
      (swap-state! class update ::observed-attributes conj attr-name)
      #(swap-state! class update ::observed-attributes disj attr-name)))
  (-eis-attach-instance
    [ais element input-key]
    (let [{!inputs ::inputs !attributes ::attributes} (oget element state-prop-name)
          attr-reader (or (.-reader ais) identity)
          watch-key (gensym)
          attr-name (or (.-attr-name ais) (name input-key))
          attr-value (.getAttribute element attr-name)
          input-value (some-> attr-value attr-reader)]

      (if (nil? attr-value)
        (do
          (swap! !inputs dissoc attr-name)
          (when (some? (get @!attributes attr-name))
            (swap! !attributes dissoc attr-name)))
        (do
          (swap! !inputs assoc input-key input-value)
          (when (not= attr-value (get @!attributes attr-name))
            (swap! !attributes assoc attr-name attr-value))))

      (add-watch !attributes watch-key
        (fn [_ _ old-value new-value]
          (let [new-attr-value (get new-value attr-name)]
            (when (not= attr-value (get old-value attr-name))
              (if (nil? new-attr-value)
                (swap! !inputs dissoc input-key)
                (swap! !inputs assoc input-key (some-> new-attr-value attr-reader)))))))

      #(remove-watch !attributes watch-key)))
  
  StateInputSource
  (-eis-attach-class
    [sis class input-key]
    nil)
  (-eis-attach-instance
    [sis element input-key]
    (when-some [{!state ::state !inputs ::inputs} (oget element state-prop-name)]
      (swap! !inputs assoc input-key (get-in @!state (:path sis)))
      (cond
        (satisfies? PathWatchable !state)
        (let [watch-key (gensym)]
          (path-watch !state watch-key (:path sis)
            (fn [old-val new-val]
              (when (not= old-val new-val)
                (swap! !inputs assoc input-key new-val))))
          #(path-unwatch !state watch-key))
        
        (satisfies? IWatchable !state)
        (let [watch-key (gensym)]
          (add-watch !state watch-key
            (fn [_ _ old-val new-val]
              (let [path-val (get-in new-val (:path sis))]
                (when (not= path-val (get-in old-val (:path sis)))
                  (swap! !inputs assoc input-key path-val)))))
          #(remove-watch !state watch-key))))))

(defn prop-in
  [& {:keys [name]}]
  (->PropInputSource name))

(defn attr-in
  [& {:keys [name reader]}]
  (->AttrInputSource name reader))

(defn state-in
  [& {:keys [path]}]
  (->StateInputSource (vec path)))

(defn- reconcile-internals!
  [^js/ElementInternals internals internals-map old-internals-map]
  (when (some? internals)
    (doseq [[k v] internals-map :when (not= v (get old-internals-map k))]
      (case k
        :value (.setFormValue internals (str v))
        :validity (.setValidity internals (clj->js (:flags v)) (:message v) (:anchor v))
        (oset! internals (name k) v)))
    (doseq [k (keys old-internals-map) :when (not (contains? internals-map k))]
      (case k
        :value (.setFormValue internals nil)
        :validity (.setValidity #js{})
        (oset! internals (name k) nil))))
  nil)

(defn- reconcile-style!
  [^js style-obj style-map old-style-map]
  (doseq [[k v] style-map :when (not= v (get old-style-map k))]
    (if (some? v)
      (.setProperty style-obj (name k) (if (keyword? v) (name v) (str v)))
      (.removeProperty style-obj (name k))))
  (doseq [k (keys old-style-map) :when (not (contains? style-map k))]
    (.removeProperty style-obj (name k)))
  nil)

(defn- reconcile!
  [^js/Window window]
  (let [{^js/Set disconnecting ::disconnecting
         ^js/Set dirty ::dirty
         vdom-driver ::vdom-driver} (oget window state-prop-name)
        errors #js[]]
    (while (pos? (.-size disconnecting))
      (let [elements (-> disconnecting .values es6-iterator-seq doall)]
        (.clear disconnecting)
        (doseq [element elements
                :let [component-class-state (oget (.-constructor element) state-prop-name)
                      {!inputs ::inputs !state ::state} (oget element state-prop-name)
                      on-disconnect (:on-disconnect (::opts component-class-state))]
                :when (ifn? on-disconnect)]
          (swap-state! element assoc ::disconnecting false)
          (try
            (on-disconnect @!inputs !state)
            (catch :default ex
              (.push errors ex))))))
    
    (while (pos? (.-size dirty))
      (let [elements (-> dirty .values es6-iterator-seq doall)]
        (.clear dirty)
        (doseq [^js/HTMLElement element elements
                :let [element-state (oget element state-prop-name)
                      class-state (oget (.-constructor element) state-prop-name)
                      component-opts (::opts class-state)
                      !inputs (::inputs element-state)
                      !state (::state element-state)
                      shadow (::shadow element-state)
                      {:keys [render on-update]} component-opts]]
          (swap-state! element assoc ::dirty false)
          (try
            (when (ifn? render)
              (let [vdom (render @(::inputs element-state))
                    old-vdom (::vdom element-state)]
                (when (not= vdom old-vdom)
                  (let [[internals-map style-map content]
                        (if (map? vdom)
                          [(or (:internals vdom) {}) (or (:style vdom) {}) (:content vdom)]
                          [{} {} vdom])

                        [old-internals-map old-style-map]
                        (if (map? old-vdom)
                          (select-keys old-vdom [:internals :style])
                          [{} {}])]
                    (vdom/render! vdom-driver shadow (burp content))
                    (reconcile-internals! (::internals element-state)
                      internals-map old-internals-map)
                    (reconcile-style! (-> ^js (::style element-state) .-cssRules (aget 0) .-style)
                       style-map old-style-map)
                    (swap-state! element assoc ::vdom vdom)))))
            (when (ifn? on-update)
              (on-update @!inputs !state))
            (catch :default ex
              (.push errors ex))))))
    (doseq [ex errors] (log :error :ex ex)))
  nil)

(defn- ensure-window-init!
  [^js/Window window]
  (when-not (oget window state-prop-name)
    (let [before-reconcile-sig (signal)
          after-reconcile-sig (signal)]
      (oset! window state-prop-name
        {::dirty (js/Set.)
         ::disconnecting (js/Set.)
         ::vdom-driver (browser-vdom/driver (.-document window))
         ::before-reconcile-sig before-reconcile-sig
         ::after-reconcile-sig after-reconcile-sig
         ::component-classes (js/Set.)})

      (.requestAnimationFrame window
        (fn reconcile-loop []
          (.requestAnimationFrame window reconcile-loop)
          (before-reconcile-sig)
          (reconcile! window)
          (after-reconcile-sig)))
      
      ;; clean up expired weak refs
      (js/setInterval
        (fn []
          (let [component-classes ^js/Set (::component-classes (oget window state-prop-name))]
            (doseq [^js/WeakRef class-ref (-> component-classes .values es6-iterator-seq doall)
                    :let [component-class ^js/Object (.deref class-ref)]]
              (if (nil? component-class)
                (.delete component-classes class-ref)
                (let [instances ^js/Set (::instances (oget component-class state-prop-name))]
                  (doseq [^js/WeakRef instance-ref instances
                          :when (nil? (.deref instance-ref))]
                    (.delete instances instance-ref)))))))
        150)))
  nil)

(defn- invalidate!
  [element]
  (when-not (::dirty (oget element state-prop-name))
    (let [win ^js/Window (-> element .-constructor (oget state-prop-name) ::window)]
      (.add ^js/Set (::dirty (oget win state-prop-name)) element)
      (swap-state! element assoc ::dirty true)
      nil)))

(defn- disconnecting!
  [element]
  (when-not (::disconnecting (oget element state-prop-name))
    (let [win ^js/Window (-> element .-constructor (oget state-prop-name) ::window)]
      (.add ^js/Set (::disconnecting (oget win state-prop-name)) element)
      (swap-state! element assoc ::disconnecting true)
      nil)))

(defn- cancel-disconnect!
  [element]
  (let [win ^js/Window (-> element .-constructor (oget state-prop-name) ::window)]
    (.delete ^js/Set (::disconnecting (oget win state-prop-name)) element)
    nil))

(defn create-component-class
  [component-name element-name opts]
  (let [component-class
        (js* "(class extends ~{} {
                  constructor() {
                      super();
                      this[~{}]();
                      this[~{}]();
                  }
              })"
          (.-HTMLElement *window*)
          init-method-prop-name
          reload-method-prop-name)
        
        CSSStyleSheet (.-CSSStyleSheet *window*)]
    (js/Object.defineProperty
      (.-prototype component-class) init-method-prop-name
      #js{:value
          (fn []
            (let [this ^js/HTMLElement (js* "this")

                  style
                  (doto (CSSStyleSheet.)
                    (.replaceSync ":host {}"))

                  shadow
                  (.attachShadow this
                    #js{:mode "open"
                        :delegatesFocus (= (:focus opts) ::delegate)})

                  internals
                  (when (fn? (.-attachInternals this))
                    (.attachInternals this))

                  !inputs (atom {})
                  !attributes (atom {})

                  !state (when-some [state (:state opts)]
                           (cond
                             (ifn? state)
                             (state this)

                             (and (satisfies? IWatchable state) (satisfies? IDeref state))
                             state

                             :else
                             (throw (err-invalid-opt ::option :state))))]

              (add-watch !inputs ::update #(invalidate! this))

              (oset! this state-prop-name
                {::shadow shadow
                 ::internals internals
                 ::style style
                 ::inputs !inputs
                 ::attributes !attributes
                 ::state !state
                 ::dirty false
                 ::disconnecting false
                 ::cleanup-fns #js[]})
              (.add (::instances (oget component-class state-prop-name)) (js/WeakRef. this))
              nil))
          
          :configurable true})
    (js/Object.defineProperty
      (.-prototype component-class) reload-method-prop-name
      #js{:value
          (fn []
            (let [this ^js/HTMLElement (js* "this")
                  class-state (oget (.-constructor this) state-prop-name)
                  opts (::opts class-state)
                  instance-state (oget this state-prop-name)
                  shadow ^js/ShadowRoot (::shadow instance-state)
                  cleanup-fns ^js (::cleanup-fns instance-state)
                  !attributes (::attributes instance-state)
                  styles (to-array
                           (concat
                             (keep
                               (fn [style-val]
                                 (cond
                                   (string? style-val)
                                   (doto (CSSStyleSheet.)
                                     (.replaceSync style-val))

                                   (instance? CSSStyleSheet style-val)
                                   style-val))
                               (cond
                                 (string? (:style opts))
                                 [(:style opts)]

                                 (sequential? (:style opts))
                                 (:style opts)))
                             [(::style instance-state)]))]
              (doseq [cleanup-fn cleanup-fns]
                (cleanup-fn))
              (.splice cleanup-fns 0)
              (when (and (= ::self (:focus opts)) (neg? (.-tabIndex this)))
                (set! (.-tabIndex this) 0))
              (doseq [[k v] (:inputs opts)]
                (when-some [cleanup-fn (-eis-attach-instance v this k)]
                  (when (ifn? cleanup-fn)
                    (.push cleanup-fns cleanup-fn))))

              (set! (.-adoptedStyleSheets shadow) styles)
              (invalidate! this))
            nil)
          
          :configurable true})

    (js/Object.defineProperty
      (.-prototype component-class) "connectedCallback"
      #js{:value
          (fn []
            (let [component-class-state (oget component-class state-prop-name)
                  instance (js* "this")
                  instance-state (oget instance state-prop-name)]
              (cond
                (::disconnecting instance-state)
                (cancel-disconnect! instance)

                :else
                (let [on-connect (some-> component-class-state ::opts :on-connect)]
                  (when (ifn? on-connect)
                    (on-connect @(::inputs instance-state) (::state instance-state))))))
            nil)
          
          :configurable true})

    (js/Object.defineProperty
      (.-prototype component-class) "disconnectedCallback"
      #js{:value
          (fn []
            (disconnecting! (js* "this"))
            nil)
          
          :configurable true})

    (js/Object.defineProperty
      (.-prototype component-class) "attributeChangedCallback"
      #js{:value
          (fn [attribute-name _ attribute-value]
            (let [!attributes (::attributes (oget (js* "this") state-prop-name))]
                (if (nil? attribute-value)
                  (swap! !attributes dissoc attribute-name)
                  (swap! !attributes assoc attribute-name attribute-value)))
            nil)
          
          :configurable true})

    (js/Object.defineProperty
      component-class "observedAttributes"
      #js{:get
          (fn []
            (some-> (oget component-class state-prop-name) ::observed-attributes to-array))
          
          :configurable true})

    (js/Object.defineProperty
      component-class "formAssociated"
      #js{:get
          (fn []
            (:form-associated (oget component-class state-prop-name)))
          
          :configurable true})

    (oset! component-class state-prop-name
      {::instances (js/Set.)
       ::window *window*
       ::component-name component-name
       ::observed-attributes #{}
       ::cleanup-fns #js[]})
    (ensure-window-init! *window*)
    (.add ^js/Set (::component-classes (oget *window* state-prop-name)) (js/WeakRef. component-class))
    
    component-class))

(defn update-component-class!
  [^js element-class component-name opts]
  (let [class-state (oget element-class state-prop-name)
        old-opts (::opts class-state)
        cleanup-fns ^js/Array (::cleanup-fns class-state)]

    (when-not (= (::component-name class-state) component-name)
      (throw
        (err-element-name-collision
          ::component-name component-name
          ::element-name (node-type-keyword->element-name component-name))))
    (when (and (some? (:render old-opts)) (nil? (:render opts)))
      (throw (err-invalid-hot-reload-opt-remove ::option :render)))
    (when (not= (:focus old-opts) (:focus opts))
      (throw (err-invalid-hot-reload-opt-change ::option :focus)))

    (doseq [[input-key input-source :as entry] (:inputs opts)
            :when (not
                    (and (keyword? input-key)
                      (satisfies? ComponentInputSource input-source)))]
      (throw (err-invalid-input ::entry entry)))

    (let [generated-prop-names
          (set
            (concat
              (keep
                (fn [[k v]]
                  (when (instance? PropInputSource v)
                    (or (some-> (:prop-name v) keyword) k)))
                (:inputs opts))
              (when (:form-associated opts)
                [:checkValidity :reportValidity])))

          custom-props (:props opts)]

      (doseq [[prop-name prop-value :as entry] custom-props]
        (cond
          (not
            (and (keyword? prop-name)
              (or
                (and (map? prop-value) (seq (select-keys prop-value [:get :set])))
                (fn? prop-value))))
          (throw (err-invalid-custom-property ::entry entry))
          
          (contains? generated-prop-names prop-name)
          (throw (err-prop-name-collision ::entry entry)))))

    ;; cleanup the old class
    (doseq [cleanup-fn cleanup-fns]
      (cleanup-fn))
    (.splice cleanup-fns 0)

    ;; attach inputs
    (doseq [[input-key input-source] (:inputs opts)
            :let [cleanup-fn (-eis-attach-class input-source element-class input-key)]]
      (when (ifn? cleanup-fn)
        (.push cleanup-fns cleanup-fn)))

    ;; custom properties
    (doseq [[prop-key prop-value] (:props opts)
            :let [prop-name (name prop-key)]]
      (cond
        (map? prop-value)
        (js/Object.defineProperty (.-prototype element-class)
          prop-name #js{:get (:get prop-value) :set (:set prop-value) :configurable true})

        (fn? prop-value)
        (js/Object.defineProperty (.-prototype element-class)
          prop-name #js{:get (constantly prop-value) :configurable true}))

      (.push cleanup-fns #(js/Object.defineProperty (.-prototype element-class) prop-name #js{})))

    (swap-state! element-class assoc ::opts opts)

    (doseq [^js/WeakRef instance-ref (::instances class-state)
            :let [instance (.deref instance-ref)]
            :when (some? instance)]
      (js* "(~{}[~{}]())" instance reload-method-prop-name)))
  nil)

(defn define "
Define a custom element class.  The `component-name` should
be a string or keyword.  If given as a string, it'll be used
literally as the element name.  If given as a keyword, it'll
be translated to a valid element name.  Namespace qualified
keywords are allowed and encouraged.

:inputs
Map of input names to sources.
" [component-name & {:as opts}]
  (let [element-name (cond-> component-name
                       (not (string? component-name))
                       node-type-keyword->element-name)
        existing-class (.get (.-customElements *window*) element-name)]
    (cond
      (some? existing-class)
      (update-component-class! existing-class component-name opts)
      
      :else
      (let [component-class (create-component-class component-name element-name opts)]
        (update-component-class! component-class component-name opts)
        (.define (.-customElements *window*) element-name component-class))))
  nil)


(defn after-reconcile-sig
  ([] (after-reconcile-sig *window*))
  ([^js/Window win]
   (ensure-window-init! win)
   (::after-reconcile-sig (oget win state-prop-name))))

(defn before-reconcile-sig
  ([] (before-reconcile-sig *window*))
  ([^js/Window win]
   (ensure-window-init! win)
   (::before-reconcile-sig (oget win state-prop-name))))
