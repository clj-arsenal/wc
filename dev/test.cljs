(ns test
  (:require
   [clj-arsenal.wc :as wc]
   [clj-arsenal.vdom :as vdom]
   [clj-arsenal.vdom.browser :as browser-vdom]
   [clj-arsenal.burp :refer [burp]]
   [clj-arsenal.check :refer [check samp expect when-check]]
   [clj-arsenal.basis.protocols.chain :refer [chain chainable]]
   [clj-arsenal.basis :refer [sig-listen sig-unlisten error?]]
   ["happy-dom" :as happy-dom]))

(comment
  (require 'shadow.cljs.devtools.api)
  (shadow.cljs.devtools.api/repl :dev))

(when-check
  (print "Checking..."))

(defn- steps*
  [^js/Window win steps]
  (chainable
    (fn [continue]
      (if (empty? steps)
        (-> win .-happyDOM .waitUntilComplete
          (.then
            (fn []
              (-> win .-happyDOM .close)
              (continue nil))))
        (try
          (binding [wc/*window* win]
            (chain ((first steps) (.-document win))
              (fn [x]
                (if (error? x)
                  (continue x)
                  (let [after-reconcile-sig (wc/after-reconcile-sig)]
                    (sig-listen after-reconcile-sig
                      (fn listener []
                        (sig-unlisten after-reconcile-sig listener)
                        (chain (steps* win (rest steps)) continue))))))))
          (catch :default ex
            (continue ex)))))))

(defn steps
  [& steps]
  (let [win ^js/Window (happy-dom/Window.)]
    (steps* win steps)))

(check ::render-with-props
  (let [foo-value-1 (samp :string)
        foo-value-2 (samp :string)
        bar-value (samp :string)]
    (steps
      (fn [^js/Document doc]
        (wc/define ::my-component
          :inputs
          {:foo (wc/prop-in)
           :bar (wc/prop-in)}

          :render
          (fn [{:keys [foo bar]}]
            (burp
              [:div#foo foo]
              [:div#bar bar])))
        (vdom/render! (browser-vdom/driver doc) (.-body doc)
          (burp
            [::my-component {:foo foo-value-1 :bar bar-value}])))
      (fn [^js/Document doc]
        (let [foo-div (-> doc .-body .-firstChild .-shadowRoot .-childNodes (.item 0))]
          (expect = "foo" (.-id foo-div))
          (expect = foo-value-1 (.-textContent foo-div)))
        (let [bar-div (-> doc .-body .-firstChild .-shadowRoot .-childNodes (.item 1))]
          (expect = "bar" (.-id bar-div))
          (expect = bar-value (.-textContent bar-div)))
        
        (vdom/render! (browser-vdom/driver doc) (.-body doc)
          (burp
            [::my-component {:foo foo-value-2 :bar bar-value}])))
      (fn [^js/Document doc]
        (let [foo-div (-> doc .-body .-firstChild .-shadowRoot .-childNodes (.item 0))]
          (expect = "foo" (.-id foo-div))
          (expect = foo-value-2 (.-textContent foo-div)))
        (let [bar-div (-> doc .-body .-firstChild .-shadowRoot .-childNodes (.item 1))]
          (expect = "bar" (.-id bar-div))
          (expect = bar-value (.-textContent bar-div)))))))

(check ::render-with-attrs
  (let [foo-value-1 (samp :string)
        foo-value-2 (samp :string)
        bar-value (samp :string)]
    (steps
      (fn [^js/Document doc]
        (wc/define ::my-component
          :inputs
          {:foo (wc/attr-in)
           :bar (wc/attr-in)}

          :render
          (fn [{:keys [foo bar]}]
            (burp
              [:div#foo foo]
              [:div#bar bar])))
        (vdom/render! (browser-vdom/driver doc) (.-body doc)
          (burp
            [::my-component {:a/foo foo-value-1 :a/bar bar-value}])))
      (fn [^js/Document doc]
        (let [foo-div (-> doc .-body .-firstChild .-shadowRoot .-childNodes (.item 0))]
          (expect = "foo" (.-id foo-div))
          (expect = foo-value-1 (.-textContent foo-div)))
        (let [bar-div (-> doc .-body .-firstChild .-shadowRoot .-childNodes (.item 1))]
          (expect = "bar" (.-id bar-div))
          (expect = bar-value (.-textContent bar-div)))
        
        (vdom/render! (browser-vdom/driver doc) (.-body doc)
          (burp
            [::my-component {:a/foo foo-value-2 :a/bar bar-value}])))
      (fn [^js/Document doc]
        (let [foo-div (-> doc .-body .-firstChild .-shadowRoot .-childNodes (.item 0))]
          (expect = "foo" (.-id foo-div))
          (expect = foo-value-2 (.-textContent foo-div)))
        (let [bar-div (-> doc .-body .-firstChild .-shadowRoot .-childNodes (.item 1))]
          (expect = "bar" (.-id bar-div))
          (expect = bar-value (.-textContent bar-div)))))))

(defn run
  []
  nil)
