Build web components with a declarative react-like vDOM rendering
system.

```clojure
(ns example)

(require '[clj-arsenal.wc :as wc])
(require '[clj-arsenal.burp :refer [burp]])

(wc/define ::my-component
 :inputs
 {:foo (wc/prop-in)
  :foo-attr (wc/attr-in :name "foo" :reader keyword)
  :bar (wc/state-in :path [:bar])}
 
 :style "
  :host {
    color: red;
    border: 1px solid blue;
  }
 "

 :state
 (fn []
   (atom {:bar "something"}))

 :on-connect
 (fn []
   ;; Do something when the element is connected to the document DOM
   )
 
 :on-update
 (fn []
  ;; Do something after vDOM reconciliation
  )

 :on-disconnect
 (fn []
  ;; Do something when the element is disconnected from the document DOM
  )

  :render
  (fn [{:keys [foo foo-attr bar]}]
   ;; Render function is optional.  IF provided, the instances of this
   ;; component will have a ShadowRoot attached, to which the vDOM
   ;; markup from this function will be rendered.
  (let [foo (or foo foo-attr)]
   (burp
    [:div "foo: " foo]
    [:div "bar: " bar]))))
```

```html
<example.my-component foo="something"></example.my-component>
```

- Hot-reloadable
- Simple, lightweight, no React dependency
