(ns representation.view
  (:use [hiccup core page]
        [hiccup-bridge.core :as conv]
        evolution.evolution_opt)
   (:require [compojure.response :refer [render]]
            [clojure.java.io :as io]))

(defn index-page []
  "Creates index page"
  (html5 (conv/html-file->hiccup "resources/views/index.html")))

(defn not-found []
  "Creates page not found"
  (html5 [:div#not-found.red "Page Not Found!"]))


(defn createScript
  "Creates a script for path drawing"
  [array]
  (for [index array]
    (str "document.getElementById(\"" (inc (get index 0))(inc (get index 1)) "\").style.backgroundColor = \"#900000 \";")))

(defn perform
  "Executes algorithm and displays results on a page"
  []
  (do 
    (html
      (conv/html-file->hiccup "resources/views/home.html")   
      [:script {:type "text/javascript"} (apply str (createScript (evolve-result 10 0.2 0.4 0.2 0.97 200)))])))
