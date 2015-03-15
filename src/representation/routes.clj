(ns representation.routes  
  (:use compojure.core        
        [hiccup.middleware :only (wrap-base-url)]
        [hiccup core page])
  (:require [compojure.route :as route]
            [compojure.handler :as handler]
            [compojure.response :as response]
            [representation.view :as view]))



(defroutes main-routes
  (GET "/" [] (view/index-page))
  (route/resources "/static")
  (GET "/result" [] (view/perform))
  (route/not-found (view/not-found)))

(def app
  (-> (handler/site main-routes)
    (wrap-base-url)))

