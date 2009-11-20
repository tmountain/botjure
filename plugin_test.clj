(ns plugin-test
  (:use plugin-loader))

(println (pr-str (dispatch plugins "@count")))
(println (pr-str (dispatch plugins "holla back ya'll")))
