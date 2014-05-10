(ns fetcher.core
  (:require [monger.collection :as mc]
            [monger.core :as mg])
  (:use [clojure.java.shell :only (sh)])
  (:import java.io.File))

(mg/connect!)

(mg/set-db! (mg/get-db "github"))

(defn qq [q]
  (mc/find "repos" q))

(defn qc [q]
  (mc/count "repos" q))

(defn java-projs []
  (mc/find "repos" {"language" "Java"}))


(defn clone-proj [url path]
  (sh "/usr/bin/git" "clone" "--depth" "1" url path))

(defn clear-proj [path]
  (sh "/bin/rm" path))

(defn java-files [path]
  (let [f (clojure.java.io/file path)]
    (if (.exists f)
      (filter (fn [file] (.endsWith (.getName file) ".java")) (file-seq f)))))

(defn java-files' [file]
  (if (.isDirectory file)
    (java-files' ())))
