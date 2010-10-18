(ns #^{:doc "Strange Loop 2010 twitter hackery"
       :author "Kevin A. Archie <karchie@alumni.caltech.edu>"}
  stl2010.tweets
  (:gen-class)
  (:use [clojure.contrib.string :only (join)])
  (:require [clojure.contrib.http.agent :as http-agent]
	    [clojure.contrib.json :as json]))

(defn build-nodes
  "Builds a set of unique nodes (Twitter users) from a collection of
tweets."
  [tweets]
  (reduce (fn [nodes tweet]
	    (let [from_node {:nodeName (:from_user tweet)
			     :nodeValue (:from_user_id tweet)}
		  to_node {:nodeName (:to_user tweet)
			   :nodeValue (:to_user_id tweet)}]
	      (if (:nodeName to_node)
		(conj nodes from_node to_node)
		(conj nodes from_node))))
	  #{} tweets))

(defn build-nodes-index
  "Builds a map from node name to 0-offset integer index"
  [nodes]
  (zipmap (map :nodeName nodes) (range)))

(defn build-edges-map
  "From a collection of tweets, builds a map from edge identity (as a
map with :source and :target) to edge weight (number of occurrences)"
  [tweets]
  (reduce
   (fn [m tweet]
     (let [edge {:source (:from_user tweet) :target (:to_user tweet)}]
       (if (and (:source edge) (:target edge))
	 (assoc m edge (inc (get m edge 0)))
	 m)))
   {} tweets))

(defn build-edges
  "Builds full edge records using zero-offset indices for the given
tweets over the given nodes"
  [tweets nodes]
  (let [indices (build-nodes-index nodes)]
    (map
     (fn [[edge weight]]
       {:source (indices (:source edge))
	:target (indices (:target edge))
	:linkValue weight})
     (build-edges-map tweets))))

(defn build-network
  "Builds a protovis-style network from the given collection of
tweets"
  [tweets]
  (let [nodes (build-nodes tweets)
	edges (build-edges tweets nodes)]
    {:nodes nodes :links edges}))

(defn get-some-tweets
  "Get some tweets matching the given search term(s), up to (not
including) the optional tweet id :max-id. The search terms argument
may be a string or a collection. Other optional keyword arguments:
  :results-per-page (default 100) - how many tweets per request"
  [terms & {:keys [max-id results-per-page]
	    :or {results-per-page 100}}]
  (let [query (str "http://search.twitter.com/search.json?"
		   "q="
		   (if (coll? terms) (join "+" terms) terms)
		   "&rpp=" results-per-page
		   (if max-id (str "&max_id=" max-id) ""))
	entity (http-agent/string (http-agent/http-agent query))]
    (:results (json/read-json entity))))
  
(defn get-tweets
  "Get all the tweets available for the given search terms. Makes a
request, and if the response is nonempty, uses the last id as max_id
in another search request."
  ([terms acc]
     (let [oldest (if (empty? acc) nil (:id (last acc)))
	   tweets (if oldest
		    (do
		      (print "have" (count acc) "tweets so far; ")
		      (println "getting tweets before id" oldest)
		      (get-some-tweets terms :max-id (dec oldest)))
		    (get-some-tweets terms))]
       (if (empty? tweets)
	 acc
	 (get-tweets terms (concat acc tweets)))))
  ([terms] (get-tweets terms [])))

(defn extract-network
  "Retrieves tweets matching the given search term(s), extracts a
protovis-style network from them, and writes as JavaScript into
outfile; returns outfile"
  [outfile terms]
  (let [tweets (get-tweets terms)
	network (build-network tweets)]
    (with-open [fw (java.io.FileWriter. outfile)
		pw (java.io.PrintWriter. fw)]
      (.print pw "var strange_tweets = ")
      (json/write-json network pw)
      (.print pw ";"))
    {:node-count (count (:nodes network))
     :link-count (count (:links network))
     :tweet-count (count tweets)
     :output outfile}))

(defn -main
  ([outfile & args]
     (println "done:"
	      (extract-network outfile
			       (if (seq args) args "strangeloop")) 
     (System/exit 0)))
  ([]
     (println "Usage: tweets output-file [search-term ...]")))
