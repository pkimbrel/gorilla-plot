;;;; This file is part of gorilla-repl. Copyright (C) 2014-, Jony Hudson.
;;;;
;;;; gorilla-repl is licenced to you under the MIT licence. See the file LICENCE.txt for full details.

;;; Functions for constructing vega specs. Many of the defaults are adapted from the vega examples.

(ns gorilla-plot.vega
  (:require [gorilla-repl.vega :as vega]))

;; Constants for padding and offsets are chosen so
;; that simple axis titles are visible and do not
;; obstruct axis labels, for common ranges. A smarter
;; dynamic approach is probably possible, but for most
;; practical cases this is sufficient.

(defn container
  [plot-size aspect-ratio]
  {:width   plot-size
   :height  (float (/ plot-size aspect-ratio))
   :padding {:top 10, :left 55, :bottom 40, :right 10}})

(defn data-from-list
  [data-key data]
  {:data [{:name   data-key,
           :values (map (fn [[x y]] {:x x :y y}) data)}]
   })

(defn default-plot-axes
  [x-title y-title]
  {:axes [(merge {:type "x" :scale "x"}
                 (when x-title {:title x-title :titleOffset 30}))
          (merge {:type "y" :scale "y"}
                 (when y-title {:title y-title :titleOffset 45}))]})

;;; Scatter/list plots

(defn- domain-helper
  [data-key axis-plot-range axis]
  (if (= axis-plot-range :all) {:data data-key, :field (str axis)} axis-plot-range))

(defn default-list-plot-scales
  [data-key plot-range]
  {:scales [{:name   "x"
             :type   "linear"
             :range  "width"
             :zero   false
             :domain (domain-helper data-key (first plot-range) "x")
             },
            {:name   "y"
             :type   "linear"
             :range  "height"
             :nice   true
             :zero   false
             :domain (domain-helper data-key (second plot-range) "y")
             }
            ]})

(defn list-plot-marks
  [data-key colour #_shape size opacity]
  {:marks [{:type        "symbol",
            :from       {:data data-key}
            :properties {:enter  {:x           {:scale "x", :field "x"}
                                  :y           {:scale "y", :field "y"}
                                  :fill        {:value (or colour "steelblue")}
                                  :fillOpacity {:value opacity}
                                  }
                         :update {:shape #_shape "circle"
                                  :size        {:value size}
                                  :stroke      {:value "transparent"}
                                  }
                         :hover  {:size   {:value (* 3 size)}
                                  :stroke {:value "white"}
                                  }}}]})

(defn line-plot-marks
  [data-key colour opacity]
  {:marks [{:type       "line",
            :from       {:data data-key}
            :properties {:enter {:x             {:scale "x", :field "x"}
                                 :y             {:scale "y", :field "y"}
                                 :stroke        {:value (or colour "#FF29D2")}
                                 :strokeWidth   {:value 2}
                                 :strokeOpacity {:value opacity}
                                 }}}]})


;;; Bar charts

(defn default-bar-chart-scales
  [data-key plot-range]
  {:scales [{:name   "x"
             :type   "ordinal"
             :range  "width"
             :domain (domain-helper data-key (first plot-range) "x")}
            {:name   "y"
             :range  "height"
             :nice   true
             :domain (domain-helper data-key (second plot-range) "y")}
            ]})

(defn bar-chart-marks
  [data-key colour opacity]
  {:marks [{:type       "rect"
            :from       {:data data-key}
            :properties {:enter {:x     {:scale "x", :field "x"}
                                 :width {:scale "x", :band true, :offset -1}
                                 :y     {:scale "y", :field "y"}
                                 :y2    {:scale "y", :value 0}}
                         :update {:fill    {:value (or colour "steelblue")}
                                  :opacity {:value opacity}}
                         :hover  {:fill {:value "#FF29D2"}}}}]})


;;; Histograms

(defn histogram-marks
  [data-key colour opacity fillOpacity]
  {:marks [{:type       "line",
            :from       {:data data-key}
            :properties {:enter {:x             {:scale "x", :field "x"}
                                 :y             {:scale "y", :field "y"}
                                 :interpolate   {:value "step-before"}
                                 :fill          {:value (or colour "steelblue")}
                                 :fillOpacity   {:value fillOpacity}
                                 :stroke        {:value (or colour "steelblue")}
                                 :strokeWidth   {:value 2}
                                 :strokeOpacity {:value opacity}
                                 }}}]})

;;; Choropleth

(defn choropleth-marks
  [data-key map-key]
  {:marks [{:type       "path",
            :name       "cell"
            :from       {:data map-key}
            :properties {:enter {:path          {:field "layout_path"}
                                 :stroke        {:value "black"}
                                 :strokeWidth   {:value 0.5}}
                        :update {:fill          {:scale "color" :field (str data-key "." data-key)}}
                        :hover  {:fill          {:value "red"}}
                         }}
           {:type          "text",
            :interactive   false,
            :properties {:enter {:x             {:value 0}
                                 :y             {:value 10}
                                 :fill          {:value "black"}
                                 :fontSize      {:value 12}
                                 :align         {:value "left"}}
                         :update {:text         {:signal "value"}}}}
           {:type          "text",
            :interactive   false,
            :properties {:enter {:x             {:value 0}
                                 :y             {:value 22}
                                 :fill          {:value "black"}
                                 :fontSize      {:value 12}
                                 :align         {:value "left"}}
                         :update {:text         {:signal "name"}}}}
           ]
    :signals [
        {:name "hover"
         :init nil
         :streams [
            {
                :type "@cell:mouseover"
                :expr "datum"
            }
            {
                :type "@cell:mouseout"
                :expr nil
            }
         ]}
        {:name "value"
         :init map-key
         :streams [
            {
                :type "hover"
                :expr (str "hover ? hover.value.value : '" map-key "'")
            }
         ]}
              {:name "name"
               :init ""
               :streams [
                         {
                          :type "hover"
                          :expr (str "hover ? hover.value.name : ''")
                          }
                         ]}
        ]})

(defn choropleth-scales
    [data domain range]
  {:scales [{:name   "color"
             :type   "linear"
             :domain domain
             :range  range
            }]})

(defn choropleth-data
    [data data-id data-key map-topology map-key map-projection map-center map-scale map-translate]
    {:data [{:name data-key
             :values data}
            {:name map-key
             :url map-topology
             :format {:feature map-key :type "topojson"}
             :transform [
                {:type "geopath"
                 :projection map-projection
                 :center map-center
                 :scale map-scale
                 :translate map-translate}
                {:type "lookup"
                  :on data-key
                  :onKey data-id
                  :keys [data-id]
                  :as [data-key]}
                {:type "filter"
                  :test (str "datum.layout_path!=null && datum." data-key "!=null")}]}
        ]})

(defn from-vega
  [g]
  (:content g))
