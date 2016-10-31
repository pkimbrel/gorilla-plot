;;;; This file is part of gorilla-repl. Copyright (C) 2014-, Jony Hudson.
;;;;
;;;; gorilla-repl is licenced to you under the MIT licence. See the file LICENCE.txt for full details.

(ns gorilla-plot.util)

(defn count-in-range
  [data min max]
  (count (filter #(and (< % max) (>= % min)) data)))

(defn bin-counts
  [data min max bin-width]
  (let [bin-starts (range min max bin-width)]
    (map #(count-in-range data % (+ % bin-width)) bin-starts)))

(def state-data {
 :us-10m
 {:projection "albersUsa"
  :key "counties"
  :scale 850
  :center [0 0]}
 :AL
 {:projection "mercator"
  :key "AL"
  :scale 4100
  :name "Alabama"
  :center [-86.9023 32.85]
  :fips "01"}
 :AK
 {:projection "mercator"
  :key "AK"
  :scale 500
  :name "Alaska"
  :center [-155 64]
  :fips "02"}
 :AR
 {:projection "mercator"
  :key "AR"
  :scale 5000
  :name "Arkansas"
  :center [-92.3 34.9]
  :fips "05"}
 :AZ
 {:projection "mercator"
  :key "AZ"
  :scale 3200
  :name "Arizona"
  :center [-112.0937 34.5]
  :fips "04"}
 :CA
 {:projection "mercator"
  :key "CA"
  :scale 1900
  :name "California"
  :center [-119 37.9]
  :fips "06"}
 :CO
 {:projection "mercator"
  :key "CO"
  :scale 4000
  :name "Colorado"
  :center [-105.3 39.2]
  :fips "08"}
 :CT
 {:projection "mercator"
  :key "CT"
  :scale 15000
  :name "Connecticut"
  :center [-72.8 41.55]
  :fips "09"}
 :DC
 {:projection "mercator"
  :key "DC"
  :scale 75000
  :name "Washington, D.C."
  :center [-77.0369 38.9072]
  :fips "11"}
 :DE
 {:projection "mercator"
  :key "DE"
  :scale 11500
  :name "Delaware"
  :center [-75.5277 39.2]
  :fips "10"}
 :FL
 {:projection "mercator"
  :key "FL"
  :scale 3500
  :name "Florida"
  :center [-83.5 28.3]
  :fips "12"}
 :GA
 {:projection "mercator"
  :key "GA"
  :scale 4100
  :name "Georgia"
  :center [-83.2 32.9]
  :fips "13"}
 :HI
 {:projection "mercator"
  :key "HI"
  :scale 6000
  :name "Hawaii"
  :center [-157.2 20.7]
  :fips "15"}
 :IA
 {:projection "mercator"
  :key "IA"
  :scale 5000
  :name "Iowa"
  :center [-93.5 42.1]
  :fips "19"}
 :ID
 {:projection "mercator"
  :key "ID"
  :scale 2200
  :name "Idaho"
  :center [-114.5 46]
  :fips "16"}
 :IL
 {:projection "mercator"
  :key "IL"
  :scale 3200
  :name "Illinois"
  :center [-89.5 40.05]
  :fips "17"}
 :IN
 {:projection "mercator"
  :key "IN"
  :scale 4400
  :name "Indiana"
  :center [-86.6 40]
  :fips "18"}
 :KS
 {:projection "mercator"
  :key "KS"
  :scale 5100
  :name "Kansas"
  :center [-98.1 38.6]
  :fips "20"}
 :KY
 {:projection "mercator"
  :key "KY"
  :scale 5000
  :name "Kentucky"
  :center [-85.5 37.8]
  :fips "21"}
 :LA
 {:projection "mercator"
  :key "LA"
  :scale 4800
  :name "Louisiana"
  :center [-91.2 31.2]
  :fips "22"}
 :MA
 {:projection "mercator"
  :key "MA"
  :scale 9600
  :name "Massachusetts"
  :center [-71.6 42.1]
  :fips "25"}
 :MD
 {:projection "mercator"
  :key "MD"
  :scale 8900
  :name "Maryland"
  :center [-77.1 38.9]
  :fips "24"}
 :ME
 {:projection "mercator"
  :key "ME"
  :scale 3700
  :name "Maine"
  :center [-69.4455 45.5]
  :fips "23"}
 :MI
 {:projection "mercator"
  :key "MI"
  :scale 2850
  :name "Michigan"
  :center [-86.5 44.9]
  :fips "26"}
 :MN
 {:projection "mercator"
  :key "MN"
  :scale 2700
  :name "Minnesota"
  :center [-94 46.7]
  :fips "27"}
 :MO
 {:projection "mercator"
  :key "MO"
  :scale 3800
  :name "Missouri"
  :center [-92.5 38.5]
  :fips "29"}
 :MS
 {:projection "mercator"
  :key "MS"
  :scale 3900
  :name "Mississippi"
  :center [-90 32.8]
  :fips "28"}
 :MT
 {:projection "mercator"
  :key "MT"
  :scale 3200
  :name "Montana"
  :center [-109.6 47]
  :fips "30"}
 :NC
 {:projection "mercator"
  :key "NC"
  :scale 4500
  :name "North Carolina"
  :center [-79.6 35.5]
  :fips "37"}
 :ND
 {:projection "mercator"
  :key "ND"
  :scale 4500
  :name "North Dakota"
  :center [-100 47.6]
  :fips "38"}
 :NE
 {:projection "mercator"
  :key "NE"
  :scale 4400
  :name "Nebraska"
  :center [-99.5 41.7]
  :fips "31"}
 :NH
 {:projection "mercator"
  :key "NH"
  :scale 6000
  :name "New Hampshire"
  :center [-71.7 44.1]
  :fips "33"}
 :NJ
 {:projection "mercator"
  :key "NJ"
  :scale 7000
  :name "New Jersey"
  :center [-74.8 40.25]
  :fips "34"}
 :NM
 {:projection "mercator"
  :key "NM"
  :scale 3300
  :name "New Mexico"
  :center [-106.5 34.5]
  :fips "35"}
 :NV
 {:projection "mercator"
  :key "NV"
  :scale 2600
  :name "Nevada"
  :center [-117 38.9]
  :fips "32"}
 :NY
 {:projection "mercator"
  :key "NY"
  :scale 3700
  :name "New York"
  :center [-75.5 43]
  :fips "36"}
 :OH
 {:projection "mercator"
  :key "OH"
  :scale 5000
  :name "Ohio"
  :center [-82.5 40.35]
  :fips "39"}
 :OK
 {:projection "mercator"
  :key "OK"
  :scale 4700
  :name "Oklahoma"
  :center [-98.4 35.5]
  :fips "40"}
 :OR
 {:projection "mercator"
  :key "OR"
  :scale 3800
  :name "Oregon"
  :center [-120.5 44.3]
  :fips "41"}
 :PA
 {:projection "mercator"
  :key "PA"
  :scale 6600
  :name "Pennsylvania"
  :center [-77.4 41.1]
  :fips "42"}
 :RI
 {:projection "mercator"
  :key "RI"
  :scale 18000
  :name "Rhode Island"
  :center [-71.4 41.62]
  :fips "44"}
 :SC
 {:projection "mercator"
  :key "SC"
  :scale 6000
  :name "South Carolina"
  :center [-81.2 33.8]
  :fips "45"}
 :SD
 {:projection "mercator"
  :key "SD"
  :scale 4600
  :name "South Dakota"
  :center [-100 44.4]
  :fips "46"}
 :TN
 {:projection "mercator"
  :key "TN"
  :scale 4700
  :name "Tennessee"
  :center [-85.7 36]
  :fips "47"}
 :TX
 {:projection "mercator"
  :key "TX"
  :scale 1850
  :name "Texas"
  :center [-99.9 31.75]
  :fips "48"}
 :UT
 {:projection "mercator"
  :key "UT"
  :scale 3600
  :name "Utah"
  :center [-112 39.75]
  :fips "49"}
 :VA
 {:projection "mercator"
  :key "VA"
  :scale 4800
  :name "Virginia"
  :center [-79.2 38]
  :fips "51"}
 :VT
 {:projection "mercator"
  :key "VT"
  :scale 7000
  :name "Vermont"
  :center [-72.6 44]
  :fips "50"}
 :WA
 {:projection "mercator"
  :key "WA"
  :scale 4500
  :name "Washington"
  :center [-120.4 47.45]
  :fips "53"}
 :WI
 {:projection "mercator"
  :key "WI"
  :scale 3600
  :name "Wisconsin"
  :center [-90 45]
  :fips "55"}
 :WV
 {:projection "mercator"
  :key "WV"
  :scale 5300
  :name "West Virginia"
  :center [-80.2 39.08]
  :fips "54"}
 :WY
 {:projection "mercator"
  :key "WY"
  :scale 4200
  :name "Wyoming"
  :center [-107.2 43.2]
  :fips "56"}
})
