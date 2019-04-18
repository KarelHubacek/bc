(ns instrument-maker.core
  (:require [clojure.string :as str]))

(defn split-by-coma
  [str]
  (str/split str #","))

(defn zipmap-curry
  [keys]
  (fn [values]
    (zipmap keys values)))

(defn replace-curry
  [match replacement]
  (fn [string]
    (str/replace string match replacement)))

(defn vector-without-blanks
  [vec]
  (map (replace-curry " " "_") vec))

(defn vector-to-symbols
  [vec]
  (map symbol vec))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn transform-file
  [str]
  (map vector-without-blanks
       (map split-by-coma
            (str/split-lines str))))

(defn get-keywords
  [vec]
  (map keyword (first vec)))

(defn get-data
  [vec]
  (map vector-to-symbols (rest vec)))

(defn get-dates
  [vec]
  (map keyword
       (map first (rest vec))))


(defn make-instrument
  "Transforms DucasCopy datafeed file
  into an instrument"
  [file-path]
  (let [file (slurp file-path)
        transformed-file (transform-file file)
        keywords (get-keywords transformed-file)
        data (get-data transformed-file)
        dates (get-dates transformed-file)]
   
     (zipmap
      dates
      (map (zipmap-curry keywords) data))
     ))    

(defn make-instrument2
  [file-path interval]
  (let [file (slurp file-path)
        transformed-file (transform-file file)
        keywords (get-keywords transformed-file)
        data (get-data transformed-file)
        dates (get-dates transformed-file)]
    (conj
     (zipmap
      dates
      (map (zipmap-curry keywords) data))
     [:interval interval])))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(def a (make-instrument "c:/Users/Martin/Desktop/Kameron/database/AAPL.USUSD_Candlestick_1_D_BID_11.02.2019-16.02.2019.csv"))
;(def b (make-instrument2 "c:/Users/Martin/Desktop/Kameron/database/AAPL.USUSD_Candlestick_1_D_BID_11.02.2019-16.02.2019.csv" 86400))

;a
;b



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn date-as-number
  [sym]
  (let [string (str sym)]
    (+
     (* (read-string (subs string 0 2)) 86400)
     (* (read-string (subs string 3 5)) (* 30 86400))
     (* (read-string (subs string 6 10))(* 365 86400))
     (* (read-string (subs string 11 13)) 3600)
     (* (read-string (subs string 14 16)) 60)
     (read-string (subs string 17 19)))))

(defn abs
  [num]
  (max num (- num)))

(defn find-interval
  "Finds the length of the interval
  between bars of an instrument
  Nefunguje pro mesice!!!"
  [instrument]
  (conj instrument [:interval
                    (abs
                     (- (date-as-number (get (peek (first instrument)) :Local_time))
                        (date-as-number (get (peek (second instrument)) :Local_time))))]
        ))

(defn thin-an-instrument
  [instrument interval]
  (if (get instrument :interval)
    (thin-an-instrument* instrument
                         (/ interval (get instrument :interval))
                         (/ interval (get instrument :interval))
                         {})
    nil))

(defn thin-an-instrument*
  [instrument ratio counter result]
  (if (empty? instrument)
    result
    (if (= ratio counter)
      (thin-an-instrument*
       (rest instrument)
       ratio
       1
       (conj result (first instrument)))
      (thin-an-instrument*
       (rest instrument)
       ratio
       (+ 1 counter)
       result))))
       
       
    
  
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(find-interval a)

(date-as-number (get (peek (first a)) :Local_time))
(date-as-number (get (peek (second a)) :Local_time))
(conj {:a 1} [:b 2]) 

(get a :interval)
(get b :interval)
(get (find-interval a) :interval)

(get a :11.02.2019_23:00:00.000_GMT+0100) 
(thin-an-instrument a 5)
(thin-an-instrument b (* 40000))
(empty? {})
(rest {:a 1 :b 2 :c 3})

;moving average!!!
