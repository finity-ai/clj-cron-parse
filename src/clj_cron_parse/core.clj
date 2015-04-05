(ns clj-cron-parse.core
  (:require [clojure.string :as s]
            [clj-time.core :as t]
            [clojure.core.match :refer [match]]))

(defn int-or-nil
  [s]
  (try
    (Integer/parseInt s)
    (catch Exception _ nil)))

(defn num-or-range
  [s]
  (let [x (re-find #"^\d+$" s)]
    (if-not (empty? x)
      (Integer/parseInt x)
      (if-let [[_ b t _ s] (re-find #"^(\d+)-(\d+)(/(\d+))?$" s)]
        (let [r (range (Integer/parseInt b) (inc (Integer/parseInt t)) (Integer/parseInt (or s "1")))]
          (if (empty? r) nil r))
        (if-let [[_ step] (re-find #"^\*/(\d+)$" s)]
          {:range (Integer/parseInt step)}
          nil)))))

(defn parse-single-month
  [s]
  (cond
    (= "JAN" s) 1
    (= "FEB" s) 2
    (= "MAR" s) 3
    (= "APR" s) 4
    (= "MAY" s) 5
    (= "JUN" s) 6
    (= "JUL" s) 7
    (= "AUG" s) 8
    (= "SEP" s) 9
    (= "OCT" s) 10
    (= "NOV" s) 11
    (= "DEC" s) 12
    :else (int-or-nil s)))

(defn month-num-or-range
  [s]
  (let [x (re-find #"^(\d+|\w{3})$" s)]
    (if-not (empty? x)
      (parse-single-month (second x))
      (if-let [[_ b t _ s] (re-find #"^(\d+|\w{3})-(\d+|\w{3})(/(\d+))?$" s)]
        (let [r (range (parse-single-month b) (inc (parse-single-month t)) (Integer/parseInt (or s "1")))]
          (if (empty? r) nil r))
        (if-let [[_ step] (re-find #"^\*/(\d+)$" s)]
          {:range (Integer/parseInt step)}
          nil)))))

(defn parse-single-day
  [s]
  (cond
    (= "MON" s) 1
    (= "TUE" s) 2
    (= "WED" s) 3
    (= "THU" s) 4
    (= "FRI" s) 5
    (= "SAT" s) 6
    (= "SUN" s) 7
    (= "W" s) [1 2 3 4 5]
    (= "1L" s) :L1
    (= "2L" s) :L2
    (= "3L" s) :L3
    (= "4L" s) :L4
    (= "5L" s) :L5
    :else (let [n (int-or-nil s)]
            (if (= 0 n) 7 n))))

(defn day-num-or-range
  [s]
  (let [x (re-find #"^(\d+|\w{3}|L|W|1L|2L|3L|4L|5L)$" s)]
    (if-not (empty? x)
      (parse-single-day (second x))
      (let [y (re-find #"^(\d+|\w{3})-(\d+|\w{3})$" s)]
        (if y (->> (rest y)
                   (map parse-single-day)
                   ((fn [[a b]] (concat (range a b) [b]))))
              nil)))))

(defn parse-item
  [s range-fn]
  (cond
    (= s "*") :star
    :else (let [x (s/split s #",")]
            (if (empty? x) nil
                           (let [y (->> x
                                        (map range-fn)
                                        flatten
                                        distinct)]
                             (if (and
                                   (every? identity y)
                                   (not (empty? y)))
                               (sort y)
                               nil))))))

(defn parse-dow-item
  [s]
  (cond
    (= s "*") :star
    :else (let [x (s/split s #",")]
            (if (empty? x) nil
                           (let [y (->> x
                                        (map day-num-or-range)
                                        flatten
                                        distinct)]
                             (if (and
                                   (every? identity y)
                                   (not (empty? y)))
                               {:keywords (filter keyword? y)
                                :numbers  (sort (filter number? y))}
                               nil))))))

(defn between
  [minimum maximum xs]
  (match [xs]
         [([a & bs] :seq)] (<= minimum a (last xs) maximum)
         :else false))

(defn parse-field
  [minimum maximum range-fn s]
  (let [d (parse-item s range-fn)]
    (match [d]
           [:star] :star
           [([{:range _} :as r] :seq)] r
           [([& xs] :seq) :guard (partial between minimum maximum)] d
           :else nil)))

(defn parse-dom
  [s]
  (parse-field 1 31 num-or-range s))

(defn parse-minutes-or-seconds
  [s]
  (parse-field 0 59 num-or-range s))

(defn parse-hours
  [s]
  (parse-field 0 23 num-or-range s))

(defn parse-month
  [s]
  (parse-field 1 12 month-num-or-range s))

(defn parse-dow
  [s]
  (let [d (parse-dow-item s)]
    (match [d]
           [:star] :star
           [{:numbers  (xs :guard (partial between 1 7))
             :keywords _}] d
           :else nil)))

(defn make-cron-map
  [cron]
  (let [fields (s/split cron #" ")]
    (if (= 6 (count fields))
      (let [cron-map {:dow    (parse-dow (nth fields 5))
                      :month  (parse-month (nth fields 4))
                      :dom    (parse-dom (nth fields 3))
                      :hour   (parse-hours (nth fields 2))
                      :minute (parse-minutes-or-seconds (nth fields 1))
                      :sec    (parse-minutes-or-seconds (nth fields 0))}]
        (if (every? identity (vals cron-map))
          cron-map
          nil))
      nil)))

(defn next-val
  [now as]
  (->> as
       (filter #(> % now))
       first))

(defn now-with-seconds
  [now sec]
  (match [sec]
         [([& xs] :seq)] (if-let [ns (next-val (t/second now) xs)]
                           (t/plus now (t/seconds (- ns (t/second now))))
                           (t/plus now (t/minutes 1) (t/seconds (- (first xs) (t/second now)))))
         :else now))

(defn now-with-minutes
  [now minute]
  (match [minute]
         [([& xs] :seq)] (if-let [ns (next-val (t/minute now) xs)]
                           (t/plus now (t/minutes (- ns (t/minute now))))
                           (t/plus now (t/hours 1) (t/minutes (- (first xs) (t/minute now)))))
         :else now))

(defn now-with-hours
  [now hour]
  (match [hour]
         [([& xs] :seq)] (if-let [ns (next-val (t/hour now) xs)]
                           (t/plus now (t/hours (- ns (t/hour now))))
                           (t/plus now (t/days 1) (t/hours (- (first xs) (t/hour now)))))
         :else now))

(defn now-with-doms
  [now dom]
  (match [dom]
         [([& xs] :seq)] (if-let [ns (next-val (t/day now) xs)]
                           (t/plus now (t/days (- ns (t/day now))))
                           (t/plus now (t/months 1) (t/days (- (first xs) (t/day now)))))
         :else now))

(defn now-with-dows
  [now dow]
  (match [dow]
         [:star] now
         [{:numbers  (xs :guard (partial between 1 7))
           :keywords _}] (if-let [ns (next-val (t/day-of-week now) xs)]
                           (t/plus now (t/days (- ns (t/day-of-week now))))
                           (t/plus now (t/days (- 7 (t/day-of-week now) (* -1 (first xs))))))
         :else now))

(defn now-with-months
  [now month]
  (match [month]
         [([& xs] :seq)] (if-let [ns (next-val (t/month now) xs)]
                           (t/plus now (t/months (- ns (t/month now))))
                           (t/plus now (t/years 1) (t/months (- (first xs) (t/month now)))))
         :else now))

(defn next-date-by-dom
  [now {:keys [sec minute hour dom month]}]
  (-> now
      (now-with-seconds sec)
      (now-with-minutes minute)
      (now-with-hours hour)
      (now-with-doms dom)
      (now-with-months month)))

(defn next-date-by-dow
  [now {:keys [sec minute hour dow month]}]
  (-> now
      (now-with-seconds sec)
      (now-with-minutes minute)
      (now-with-hours hour)
      (now-with-dows dow)
      (now-with-months month)))

(defn next-date
  "takes an org.joda.time.DateTime representing now and a cron expression and returns the next org.joda.time.DateTime to occur for that cron exp.

  The cron expressions attempt to follow BSD crontab by Paul Vixie with the addition of seconds in the first position

         field         allowed values
         -----         --------------
         second        0-59
         minute        0-59
         hour          0-23
         day of month  1-31
         month         1-12 (or names, see below)
         day of week   0-7 (0 or 7 is Sun, or use names)

   A field may be an asterisk (*), which always stands for ``first-last''.

   Ranges of numbers are allowed.  Ranges are two numbers separated with a hyphen.  The specified range is inclusive.  For example, 8-11 for an ``hours'' entry specifies execution
   at hours 8, 9, 10 and 11.

   Lists are allowed.  A list is a set of numbers (or ranges) separated by commas.  Examples: ``1,2,5,9'', ``0-4,8-12''.

   Step values can be used in conjunction with ranges.  Following a range with ``/<number>'' specifies skips of the number's value through the range.  For example, ``0-23/2'' can be
   used in the hours field to specify command execution every other hour (the alternative in the V7 standard is ``0,2,4,6,8,10,12,14,16,18,20,22'').  Steps are also permitted after
   an asterisk, so if you want to say ``every two hours'', just use ``*/2''.

   Names can also be used for the ``month'' and ``day of week'' fields.  Use the first three letters of the particular day or month (case does not matter).

   Note: The day of a command's execution can be specified by two fields -- day of month, and day of week.  If both fields are restricted (ie, are not *), the command will be run
   when either field matches the current time.  For example, ``30 4 1,15 * 5'' would cause a command to be run at 4:30 am on the 1st and 15th of each month, plus every Friday.

   Instead of the first five fields, one of eight special strings may appear:

         string          meaning
         ------          -------
         @reboot         Run once, at startup.
         @yearly         Run once a year, \"0 0 1 1 *\".
         @annually       (same as @yearly)
         @monthly        Run once a month, \"0 0 1 * *\".
         @weekly         Run once a week, \"0 0 * * 0\".
         @daily          Run once a day, \"0 0 * * *\".
         @midnight       (same as @daily)
         @hourly         Run once an hour, \"0 * * * *\".
         "

  [now cron]
  (if-let [{:keys [dom dow] :as cron-map} (time (make-cron-map cron))]
    (match [dom dow]
           [:star :star] (next-date-by-dom now cron-map)
           [_ :star] (next-date-by-dom now cron-map)
           [:star _] (next-date-by-dow now cron-map)
           :else (let [by-dom (next-date-by-dom now cron-map)
                       by-dow (next-date-by-dow now cron-map)]
                   (-> [by-dom by-dow] sort first)))
    nil))
