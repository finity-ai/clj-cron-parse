(ns clj-cron-parse.core
  (:require [clojure.string :as s]
            [clj-time.core :as t]
            [clojure.core.match :refer [match]]
            [clj-time.predicates :as pr]))

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
  (let [s (s/upper-case s)]
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
      :else (int-or-nil s))))

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
  (let [s (s/upper-case s)]
    (cond
      (= "MON" s) 1
      (= "TUE" s) 2
      (= "WED" s) 3
      (= "THU" s) 4
      (= "FRI" s) 5
      (= "SAT" s) 6
      (= "SUN" s) 7
      (= "W" s) [1 2 3 4 5]
      (= "1L" s) :1L
      (= "2L" s) :2L
      (= "3L" s) :3L
      (= "4L" s) :4L
      (= "5L" s) :5L
      (= "6L" s) :6L
      (= "7L" s) :7L
      :else (let [n (int-or-nil s)]
              (if (= 0 n) 7 n)))))

(defn dow-num-or-range
  [s]
  (let [x (re-find #"^(\d+|\w{3}|L|W|1L|2L|3L|4L|5L|6L|7L)$" s)]
    (if-not (empty? x)
      (parse-single-day (second x))
      (let [y (re-find #"^(\d+|\w{3})-(\d+|\w{3})$" s)]
        (if y (->> (rest y)
                   (map parse-single-day)
                   ((fn [[a b]] (concat (range a b) [b]))))
            (if-let [[_ step] (re-find #"^\*/(\d+)$" s)]
              (range 1 7 (Integer/parseInt step))
              nil))))))

(defn parse-item
  [s range-fn]
  (cond
    (= s "*") :star
    (= s "L") :L
    (= s "W") :W
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
                             (map dow-num-or-range)
                             flatten
                             distinct)]
                  (if (and
                       (every? identity y)
                       (not (empty? y)))
                    (sort y)
                    nil))))))

(defn bound-seq?
  [minimum maximum xs]
  (and (coll? xs)
       (not-empty xs)
       (every? number? xs)
       (<= minimum (first xs) (last xs) maximum)))

(defn next-divisible
  [x d]
  (* d (+ 1 (quot x d))))

(defn parse-field
  [minimum maximum range-fn s]
  (let [d (parse-item s range-fn)]
    (match (parse-item s range-fn)
      nil nil
      :star :star
      ([{:range r}] :seq) (range (next-divisible minimum r) maximum r)
      (xs :guard (partial bound-seq? minimum maximum)) d
      :else nil)))

(defn parse-dom
  [s]
  (let [d (parse-item s num-or-range)]
    (match d
      nil nil
      :star :star
      :L :L
      :W :W
      ([{:range _} :as r] :seq) r
      (xs :guard (partial bound-seq? 1 31)) d
      :else nil)))

(defn parse-dow
  [s]
  (let [d (parse-dow-item s)]
    (match d
      nil nil
      :star :star
      (xs :guard (partial bound-seq? 1 7)) d
      (xs :guard #(not (empty? %))) d
      :else nil)))

(defn make-cron-map
  [cron]
  (match (s/split cron #" ")
    [sec minute hour dom month dow] (let [cron-map {:dow    (parse-dow dow)
                                                    :month  (parse-field 1 12 month-num-or-range month)
                                                    :dom    (parse-dom dom)
                                                    :hour   (parse-field 0 23 num-or-range hour)
                                                    :minute (parse-field 0 59 num-or-range minute)
                                                    :sec    (parse-field 0 59 num-or-range sec)}]
                                      (if (every? identity (vals cron-map))
                                        cron-map
                                        nil))
    ["@yearly"] {:dow :star :month [1] :dom [1] :hour [0] :minute [0] :sec [0]}
    ["@annually"] {:dow :star :month [1] :dom [1] :hour [0] :minute [0] :sec [0]}
    ["@monthly"] {:dow :star :month :star :dom [1] :hour [0] :minute [0] :sec [0]}
    ["@weekly"] {:dow [1] :month :star :dom :star :hour [0] :minute [0] :sec [0]}
    ["@daily"] {:dow :star :month :star :dom :star :hour [0] :minute [0] :sec [0]}
    ["@midnight"] {:dow :star :month :star :dom :star :hour [0] :minute [0] :sec [0]}
    ["@hourly"] {:dow :star :month :star :dom :star :hour :star :minute [0] :sec [0]}
    :else nil))

(defn next-val
  [now as]
  (->> as
       (filter #(>= % now))
       first))

(defn now-with-seconds
  [now sec]
  (match sec
    ([& xs] :seq) (if-let [ns (next-val (t/second (t/plus now (t/seconds 1))) xs)]
                    (t/plus now (t/seconds (- ns (t/second now))))
                    (t/plus now (t/minutes 1) (t/seconds (- (first xs) (t/second now)))))
    :else now))

(defn now-with-minutes
  [now minute]
  (match minute
    ([& xs] :seq) (if-let [ns (next-val (t/minute now) xs)]
                    (t/plus now (t/minutes (- ns (t/minute now))))
                    (t/plus now (t/hours 1) (t/minutes (- (first xs) (t/minute now)))))
    :else now))

(defn now-with-hours
  [now hour]
  (match hour
    ([& xs] :seq) (if-let [ns (next-val (t/hour now) xs)]
                    (t/plus now (t/hours (- ns (t/hour now))))
                    (t/plus now (t/days 1) (t/hours (- (first xs) (t/hour now)))))
    :else now))

(defn next-week-dom
  [now]
  (let [tomorrow (t/plus now (t/days 1))]
    (if (pr/weekday? tomorrow)
      tomorrow
      (next-week-dom tomorrow))))

(defn now-with-doms
  [now dom]
  (match dom
    :L (-> now
           (t/plus (t/months 1))
           (t/minus (t/days (t/day now))))
    :W (next-week-dom now)
    ([& xs] :seq) (if-let [ns (next-val (t/day now) xs)]
                    (t/plus now (t/days (- ns (t/day now))))
                    (t/plus (t/minus now (t/days (- (t/day now) 1)))
                            (t/months 1)
                            (t/days (- (first xs) 1))))
    {:range x} (t/plus now (t/days 1))
    :else now))

(defn last-dow-of-month
  [now ls]
  (letfn [(f [d]
            (let [day-of-week (t/day-of-week (t/last-day-of-the-month now))
                  diff (- day-of-week d)
                  diff2 (if (neg? diff)
                          (- (+ 7 day-of-week) d)
                          diff)]
              (t/minus (t/last-day-of-the-month now) (t/days diff2))))]
    (->> ls
         (map #(match %
                 :1L (f 1)
                 :2L (f 2)
                 :3L (f 3)
                 :4L (f 4)
                 :5L (f 5)
                 :6L (f 6)
                 :7L (f 7)
                 :else nil))
         sort
         first)))

(defn now-with-dows
  [now dow]
  (match dow
    :star now
    (xs :guard (partial bound-seq? 1 7)) (if-let [ns (next-val (t/day-of-week now) xs)]
                                           (t/plus now (t/days (- ns (t/day-of-week now))))
                                           (t/plus now (t/days (- 7 (t/day-of-week now) (* -1 (first xs))))))
    (xs :guard #(not (empty? %))) (last-dow-of-month now xs)
    :else now))

(defn now-with-months
  [now month]
  (match month
    ([& xs] :seq) (if-let [ns (next-val (t/month now) xs)]
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
  [now {:keys [sec minute hour dow month] :as e}]
  (-> now
      (now-with-seconds sec)
      (now-with-minutes minute)
      (now-with-hours hour)
      (now-with-dows dow)
      (now-with-months month)))

(defn next-date
  "takes an org.joda.time.DateTime representing now and a cron expression and returns the next org.joda.time.DateTime to occur for that cron exp.
  If it is not possible to parse the cron expression then nil is returned.

  If you provide the value of now as a DateTime in a timezone, it will return the cron calculated within that timezone.
  To view the value in UTC simply convert it to the UTC timezone.

  Alternatively you can provide a timezone id such as \"Asia/Seoul\" (defaults to \"UTC\") as the final argument and you
  will get back the next date in UTC time for the cron expression calculated in that timezone.  For example, you want the next UTC
  date for every day at midday in Seoul (next-date now \"0 0 12 * * *\" \"Asia/Seoul\").

  The cron expressions attempt to follow BSD crontab by Paul Vixie with the addition of seconds in the first position and W can be used for both
  day of month and day of week.

         field         allowed values
         -----         --------------
         second        0-59
         minute        0-59
         hour          0-23
         day of month  1-31 L W
         month         1-12 (or names, see below)
         day of week   0-7 (0 or 7 is Sun, or use names) W 1L 2L 3L 4L 5L 6L 7L

   A field may be an asterisk (*), which always stands for ``first-last''.

   Ranges of numbers are allowed.  Ranges are two numbers separated with a hyphen.  The specified range is inclusive.  For example, 8-11 for an ``hours'' entry specifies execution
   at hours 8, 9, 10 and 11.

   Lists are allowed.  A list is a set of numbers (or ranges) separated by commas.  Examples: ``1,2,5,9'', ``0-4,8-12''.

   Step values can be used in conjunction with ranges.  Following a range with ``/<number>'' specifies skips of the number's value through the range.  For example, ``0-23/2'' can be
   used in the hours field to specify command execution every other hour (the alternative in the V7 standard is ``0,2,4,6,8,10,12,14,16,18,20,22'').  Steps are also permitted after
   an asterisk, so if you want to say ``every two hours'', just use ``*/2''.

   Names can also be used for the ``month'' and ``day of week'' fields.  Use the first three letters of the particular day or month (case does not matter).

   Note: The day of a command's execution can be specified by two fields -- day of month, and day of week.
   If both fields are restricted (ie, are not *), the earliest time will be returned.
   For example, ``30 4 1,15 * 5'' would return the next date matching 4:30 am on the 1st and 15th of each month, plus every Friday.

   Instead of the first five fields, one of eight special strings may appear:

         string          meaning
         ------          -------
         @yearly         Run once a year, \"0 0 0 1 1 *\".
         @annually       (same as @yearly)
         @monthly        Run once a month, \"0 0 0 1 * *\".
         @weekly         Run once a week, \"0 0 0 * * 0\".
         @daily          Run once a day, \"0 0 0 * * *\".
         @midnight       (same as @daily)
         @hourly         Run once an hour, \"0 0 * * * *\".
         "

  ([now cron]
   (if-let [{:keys [dom dow] :as cron-map} (make-cron-map cron)]
     (match [dom dow]
       [:star :star] (next-date-by-dom now cron-map)
       [_ :star] (next-date-by-dom now cron-map)
       [:star _] (next-date-by-dow now cron-map)
       :else (let [by-dom (next-date-by-dom now cron-map)
                   by-dow (next-date-by-dow now cron-map)]
               (-> [by-dom by-dow] sort first)))
     nil))
  ([now cron timezone]
   (if timezone
     (let [tz-now (t/to-time-zone now (t/time-zone-for-id timezone))]
       (t/to-time-zone (next-date tz-now cron) (t/time-zone-for-id "UTC")))
     (next-date now cron))))
