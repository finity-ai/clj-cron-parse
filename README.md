# clj-cron-parse

A Clojure library for using cron expressions.

Current build status: [![Build Status](https://travis-ci.org/shmish111/clj-cron-parse.png)](https://travis-ci.org/shmish111/clj-cron-parse)

The library mainly consists of one function, `next-date`, which takes a cron expression and a from date and returns the
next `org.joda.time.DateTime` to occur for that cron expression.  From this function you should be able to do anything
you want using cron expressions.

## Usage

In [Leiningen](http://github.com/technomancy/leiningen/) add the dependency (http://clojars.org/clj-cron-parse/latest-version.svg)](http://clojars.org/clj-cron-parse)

```clojure
(require '[clj-cron-parse.core :refer [next-date]])
(def now (t/date-time 2015 01 01 12 00 00 000))

(def next-every-second
    (next-date now "1 * * * * *"))

(def next-every-year
    (next-date now "@yearly"))
```

## License

Copyright © 2015 David Smith

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
