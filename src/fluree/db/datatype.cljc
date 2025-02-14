(ns fluree.db.datatype
  (:require [fluree.db.constants :as const]
            [fluree.db.util.log :as log]
            [clojure.string :as str]
            #?(:clj  [fluree.db.util.clj-const :as uc]
               :cljs [fluree.db.util.cljs-const :as uc]))
  #?(:clj (:import (java.time OffsetDateTime OffsetTime LocalDate LocalTime
                              LocalDateTime ZoneOffset)
                   (java.time.format DateTimeFormatter)
                   (java.math BigDecimal))))

#?(:clj (set! *warn-on-reflection* true))

(def default-data-types
  {"http://www.w3.org/2001/XMLSchema#anyURI"             const/$xsd:anyURI
   "http://www.w3.org/2001/XMLSchema#string"             const/$xsd:string
   "http://www.w3.org/2001/XMLSchema#boolean"            const/$xsd:boolean
   "http://www.w3.org/2001/XMLSchema#date"               const/$xsd:date
   "http://www.w3.org/2001/XMLSchema#dateTime"           const/$xsd:dateTime
   "http://www.w3.org/2001/XMLSchema#decimal"            const/$xsd:decimal
   "http://www.w3.org/2001/XMLSchema#double"             const/$xsd:double
   "http://www.w3.org/2001/XMLSchema#integer"            const/$xsd:integer
   "http://www.w3.org/2001/XMLSchema#long"               const/$xsd:long
   "http://www.w3.org/2001/XMLSchema#int"                const/$xsd:int
   "http://www.w3.org/2001/XMLSchema#short"              const/$xsd:short
   "http://www.w3.org/2001/XMLSchema#float"              const/$xsd:float
   "http://www.w3.org/2001/XMLSchema#unsignedLong"       const/$xsd:unsignedLong
   "http://www.w3.org/2001/XMLSchema#unsignedInt"        const/$xsd:unsignedInt
   "http://www.w3.org/2001/XMLSchema#unsignedShort"      const/$xsd:unsignedShort
   "http://www.w3.org/2001/XMLSchema#positiveInteger"    const/$xsd:positiveInteger
   "http://www.w3.org/2001/XMLSchema#nonPositiveInteger" const/$xsd:nonPositiveInteger
   "http://www.w3.org/2001/XMLSchema#negativeInteger"    const/$xsd:negativeInteger
   "http://www.w3.org/2001/XMLSchema#nonNegativeInteger" const/$xsd:nonNegativeInteger
   "http://www.w3.org/2001/XMLSchema#duration"           const/$xsd:duration
   "http://www.w3.org/2001/XMLSchema#gDay"               const/$xsd:gDay
   "http://www.w3.org/2001/XMLSchema#gMonth"             const/$xsd:gMonth
   "http://www.w3.org/2001/XMLSchema#gMonthDay"          const/$xsd:gMonthDay
   "http://www.w3.org/2001/XMLSchema#gYear"              const/$xsd:gYear
   "http://www.w3.org/2001/XMLSchema#gYearMonth"         const/$xsd:gYearMonth
   "http://www.w3.org/2001/XMLSchema#time"               const/$xsd:time
   "http://www.w3.org/2001/XMLSchema#normalizedString"   const/$xsd:normalizedString
   "http://www.w3.org/2001/XMLSchema#token"              const/$xsd:token
   "http://www.w3.org/2001/XMLSchema#language"           const/$xsd:language
   "http://www.w3.org/2001/XMLSchema#byte"               const/$xsd:byte
   "http://www.w3.org/2001/XMLSchema#unsignedByte"       const/$xsd:unsignedByte
   "http://www.w3.org/2001/XMLSchema#hexBinary"          const/$xsd:hexBinary
   "http://www.w3.org/2001/XMLSchema#base64Binary"       const/$xsd:base64Binary})

(def iso8601-date-component-pattern
  "This is slightly more forgiving than the xsd:date spec:
  http://books.xmlschemata.org/relaxng/ch19-77041.html
  Note there is no need to be extra strict with the numeric ranges in here as
  the java.time constructors will take care of that for us."
  "((?:-)?[0-9]{4})-([0-9]{1,2})-([0-9]{1,2})")

(def iso8601-date-pattern
  "Defines the pattern for dates w/o times where an offset is still allowed on
  the end."
  (str iso8601-date-component-pattern "(Z|(?:[+-][0-9]{1,2}:[0-9]{2}))?"))

(def iso8601-date-re
  (re-pattern iso8601-date-pattern))

(def iso8601-time-pattern
  "This is slightly more forgiving than the xsd:time spec:
  http://books.xmlschemata.org/relaxng/ch19-77311.html
  Note there is no need to be extra strict with the numeric ranges in here as
  the java.time constructors will take care of that for us."
  "([0-9]{1,2}):([0-9]{1,2}):([0-9]{1,2})(Z|(?:[+-][0-9]{1,2}:[0-9]{2}))?")

(def iso8601-time-re
  (re-pattern iso8601-time-pattern))

(def iso8601-datetime-pattern
  "This is slightly more forgiving than the xsd:dateTime spec:
  http://books.xmlschemata.org/relaxng/ch19-77049.html

  Note there is no need to be extra strict with the numeric ranges in here as
  the java.time constructors will take care of that for us."
  (str iso8601-date-component-pattern "T" iso8601-time-pattern))

(def iso8601-datetime-re
  (re-pattern iso8601-datetime-pattern))

(defn infer
  "Infers a default data type if not otherwise provided."
  [x]
  (cond
    (string? x) const/$xsd:string
    (integer? x) const/$xsd:integer
    (number? x) const/$xsd:decimal
    (boolean? x) const/$xsd:boolean))


#?(:cljs
   (defn- left-pad
     [s pad len]
     (let [diff (- len (count s))]
       (if (< 0 diff)
         (-> diff
             (repeat pad)
             (concat s)
             (->> (apply str)))
         s))))

(defn- parse-iso8601-date
  "Parses string s into one of the following if it can (returns nil o/w):
  - JVM: either a java.time.OffsetDateTime (with time set to
         midnight) or a java.time.LocalDate if no timezone offset is present.
  - JS: a Javascript Date object with time set to midnight. NB: If you don't
        supply a timezone JS will assume it's in your current, local timezone
        according to your device."
  [s]
  (when-let [matches (re-matches iso8601-date-re s)]
    (let [date   (-> matches rest butlast)
          offset (last matches)
          [year month day] (map #?(:clj  #(Integer/parseInt %)
                                   :cljs #(left-pad % "0" 2))
                                date)]
      #?(:clj  (if offset
                 (OffsetDateTime/of year month day 0 0 0 0
                                    (ZoneOffset/of ^String offset))
                 (LocalDate/of ^int year ^int month ^int day))
         :cljs (js/Date. (str year "-" month "-" day "T00:00:00" offset))))))

(defn- parse-iso8601-time
  "Parses string s into one of the following if it can (returns nil o/w):
  - JVM: either a java.time.OffsetTime of a java.time.LocalTime
         if no timezone offset is present.
  - JS: a Javascript Date object with the date values set to January 1, 1970.
        NB: If you don't supply a timezone JS will assume it's in your current,
        local timezone according to your device."
  [s]
  (when-let [matches (re-matches iso8601-time-re s)]
    (let [time   (->> matches rest butlast)
          offset (last matches)
          [hour min sec] (map #?(:clj  #(Integer/parseInt %)
                                 :cljs #(left-pad % "0" 2))
                              time)]
      #?(:clj  (if offset
                 (OffsetTime/of hour min sec 0 (ZoneOffset/of ^String offset))
                 (LocalTime/of hour min sec))
         :cljs (js/Date. (str "1970-01-01T" hour ":" min ":" sec offset))))))

(defn- parse-iso8601-datetime
  "Parses string s into one of the following:
  - JVM: either a java.time.OffsetDateTime or a java.time.LocalDateTime if no
         timezone offset if present.
  - JS: a Javascript Date object. NB: If you don't supply a timezone JS will
        assume it's in your current, local timezone according to your device."
  [s]
  (when-let [matches (re-matches iso8601-datetime-re s)]
    (let [datetime (->> matches rest (take 6))
          offset   (last matches)
          [year month day hour min sec] (map #?(:clj  #(Integer/parseInt %)
                                                :cljs #(left-pad % "0" 2))
                                             datetime)]
      #?(:clj  (if offset
                 (OffsetDateTime/of year month day hour min sec 0
                                    (ZoneOffset/of ^String offset))
                 (LocalDateTime/of ^int year ^int month ^int day ^int hour
                                   ^int min ^int sec))
         :cljs (js/Date. (str year "-" month "-" day "T" hour ":" min ":" sec
                              offset))))))

(defn- coerce-boolean
  [value]
  (cond
    (boolean? value)
    value

    (string? value)
    (cond
      (= "true" (str/lower-case value))
      true

      (= "false" (str/lower-case value))
      false

      :else
      nil)))

(defn- coerce-decimal
  [value]
  (cond
    (string? value)
    #?(:clj  (try (BigDecimal. ^String value) (catch Exception _ nil))
       :cljs (let [n (js/parseFloat value)] (if (js/Number.isNaN n) nil n)))

    (integer? value)
    #?(:clj  (BigDecimal. ^int value)
       :cljs value)

    (float? value)
    ;; convert to string first to keep float precision explosion at bay
    #?(:clj  (BigDecimal. ^String (Float/toString value))
       :cljs value)

    :else nil))

(defn- coerce-double
  [value]
  (cond
    (string? value)
    (case value
      "INF" #?(:clj  Double/POSITIVE_INFINITY
               :cljs js/Number.POSITIVE_INFINITY)
      "-INF" #?(:clj  Double/NEGATIVE_INFINITY
                :cljs js/Number.NEGATIVE_INFINITY)
      #?(:clj  (try (Double/parseDouble value) (catch Exception _ nil))
         :cljs (let [n (js/parseFloat value)] (if (js/Number.isNaN n) nil n))))

    (float? value)
    value

    (integer? value)
    #?(:clj  (Double/parseDouble (str value ".0"))
       :cljs value)

    :else nil))

(defn- coerce-float
  [value]
  (cond
    (string? value)
    (case value
      "INF" #?(:clj  Float/POSITIVE_INFINITY
               :cljs js/Number.POSITIVE_INFINITY)
      "-INF" #?(:clj  Float/NEGATIVE_INFINITY
                :cljs js/Number.NEGATIVE_INFINITY)
      #?(:clj  (try (Float/parseFloat value) (catch Exception _ nil))
         :cljs (let [n (js/parseFloat value)] (if (js/Number.isNaN n) nil n))))

    (float? value)
    value

    (integer? value)
    #?(:clj  (Float/parseFloat (str value ".0"))
       :cljs value)

    :else nil))

(defn- coerce-int-fn
  "Returns a fn for coercing int-like values (e.g. short, long) from strings and
  integers. Arguments are CLJ-only parse-str and cast-num fns (CLJS is always
  the same because in JS it's all just Numbers)."
  [parse-str cast-num]
  (fn [value]
    (cond
      (string? value)
      #?(:clj  (try (parse-str value) (catch Exception _ nil))
         :cljs (when-not (str/includes? value ".")
                 (let [n (js/parseInt value)] (if (js/Number.isNaN n) nil n))))

      (integer? value)
      #?(:clj (try (cast-num value) (catch Exception _ nil)) :cljs value)

      :else nil)))

(defn- coerce-integer
  [value]
  (let [coerce-fn (coerce-int-fn #?(:clj #(Integer/parseInt %) :cljs nil)
                                 #?(:clj int :cljs nil))]
    (coerce-fn value)))

(defn- coerce-long
  [value]
  (let [coerce-fn (coerce-int-fn #?(:clj #(Long/parseLong %) :cljs nil)
                                 #?(:clj long :cljs nil))]
    (coerce-fn value)))

(defn- coerce-short
  [value]
  (let [coerce-fn (coerce-int-fn #?(:clj #(Short/parseShort %) :cljs nil)
                                 #?(:clj short :cljs nil))]
    (coerce-fn value)))

(defn- coerce-byte
  [value]
  (let [coerce-fn (coerce-int-fn #?(:clj #(Byte/parseByte %) :cljs nil)
                                 #?(:clj byte :cljs nil))]
    (coerce-fn value)))

(defn- coerce-normalized-string
  [value]
  (when (string? value)
    (str/replace value #"\s" " ")))

(defn- coerce-token
  [value]
  (when (string? value)
    (-> value
        (str/replace #"\s+" " ")
        str/trim)))

(defn- check-signed
  "Returns nil if required-type and n conflict in terms of signedness
  (e.g. unsignedInt but n is negative, nonPositiveInteger but n is greater than
  zero). Returns n otherwise."
  [n required-type]
  (when (number? n) ; these are all integer types, but this fn shouldn't care
    (uc/case (int required-type)
      const/$xsd:positiveInteger
      (if (>= 0 n) nil n)

      (const/$xsd:nonNegativeInteger const/$xsd:unsignedInt
       const/$xsd:unsignedLong const/$xsd:unsignedByte const/$xsd:unsignedShort)
      (if (> 0 n) nil n)

      const/$xsd:negativeInteger
      (if (<= 0 n) nil n)

      const/$xsd:nonPositiveInteger
      (if (< 0 n) nil n)

      ;; else
      n)))

(defn coerce
  "Given a value and required type, attempts to return a coerced value or nil (not coercible).
  We should be cautious about what we coerce, it is really a judgement decision in some
  circumstances. While we could coerce, e.g. numbers to strings, an exception is likely the most ideal behavior.
  Examples of things that seem OK to coerce are:
   - a string type to a date and/or time, assuming it meets the formatting
   - numbers in strings
   - the strings 'true' or 'false' to a boolean"
  [value required-type]
  (log/debug "coerce value:" value "to type:" required-type)
  (uc/case (int required-type)
    const/$xsd:string
    (when (string? value)
      value)

    const/$xsd:anyURI
    (when (string? value)
      value)

    const/$xsd:boolean
    (coerce-boolean value)

    const/$xsd:date
    (when (string? value)
      (parse-iso8601-date value))

    const/$xsd:dateTime
    (when (string? value)
      (parse-iso8601-datetime value))

    const/$xsd:time
    (when (string? value)
      (parse-iso8601-time value))

    const/$xsd:decimal
    (coerce-decimal value)

    const/$xsd:double
    (coerce-double value)

    const/$xsd:float
    (coerce-float value)

    (const/$xsd:integer const/$xsd:int const/$xsd:unsignedInt
     const/$xsd:nonPositiveInteger const/$xsd:positiveInteger
     const/$xsd:nonNegativeInteger const/$xsd:negativeInteger)
    (-> value coerce-integer (check-signed required-type))

    (const/$xsd:long const/$xsd:unsignedLong)
    (-> value coerce-long (check-signed required-type))

    (const/$xsd:short const/$xsd:unsignedShort)
    (-> value coerce-short (check-signed required-type))

    (const/$xsd:byte const/$xsd:unsignedByte)
    (-> value coerce-byte (check-signed required-type))

    const/$xsd:normalizedString
    (coerce-normalized-string value)

    (const/$xsd:token const/$xsd:language)
    (coerce-token value)

    ;; else
    value))

(defn from-expanded
  "Returns a tuple of the value (possibly coerced from string) and the data type sid from
  an expanded json-ld value map. If type is defined but not a predefined data type, will
  return nil prompting downstream process to look up (or create) a custom data
  type. Value coercion is only attempted when a required-type is supplied."
  [{:keys [type value] :as _value-map} required-type]
  (let [type-id (if type
                  (get default-data-types type)
                  (infer value))
        _       (log/debug "from-expanded type:" type-id)
        value*  (coerce value type-id)]
    (cond (and required-type (not= type-id required-type))
          (throw (ex-info (str "Required data type " required-type
                               " does not match provided data type: " type ".")
                          {:status 400 :error :db/shacl-required}))

          (nil? value*)
          (throw (ex-info (str "Data type " type-id
                               " cannot be coerced from provided value: " value ".")
                          {:status 400 :error :db/shacl-value-coercion}))

          :else
          [value type-id])))
