(ns default-syntax)

(defn self-evaluating? [exp]
  (or (number? exp)
      (string? exp)))

(defn variable? [exp]
  (symbol? exp))

(def special-clauses {
                    :quote :quotation
                    :set! :assignment
                    :define :definition
                    :if :if
                    :lambda :procedure
                    :begin :actions
                    :cond :cond
                    })

(defn clause-type [exp]
  (if (empty? exp)
    (throw (Exception. (str "Unquoted empty list " exp)))
    (get special-clauses (first exp) :application)))

(defn text-of-quotation [exp]
  (second exp))





(defn operator [exp]
  (first exp))

(defn operands [exp]
  (next exp))


(defn no-operands? [ops] (empty? ops))
(defn first-operand [ops] (first ops))
(defn rest-operands [ops] (next ops))
