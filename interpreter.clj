(ns interpreter
  (:require [default-syntax :as s]))

(defn -dispatch [exp]
  (cond (s/self-evaluating? exp) :to-self
        (s/variable? exp) :variable
        (list? exp) (s/clause-type exp)
        :else (throw (Exception. (str "Unknown form " exp)))))

(defmulti eval-exp (fn [exp env] (-dispatch exp)))

(defn eval [exp env]
  (eval-exp exp env))

(defmethod eval-exp ::to-self [exp env]
  exp)

(defmethod eval-exp ::variable [exp env]
  (get env exp))

(defmethod eval-exp ::quotation [exp env]
  (s/text-of-quotation exp))

(defmethod eval-exp ::assignment [exp env]
  ())

(defmethod eval-exp ::definition [exp env]
  ())

(defmethod eval-exp ::if [exp env]
  ())

(defmethod eval-exp ::procedure [exp env]
  (s/make-procedure (s/procedure-params exp) (s/procedure-body exp) env))

(defmethod eval-exp ::actions [exp env]
  (eva))

(defmethod eval-exp ::cond [exp env]
  ())

(defmethod eval-exp :application [exp env]
  (apply (eval (operator exp) env)
         (list-of-values-left-eval (operands exp) env)))
