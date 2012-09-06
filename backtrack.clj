(defn lookup 
  "Lookup a variable in the model.  Respects inversion of the variable if it is
  specified with a - symbol.  Returns nil if the variable is unset."
  [v model]

  (if (= (get v 0) \-)                   ; If the first character is '-'
    (let [value (get model (subs v 1))]  ; let value equal the value of the var
      (if (nil? value)                   ; if the variable is unnassigned 
        nil                              ; return nil 
        (not value)))                    ; otherwise return not value
    (get model v)))

(def foobar {"A" false, "B" true})

(println "should return false:" (lookup "A" foobar))
(println "should return false:" (lookup "-B" foobar))
(println "should return true:" (lookup "B" foobar))
(println "should return nil:" (lookup "-C" foobar))

(defn satisfiable? 
  "Checks to see if a given clause is satisfiable given a model."
  [clause, model]

  (cond 
    ; If every variable is exactly false, the clause is false.
    (every? (fn [variable] (false? (lookup variable model))) clause)
    false

    ; If some variable is exactly true, the clause is true.
    (some (fn [variable] (true? (lookup variable model))) clause)
    true))

(def barbaz '("A" "-B"))
(def buzfuz '("-A" "-B"))
(def sprangadang '("A" "-B" "C"))

(println "should return false:" (satisfiable? barbaz foobar))
(println "should return true:" (satisfiable? buzfuz foobar))
(println "should return nil:" (satisfiable? sprangadang foobar))

(defn solve 
  "Solves some set of clauses recursively, given some model and some list of
  variables present in the clauses."
  [variables, clauses, model]

  (cond
    ; If some clause can't be satisfied, the set is not satisfiable given this
    ; model.
    (some (fn [clause] (false? (satisfiable? clause model))) clauses) false

    ; If every clause is satisfiable, the set of clauses is satisfied by the
    ; current model, so we should return it.
    (every? (fn [clause] (satisfiable? clause model)) clauses) model

    true ; so that the last form is chosen if the others were not satisfied

    ; Choose the first variable that is not yet defined ..
    (if-let [choice (first (filter (fn [v] (= nil (get model v))) variables))]

      ; And try setting it to true; if that doesn't work, try false.
      (or
        (solve variables clauses (assoc model choice true))
        (solve variables clauses (assoc model choice false)))

      false)))


(def rock '(("A" "-B") ("-A" "-B")))
(def paper '(("A" "-B") ("-A" "-B") ("A" "B" "C")))
(def scissors '(("-A" "B") ("-B") ("A")))

(println "should return a model:" (solve '("A" "B") rock {}))
(println "should return a model:" (solve '("A" "B" "C") paper {}))
(println "should return false:" (solve '("A" "B") scissors {}))
