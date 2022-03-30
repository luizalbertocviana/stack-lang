(ns stack-lang.core)

(defn is-pop [sybl]
  (and (symbol? sybl)
       (-> sybl
           name
           (= "<pop>"))))

(defn is-env-pop [sybl]
  (and (symbol? sybl)
       (-> sybl
           name
           (= "env-pop>"))))

(defn is-variable [sybl]
  (and (symbol? sybl)
       (-> sybl
           name
           first
           (= \!))))

(defn is-assign-operation [sybl]
  (and (is-variable sybl)
       (-> sybl
           name
           last
           (= \+))))

(defn assign->variable [sybl]
  (->> sybl
       name
       butlast
       (apply str)
       symbol))

(defn variable->assign [sybl]
  (-> sybl
      name
      (str \+)
      symbol))

(defn is-constant [expr]
  (not (or (coll? expr)
           (symbol? expr)
           (keyword? expr)))) 

(defn expr-begins-with [sybl]
  (fn [expr]
    (and (seq? expr)
         (-> expr
             first
             (= sybl)))))

(def is-invoke-expr      (expr-begins-with 'invoke>))
(def is-java-invoke-expr (expr-begins-with 'java-invoke>))
(def is-if-expr          (expr-begins-with 'if>))
(def is-loop-expr        (expr-begins-with 'loop>))
(def is-env-new-expr     (expr-begins-with 'env-new>))
(def is-env-push-expr    (expr-begins-with 'env-push>))
(def is-defn-expr        (expr-begins-with 'defn>))
(def is-call-expr        (expr-begins-with 'call>))

(defn destruct-defn-expr [defn-expr]
  {:fn-name      (nth defn-expr 1)
   :arg-list     (nth defn-expr 2)
   :instructions (drop 3 defn-expr)})

(defn split-if-expr [if-expr]
  (let [[if-branch else-branch]
        (split-with #(not= 'else> %) if-expr)]
    {:if-instructions (rest if-branch)
     :else-instructions (rest else-branch)}))

(defn get-loop-instructions [loop-expr]
  (rest loop-expr))

(defn empty-stack-evaluation-context [base-namespace]
  {:stack []
   :environments {:global {}}
   :environment-precedence '(:global)
   :functions {}
   :operations []
   :status ::start
   :namespace base-namespace})

(defn resolve-var [stk-ctx var]
  (let [envs (map (:environments stk-ctx) (:environment-precedence stk-ctx))
        vals (->> envs
                  (map #(get % var))
                  (filter #(some? %)))]
    (first vals)))

(defn var-set? [stk-ctx var]
  (let [envs (map (:environments stk-ctx) (:environment-precedence stk-ctx))
        vals (->> envs
                  (map #(get % var))
                  (filter #(some? %)))]
    (not-empty vals)))

(defn bind-var [stk-ctx var val]
  (let [active-env (-> stk-ctx :environment-precedence first)]
    (update stk-ctx :environments
            (fn [envs]
              (update envs active-env #(assoc % var val))))))

(defn java-fn [mthd n]
  (let [arg-symbols (map (comp symbol str)
                         (repeat \n)
                         (range 1 (inc n)))]
    (eval `(fn [args#]
             (apply (fn [~@arg-symbols] (~mthd ~@arg-symbols)) args#)))))

(defn perform-operation [stk-ctx]
  (if (empty? (:operations stk-ctx))
    (assoc stk-ctx :status ::finished)
    (let [operation (-> stk-ctx :operations first)
          next-stk-ctx (-> stk-ctx
                           (update :operations rest)
                           (assoc :status ::running))]
      (letfn [(push [value]
                (update next-stk-ctx :stack conj value))
              (tip []
                (-> next-stk-ctx :stack last))]
        (cond
          (is-pop operation)
          (update next-stk-ctx :stack pop)

          (is-env-new-expr operation)
          (let [env-key (nth operation 1)]
            (update next-stk-ctx :environments #(assoc % env-key {})))

          (is-env-push-expr operation)
          (let [env-key (nth operation 1)]
            (update next-stk-ctx :environment-precedence #(conj % env-key)))

          (is-env-pop operation)
          (update next-stk-ctx :environment-precedence rest)

          (is-defn-expr operation)
          (let [{:keys [fn-name arg-list instructions]} (destruct-defn-expr operation)
                fn-env-key (gensym)]
            (-> next-stk-ctx
                (update :operations
                        #(conj % `(~(symbol "env-new>") ~fn-env-key)))
                (update :functions
                        #(assoc % fn-name {:arg-list arg-list
                                           :instructions (concat `((~(symbol "env-push>") ~fn-env-key))
                                                                 instructions
                                                                 `(~(symbol "env-pop>")))}))))

          (is-call-expr operation)
          (let [fn-name (nth operation 1)]
            (if (contains? (:functions next-stk-ctx) fn-name)
              (let [{:keys [arg-list instructions]} (get-in next-stk-ctx [:functions fn-name])
                    assign-list (map variable->assign arg-list)
                    arg-bind-operations (concat (interpose '<pop> assign-list) '(<pop>))
                    instructions (concat arg-bind-operations instructions)]
                (update next-stk-ctx :operations #(concat instructions %)))
              (assoc next-stk-ctx :status [::undefined-function fn-name operation])))

          (is-constant operation)
          (push operation)

          (is-assign-operation operation)
          (bind-var next-stk-ctx (assign->variable operation) (tip))

          (is-variable operation)
          (if (var-set? next-stk-ctx operation)
            (let [variable-val
                  (resolve-var next-stk-ctx operation)]
              (push variable-val))
            (assoc next-stk-ctx :status [::variable-not-found operation]))

          (is-if-expr operation)
          (let [{:keys [if-instructions else-instructions]} (split-if-expr operation)
                instructions (if (tip) if-instructions else-instructions)]
            (-> next-stk-ctx
                (update :operations #(concat instructions %))
                (update :stack pop)))

          (is-loop-expr operation)
          (let [loop-instructions (get-loop-instructions operation)
                instructions (if (tip)
                               (concat loop-instructions [operation])
                               [])]
            (-> next-stk-ctx
                (update :operations #(concat instructions %))
                (update :stack pop)))

          (is-invoke-expr operation)
          (let [function (nth operation 1)
                num-args-source (nth operation 2)
                num-args (if (int? num-args-source)
                           num-args-source
                           (resolve-var next-stk-ctx num-args-source))]
            (if (int? num-args)
              (let [stack (:stack next-stk-ctx)
                    enough-stack-elements (<= num-args (count stack))]
                (if enough-stack-elements
                  (let [transient-stack (->> stack
                                             (drop-last num-args)
                                             vec)
                        args (->> stack
                                  (take-last num-args)
                                  reverse)
                        result (apply (ns-resolve (:namespace next-stk-ctx) function) args)
                        new-stack (conj transient-stack result)]
                    (assoc next-stk-ctx :stack new-stack))
                  (assoc next-stk-ctx :status [::insufficient-stack-elements-to-perform operation])))
              (assoc next-stk-ctx :status [::invalid-invoke-arity-argument num-args operation])))

          (is-java-invoke-expr operation)
          (let [method (nth operation 1)
                num-args (nth operation 2)
                stack (:stack next-stk-ctx)
                enough-stack-elements (<= num-args (count stack))]
            (if enough-stack-elements
              (let [transient-stack (->> stack
                                         (drop-last num-args)
                                         vec)
                    args (->> stack
                              (take-last num-args)
                              reverse)
                    function (java-fn method num-args)
                    result (function args)
                    new-stack (conj transient-stack result)]
                (assoc next-stk-ctx :stack new-stack))
              (assoc next-stk-ctx :status [::insufficient-stack-elements-to-perform operation])))

          :else
          (assoc next-stk-ctx :status [::invalid-operation operation]))))))

(defn run-stack-evaluation-context [stk-ctx]
  (loop [ctx stk-ctx]
    (case (:status ctx)
      (::start ::running) (recur (perform-operation ctx))
      ::finished (-> ctx :stack last)
      (throw (ex-info "error evaluating stackfn function"
                      {:reason (:status ctx)})))))

(defmacro stackfn [args & body]
  (if (every? is-variable args)
    `(fn ~args
       (let [ctx#
             (-> (empty-stack-evaluation-context ,*ns*)
                 (assoc :operations '~body)
                 ~@(loop [binding-list nil
                          remaining-args args]
                     (if (empty? remaining-args)
                       binding-list
                       (let [arg (first remaining-args)]
                         (recur (conj binding-list
                                      `(assoc-in [:environments :global '~arg] ~arg))
                                (rest remaining-args))))))]
         (run-stack-evaluation-context ctx#)))
    (throw (Exception. "argument names should start with !"))))

(defmacro defstackfn [name args & body]
  `(def ~name (stackfn ~args ~@body)))
