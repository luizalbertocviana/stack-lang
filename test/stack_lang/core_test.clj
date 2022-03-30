(ns stack-lang.core-test
  (:require [clojure.test :as t]
            [stack-lang.core :as stk])
  (:import java.util.List))

(t/deftest is-pop-test
  (t/testing "is-pop accepts '<pop> symbol"
    (t/is (stk/is-pop '<pop>)))
  (t/testing "is-pop does not accept other symbols"
    (t/is (not (stk/is-pop 'pop>)))
    (t/is (not (stk/is-pop '<apple>)))))

(t/deftest is-variable-test
  (t/testing "is-variable accepts symbols like '!a and '!word"
    (t/is (stk/is-variable '!a))
    (t/is (stk/is-variable '!word)))
  (t/testing "is-variable does not accept symbols whose name does not start with !"
    (t/is (not (stk/is-variable 'a)))
    (t/is (not (stk/is-variable 'word!)))))

(t/deftest is-assign-operation-test
  (t/testing "is-assign-operation accepts symbols like '!a+ and '!word+"
    (t/is (stk/is-assign-operation '!a+))
    (t/is (stk/is-assign-operation '!word+)))
  (t/testing "is-assign-operation does not accept symbols whose name is not enclosed by ! and +"
    (t/is (not (stk/is-assign-operation '!a)))
    (t/is (not (stk/is-assign-operation 'word+)))))

(t/deftest assign->variable-test
  (t/testing "assign->variable transforms an assign symbol into the corresponding variable symbol"
    (t/is (= (stk/assign->variable '!a+) '!a))
    (t/is (= (stk/assign->variable '!word+) '!word))))

(t/deftest is-constant-test
  (t/testing "is-constant accepts strings and primitive values such as ints and floats"
    (t/is (stk/is-constant "a string"))
    (t/is (stk/is-constant 42))
    (t/is (stk/is-constant 3.14159)))
  (t/testing "is-constant does not accept data structures and symbols"
    (t/is (not (stk/is-constant {:a 12 :b 23})))
    (t/is (not (stk/is-constant [1 2 3])))
    (t/is (not (stk/is-constant 'symbol)))))

(t/deftest is-invoke-expr-test
  (t/testing "is-invoke-expr accepts sequences of symbols whose first symbol is 'invoke>"
    (t/is (stk/is-invoke-expr '(invoke> + 2)))
    (t/is (stk/is-invoke-expr '(invoke> * 3))))
  (t/testing "is-invoke-expr does not accept sequences of symbols not starting with '>invoke>"
    (t/is (not (stk/is-invoke-expr '(+ 2))))
    (t/is (not (stk/is-invoke-expr '(invoke + 2))))))

(t/deftest is-if-expr-test
  (t/testing "is-if-expr accepts sequences of symbols whose first symbol is 'if>"
    (t/is (stk/is-if-expr '(if> 1 !a+ 2 !b+ !a !b (invoke + 2))))
    (t/is (stk/is-if-expr '(if> !a else> !b))))
  (t/testing "is-if-expr does not accept sequences of symbols not starting with 'if>"
    (t/is (not (stk/is-if-expr '(else> !b))))
    (t/is (not (stk/is-if-expr '(if !a else> !b))))))

(t/deftest split-if-expr-test
  (t/testing "split-if-expr splits if instructions and else instructions correctly"
    (t/is (= (stk/split-if-expr '(if> 1 !a+ else> 2 !a+))
             {:if-instructions '(1 !a+)
              :else-instructions '(2 !a+)}))
    (t/is (= (stk/split-if-expr '(if> 1 !a+))
             {:if-instructions '(1 !a+)
              :else-instructions '()}))))

(t/deftest destruct-defn-expr-test
  (t/testing "destruct-defn-expr destructures a well formed defn expression correctly"
    (let [defn-expr '(defn> add [!a !b]
                       !a
                       !b
                       (invoke> + 2))]
      (t/is (= (stk/destruct-defn-expr defn-expr)
               {:fn-name 'add
                :arg-list ['!a '!b]
                :instructions '(!a !b (invoke> + 2))})))))

(t/deftest is-loop-expr-test
  (t/testing "is-loop-expr accepts sequences of symbols whose first symbol is 'loop>"
    (t/is (stk/is-loop-expr '(loop> !a 1 (invoke> + 2) !a+ 10 (invoke> = 2)))))
  (t/testing "is-loop-expr does not accept sequences of symbols not starting with 'loop>"
    (t/is (not (stk/is-loop-expr '(loop !a 1 (invoke> + 2) !a+ 10 (invoke> = 2)))))))

(t/deftest is-defn-expr-test
  (t/testing "is-defn-expr accepts sequences of symbols whose first symbol is 'defn>"
    (t/is (stk/is-defn-expr '(defn> a))))
  (t/testing  "is-defn-expr does not accept sequences of symbols not starting with 'defn>"
    (t/is (not (stk/is-defn-expr '(defn a))))))

(t/deftest is-call-expr-test
  (t/testing "is-call-expr accepts sequences of symbols whose first symbol is 'call>"
    (t/is (stk/is-call-expr '(call> a))))
  (t/testing  "is-call-expr does not accept sequences of symbols not starting with 'call>"
    (t/is (not (stk/is-call-expr '(call a))))))

(t/deftest is-env-new-expr-test
  (t/testing "is-env-new-expr accepts sequences of symbols whose first symbol is 'env-new>"
    (t/is (stk/is-env-new-expr '(env-new> a))))
  (t/testing  "is-env-new-expr does not accept sequences of symbols not starting with 'env-new>"
    (t/is (not (stk/is-env-new-expr '(env-new a))))))

(t/deftest is-env-push-expr-test
  (t/testing "is-env-push-expr accepts sequences of symbols whose first symbol is 'env-push>"
    (t/is (stk/is-env-push-expr '(env-push> a))))
  (t/testing  "is-env-push-expr does not accept sequences of symbols not starting with 'env-push>"
    (t/is (not (stk/is-env-push-expr '(env-push a))))))

(t/deftest is-env-pop-test
  (t/testing "is-env-pop accepts 'env-pop> symbol"
    (t/is (stk/is-env-pop 'env-pop>)))
  (t/testing  "is-env-pop does not accept other symbols"
    (t/is (not (stk/is-env-pop 'env-pop)))))

(t/deftest get-loop-instructions-test
  (t/testing "get-loop-instructions returns the nested instructions of a loop> instruction correctly"
    (t/is (= (stk/get-loop-instructions '(loop> !a 1 (invoke> + 2) !a+ 10 (invoke> = 2)))
             '(!a 1 (invoke> + 2) !a+ 10 (invoke> = 2))))))

(t/deftest empty-stack-evaluation-context-test
  (t/testing "empty-stack-evaluation-context always returns a map representing an empty evaluation context"
    (t/is (= (stk/empty-stack-evaluation-context *ns*)
             {:stack []
              :environments {:global {}}
              :environment-precedence '(:global)
              :functions {}
              :operations []
              :status :stack-lang.core/start
              :namespace *ns*}))))

(t/deftest resolve-var-test
  (t/testing "resolve-var retrieves the correct value of a var according to the precedence order of environments"
    (let [ctx {:environments {:global {'!a 1 '!b 2} 'other {'!a 10 '!b 20}}
               :environment-precedence '(other :global)}]
      (t/is (= 10 (stk/resolve-var ctx '!a)))
      (t/is (stk/var-set? ctx '!a))
      (t/is (= 20 (stk/resolve-var ctx '!b)))
      (t/is (= 1 (stk/resolve-var (update ctx :environment-precedence rest) '!a)))
      (t/is (= 2 (stk/resolve-var (update ctx :environment-precedence rest) '!b)))
      (t/is (nil? (stk/resolve-var ctx '!c)))
      (t/is (not (stk/var-set? ctx '!c))))))

(t/deftest bind-var-test
  (t/testing "bind-var correctly binds val to var considering the active environment"
    (let [ctx {:environments {:global {'!a 1 '!b 20} 'other {'!a 10 '!b 20}}
               :environment-precedence '(other :global)}]
      (t/is (= (stk/bind-var ctx '!b 25)
               {:environments {:global {'!a 1 '!b 20} 'other {'!a 10 '!b 25}}
                :environment-precedence '(other :global)}))
      (t/is (= (stk/bind-var ctx '!c 30)
               {:environments {:global {'!a 1 '!b 20} 'other {'!a 10 '!b 20 '!c 30}}
                :environment-precedence '(other :global)}))
      (t/is (= (stk/bind-var (update ctx :environment-precedence rest) '!a 5)
               {:environments {:global {'!a 5 '!b 20} 'other {'!a 10 '!b 20}}
                :environment-precedence '(:global)})))))

(t/deftest perform-operation-test
  (t/testing "perform-operation creates a finished context when there are no more operations to perform"
    (let [finished-ctx {:operations '()}]
      (t/is (= (stk/perform-operation finished-ctx)
               (assoc finished-ctx :status :stack-lang.core/finished)))
      (t/is (= (stk/perform-operation (assoc finished-ctx :status :stack-lang.core/running))
               (assoc finished-ctx :status :stack-lang.core/finished)))))
  (t/testing "perform-operation handles <pop> operations correctly"
    (let [ctx {:stack [1 2 3]
               :operations '(<pop>)}]
      (t/is (= (stk/perform-operation ctx)
               {:stack [1 2]
                :operations '()
                :status :stack-lang.core/running}))))
  (t/testing "perform-operation handles env-new> operations correctly"
    (let [ctx {:environments {:global {'!a 3}}
               :operations '((env-new> env-a))}]
      (t/is (= (stk/perform-operation ctx)
               {:environments {:global {'!a 3} 'env-a {}}
                :operations '()
                :status :stack-lang.core/running}))))
  (t/testing "perform-operation handles env-push> operations correctly"
    (let [ctx {:environment-precedence '(:global)
               :operations '((env-push> 'env-a))}]
      (t/is (= (stk/perform-operation ctx)
               {:environment-precedence '('env-a :global)
                :operations '()
                :status :stack-lang.core/running}))))
  (t/testing "perform-operation handles env-pop> operations correctly"
    (let [ctx {:environment-precedence '(env-a env-b env-c :global)
               :operations '(env-pop>)}]
      (t/is (= (stk/perform-operation ctx)
               {:environment-precedence '(env-b env-c :global)
                :operations '()
                :status :stack-lang.core/running}))))
  (t/testing "perform-operation handles defn> operations correctly"
    (let [ctx {:operations '((defn> f [!a] !a))
               :functions {}}
          next-ctx (stk/perform-operation ctx)]
      (t/is (= (-> next-ctx :operations count) 1))
      (t/is (stk/is-env-new-expr (-> next-ctx :operations first)))
      (t/is (contains? (:functions next-ctx) 'f))
      (t/is (= (get-in next-ctx [:functions 'f :arg-list]) ['!a]))
      (t/is (= (count (get-in next-ctx [:functions 'f :instructions])) 3))))
  (t/testing "perform-operation handles call> operations correctly"
    (let [ctx {:operations '((call> add))
               :functions {'add {:arg-list ['!a '!b]
                                 :instructions '(!a !b (invoke> + 2))}}}]
      (t/is (= (stk/perform-operation ctx)
               {:operations '(!a+ <pop> !b+ <pop> !a !b (invoke> + 2))
                :functions {'add {:arg-list ['!a '!b]
                                  :instructions '(!a !b (invoke> + 2))}}
                :status :stack-lang.core/running}))))
  (t/testing "perform-operation handles constants as operations correctly"
    (let [ctx {:stack [1 2 3]
               :operations '(42)}]
      (t/is (= (stk/perform-operation ctx)
               {:stack [1 2 3 42]
                :operations '()
                :status :stack-lang.core/running}))))
  (t/testing "perform-operation handles assign operations correctly"
    (let [ctx {:stack [1 2 3]
               :environments {:global {} 'other {}}
               :environment-precedence '(:global)
               :operations '(!c+)}]
      (t/is (= (stk/perform-operation ctx)
               {:stack [1 2 3]
                :environments {:global {'!c 3} 'other {}}
                :environment-precedence '(:global)
                :operations '()
                :status :stack-lang.core/running}))
      (t/is (= (stk/perform-operation (assoc ctx :environment-precedence '(other :global)))
               {:stack [1 2 3]
                :environments {:global {} 'other {'!c 3}}
                :environment-precedence '(other :global)
                :operations '()
                :status :stack-lang.core/running}))))
  (t/testing "perform-operation handles variables as operations correctly"
    (let [ctx {:stack [1 2 3]
               :environments {:global {'!a 10 '!b 20} 'other {'!b 30}}
               :environment-precedence '(:global)
               :operations '(!b)
               :status :stack-lang.core/running}]
      (t/is (= (stk/perform-operation ctx)
               {:stack [1 2 3 20]
                :environments {:global {'!a 10 '!b 20} 'other {'!b 30}}
                :environment-precedence '(:global)
                :operations '()
                :status :stack-lang.core/running}))
      (t/is (= (stk/perform-operation (assoc ctx :operations '(!c)))
               {:stack [1 2 3]
                :environments {:global {'!a 10 '!b 20} 'other {'!b 30}}
                :environment-precedence '(:global)
                :operations '()
                :status [:stack-lang.core/variable-not-found '!c]}))))
  (t/testing "perform-operation handles if> operations correctly"
    (let [ctx {:stack [1 2 3 true]
               :operations '((if> !a else> !b))
               :status :stack-lang.core/running}]
      (t/is (= (stk/perform-operation ctx)
               {:stack [1 2 3]
                :operations '(!a)
                :status :stack-lang.core/running}))
      (t/is (= (stk/perform-operation (assoc ctx :stack [1 2 3 false]))
               {:stack [1 2 3]
                :operations '(!b)
                :status :stack-lang.core/running}))
      (t/is (= (stk/perform-operation (-> ctx
                                          (assoc :stack [1 2 3 false])
                                          (assoc :operations '((if> !a) 10))))
               {:stack [1 2 3]
                :operations '(10)
                :status :stack-lang.core/running}))))
  (t/testing "perform-operation handles loop> operations correctly"
    (let [ctx {:stack [1 2 3 true]
               :operations '((loop> !a !b) !c)
               :status :stack-lang.core/running}]
      (t/is (= (stk/perform-operation ctx)
               {:stack [1 2 3]
                :operations '(!a !b (loop> !a !b) !c)
                :status :stack-lang.core/running}))
      (t/is (= (stk/perform-operation (assoc ctx :stack [1 2 3 false]))
               {:stack [1 2 3]
                :operations '(!c)
                :status :stack-lang.core/running}))))
  (t/testing "perform-operation handles invoke> operations correctly"
    (let [ctx {:stack [1 2 3]
               :operations '((invoke> + 2))
               :status :stack-lang.core/running
               :namespace *ns*}
          ctx-dynamic-arity {:stack [1 2 3]
                             :environments {:global {'!a 10 '!b 20 '!c 3}}
                             :environment-precedence '(:global)
                             :operations '((invoke> + !c))
                             :status :stack-lang.core/running
                             :namespace *ns*}]
      (t/is (= (stk/perform-operation ctx)
               {:stack [1 5]
                :operations '()
                :status :stack-lang.core/running
                :namespace *ns*}))
      (t/is (= (stk/perform-operation (update ctx :stack #(vec (drop 2 %))))
               {:stack [3]
                :operations '()
                :status [:stack-lang.core/insufficient-stack-elements-to-perform '(invoke> + 2)]
                :namespace *ns*}))
      (t/is (= (stk/perform-operation ctx-dynamic-arity)
               {:stack [6]
                :environments {:global {'!a 10 '!b 20 '!c 3}}
                :environment-precedence '(:global)
                :operations '()
                :status :stack-lang.core/running
                :namespace *ns*}))
      (t/is (= (stk/perform-operation (assoc ctx-dynamic-arity :operations '((invoke> + !d))))
               {:stack [1 2 3]
                :environments {:global {'!a 10 '!b 20 '!c 3}}
                :environment-precedence '(:global)
                :operations '()
                :status [:stack-lang.core/invalid-invoke-arity-argument nil '(invoke> + !d)]
                :namespace *ns*}))
      (t/is (= (stk/perform-operation (assoc ctx-dynamic-arity
                                             :operations '((invoke> + !d))
                                             :environments {:global{'!a 10 '!b 20 '!c 3 '!d "word"}}))
               {:stack [1 2 3]
                :environments {:global {'!a 10 '!b 20 '!c 3 '!d "word"}}
                :environment-precedence '(:global)
                :operations '()
                :status [:stack-lang.core/invalid-invoke-arity-argument "word" '(invoke> + !d)]
                :namespace *ns*}))))
  (t/testing "perform-operation properly signals invalid operations"
    (let [ctx {:stack [1 2 3]
               :environment {'!a 10 '!b 20}
               :operations '(invalid>)
               :status :stack-lang.core/running}]
      (t/is (= (stk/perform-operation ctx)
               {:stack [1 2 3]
                :environment {'!a 10 '!b 20}
                :operations '()
                :status [:stack-lang.core/invalid-operation 'invalid>]})))))

(defmacro extract-ex-info-fields [form]
  `(try
     ~form
     (catch clojure.lang.ExceptionInfo e#
       {:message (ex-message e#) :data (ex-data e#)})))

(t/deftest run-stack-evaluation-context-test
  (t/testing "run-stack-evaluation-context properly executes a context"
    (let [ctx {:stack []
               :environments {:global {'!a 10 '!b 20}}
               :environment-precedence '(:global)
               :operations '(!a !b (invoke> + 2))
               :status :stack-lang.core/start
               :namespace *ns*}]
      (t/is (= (stk/run-stack-evaluation-context ctx)
               30))
      (t/is (= (extract-ex-info-fields
                (stk/run-stack-evaluation-context (assoc ctx :operations
                                                         '(!a !c (invoke> + 2)))))
               {:message "error evaluating stackfn function"
                :data {:reason [:stack-lang.core/variable-not-found '!c]}}))
      (t/is (= (extract-ex-info-fields
                (stk/run-stack-evaluation-context (assoc ctx :operations
                                                         '(!a !b invalid>))))
               {:message "error evaluating stackfn function"
                :data {:reason [:stack-lang.core/invalid-operation 'invalid>]}})))))

(def test-fn
  (stk/stackfn [!i]
               1
               2
               3
               (java-invoke> List/of 3)
               !a+
               <pop>
               !i
               !a
               (java-invoke> .get 2)))

(def test-fn-results
  (into {} (map #(vector % (test-fn %)) [0 1 2])))

(t/deftest stackfn-test
  (t/testing "stackfn is able to properly declare an anonymous stack function"
    (let [factorial (fn [n]
                      (loop [n n acc 1]
                        (case n
                          (0 1) acc
                          (recur (dec n) (* acc n)))))
          stk-factorial (stk/stackfn [!n]
                                     !n
                                     !acc+
                                     <pop>
                                     !n
                                     1
                                     (invoke> not= 2)
                                     (loop>
                                      !n
                                      (invoke> dec 1)
                                      !n+
                                      <pop>
                                      !acc
                                      !n
                                      (invoke> * 2)
                                      !acc+
                                      <pop>
                                      !n
                                      1
                                      (invoke> not= 2))
                                     !acc)]
      (t/is (= (factorial 5) (stk-factorial 5)))
      (t/is (= (factorial 10) (stk-factorial 10)))))
  (t/testing "stackfn is able to properly declare an anonymous stack function which calls java methods"
    (t/is (= 3 (test-fn-results 0)))
    (t/is (= 2 (test-fn-results 1)))
    (t/is (= 1 (test-fn-results 2))))
  (t/testing "stackfn is able to properly declare an anonymous stack function containing functions declarations"
    (let [stk-fn (stk/stackfn [!a !b]
                              (defn> add [!x !y] !x !y (invoke> + 2))
                              !a
                              !b
                              (call> add))]
      (t/is (= 10 (stk-fn 3 7)))))
  (t/testing "stackfn is able to properly declare an anonymous stack function containing closure declarations"
    (let [stk-fn (stk/stackfn []
                              1
                              2
                              3
                              (defn> accumulator [!action !x]
                                !action
                                "reset"
                                (invoke> = 2)
                                (if> !x !acc+ <pop>)
                                !action
                                "acc"
                                (invoke> = 2)
                                (if> !acc !x (invoke> + 2) !acc+ <pop>)
                                !action
                                "get"
                                (invoke> = 2)
                                (if> !acc))
                              0
                              "reset"
                              (call> accumulator)
                              "acc"
                              (call> accumulator)
                              "acc"
                              (call> accumulator)
                              "acc"
                              (call> accumulator)
                              0
                              "get"
                              (call> accumulator))]
      (t/is (= (stk-fn) 6)))))

(def defstackfn-form
  '(stk/defstackfn addition [!a !b] !a !b (invoke> + 2)))

(def expected-expanded-form
  '(def addition (stack-lang.core/stackfn [!a !b] !a !b (invoke> + 2))))

(def expanded-form
  (macroexpand defstackfn-form))

(t/deftest defstackfn-test
  (t/testing "defstackfn expands to a proper def whose binding value is a stackfn macro call"
    (t/is (= expanded-form expected-expanded-form))))
