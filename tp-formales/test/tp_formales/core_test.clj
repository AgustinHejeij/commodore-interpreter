(ns tp-formales.core-test
  (:require [clojure.test :refer :all]
            [tp-formales.core :refer :all]
            [tp-formales.main :refer :all]))

(deftest operador?-test
  (testing "Casos validos"
    (is (= true (operador? '+)))
    (is (= true (operador? '-)))
    (is (= true (operador? '*)))
    (is (= true (operador? '/)))
    (is (= true (operador? '=)))
    (is (= true (operador? '<>)))
    (is (= true (operador? '<)))
    (is (= true (operador? '<=)))
    (is (= true (operador? '>)))
    (is (= true (operador? '>=)))
    (is (= true (operador? 'AND)))
    (is (= true (operador? 'OR)))
    (is (= true (operador? (symbol "+"))))
  )
  
  (testing "Casos invalidos"
    (is (= false (operador? 'XOR)))
    (is (= false (operador? 'foo)))
    (is (= false (operador? 123)))
    (is (= false (operador? (symbol "%"))))
  )
)

(deftest variable-float?-test
  (testing "Casos validos"
    (is (= true (variable-float? 'X)))
    (is (= true (variable-float? 'X1)))
    (is (= true (variable-float? 'XY)))
  )
  
  (testing "Casos invalidos"
    (is (= false (variable-float? 'X%)))
    (is (= false (variable-float? 'X1%)))
    (is (= false (variable-float? 'XY%)))
    (is (= false (variable-float? 'X$)))
    (is (= false (variable-float? 'X1$)))
    (is (= false (variable-float? 'XY$)))
    (is (= false (variable-float? 'X1.0$)))
    (is (= false (variable-float? 'XLET)))
    (is (= false (variable-float? (symbol "1X"))))
  )
)

(deftest variable-integer?-test
  (testing "Casos validos"
    (is (= true (variable-integer? 'X%)))
    (is (= true (variable-integer? 'X1%)))
    (is (= true (variable-integer? 'XY%)))
  )
  
  (testing "Casos invalidos"
    (is (= false (variable-integer? 'X)))
    (is (= false (variable-integer? 'X1)))
    (is (= false (variable-integer? 'XY)))
    (is (= false (variable-integer? 'X$)))
    (is (= false (variable-integer? 'X1$)))
    (is (= false (variable-integer? 'XY$)))
  )
)

(deftest variable-string?-test
  (testing "Casos validos"
    (is (= true (variable-string? 'X$)))
    (is (= true (variable-string? 'X1$)))
    (is (= true (variable-string? 'XY$)))
  )
  
  (testing "Casos invalidos"
    (is (= false (variable-string? 'X)))
    (is (= false (variable-string? 'X1)))
    (is (= false (variable-string? 'XY)))
    (is (= false (variable-string? 'X%)))
    (is (= false (variable-string? 'X1%)))
    (is (= false (variable-string? 'XY%)))
  )
)

(deftest variable-integer?-test
  (testing "Casos validos"
    (is (= true (variable-integer? 'X%)))
    (is (= true (variable-integer? 'X1%)))
    (is (= true (variable-integer? 'XY%)))
  )
  
  (testing "Casos invalidos"
    (is (= false (variable-integer? 'X)))
    (is (= false (variable-integer? 'X1)))
    (is (= false (variable-integer? 'XY)))
    (is (= false (variable-integer? 'X$)))
    (is (= false (variable-integer? 'X1$)))
    (is (= false (variable-integer? 'XY$)))
  )
)

(deftest expandir-nexts-test
  (testing "Test funcion expandir-nexts"
    (is (= '((PRINT 1) (NEXT A) (NEXT B)) (expandir-nexts (list '(PRINT 1) (list 'NEXT 'A (symbol ",") 'B)))))
    (is (= '((PRINT 1) (NEXT A) (NEXT B) (NEXT A) (NEXT B) (PRINT 2)) (expandir-nexts (list '(PRINT 1) (list 'NEXT 'A (symbol ",") 'B) '(NEXT A) '(NEXT B) '(PRINT 2)))))
    (is (= '((PRINT 1) (NEXT A) (NEXT B) (NEXT A) ) (expandir-nexts (list '(PRINT 1) (list 'NEXT 'A (symbol ",") 'B) '(NEXT A)))))
    (is (= '((PRINT 1) (PRINT 2) ) (expandir-nexts (list '(PRINT 1) '(PRINT 2)))))
    (is (= '((NEXT A) (NEXT B) ) (expandir-nexts (list (list 'NEXT 'A (symbol ",") 'B)))))
  )
)

(deftest contar-sentencias-test
  (testing "Test funcion contar-sentencias"
    (is (= 2 (contar-sentencias 10 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])))
    (is (= 1 (contar-sentencias 15 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])))
    (is (= 2 (contar-sentencias 20 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])))
    (is (= 2 (contar-sentencias 15 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1) (NEXT A)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])))
    (is (= 3 (contar-sentencias 15 [(list '(10 (PRINT X) (PRINT Y)) (list 15 '(X = X + 1) (list 'NEXT 'I (symbol ",") 'J)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])))
  )
)

(deftest palabra-reservada?-test
  (testing "Casos validos"
    (is (= true (palabra-reservada? 'ENV)))
    (is (= true (palabra-reservada? 'LOAD)))
    (is (= true (palabra-reservada? 'SAVE)))
    (is (= true (palabra-reservada? 'EXIT)))
    (is (= true (palabra-reservada? 'INPUT)))
    (is (= true (palabra-reservada? 'PRINT)))
    (is (= true (palabra-reservada? '?)))
    (is (= true (palabra-reservada? 'DATA)))
    (is (= true (palabra-reservada? 'READ)))
    (is (= true (palabra-reservada? 'REM)))
    (is (= true (palabra-reservada? 'RESTORE)))
    (is (= true (palabra-reservada? 'CLEAR)))
    (is (= true (palabra-reservada? 'LET)))
    (is (= true (palabra-reservada? 'LIST)))
    (is (= true (palabra-reservada? 'NEW)))
    (is (= true (palabra-reservada? 'END)))
    (is (= true (palabra-reservada? 'FOR)))
    (is (= true (palabra-reservada? 'GOSUB)))
    (is (= true (palabra-reservada? 'RETURN)))
    (is (= true (palabra-reservada? 'GOTO)))
    (is (= true (palabra-reservada? 'IF)))
    (is (= true (palabra-reservada? 'THEN)))
    (is (= true (palabra-reservada? 'ON)))
    (is (= true (palabra-reservada? 'ATN)))
    (is (= true (palabra-reservada? 'INT)))
    (is (= true (palabra-reservada? 'SIN)))
    (is (= true (palabra-reservada? 'EXP)))
    (is (= true (palabra-reservada? 'LOG)))
    (is (= true (palabra-reservada? 'LEN)))
    (is (= true (palabra-reservada? 'MID$)))
    (is (= true (palabra-reservada? 'ASC)))
    (is (= true (palabra-reservada? 'CHR$)))
    (is (= true (palabra-reservada? 'STR$)))
  )

  (testing "Casos invalidos"
    (is (= false (palabra-reservada? 'X)))
    (is (= false (palabra-reservada? 'X1)))
    (is (= false (palabra-reservada? 'XY)))
    (is (= false (palabra-reservada? 'X%)))
    (is (= false (palabra-reservada? 'X1%)))
    (is (= false (palabra-reservada? 'XY%)))
    (is (= false (palabra-reservada? 'X$)))
    (is (= false (palabra-reservada? 'X1$)))
    (is (= false (palabra-reservada? 'XY$)))
  )
)

(deftest anular-invalidos-test
  (testing "Test anular invalidos"
    (is (= '(IF X nil * Y < 12 THEN LET nil X = 0) (anular-invalidos '(IF X & * Y < 12 THEN LET ! X = 0))))
  )
)
    