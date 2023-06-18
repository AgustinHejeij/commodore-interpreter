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
    (is (= false (variable-float? "1X")))
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

(deftest aridad-test
  (testing "Test funcion aridad"
    (is (= 0 (aridad 'THEN)))
    (is (= 0 (aridad 'IF)))
    (is (= 1 (aridad 'SIN)))
    (is (= 1 (aridad 'ATN)))
    (is (= 1 (aridad 'INT)))
    (is (= 1 (aridad 'EXP)))
    (is (= 1 (aridad 'LOG)))
    (is (= 1 (aridad 'LEN)))
    (is (= 1 (aridad 'ASC)))
    (is (= 1 (aridad 'CHR$)))
    (is (= 1 (aridad 'STR$)))
    (is (= 1 (aridad '-u)))
    (is (= 2 (aridad '*)))
    (is (= 2 (aridad 'OR)))
    (is (= 2 (aridad 'AND)))
    (is (= 2 (aridad '+)))
    (is (= 2 (aridad '-)))
    (is (= 2 (aridad '/)))
    (is (= 2 (aridad '<)))
    (is (= 2 (aridad '>)))
    (is (= 2 (aridad '=)))
    (is (= 2 (aridad '<=)))
    (is (= 2 (aridad '>=)))
    (is (= 2 (aridad '<>)))
    (is (= 2 (aridad 'MID$)))
    (is (= 3 (aridad 'MID3$)))
  )
)

(deftest precedencia-test
  (testing "Test funcion precedencia"
    (is (= 1 (precedencia 'OR)))
    (is (= 2 (precedencia 'AND)))
    (is (= 4 (precedencia '<)))
    (is (= 4 (precedencia '>)))
    (is (= 4 (precedencia '=)))
    (is (= 4 (precedencia '<=)))
    (is (= 4 (precedencia '>=)))
    (is (= 4 (precedencia '<>)))
    (is (= 5 (precedencia '+)))
    (is (= 5 (precedencia '-)))
    (is (= 6 (precedencia '*)))
    (is (= 6 (precedencia '/)))
    (is (= 7 (precedencia '-u)))
    (is (= 8 (precedencia 'MID$)))
    (is (= 8 (precedencia 'MID3$)))
    (is (= 8 (precedencia 'LEN)))
  )
)

(deftest eliminar-cero-decimal-test
  (testing "Test funcion eliminar cero decimal"
    (is (= 1 (eliminar-cero-decimal 1)))
    (is (= 1 (eliminar-cero-decimal 1.0)))
    (is (= 1 (eliminar-cero-decimal 1.00)))
    (is (= 'A (eliminar-cero-decimal 'A)))
    (is (= 1.1 (eliminar-cero-decimal 1.1)))
    (is (= 1.1 (eliminar-cero-decimal 1.10)))
    (is (= 1.05 (eliminar-cero-decimal 1.05)))
    (is (= 1.05 (eliminar-cero-decimal 1.050)))
  )
)

(deftest eliminar-cero-entero-test
  (testing "Test funcion eliminar cero entero"
    (is (= nil (eliminar-cero-entero nil)))
    (is (= "A" (eliminar-cero-entero 'A)))
    (is (= " 0" (eliminar-cero-entero 0)))
    (is (= " 1.5" (eliminar-cero-entero 1.5)))
    (is (= " 1" (eliminar-cero-entero 1)))
    (is (= "-1" (eliminar-cero-entero -1)))
    (is (= "-1.5" (eliminar-cero-entero -1.5)))
    (is (= " .5" (eliminar-cero-entero 0.5)))
    (is (= " .5" (eliminar-cero-entero 000.5)))
    (is (= "-.5" (eliminar-cero-entero -0.5)))
    (is (= "-.5" (eliminar-cero-entero -0000.5)))
  )
)

(deftest cargar-linea-test
  (testing "Test funcion cargar linea"
    (is (= ['((10 (PRINT X))) [:ejecucion-inmediata 0] [] [] [] 0 {}] (cargar-linea '(10 (PRINT X)) [() [:ejecucion-inmediata 0] [] [] [] 0 {}])))
    (is (= ['((10 (PRINT X)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}] (cargar-linea '(20 (X = 100)) ['((10 (PRINT X))) [:ejecucion-inmediata 0] [] [] [] 0 {}])))
    (is (= ['((5 (X = 100)) (10 (PRINT X))) [:ejecucion-inmediata 0] [] [] [] 0 {}] (cargar-linea '(5 (X = 100)) ['((10 (PRINT X))) [:ejecucion-inmediata 0] [] [] [] 0 {}])))
    (is (= ['((10 (PRINT X)) (15 (X = X + 1)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}] (cargar-linea '(15 (X = X + 1)) ['((10 (PRINT X)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}])))
    (is (= ['((10 (PRINT X)) (15 (X = X - 1)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}] (cargar-linea '(15 (X = X - 1)) ['((10 (PRINT X)) (15 (X = X + 1)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}])))
  )
)

(deftest dar-error-test
  (testing "Test funcion dar error"
    (is (= nil (dar-error "?ERROR DISK FULL" [:ejecucion-inmediata 4])))
    (is (= nil (dar-error 16 [:ejecucion-inmediata 4])))
    (is (= nil (dar-error 16 [100 3])))
    (is (= nil (dar-error "?ERROR DISK FULL" [100 3])))
  )
)

(deftest buscar-lineas-restantes-test
  (testing "Test funcion buscar lineas restantes"
    (is (= nil (buscar-lineas-restantes [() [:ejecucion-inmediata 0] [] [] [] 0 {}])))
    (is (= nil (buscar-lineas-restantes ['((PRINT X) (PRINT Y)) [:ejecucion-inmediata 2] [] [] [] 0 {}])))
    (is (= (list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 2] [] [] [] 0 {}])))
    (is (= (list '(10 (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])))
    (is (= (list '(10) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 0] [] [] [] 0 {}])))
    (is (= (list '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 1] [] [] [] 0 {}])))
    (is (= (list '(15) (list 20 (list 'NEXT 'I (symbol ",") 'J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 0] [] [] [] 0 {}])))
    (is (= '((20 (NEXT I) (NEXT J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}])))
    (is (= '((20 (NEXT I) (NEXT J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 2] [] [] [] 0 {}])))
    (is (= '((20 (NEXT J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 1] [] [] [] 0 {}])))
    (is (= '((20)) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 0] [] [] [] 0 {}])))
    (is (= '((20)) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 -1] [] [] [] 0 {}])))
    (is (= nil (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [25 0] [] [] [] 0 {}])))
  )
)

(deftest continuar-linea-test
  (testing "Test funcion buscar lineas restantes"
    (is (= [nil [(list '(10 (PRINT X)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}]] (continuar-linea [(list '(10 (PRINT X)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}])))
    (is (= [:omitir-restante [(list '(10 (PRINT X)) '(15 (GOSUB 100) (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 1] [] [] [] 0 {}]] (continuar-linea [(list '(10 (PRINT X)) '(15 (GOSUB 100) (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [[15 2]] [] [] 0 {}])))
  )
)

(deftest extraer-data-test
  (testing "Test funcion extraer data"
    (is (= '() (extraer-data '(()))))
    (is (= '("HOLA" "MUNDO" 10 20) (extraer-data (list '(10 (PRINT X) (REM ESTE NO) (DATA 30)) '(20 (DATA HOLA)) (list 100 (list 'DATA 'MUNDO (symbol ",") 10 (symbol ",") 20))))))
    (is (= '("HOLA" "MUNDO" 10.5 20) (extraer-data (list '(10 (PRINT X) (REM ESTE NO) (DATA 30)) '(20 (DATA HOLA)) (list 100 (list 'DATA 'MUNDO (symbol ",") 10.5 (symbol ",") 20))))))
    (is (= '("HOLA" "CHAU" "MUNDO" 10 20) (extraer-data (list '(10 (PRINT X) (REM ESTE NO) (DATA 30)) '(20 (DATA HOLA) (DATA CHAU)) (list 100 (list 'DATA 'MUNDO (symbol ",") 10 (symbol ",") 20))))))
    (is (= '("HOLA" "MUNDO 10" 10 20) (extraer-data (list '(10 (PRINT X) (REM ESTE NO) (DATA 30)) '(20 (DATA HOLA)) (list 100 (list 'DATA 'MUNDO 10(symbol ",") 10 (symbol ",") 20))))))
    (is (= '("HOLA" "HOLA 2 MUNDO" 10 20) (extraer-data (list '(10 (PRINT X) (REM ESTE NO) (DATA 30)) '(20 (DATA HOLA)) (list 100 (list 'DATA 'HOLA 2 'MUNDO (symbol ",") 10 (symbol ",") 20))))))
  )
)

(deftest preprocesar-expresion-test
  (testing "Test funcion preprocesar expresion"
    (is (= '("HOLA" + " MUNDO" + "") (preprocesar-expresion '(X$ + " MUNDO" + Z$) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HOLA"}])))
    (is (= '(5 + 0 / 2 * 0) (preprocesar-expresion '(X + . / Y% * Z) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 5 Y% 2}])))
    (is (= '(0 + 0 / 2 * 0) (preprocesar-expresion '(X + . / Y% * Z) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{Y% 2}])))
    (is (= (list 'MID$ (symbol "(") "HOLA" (symbol ",") 1 (symbol ")")) (preprocesar-expresion (list 'MID$ (symbol "(") 'X$ (symbol ",") 1 (symbol ")")) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HOLA"}])))
  )
)

(deftest desambiguar-test
  (testing "Test funcion desambiguar"
    (is (= (list '-u 2 '* (symbol "(") '-u 3 '+ 5 '- (symbol "(") 2 '/ 7 (symbol ")") (symbol ")")) (desambiguar (list '- 2 '* (symbol "(") '- 3 '+ 5 '- (symbol "(") '+ 2 '/ 7 (symbol ")") (symbol ")")))))
    (is (= (list 'MID$ (symbol "(") 1 (symbol ",") 2 (symbol ")")) (desambiguar (list 'MID$ (symbol "(") 1 (symbol ",") 2 (symbol ")")))))
    (is (= (list 'MID3$ (symbol "(") 1 (symbol ",") 2 (symbol ",") 3 (symbol ")")) (desambiguar (list 'MID$ (symbol "(") 1 (symbol ",") 2 (symbol ",") 3 (symbol ")")))))
    (is (= (list 'MID3$ (symbol "(") 1 (symbol ",") '-u 2 '+ 'K (symbol ",") 3 (symbol ")")) (desambiguar (list 'MID$ (symbol "(") 1 (symbol ",") '- 2 '+ 'K (symbol ",") 3 (symbol ")")))))
  )
)

(deftest ejecutar-asignacion-test
  (testing "Test funcion ejecutar asignacion"
    (is (= '[((10 (PRINT X))) [10 1] [] [] [] 0 {X 5}] (ejecutar-asignacion '(X = 5) ['((10 (PRINT X))) [10 1] [] [] [] 0 {}])))
    (is (= '[((10 (PRINT X))) [10 1] [] [] [] 0 {X 5}] (ejecutar-asignacion '(X = 5) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}])))
    (is (= '[((10 (PRINT X))) [10 1] [] [] [] 0 {X 3}] (ejecutar-asignacion '(X = X + 1) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}])))
    (is (= '[((10 (PRINT X))) [10 1] [] [] [] 0 {X$ "HOLA MUNDO"}] (ejecutar-asignacion '(X$ = X$ + " MUNDO") ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HOLA"}])))
    (is (= '[((10 (PRINT X))) [10 1] [] [] [] 0 {X$ "HOLA" L 4}] (ejecutar-asignacion (list 'L '= 'LEN (symbol "(") 'X$ (symbol ")")) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HOLA"}])))
  )
)
