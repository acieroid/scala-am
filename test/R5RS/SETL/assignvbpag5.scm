(define (when p body)(if b body))
(define (unless p body)(if (not p) body))

(define (char-downcase c)
  (let ((i (assoc c '((#\A #\a) (#\B #\b) (#\C #\c) (#\D #\d) (#\E #\e) (#\F #\f) (#\G #\g) (#\H #\h) (#\I #\i) (#\j #\j) (#\K #\k) (#\L #\l) (#\M #\m) (#\N #\n) (#\O #\o) (#\P #\p) (#\Q #\q) (#\R #\r) (#\S #\s) (#\T #\t) (#\U #\u) (#\V #\v) (#\W #\w) (#\X #\x) (#\Y #\y) (#\Z #\z)))))
    (if i (cadr i) c)))
(define (char->string c)
  (let ((i (assoc c '((#\tab "\t")(#\newline "\n")('EOF "") (#\space " ") (#\! "!") (#\" "\"") (#\# "#") (#\$ "$") (#\% "%") (#\& "&") (#\' "'") (#\( "(") (#\) ")") (#\* "*") (#\+ "+") (#\, ",") (#\- "-") (#\. ".") (#\/ "/") (#\0 "0") (#\1 "1") (#\2 "2") (#\3 "3") (#\4 "4") (#\5 "5") (#\6 "6") (#\7 "7") (#\8 "8") (#\9 "9") (#\: ":") (#\; ";") (#\< "<") (#\= "=") (#\> ">") (#\? "?") (#\@ "@") (#\A "A") (#\B "B") (#\C "C") (#\D "D") (#\E "E") (#\F "F") (#\G "G") (#\H "H") (#\I "I") (#\J "J") (#\K "K") (#\L "L") (#\M "M") (#\N "N") (#\O "O") (#\P "P") (#\Q "Q") (#\R "R") (#\S "S") (#\T "T") (#\U "U") (#\V "V") (#\W "W") (#\X "X") (#\Y "Y") (#\Z "Z") (#\[ "[") (#\\ "\\") (#\] "]") (#\^ "^") (#\_ "_") (#\` "`") (#\a "a") (#\b "b") (#\c "c") (#\d "d") (#\e "e") (#\f "f") (#\g "g") (#\h "h") (#\i "i") (#\j "j") (#\k "k") (#\l "l") (#\m "m") (#\n "n") (#\o "o") (#\p "p") (#\q "q") (#\r "r") (#\s "s") (#\t "t") (#\u "u") (#\v "v") (#\w "w") (#\x "x") (#\y "y") (#\z "z") (#\{ "{") (#\| "|") (#\} "}") (#\~ "~")))))
    (if i (cadr i) (begin (display c)(error "Unknown character.")))))

(define (list->string l)
  (if (null? l)
    ""
    (string-append (char->string (car l))
      (list->string (cdr l)))))

(define (make-hash-table) (cons 'ht '()))
(define (hash-table? t)(eq? (car t) 'ht))
(define (hash-table-get table key err)
  (let ((r (assoc key (cdr table))))
    (if r
      r
      err)))
(define (hash-table-put! table key val)
  (set-cdr! table (cons (cons key val) (cdr table))))

(define ctr 0)
(define (read-char str)
  (if (= ctr (string-length str))
      'EOF
    (begin
      (set! ctr (+ ctr 1))
      (string-ref str (- ctr 1)))))
(define (peek-char str)
  (if (= ctr (string-length str))
      'EOF
    (string-ref str ctr)))
(define (char-ready? str) (< ctr (string-length str)))
(define (eof-object? x) (equal? 'EOF x))

(define (char->integer c)
  (let ((i (assoc c '(('EOF 26)(#\space 32) (#\! 33) (#\" 34) (#\# 35) (#\$ 36) (#\% 37) (#\& 38) (#\' 39) (#\( 40) (#\) 41) (#\* 42) (#\+ 43) (#\, 44) (#\- 45) (#\. 46) (#\/ 47) (#\0 48) (#\1 49) (#\2 50) (#\3 51) (#\4 52) (#\5 53) (#\6 54) (#\7 55) (#\8 56) (#\9 57) (#\: 58) (#\; 59) (#\< 60) (#\= 61) (#\> 62) (#\? 63) (#\@ 64) (#\A 65) (#\B 66) (#\C 67) (#\D 68) (#\E 69) (#\F 70) (#\G 71) (#\H 72) (#\I 73) (#\J 74) (#\K 75) (#\L 76) (#\M 77) (#\N 78) (#\O 79) (#\P 80) (#\Q 81) (#\R 82) (#\S 83) (#\T 84) (#\U 85) (#\V 86) (#\W 87) (#\X 88) (#\Y 89) (#\Z 90) (#\[ 91) (#\\ 92) (#\] 93) (#\^ 94) (#\_ 95) (#\` 96) (#\a 97) (#\b 98) (#\c 99) (#\d 100) (#\e 101) (#\f 102) (#\g 103) (#\h 104) (#\i 105) (#\j 106) (#\k 107) (#\l 108) (#\m 109) (#\n 110) (#\o 111) (#\p 112) (#\q 113) (#\r 114) (#\s 115) (#\t 116) (#\u 117) (#\v 118) (#\w 119) (#\x 120) (#\y 121) (#\z 122) (#\{ 123) (#\| 124) (#\} 125) (#\~ 126)))))
    (if i i (error "Unknown character."))))
(define (make-hash-table) (cons 'ht '()))
(define (hash-table? t)(eq? (car t) 'ht))
(define (hash-table-get table key err)
  (let ((r (assoc key (cdr table))))
    (if r
      r
      err)))
(define (hash-table-put! table key val)
  (set-cdr! table (cons (cons key val) (cdr table))))



(define ctr 0)
(define (read-char str)
  (if (= ctr (string-length str))
      'EOF
    (begin
      (set! ctr (+ ctr 1))
      (string-ref str (- ctr 1)))))
(define (peek-char str)
  (if (= ctr (string-length str))
      'EOF
    (string-ref str ctr)))
(define (char-ready? str) (< ctr (string-length str)))
(define (eof-object? x) (equal? 'EOF x))

(define (char->integer c)
  (let ((i (assoc c '((#\tab 09)(#\newline 10)('EOF 26)(#\space 32) (#\! 33) (#\" 34) (#\# 35) (#\$ 36) (#\% 37) (#\& 38) (#\' 39) (#\( 40) (#\) 41) (#\* 42) (#\+ 43) (#\, 44) (#\- 45) (#\. 46) (#\/ 47) (#\0 48) (#\1 49) (#\2 50) (#\3 51) (#\4 52) (#\5 53) (#\6 54) (#\7 55) (#\8 56) (#\9 57) (#\: 58) (#\; 59) (#\< 60) (#\= 61) (#\> 62) (#\? 63) (#\@ 64) (#\A 65) (#\B 66) (#\C 67) (#\D 68) (#\E 69) (#\F 70) (#\G 71) (#\H 72) (#\I 73) (#\J 74) (#\K 75) (#\L 76) (#\M 77) (#\N 78) (#\O 79) (#\P 80) (#\Q 81) (#\R 82) (#\S 83) (#\T 84) (#\U 85) (#\V 86) (#\W 87) (#\X 88) (#\Y 89) (#\Z 90) (#\[ 91) (#\\ 92) (#\] 93) (#\^ 94) (#\_ 95) (#\` 96) (#\a 97) (#\b 98) (#\c 99) (#\d 100) (#\e 101) (#\f 102) (#\g 103) (#\h 104) (#\i 105) (#\j 106) (#\k 107) (#\l 108) (#\m 109) (#\n 110) (#\o 111) (#\p 112) (#\q 113) (#\r 114) (#\s 115) (#\t 116) (#\u 117) (#\v 118) (#\w 119) (#\x 120) (#\y 121) (#\z 122) (#\{ 123) (#\| 124) (#\} 125) (#\~ 126)))))
    (if i (cadr i) (begin (display c)(error "Unknown character.")))))

(define (send-message object message . parameters)
  (let ((method (object message)))
    (apply method parameters)))

(define (out . params)
  (for-each (lambda (x) (display x)) params))

(define identity (lambda (x) x))

(define (insert-in-list lst begn end val)
  (define (insert lst pos val)
    (cond ((null? lst) (list val))
      ((< pos begn) (cons (car lst) (insert (cdr lst) (+ pos 1) val)))
      ((<= pos end) (if (null? val)
                      (insert (cdr lst) (+ pos 1) val)
                      (cons (car val) (insert lst pos (cdr val)))))
      ((> pos end) (cons (car lst) (insert (cdr lst) (+ pos 1) val)))))
  (if (or (> begn (length lst)) (> end (length lst)))
    #f
    (insert lst 1 val)))

(define (list-range lst bg end)
  (define (range lst pos)
    (cond ((null? lst) '())
      ((and (>= pos bg) (<= pos end))
        (cons (car lst) (range (cdr lst) (+ pos 1))))
      (else (range (cdr lst) (+ pos 1)))))
  (cond ((or (> bg (length lst)) (> end (length lst))) #f)
    ((> bg end) '())
    (else (range lst 1))))

(define (alter-pos-in-list lst pos val fill)
  (define (loop lst cur)
    (cond ((and (null? lst) (= cur pos)) (cons val (loop lst (+ cur 1))))
      ((and (null? lst) (< cur pos)) (cons fill (loop lst (+ cur 1))))
      ((null? lst) '())
      ((= pos cur) (cons val (loop (cdr lst) (+ cur 1))))
      (else (cons (car lst) (loop (cdr lst) (+ cur 1))))))
  (loop lst 1))

(define (form-list bgn step end constructor)
  (let loop ((i bgn))
    (cond ((= i end) (list (constructor end)))
      ((or (and (positive? step) (> i end))
         (and (not (positive? step)) (< i end)))
          '())
      (else (cons (constructor i) (loop (+ i step)))))))


(define (duplicate-list lst dup)
  (if (= dup 0)
      '()
    (append lst (duplicate-list lst (- dup 1)))))

(define (duplicate-string str dup)
  (if (= dup 0)
    ""
    (string-append str (duplicate-string str (- dup 1)))))

(define (sublist sub lst)
  (define (match lst part)
    (cond ((and (null? part) (null? lst)) #t)
      ((null? lst) #f)
      ((null? part) #t)
      ((equal? (car lst) (car part))
        (match (cdr lst) (cdr part)))
      (else #f)))
  (define (loop lst part)
    (cond ((null? lst) #f)
      ((equal? (car lst) (car part))
        (or (match (cdr lst) (cdr part))
          (loop (cdr lst) part)))
      (else (loop (cdr lst) part))))
  (and (not (null? sub))
    (loop lst sub)))

(define (lower ch)
  (if (eof-object? ch)
    ch
    (char-downcase ch)))


;; Tokens - used for communication between scanner and parser
;; ==========================================================

;; Token symbols

(define lpr-symbol 'left-parenthesis)
(define rpr-symbol 'right-parenthesis)
(define lbc-symbol 'left-brace)
(define rbc-symbol 'right-brace)
(define lbr-symbol 'left-bracket)
(define rbr-symbol 'right-bracket)
(define wrd-symbol 'word)
(define int-symbol 'integer)
(define rea-symbol 'real)
(define smc-symbol 'semicolon)
(define eol-symbol 'end-of-line)
(define end-symbol 'end-of-input)
(define cmt-symbol 'comment)
(define txt-symbol 'text)
(define com-symbol 'comma)
(define col-symbol 'colon)
(define mop-symbol 'multiplicative-operator)
(define aop-symbol 'additive-operator)
(define pls-symbol 'plus)
(define mns-symbol 'minus)
(define eql-symbol 'equality)
(define ceq-symbol 'colon-equality)
(define xop-symbol 'exponentiation-operator)
(define sch-symbol 'such-that)
(define err-symbol 'error)
(define mul-symbol 'multiplication)
(define div-symbol 'division)
(define dcl-symbol 'double-colon)
(define dif-symbol 'difference)
(define lss-symbol 'less)
(define els-symbol 'equal-or-less)
(define grt-symbol 'greater)
(define egr-symbol 'equal-or-greater)
(define amt-symbol 'amount)

;; Token keywords
(define program-keyword 'program-keyword)
(define end-keyword 'end-keyword)
(define range-keyword 'range-keyword)
(define var-keyword 'var-keyword)
(define const-keyword 'const-keyword)
(define init-keyword 'init-keyword)
(define exit-keyword 'exit-keyword)
(define pass-keyword 'pass-keyword)
(define stop-keyword 'stop-keyword)
(define goto-keyword 'goto-keyword)
(define impl-keyword 'implication-keyword)
(define or-keyword 'or-keyword)
(define and-keyword 'and-keyword)
(define not-keyword 'not-keyword)
(define is_tuple-keyword 'is_tuple-keyword)
(define is_set-keyword 'is_set-keyword)
(define is_string-keyword 'is_string-keyword)
(define is_integer-keyword 'is_integer-keyword)
(define is_real-keyword 'is_real-keyword)
(define is_map-keyword 'is_map-keyword)
(define is_atom-keyword 'is_atom-keyword)
(define is_boolean-keyword 'is-boolean_keyword)
(define abs-keyword 'abs-keyword)
(define acos-keyword 'acos-keyword)
(define arb-keyword 'arb-keyword)
(define asin-keyword 'asin-keyword)
(define atan-keyword 'atan-keyword)
(define ceil-keyword 'ceil-keyword)
(define char-keyword 'char-keyword)
(define cos-keyword 'cos-keyword)
(define domain-keyword 'domain-keyword)
(define even-keyword 'even-keyword)
(define expr-keyword 'expr-keyword)
(define fix-keyword 'fix-keyword)
(define float-keyword 'float-keyword)
(define floor-keyword 'floor-keyword)
(define log-keyword 'log-keyword)
(define not-keyword 'not-keyword)
(define odd-keyword 'odd-keyword)
(define pow-keyword 'pow-keyword)
(define random-keyword 'random-keyword)
(define range-keyword 'range-keyword)
(define sign-keyword 'sign-keyword)
(define sin-keyword 'sin-keyword)
(define sqrt-keyword 'sqrt-keyword)
(define str-keyword 'str-keyword)
(define tan-keyword 'tan-keyword)
(define tanh-keyword 'tanh-keyword)
(define type-keyword 'type-keyword)
(define in-keyword 'in-keyword)
(define notin-keyword 'notin-keyword)
(define subset-keyword 'subset-keyword)
(define true-keyword 'true-keyword)
(define false-keyword 'false-keyword)
(define omega-keyword 'omega-keyword)
(define exists-keyword 'exists-keyword)
(define notexists-keyword 'notexists-keyword)
(define forall-keyword 'forall-keyword)
(define from-keyword 'from-keyword)
(define fromb-keyword 'fromb-keyword)
(define frome-keyword 'frome-keyword)
(define max-keyword 'max-keyword)
(define min-keyword 'min-keyword)
(define mod-keyword 'mod-keyword)
(define with-keyword 'with-keyword)
(define if-keyword 'if-keyword)
(define then-keyword 'then-keyword)
(define else-keyword 'else-keyword)
(define elseif-keyword 'elseif-keyword)
(define return-keyword 'return-keyword)
(define loop-keyword 'loop-keyword)
(define do-keyword 'do-keyword)
(define for-keyword 'for-keyword)
(define init-keyword 'init-keyword)
(define doing-keyword 'doing-keyword)
(define while-keyword 'while-keyword)
(define step-keyword 'step-keyword)
(define until-keyword 'until-keyword)
(define term-keyword 'term-keyword)
(define proc-keyword 'proc-keyword)
(define rd-keyword 'rd-keyword)
(define rw-keyword 'rw-keyword)
(define wr-keyword 'wr-keyword)
(define div-keyword 'div-keyword)

(define unary-keywords
  (list abs-keyword acos-keyword arb-keyword asin-keyword
    atan-keyword ceil-keyword char-keyword cos-keyword
    domain-keyword even-keyword expr-keyword fix-keyword
    float-keyword floor-keyword log-keyword
    odd-keyword pow-keyword random-keyword
    sign-keyword sin-keyword sqrt-keyword str-keyword
    tan-keyword tanh-keyword type-keyword amt-symbol))

(define not-is_xx (list not-keyword is_tuple-keyword
                    is_set-keyword is_string-keyword
                    is_integer-keyword is_real-keyword
                    is_map-keyword is_atom-keyword
                    is_boolean-keyword))

(define assigning
  (list impl-keyword or-keyword and-keyword in-keyword notin-keyword
    subset-keyword pls-symbol mns-symbol div-symbol mul-symbol xop-symbol max-keyword
    min-keyword mod-keyword notin-keyword with-keyword eql-symbol dif-symbol lss-symbol
    els-symbol grt-symbol egr-symbol amt-symbol))

(define rop-tokens (list in-keyword notin-keyword subset-keyword eql-symbol
                     dif-symbol lss-symbol els-symbol grt-symbol egr-symbol))

;; Token structure
;(define-struct token (symbol line position data))

(define (make-token . args) (cons 'token args))
(define (token? x)(eq? (car x) 'token))
(define token-symbol cadr)
(define token-line caddr)
(define token-position cadddr)
(define token-data (lambda (x) (car (cddddr x))))

(define (equivalent-tokens? x y)
  (and (equal? (token-symbol x)
         (token-symbol y))
    (equal? (token-data x)
      (token-data y))))

;; Scanner
;; =======


;; Scanner
(define (make-scanner)
  (let ((pos 0)
         (line 0)
         (char #f)
         (hold #f)
         (port #f)
         (keywords (list (cons "program" program-keyword)
                     (cons "end" end-keyword)
                     (cons "var" var-keyword)
                     (cons "const" const-keyword)
                     (cons "init" init-keyword)
                     (cons "exit" exit-keyword)
                     (cons "goto" goto-keyword)
                     (cons "pass" pass-keyword)
                     (cons "stop" stop-keyword)
                     (cons "impl" impl-keyword)
                     (cons "or" or-keyword)
                     (cons "and" and-keyword)
                     (cons "not" not-keyword)
                     (cons "is_tuple" is_tuple-keyword)
                     (cons "is_set" is_set-keyword)
                     (cons "is_string" is_string-keyword)
                     (cons "is_integer" is_integer-keyword)
                     (cons "is_real" is_real-keyword)
                     (cons "is_map" is_map-keyword)
                     (cons "is_atom" is_atom-keyword)
                     (cons "is_boolean" is_boolean-keyword)
                     (cons "abs" abs-keyword)
                     (cons "acos" acos-keyword)
                     (cons "arb" arb-keyword)
                     (cons "asin" asin-keyword)
                     (cons "atan" atan-keyword)
                     (cons "ceil" ceil-keyword)
                     (cons "char" char-keyword)
                     (cons "cos" cos-keyword)
                     (cons "domain" domain-keyword)
                     (cons "even" even-keyword)
                     (cons "expr" expr-keyword)
                     (cons "fix" fix-keyword)
                     (cons "float" float-keyword)
                     (cons "floor" floor-keyword)
                     (cons "log" log-keyword)
                     (cons "not" not-keyword)
                     (cons "odd" odd-keyword)
                     (cons "pow" pow-keyword)
                     (cons "random" random-keyword)
                     (cons "range" range-keyword)
                     (cons "sign" sign-keyword)
                     (cons "sin" sin-keyword)
                     (cons "sqrt" sqrt-keyword)
                     (cons "str" str-keyword)
                     (cons "tan" tan-keyword)
                     (cons "tanh" tanh-keyword)
                     (cons "type" type-keyword)
                     (cons "in" in-keyword)
                     (cons "notin" notin-keyword)
                     (cons "subset" subset-keyword)
                     (cons "true" true-keyword)
                     (cons "false" false-keyword)
                     (cons "st" sch-symbol)
                     (cons "om" omega-keyword)
                     (cons "exists" exists-keyword)
                     (cons "notexists" notexists-keyword)
                     (cons "forall" forall-keyword)
                     (cons "from" from-keyword)
                     (cons "fromb" fromb-keyword)
                     (cons "frome" frome-keyword)
                     (cons "max" max-keyword)
                     (cons "min" min-keyword)
                     (cons "mod" mod-keyword)
                     (cons "with" with-keyword)
                     (cons "if" if-keyword)
                     (cons "then" then-keyword)
                     (cons "else" else-keyword)
                     (cons "elseif" elseif-keyword)
                     (cons "return" return-keyword)
                     (cons "loop" loop-keyword)
                     (cons "do" do-keyword)
                     (cons "for" for-keyword)
                     (cons "init" init-keyword)
                     (cons "doing" doing-keyword)
                     (cons "while" while-keyword)
                     (cons "step" step-keyword)
                     (cons "until" until-keyword)
                     (cons "term" term-keyword)
                     (cons "proc" proc-keyword)
                     (cons "rd" rd-keyword)
                     (cons "rw" rw-keyword)
                     (cons "wr" wr-keyword)
                     (cons "div" div-keyword)
                   )))

    (define (keyword? text)
      (define result (assoc text keywords))
      (if result
        (cdr result)
        #f))

    (define (char-type ch)
      (if (eof-object? ch)
        eof
        (vector-ref
          (vector
            wsp wsp wsp wsp wsp wsp wsp wsp wsp wsp eol wsp wsp wsp wsp wsp ;000
            wsp wsp wsp wsp wsp wsp wsp wsp wsp wsp wsp wsp wsp wsp wsp wsp ;016
            wsp ill ill amt cmt ill ill txt lpr rpr mul pls com mns per div ;032
            dgt dgt dgt dgt dgt dgt dgt dgt dgt dgt col smc lss eql grt ill ;048
            ill ltr ltr ltr ltr exp ltr ltr ltr ltr ltr ltr ltr ltr ltr ltr ;064
            ltr ltr ltr ltr ltr ltr ltr ltr ltr ltr ltr lbr ill rbr ill ltr ;080
            ill ltr ltr ltr ltr exp ltr ltr ltr ltr ltr ltr ltr ltr ltr ltr ;096
            ltr ltr ltr ltr ltr ltr ltr ltr ltr ltr ltr lbc sch rbc ill ill ;112
            ill ill ill ill ill ill ill ill ill ill ill ill ill ill ill ill ;128
            ill ill ill ill ill ill ill ill ill ill ill ill ill ill ill ill ;144
            ill ill ill ill ill wsp ill ill ill ill ill ill ill ill ill ill ;160
            ill ill ill ill ill ill ill ill ill ill ill ill ill ill ill ill ;176
            ill ill ill ill ill ill ill ill ill ill ill ill ill ill ill ill ;192
            ill ill ill ill ill ill ill ill ill ill ill ill ill ill ill ill ;208
            ill ill ill ill ill ill ill ill ill ill ill ill ill ill ill ill ;224
            ill ill ill ill ill ill ill ill ill ill ill ill ill ill ill ill);240
          (char->integer ch))))


    (define (check . allowed)
      (member (char-type char) allowed))

    (define (peek-check . allowed)
      (member (char-type (peek)) allowed))

    (define (check-consecutive . allowed)
      (let loop ((number 0))
        (if (apply check allowed)
          (begin
            (next)
            (loop (+ number 1)))
          number)))


    (define (init-scan input-port)
      (set! pos 0)
      (set! line 1)
      (set! port input-port)
      (set! char (read-char port)))


    (define (skip)
      (define current-char char)
      (next)
      current-char)


    (define (next)
      (set! pos (+ pos 1))
      (set! char (lower (read-char port))))

    (define (peek)
      (peek-char port))

    (define (scan)
      ((char-type char)))

    (define (ill)
      (make-token err-symbol line pos "illegal character"))

    (define (lss)
      (next)
      (if (check eql)
        (begin (next) (make-token els-symbol line pos #f))
        (make-token lss-symbol line pos #f)))

    (define (grt)
      (next)
      (if (check eql)
        (begin (next) (make-token egr-symbol line pos #f))
        (make-token grt-symbol line pos #f)))

    (define (eol)
      (cond ((char-ready? port)
              (set! line (+ line 1))
              (set! pos 1)
              (next)
              (scan))
        (else (make-token end-symbol line pos #f))))

    (define (wsp)
      (next)
      (scan))

    (define (eof)
      (make-token end-symbol line pos #f))

    (define (amt)
      (next)
      (make-token amt-symbol line pos #f))

    (define (operator token)
      (next)
      (make-token token line pos #f))

    (define (cmt)
      (next)
      (if (or (check eof) (check eol))
        (scan)
        (cmt)))

    (define (lbc)
      (next)
      (make-token lbc-symbol line pos #f))

    (define (rbc)
      (next)
      (make-token rbc-symbol line pos #f))

    (define (lbr)
      (next)
      (make-token lbr-symbol line pos #f))

    (define (rbr)
      (next)
      (make-token rbr-symbol line pos #f))

    (define (sch)
      (next)
      (make-token sch-symbol line pos #f))

    (define (pls)
      (operator pls-symbol))

    (define (mul)
      (next)
      (if (check mul)
        (begin (next) (make-token xop-symbol line pos #f))
        (make-token mul-symbol line pos #f)))

    (define (div)
      (next)
      (if (check eql)
        (begin (next) (make-token dif-symbol line pos #f))
        (make-token div-symbol line pos #f)))

    (define (mns)
      (operator mns-symbol))

    (define (lpr)
      (next)
      (make-token lpr-symbol line pos #f))

    (define (rpr)
      (next)
      (make-token rpr-symbol line pos #f))

    (define (com)
      (next)
      (make-token com-symbol line pos #f))

    (define (eql)
      (next)
      (make-token eql-symbol line pos #f))

    (define (word-or-keyword word)
      (let ((keyword-symbol (keyword? word)))
        (if keyword-symbol
          (make-token keyword-symbol line pos #f)
          (make-token wrd-symbol line pos word))))

    (define (ltr)
      (define (scan-word)
        (if (check ltr dgt exp)
          (cons (skip) (scan-word))
            '()))
      (word-or-keyword (list->string (scan-word))))

    (define exp ltr)

    (define (convert-number lst)
      (let ((number (string->number (list->string lst))))
        (cond  ((integer? number) (make-token int-symbol line pos number))
          ((real? number) (make-token rea-symbol line pos number))
          (else (make-token err-symbol line pos "not a valid number value")))))

    (define (dgt)
      (define per-allowed #t)
      (define (scan-digits)
        (if (check dgt)
          (cons (skip) (scan-digits))
            '()))
      (define (scan-exponent)
        (if (check pls mns)
          (cons (skip) (scan-digits))
          (scan-digits)))
      (define (scan-number)
        (cond ((check dgt) (cons (skip) (scan-number)))
          ((check exp) (cons (skip) (scan-exponent)))
          ((check per)
            (if (and per-allowed (not (peek-check per)))
              (begin
                (set! per-allowed #f)
                (cons (skip) (scan-number)))
                '()))
          (else '())))
      (convert-number (scan-number)))

    (define (per)
      (let ((number (check-consecutive per)))
        (if (or  (= number 2) (= number 3))
          (make-token range-keyword line pos #f)
          (make-token err-symbol line pos "expected .. or ..."))))

    (define (txt)
      (define (loop scanned-so-far)
        (if (not (check txt))
          (loop (cons (skip) scanned-so-far))
          (if (peek-check txt)
            (begin
              (next)
              (loop (cons (skip) scanned-so-far)))
            (if (check txt)
              (begin
                (next)
                (make-token txt-symbol line pos (list->string (reverse scanned-so-far))))
              (make-token err-symbol line pos "expected '")))))
      (next)
      (loop '()))

    (define (col)
      (next)
      (cond ((check eql) (next) (make-token ceq-symbol line pos #f))
        ((check col) (next) (make-token dcl-symbol line pos #f))
        (else (make-token col-symbol line pos #f))))

    (define (smc)
      (next)
      (make-token smc-symbol line pos #f))


    (define (dispatch message)
      (case message
        ((init-scan) init-scan)
        ((scan) scan)
        (else (error 'scanner "unknown message"))))

    dispatch))


;(define scanner (make-scanner))
;(send-message scanner 'init-scan (open-input-string "3*5+2"))
;(define (test)
;  (let ((x (send-message scanner 'scan)))
;    (display (token-symbol x)) (display " ") (display (token-data x)) (newline)))
;(test)
;(test)
;(test)
;(test)
;(test)
;(test)
;(test)
;(test)
;(test)
;(test)

(define (tree data left right)
  (vector data
    (+ 1 (tree-size left)
      (tree-size right))
    left
    right))

(define empty-tree (vector #f 0 #f #f))

(define (tree-data t)
  (vector-ref t 0))

(define (tree-size t)
  (vector-ref t 1))

(define (tree-left t)
  (vector-ref t 2))

(define (tree-right t)
  (vector-ref t 3))

(define (index t i)
  (if (empty? t)
    #f
    (if (< i (tree-size (tree-left t)))
      (index (tree-left t) i)
      (if (> i (tree-size (tree-left t)))
        (index (tree-right t) (- i (tree-size (tree-left t)) 1))
        (tree-data t)))))


(define (empty? t)
  (= 0 (tree-size t)))

(define (singleton e)
  (tree e empty-tree empty-tree))

(define (tree-manipulations less?)
  (define (rotate-single-left d l r)
    (tree
      (tree-data r)
      (tree d l (tree-left r))
      (tree-right r)))

  (define (rotate-double-left d l r)
    (let ((lr (tree-left r)))
      (tree
        (tree-data lr)
        (tree d l (tree-left lr))
        (tree (tree-data r) (tree-right lr) (tree-right r)))))

  (define (rotate-single-right d l r)
    (tree
      (tree-data l)
      (tree-left l)
      (tree d (tree-right l) r)))

  (define (rotate-double-right d l r)
    (let ((rl (tree-right l)))
      (tree
        (tree-data rl)
        (tree (tree-data l) (tree-left l) (tree-left rl))
        (tree d (tree-right rl) r))))

  (define (balance d l r)
    (let ((sl (tree-size l))
           (sr (tree-size r)))
      (cond ((< (+ sl sr) 2) (tree d l r))
        ((>= sr (* 3 sl))
          (if (< (tree-size (tree-left r)) (tree-size (tree-right r)))
            (rotate-single-left d l r)
            (rotate-double-left d l r)))
        ((>= sl (* 3 sr))
          (if (< (tree-size (tree-right l)) (tree-size (tree-left l)))
            (rotate-single-right d l r)
            (rotate-double-right d l r)))
        (else (tree d l r)))))



  (define (element? e t)
    (cond ((empty? t) #f)
      ((less? e (tree-data t))
        (element? e (tree-left t)))
      ((less? (tree-data t) e)
        (element? e (tree-right t)))
      (else #t)))

  (define (insert e t)
    (cond ((empty? t) (singleton e))
      ((less? e (tree-data t))
        (balance
          (tree-data t)
          (insert e (tree-left t))
          (tree-right t)))
      ((less? (tree-data t) e)
        (balance
          (tree-data t)
          (tree-left t)
          (insert e (tree-right t))))
      (else t)))

  (define (concat3 l d r)
    (cond ((empty? l) (insert d r))
      ((empty? r) (insert d l))
      ((< (* 3 (tree-size l)) (tree-size r))
        (balance
          (tree-data r)
          (concat3 l d (tree-left r))
          (tree-right r)))
      ((< (* 3 (tree-size r)) (tree-size l))
        (balance
          (tree-data l)
          (tree-left l)
          (concat3 (tree-right l) d r)))
      (else (tree d l r))))


  (define (split< e t)
    (cond ((empty? t) t)
      ((less? e (tree-data t))
        (split< e (tree-left t)))
      ((less? (tree-data t) e)
        (concat3 (tree-left t) (tree-data t) (split< e (tree-right t))))
      (else (tree-left t))))

  (define (split> e t)
    (cond ((empty? t) t)
      ((less? (tree-data t) e)
        (split> e (tree-right t)))
      ((less? e (tree-data t))
        (concat3 (split> e (tree-left t)) (tree-data t) (tree-right t)))
      (else (tree-right t))))

  (define (union t1 t2)
    (cond ((empty? t1) t2)
      ((empty? t2) t1)
      (else
        (let ((d (tree-data t1)))
          (concat3 (union (tree-left t1) (split< d t2))
            d
            (union (tree-right t1) (split> d t2)))))))

  (define (delete e t)
    (cond ((empty? t) t)
      ((less? e (tree-data t))
        (balance
          (tree-data t)
          (delete e (tree-left t))
          (tree-right t)))
      ((less? (tree-data t) e)
        (balance
          (tree-data t)
          (tree-left t)
          (delete e (tree-right t))))
      (else (merge (tree-left t) (tree-right t)))))

  (define (merge l r)
    (cond ((empty? l) r)
      ((empty? r) l)
      (else
        (balance (smallest r) l (delete-smallest r)))))

  (define (smallest t)
    (if (empty? (tree-left t))
      (tree-data t)
      (smallest (tree-left t))))

  (define (delete-smallest t)
    (if (empty? (tree-left t))
      (tree-right t)
      (balance
        (tree-data t)
        (delete-smallest (tree-left t))
        (tree-right t))))

  (define (difference t1 t2)
    (cond ((empty? t1) t2)
      ((empty? t2) t1)
      (else
        (let ((d (tree-data t2)))
          (concat (difference (split< d t1) (tree-left t2))
            (difference (split> d t1) (tree-right t2)))))))

  (define (concat l r)
    (cond ((empty? l) r)
      ((empty? r) l)
      ((< (* 3 (tree-size l)) (tree-size r))
        (balance
          (tree-data r)
          (concat l (tree-left r))
          (tree-right r)))
      ((< (* 3 (tree-size r)) (tree-size l))
        (balance
          (tree-data l)
          (tree-left l)
          (concat (tree-right l) r)))
      (else
        (balance (smallest r) l (delete-smallest r)))))

  (define (intersection t1 t2)
    (cond ((empty? t1) t1)
      ((empty? t2) t2)
      (else
        (let ((d (tree-data t2)))
          (let ((l (intersection (split< d t1) (tree-left t2)))
                 (r (intersection (split> d t1) (tree-right t2))))
            (if (element? d t1)
              (concat3 l d r)
              (concat l r)))))))

  (define (adjoin e t)
    (if (element? e t)
      t
      (insert e t)))

  (define (tree-less t1 t2)
    (define (loop i)
      (cond ((>= i (tree-size t1)) #f)
        ((less? (index t1 i)
           (index t2 i))
          #t)
        (else (loop (+ i 1)))))
    (cond ((< (tree-size t1)
             (tree-size t2))
            #t)
      ((= (tree-size t1)
         (tree-size t2))
        (loop 0))
      (else #f)))

  (define (the-empty-tree) empty-tree)

  (define (list->tree l)
    (let loop ((l l)
                (t (the-empty-tree)))
      (if (null? l)
        t
        (loop (cdr l) (adjoin (car l) t)))))

  (define (tree->list t)
    (let loop ((t t)
                (l '()))
      (if (empty? t)
        l
        (loop (tree-left t)
          (loop (tree-right t) (cons (tree-data t) l))))))

  (define (tree-for-each t function)
    (unless (empty? t)
      (tree-for-each (tree-left t) function)
      (function (tree-data t))
      (tree-for-each (tree-right t) function)))

  (define (tree-map t function)
    (define result (the-empty-tree))
    (define (loop t)
      (if (empty? t)
        result
        (begin
          (set! result (adjoin (function (tree-data t))
                         result))
          (loop (tree-left t))
          (loop (tree-right t)))))
    (loop t))

  (define (remove e t)
    (if (element? e t)
      (delete e t)
      t))

  (define (dispatch message)
    (case message
      ((element?) element?)
      ((insert) adjoin)
      ((union) union)
      ((smallest-element) smallest)
      ((remove) remove)
      ((difference) difference)
      ((intersection) intersection)
      ((construct-from-list) list->tree)
      ((convert-to-list) tree->list)
      ((for-each) tree-for-each)
      ((map) tree-map)
      ((less?) tree-less)
      (else (error 'tree "unknown message ~a" message))))

  dispatch)


;; Abstract Grammar
;; ================
;(define-struct ag-program (name body))
(define (make-ag-program . args) (cons 'ag-program args))
(define (ag-program? x)(eq? (car x) 'ag-program))
(define make-ag-program list)
(define ag-program-name cadr)
(define ag-program-body caddr)
;(define-struct ag-programbody (declarations statements routines refines))
(define (make-ag-programbody . args) (cons 'ag-programbody args))
(define (ag-programbody? x)(eq? (car x) 'ag-programbody))
(define ag-programbody-declarations cadr)
(define ag-programbody-statements caddr)
(define ag-programbody-routines cadddr)
(define ag-programbody-refines (lambda (x) (car (cddddr x))))
;(define-struct ag-body (declarations statements refines))
(define (make-ag-body . args) (cons 'ag-body args))
(define (ag-body? x)(eq? (car x) 'ag-body))
(define ag-body-declarations cadr)
(define ag-body-statements caddr)
(define ag-body-refines cadddr)
;(define-struct ag-identifier (name))
(define (make-ag-identifier . args) (cons 'ag-identifier args))
(define (ag-identifier? x)(eq? (car x) 'ag-identifier))
;(define-struct ag-declarations (var const init))
(define (make-ag-declarations . args) (cons 'ag-declarations args))
(define (ag-declarations? x)(eq? (car x) 'ag-declarations))
(define ag-declarations-body cadr)
(define ag-declarations-init caddr)
;(define-struct ag-constdeclaration (var constant))
(define (make-ag-declaration . args) (cons 'ag-declaration args))
(define (ag-declaration? x)(eq? (car x) 'ag-declaration))
(define ag-declaration-var cadr)
(define ag-declaration-constant caddr)
;(define-struct ag-initdeclaration (var init))
(define (make-ag-initdeclaration . args) (cons 'ag-initdeclaration args))
(define (ag-initdeclaration? x)(eq? (car x) 'ag-initdeclaration))
(define ag-initdeclaration-var cadr)
(define ag-initdeclaration-init caddr)
;(define-struct ag-integer (value))
(define (make-ag-integer . args) (cons 'ag-integer args))
(define (ag-integer? x)(eq? (car x) 'ag-integer))
(define ag-integer-value cadr)
;(define-struct ag-real (value))
(define (make-agreal . args) (cons 'agreal args))
(define (ag-real? x)(eq? (car x) 'agreal))
(define ag-real-value cadr)
;(define-struct ag-string (value))
(define (make-ag-string . args) (cons 'ag-string args))
(define (ag-string? x)(eq? (car x) 'ag-string))
(define ag-string-value cadr)

;(define-struct ag-set (contents))
(define (make-ag-set . args) (cons 'ag-set args))
(define (ag-set? x)(eq? (car x) 'ag-set))
(define ag-set-contents cadr)

;(define-struct ag-setslice (exp1 exp2 exp3))
(define (make-ag-setslice . args) (cons 'ag-setslice args))
(define (ag-setslice? x)(eq? (car x) 'ag-setslice))
(define ag-setslice-exp1 cadr)
(define ag-setslice-exp2 caddr)
(define ag-setslice-exp3 cadddr)
;(define-struct ag-tupleslice (exp1 exp2 exp3))
(define (make-ag-tupleslice . args) (cons 'ag-tupleslice args))
(define (ag-tupleslice? x)(eq? (car x) 'ag-tupleslice))
(define ag-tupleslice-exp1 cadr)
(define ag-tupleslice-exp2 caddr)
(define ag-tupleslice-exp3 cadddr)
;(define-struct ag-range (var begin end))
(define (make-ag-range . args) (cons 'ag-range args))
(define (ag-range? x)(eq? (car x) 'ag-range))
(define ag-range-var cadr)
(define ag-range-begin caddr)
(define ag-range-end cadddr)

;(define-struct ag-statement (label body))
(define (make-ag-statement . args) (cons 'ag-statement args))
(define (ag-statement? x)(eq? (car x) 'ag-statement))
(define ag-statement-label cadr)
(define ag-statement-body caddr)
;(define-struct ag-stmt-exit ())
(define (make-ag-stmt-exit . args) (cons 'ag-stmt-exit args))
(define (ag-stmt-exit? x)(eq? (car x) 'ag-stmt-exit))
;(define-struct ag-stmt-goto (label))
(define (make-ag-stmt-goto . args) (cons 'ag-stmt-goto args))
(define (ag-stmt-goto? x)(eq? (car x) 'ag-stmt-goto))
;(define-struct ag-stmt-pass ())
(define (make-ag-stmt-pass . args) (cons 'ag-stmt-pass args))
(define (ag-stmt-pass? x)(eq? (car x) 'ag-stmt-pass))
;(define-struct ag-stmt-stop ())
(define (make-ag-stmt-stop . args) (cons 'ag-stmt-stop args))
(define (ag-stmt-stop? x)(eq? (car x) 'ag-stmt-stop))
;(define-struct ag-stmt-assignment (lhs value))
(define (make-ag-stmt-assignment . args) (cons 'ag-stmt-assignment args))
(define (ag-stmt-assignment? x)(eq? (car x) 'ag-stmt-assignment))
(define ag-stmt-assignment-lhs cadr)
(define ag-stmt-assignment-value caddr)
;(define-struct ag-application
; (operator operands)) ;
(define (make-ag-application . args) (cons 'ag-application args))
(define (ag-application? x)(eq? (car x) 'ag-application))
(define ag-application-operator cadr)
(define ag-application-operands caddr)
;(define-struct ag-stmt-call (var args))
(define (make-ag-stmt-call . args) (cons 'ag-stmt-call args))
(define (ag-stmt-call? x)(eq? (car x) 'ag-stmt-call))
(define ag-stmt-call-var cadr)
(define ag-stmt-call-args caddr)
;(define-struct ag-stmt-return (val))
(define (make-ag-stmt-return . args) (cons 'ag-stmt-return args))
(define (ag-stmt-return? x)(eq? (car x) 'ag-stmt-return))
(define ag-stmt-return-val cadr)

;(define-struct ag-boolean (value))
(define (make-ag-boolean . args) (cons 'ag-boolean args))
(define (ag-boolean? x)(eq? (car x) 'ag-boolean))
(define ag-boolean-value cadr)
;(define-struct ag-omega ())
(define (make-ag-omega . args) (cons 'ag-omega args))
(define (ag-omega? x)(eq? (car x) 'ag-omega))
;(define-struct ag-selection (lhs selection))
(define (make-ag-selection . args) (cons 'ag-selection args))
(define (ag-selection? x)(eq? (car x) 'ag-selection))
(define ag-selection-lhs cadr)
(define ag-selection-selection caddr)
;(define-struct ag-sselection (lhs selection))
(define (make-ag-sselection . args) (cons 'ag-sselection args))
(define (ag-sselection? x)(eq? (car x) 'ag-sselection))
(define ag-sselection-lhs cadr)
(define ag-sselection-selection caddr)
;(define-struct ag-iterelement-in (lhs exp))
(define (make-ag-iterelement-in . args) (cons 'ag-iterelement-in args))
(define (ag-iterelement-in? x)(eq? (car x) 'ag-iterelement-in))
(define ag-iterelement-in-lhs cadr)
(define ag-iterelement-in-exp caddr)
;(define-struct ag-iterelement-selection (lhs idf selection))
(define (make-ag-iterelement-selection . args) (cons 'ag-iterelement-selection args))
(define (ag-iterelement-selection? x)(eq? (car x) 'ag-iterelement-selection))
(define ag-iterelement-selection-lhs cadr)
(define ag-iterelement-selection-idf caddr)
(define ag-iterelement-selection-selection cadddr)
;(define-struct ag-iterelement-sselection (lhs idf selection))
(define (make-ag-iterelement-sselection . args) (cons 'ag-iterelement-sselection args))
(define (ag-iterelement-sselection? x)(eq? (car x) 'ag-iterelement-sselection))
(define ag-iterelement-sselection-lhs cadr)
(define ag-iterelement-sselection-idf caddr)
(define ag-iterelement-sselection-selection cadddr)
;(define-struct ag-iterator (itlist suchthat))
(define (make-ag-iterator . args) (cons 'ag-iterator args))
(define (ag-iterator? x)(eq? (car x) 'ag-iterator))
(define ag-iterator-itlist cadr)
(define ag-iterator-suchthat caddr)
;(define-struct ag-setiterator (expr iterator))
(define (make-ag-setiterator . args) (cons 'ag-setiterator args))
(define (ag-setiterator? x)(eq? (car x) 'ag-setiterator))
(define ag-setiterator-expr cadr)
(define ag-setiterator-iterator caddr)
;(define-struct ag-tupleiterator (expr iterator))
(define (make-ag-tupleiterator . args) (cons 'ag-tupleiterator args))
(define (ag-tupleiterator? x)(eq? (car x) 'ag-tupleiterator))
(define ag-tupleiterator-expr cadr)
(define ag-tupleiterator-iterator caddr)
;(define-struct ag-exists (iterators exp))
(define (make-ag-exists . args) (cons 'ag-exists args))
(define (ag-exists? x)(eq? (car x) 'ag-exists))
(define ag-exists-exp cadr)
;(define-struct ag-notexists (iterators exp))
(define (make-ag-notexists . args) (cons 'ag-notexists args))
(define (ag-notexists? x)(eq? (car x) 'ag-notexists))
(define ag-notexists-iterators cadr)
(define ag-notexists-exp caddr)
;(define-struct ag-forall (iterators exp))
(define (make-ag-forall . args) (cons 'ag-forall args))
(define (ag-forall? x)(eq? (car x) 'ag-forall))
(define ag-forall-iterators cadr)
(define ag-forall-exp caddr)
;(define-struct ag-skip ())
(define (make-ag-skip . args) (cons 'ag-skip args))
(define (ag-skip? x)(eq? (car x) 'ag-skip))
;(define-struct ag-from (dest source))
(define (make-ag-from . args) (cons 'ag-from args))
(define (ag-from? x)(eq? (car x) 'ag-from))
(define ag-from-dest cadr)
(define ag-from-source caddr)
;(define-struct ag-fromb (dest source))
(define (make-ag-fromb . args) (cons 'ag-fromb args))
(define (ag-fromb? x)(eq? (car x) 'ag-fromb))
(define ag-fromb-dest cadr)
(define ag-fromb-source caddr)
;(define-struct ag-frome (dest source))
(define (make-ag-frome . args) (cons 'ag-frome args))
(define (ag-frome? x)(eq? (car x) 'ag-frome))
(define ag-frome-dest cadr)
(define ag-frome-source caddr)
;(define-struct ag-mulvalmapref (var el))
(define (make-ag-mulvalmapref . args) (cons 'ag-mulvalmapref args))
(define (ag-mulvalmapref? x)(eq? (car x) 'ag-mulvalmapref))
(define ag-mulvalmapref-var cadr)
(define ag-mulvalmapref-el caddr)
;(define-struct ag-simple-call-or-selection (var el))
(define (make-ag-simple-call-or-selection . args) (cons 'ag-simple-call-or-selection args))
(define (ag-simple-call-or-selection? x)(eq? (car x) 'ag-simple-call-or-selection))
(define ag-simple-call-or-selection-val cadr)
(define ag-simple-call-or-selection-el caddr)
;(define-struct ag-stmt-if (condition then elseif else))
(define (make-ag-stmt-if . args) (cons 'ag-stmt-if args))
(define (ag-stmt-if? x)(eq? (car x) 'ag-stmt-if))
(define ag-stmt-if-condition cadr)
(define ag-stmt-if-then caddr)
(define ag-stmt-if-elseif caddr)
(define ag-stmt-if-else caddr)
;(define-struct ag-elseif (condition then))
(define (make-ag-stmt-elseif . args) (cons 'ag-stmt-elseif args))
(define (ag-stmt-elseif? x)(eq? (car x) 'ag-stmt-elseif))
(define ag-stmt-elseif-condition cadr)
(define ag-stmt-elseif-then caddr)
;(define-struct ag-stmt-loop (loopiter stmts))
(define (make-ag-stmt-loop . args) (cons 'ag-stmt-loop args))
(define (ag-stmt-loop? x)(eq? (car x) 'ag-stmt-loop))
(define ag-stmt-loop-loopiter cadr)
(define ag-stmt-loop-stmts caddr)
;(define-struct ag-loopiter-for (iterator))
(define (make-ag-loopiter-for . args) (cons 'ag-loopiter-for args))
(define (ag-loopiter-for? x)(eq? (car x) 'ag-loopiter-for))
(define ag-loopiter-for-iterator cadr)
;(define-struct ag-loopiter (init doing while step until term))
(define (make-ag-loopiter . args) (cons 'ag-loopiter args))
(define (ag-loopiter? x)(eq? (car x) 'ag-loopiter))
(define ag-loopiter-init cadr)
(define ag-loopiter-doing caddr)
(define ag-loopiter-while cadddr)
(define ag-loopiter-step (lambda (x) (car (cddddr x))))
(define ag-loopiter-until (lambda (x) (cadr (cddddr x))))
(define ag-loopiter-term (lambda (x) (caddr (cddddr x))))
;(define-struct ag-doing (stmts))
(define (make-ag-doing . args) (cons 'ag-doing args))
(define (ag-doing? x)(eq? (car x) 'ag-doing))
(define ag-doing-stmts cadr)
;(define-struct ag-while (condition))
(define (make-ag-while . args) (cons 'ag-while args))
(define (ag-while? x)(eq? (car x) 'ag-while))
(define ag-while-condition cadr)
;(define-struct ag-step (stmts))
(define (make-ag-step . args) (cons 'ag-step args))
(define (ag-step? x)(eq? (car x) 'ag-step))
(define ag-step-stmts cadr)
;(define-struct ag-until (condition))
(define (make-ag-until. args) (cons 'ag-until args))
(define (ag-until? x)(eq? (car x) 'ag-until))
(define ag-until-condition cadr)
;(define-struct ag-term (stmts))
(define (make-ag-term . args) (cons 'ag-term args))
(define (ag-term? x)(eq? (car x) 'ag-term))
(define ag-term-stmts cadr)
;(define-struct ag-formal (type var))
(define (make-ag-formal . args) (cons 'ag-formal args))
(define (ag-formal? x)(eq? (car x) 'ag-formal))
(define ag-proc-type cadr)
(define ag-proc-var caddr)
;(define-struct ag-proc (name args body))
(define (make-ag-proc . args) (cons 'ag-proc args))
(define (ag-proc? x)(eq? (car x) 'ag-proc))
(define ag-proc-name cadr)
(define ag-proc-args caddr)
(define ag-proc-body cadddr)
;(define-struct ag-refine (var stmts))
(define (make-ag-refine . args) (cons 'ag-refine args))
(define (ag-refine? x)(eq? (car x) 'ag-refine))
(define ag-refine-var cadr)
(define ag-refine-stmts caddr)

(define (lhst? x)
  (or (ag-identifier? x) (ag-skip? x)))


(define (ag-number? x)
  (or (ag-real? x)
    (ag-integer? x)))

(define (make-ag-number x)
  (cond ((integer? x) (make-ag-integer x))
    ((real? x) (make-ag-real x))
    (else (error "expects argument of number type: ~a" x))))

(define (ag-number-value x)
  (cond ((ag-integer? x) (ag-integer-value x))
    ((ag-real? x) (ag-real-value x))
    (else (error "expects argument of number type: ~a" x))))

(define (make-ag-tuple lst)
  (define tuple (make-hash-table))
  (define pos 0)
  (set-ag-tuple-highest! tuple pos)
  (for-each (lambda (val)
              (set! pos (+ pos 1))
              (ag-tuple-insert tuple pos val))
    lst)
  tuple)

(define (ag-tuple? x)
  (if  (and (hash-table? x) (ag-tuple-highest x))
    #t
    #f))

(define (ag-tuple-highest tuple)
  (hash-table-get tuple
      'highest
    (lambda ()
      #f)))

(define (set-ag-tuple-highest! tuple pos)
  (hash-table-put! tuple 'highest pos))

(define (ag-tuple-find-new-highest tuple oldhigh)
  (cond ((= oldhigh 0) 0)
    ((ag-omega? (ag-tuple-val tuple (- oldhigh 1)))
      (hash-table-remove! tuple oldhigh)
      (set-ag-tuple-highest! tuple (- oldhigh 1))
      (ag-tuple-find-new-highest tuple (- oldhigh 1)))
    (else
      (hash-table-remove! tuple oldhigh)
      (set-ag-tuple-highest! tuple (- oldhigh 1))
      (- oldhigh 1))))

(define (ag-tuple-delete tuple pos)
  (let ((high (ag-tuple-highest tuple)))
    (cond ((= high 0) #f)
      ((< pos high)
        (hash-table-remove! tuple pos))
      ((= pos high)
        (hash-table-remove! tuple pos)
        (ag-tuple-find-new-highest tuple high)))))

(define (ag-tuple-insert tuple pos val)
  (let ((high (ag-tuple-highest tuple)))
    (cond ((< pos 1) #f)
      ((<= pos high)
        (if (ag-omega? val)
          (ag-tuple-delete tuple pos)
          (hash-table-put! tuple pos val)))
      ((and (> pos high)
         (not (ag-omega? val)))
        (hash-table-put! tuple pos val)
        (set-ag-tuple-highest! tuple pos)))))

(define (ag-tuple-back-insert tuple val)
  (let ((high (ag-tuple-highest tuple)))
    (hash-table-put! tuple (+ high 1) val)
    (set-ag-tuple-highest! tuple (+ high 1))))



(define (ag-tuple-val tuple pos)
  (if (< pos 1)
    (make-ag-omega)
    (hash-table-get tuple
      pos
      (lambda ()
        (make-ag-omega)))))

(define (ag-tuple-empty? tuple)
  (= 0 (ag-tuple-highest tuple)))

(define (ag-tuple-empty)
  (define tuple (make-ag-tuple '()))
  (set-ag-tuple-highest! tuple 0)
  tuple)

(define (tuple-range t bg end)
  (let ((high (ag-tuple-highest t))
         (result (ag-tuple-empty)))
    (define (range pos rsltpos)
      (cond ((> pos high) result)
        ((and (>= pos bg) (<= pos end))
          (ag-tuple-insert result rsltpos (ag-tuple-val t pos))
          (range (+ pos 1) (+ rsltpos 1)))
        (else (range (+ pos 1) rsltpos))))
    (cond  ((ag-tuple-empty? t) t)
      ((> bg end) result)
      ((or (> bg high) (> end high)) #f)
      (else (range 1 1)))))

(define (ag-tuple->list t)
  (let ((high (ag-tuple-highest t)))
    (define (loop pos)
      (if (= pos high)
        (list (ag-tuple-val t pos))
        (cons (ag-tuple-val t pos)
          (loop (+ pos 1)))))
    (if (not (ag-tuple-empty? t))
      (loop 1)
        '())))


(define (ag-tuple-for-each f t)
  (let ((high (ag-tuple-highest t)))
    (define (loop pos)
      (if (= pos high)
        (f (ag-tuple-val t pos))
        (begin
          (f (ag-tuple-val t pos))
          (loop (+ pos 1)))))
    (if (not (ag-tuple-empty? t))
      (loop 1)
      #f)))

(define (ag-tuple-map f t)
  (make-ag-tuple (map f (ag-tuple->list t))))

(define (ag-tuple-insert-in t1 bgn end t2)
  (let ((end1 (+ 1 (ag-tuple-highest t1)))
         (end2 (+ 1 (ag-tuple-highest t2)))
         (rslt (ag-tuple-empty)))

    (define (insert-rest-of-tuple pos pos2)
      (when (< pos2 end2)
        (ag-tuple-insert rslt pos (ag-tuple-val t2 pos2))
        (insert-rest-of-tuple (+ pos 1) (+ pos2 1))))


    (define (insert pos1 pos2 pos)
      (cond ((= pos1 end1) (insert-rest-of-tuple pos pos2))
        ((< pos1 bgn)
          (ag-tuple-insert rslt
            pos
            (ag-tuple-val t1 pos1))
          (insert (+ pos1 1) pos2 (+ pos 1)))
        ((<= pos1 end)
          (if (>= pos2 end2)
            (insert (+ pos1 1) pos2 pos)
            (begin
              (ag-tuple-insert rslt
                pos
                (ag-tuple-val t2 pos2))
              (insert pos1 (+ pos2 1) (+ pos 1)))))
        ((> pos1 end)
          (ag-tuple-insert rslt
            pos
            (ag-tuple-val t1 pos1))
          (insert (+ pos1 1) pos2 (+ pos 1)))))

    (unless (or (> bgn end1 1)
              (> end end1 1))
      (insert 1 1 1))
    rslt))

;;;;;;;;;;;;;;;;;;; SETS

(define (determine-precedence x)
  (define precedences  (list ag-omega?
                         ag-boolean?
                         ag-integer?
                         ag-real?
                         ag-string?
                         ag-tuple?
                         ed-set?))
  (define (loop lst prec)
    (cond ((null? lst)
            (error 'determine-precedence "non-allowed set-element type: ~a" x))
      (((car lst) x) prec)
      (else (loop (cdr lst) (+ prec 1)))))
  (loop precedences 0))

(define (set-element-less? x y)
  (let ((px (determine-precedence x))
         (py (determine-precedence y)))
    (or (< px py)
      (and (= px py)
        (compare-types px x y)))))

(define (compare-types typenbr x y)
  (define types (vector
                  ;; om = om
                  (lambda (x y) #f)
                  ;; true < false
                  (lambda (x y) (and (ag-boolean-value x)
                                  (not (ag-boolean-value y))))
                  (lambda (x y) (< (ag-integer-value x)
                                  (ag-integer-value y)))
                  (lambda (x y) (< (ag-real-value x)
                                  (ag-real-value y)))
                  (lambda (x y) (string<? (ag-string-value x)
                                  (ag-string-value y)))
                  (lambda (x y) (ag-tuple-less? x y))
                  (lambda (x y) (ed-set-less? x y))))
  ((vector-ref types typenbr) x y))


(define (ag-tuple-less? x y)
  (let ((hx (ag-tuple-highest x))
         (hy (ag-tuple-highest y)))

    (define (compare-same-size pos)
      (cond ((= pos 1)
              (set-element-less? (ag-tuple-val x pos)
                (ag-tuple-val y pos)))
        ((set-element-less? (ag-tuple-val x pos)
           (ag-tuple-val y pos)) #t)
        (else (compare-same-size (- pos 1)))))

    (cond ((< hx hy) #t)
      ((= hx hy 0) #f)
      ((= hx hy)
        (compare-same-size hx))
      (else #f))))

(define (ed-set-less? x y)
  (send-message ed-set-manipulations 'less? x y))

(define ed-set-manipulations (tree-manipulations set-element-less?))

(define (make-ed-set contents)
  (send-message ed-set-manipulations 'construct-from-list contents))

(define (ed-set? x)
  (and (vector? x)
    (=  4 (vector-length x))))

(define (ed-set-map function x)
  (send-message ed-set-manipulations 'map x function))

(define (ed-set-for-each function x)
  (send-message ed-set-manipulations 'for-each x function))

(define (ed-set->list s)
  (send-message ed-set-manipulations 'convert-to-list s))

(define (list->ed-set l)
  (send-message ed-set-manipulations 'construct-from-list l))

(define (ed-set-size s)
  (tree-size s))

(define (ed-set-empty)
  (make-ed-set '()))

(define (ed-set-insert s e)
  (send-message ed-set-manipulations 'insert e s))

(define (ed-set-union s1 s2)
  (send-message ed-set-manipulations 'union s1 s2))

(define (ed-set-difference s1 s2)
  (send-message ed-set-manipulations 'difference s1 s2))

(define (ed-set-intersection s1 s2)
  (send-message ed-set-manipulations 'intersection s1 s2))

(define (make-reader scanner)
  (let ((setl #f)
         (token #f)
         (record-tokens #f)
         (recorded-tokens '()))

    (define (read-error message token)
      (send-message setl 'read-error message token))


    (define (read-expected-error . tokens)
      (read-error (format "expected ~a - given ~a"  tokens token) token))

    (define (parse-with-assert-expression) (parse-expression #t))
    (define (parse-without-assert-expression) (parse-expression #f))

    (define (get-token)
      (set! token (send-message scanner 'scan))
      (when (equal? (token-symbol token) err-symbol)
        (read-error (token-data token) token))
      (when record-tokens
        (set! recorded-tokens (cons token recorded-tokens))))

    (define (check-token symbol)
      (equal? (token-symbol token) symbol))

    (define (next-token token)
      (get-token)
      token)

    (define (check-and-get-token symbol)
      (cond ((check-token symbol)
              (get-token)
              #t)
        (else #f)))

    (define (check-and-get-tokens symbollist)
      (if (null? symbollist)
        #f
        (or (check-and-get-token (car symbollist))
          (check-and-get-tokens (cdr symbollist)))))

    (define (expect tkn ifok)
      (if (check-and-get-token tkn)
        (ifok)
        (read-expected-error tkn)))

    (define (start-record)
      (set! record-tokens #t)
      (set! recorded-tokens (list token)))

    (define (stop-record)
      (set! record-tokens #f)
      (reverse recorded-tokens))

    (define (match-recorded-tokens recorded)
      (define (match recorded)
        (cond ((null? recorded)
                (check-token end-symbol))
          ((check-token smc-symbol) #t)
          ((equivalent-tokens? token (car recorded))
            (get-token)
            (match (cdr recorded)))
          (else #f)))
      (unless (match recorded)
        (read-error "non-matching end tokens" token)))

    (define (read s)
      (set! setl s)
      (set! token (send-message scanner 'scan))
      (let ((prg (program)))
        (if (equal? (token-symbol token) end-symbol)
          prg
          (read-error "excess tokens" token))))

    (define (identifier assert)
      (define word (token-data token))
      (if (check-token wrd-symbol)
        (begin
          (get-token)
          (make-ag-identifier (string->symbol word)))
        (if assert
          (read-error "expected identifier" token)
          #f)))

    (define (read-list rule assert defaultconstructor)
      (define nt (rule))
      (if (assert nt)
        (cons nt (read-list rule assert defaultconstructor))
        (defaultconstructor nt)))

    (define (check-for-assignment ifok)
      (if (check-and-get-token ceq-symbol)
        (ifok)
        (read-error "expected assignment token :=" token)))

    (define (after-idf-stmt read-idf)
      (let* ((tkn token)
              (exp1 #f)
              (exp2 #f)
              (check-and-make-range
                (lambda ()
                  (check-for-assignment (lambda ()
                                          (make-ag-stmt-assignment
                                            (make-ag-range read-idf exp1 exp2)
                                            (parse-expression #t)))))))
        (cond ((check-and-get-token ceq-symbol)
                (make-ag-stmt-assignment read-idf (parse-expression #t)))
          ((check-and-get-token lpr-symbol)
            (cond ((check-and-get-token rpr-symbol)
                    (make-ag-stmt-call read-idf '()))
              ((begin
                 (set! exp1 (parse-expression #t))
                 (check-and-get-token range-keyword))
                (cond ((check-and-get-token rpr-symbol)
                        (check-and-make-range))
                  ((begin
                     (set! exp2 (parse-expression #t))
                     (check-and-get-token rpr-symbol))
                    (check-and-make-range))
                  (else
                    (read-error "expected .. or integer" token))))
              ((check-and-get-token rpr-symbol)
                (if (check-and-get-token ceq-symbol)
                  (make-ag-stmt-assignment
                    (make-ag-selection read-idf (list exp1)) (parse-expression #t))
                  (make-ag-stmt-call read-idf (list exp1))))
              ((check-and-get-token com-symbol)
                (let ((lst (cons exp1
                             (read-enumeration
                               parse-with-assert-expression
                               parse-with-assert-expression
                               rpr-symbol))))
                  (if (check-and-get-token ceq-symbol)
                    (make-ag-stmt-assignment (make-ag-selection read-idf lst)
                      (parse-expression #t))
                    (make-ag-stmt-call read-idf lst))))
              (else
                (read-error "expected a range, selection or application" token))))
          ((check-and-get-token lbc-symbol)
            (let ((exp (parse-expression #t)))
              (if (check-and-get-token rbc-symbol)
                (check-for-assignment
                  (lambda ()
                    (make-ag-stmt-assignment (make-ag-mulvalmapref read-idf exp)
                      (parse-expression #t))))
                (read-error
                  "expected end of multi-valued map reference" token))))
          ((check-token smc-symbol)
            (make-ag-stmt-call read-idf '()))
          ((check-and-get-tokens assigning)
            (expect ceq-symbol (lambda ()
                                 (make-ag-stmt-assignment
                                   read-idf
                                   (make-ag-application (token-symbol tkn)
                                     (list read-idf (parse-expression #t)))))))
          ((check-and-get-token from-keyword)
            (make-ag-from read-idf (lhs #t)))
          ((check-and-get-token fromb-keyword)
            (make-ag-fromb read-idf (lhs #t)))
          ((check-and-get-token frome-keyword)
            (make-ag-frome read-idf (lhs #t)))
          (else
            (read-error "expected application or assignment" token)))))

    (define (statements)
      (read-list statement
        (lambda (st)
          (and st (check-and-get-token smc-symbol)))
        (lambda (st)
          (if st
            (list st)
              '()))))

    (define (statement)
      (let ((idf (identifier #f)))
        (if (and idf (check-and-get-token col-symbol))
          (make-ag-statement idf (statement-body #f))
          (let ((body (statement-body idf)))
            (and body
              (make-ag-statement #f body))))))

    (define (statement-body read-idf)
      (define lh #f)
      (cond (read-idf (after-idf-stmt read-idf))
        ((check-token wrd-symbol) (after-idf-stmt (identifier #t)))
        ((begin (set! lh (lhs #f)) lh)
          (let ((tkn token))
            (cond ((check-and-get-token ceq-symbol)
                    (make-ag-stmt-assignment lh (parse-expression #t)))
              ((check-and-get-tokens assigning)
                (expect ceq-symbol
                  (lambda ()
                    (make-ag-stmt-assignment lh
                      (make-ag-application (token-symbol tkn)
                        (list lh (parse-expression #t)))))))
              ((check-and-get-token from-keyword) (make-ag-from lh (lhs #t)))
              ((check-and-get-token fromb-keyword)
                (make-ag-fromb lh (lhs #t)))
              ((check-and-get-token frome-keyword)
                (make-ag-frome lh (lhs #t)))
              (else (read-error "expected assignment" token)))))
        ((check-and-get-token exit-keyword) (make-ag-stmt-exit))
        ((check-and-get-token goto-keyword)
          (let ((idf (identifier #t)))
            (make-ag-stmt-goto idf)))
        ((check-and-get-token pass-keyword) (make-ag-stmt-pass))
        ((check-and-get-token stop-keyword) (make-ag-tmt-stop))
        ((check-token if-keyword) (if-statement))
        ((check-and-get-token return-keyword) (return-statement))
        ((check-token loop-keyword) (loop-statement))
        (else #f)))

    (define (iterlist constructor)
      (define lst (iter-elements))
      (expect sch-symbol
        (lambda () (constructor lst (parse-expression #t)))))

    (define (parse-expression assert)
      (cond ((check-and-get-token exists-keyword)
              (iterlist make-ag-exists))
        ((check-and-get-token notexists-keyword)
          (iterlist make-ag-notexists))
        ((check-and-get-token forall-keyword)
          (iterlist make-ag-forall))
        (else (parse-implication assert))))

    (define (parse-implication assert)
      (parse-op-infix parse-or (list impl-keyword) assert))

    (define (parse-or assert)
      (parse-op-infix parse-and (list or-keyword) assert))

    (define (parse-and assert)
      (parse-op-infix parse-not-is_xx (list and-keyword) assert))

    (define (parse-not-is_xx assert)
      (parse-unary-op parse-tests not-is_xx assert))

    (define (parse-tests assert)
      (parse-op-infix parse-maxmin rop-tokens assert))

    (define (parse-maxmin assert)
      (parse-op-infix parse-additive
        (list max-keyword min-keyword) assert))

    (define (parse-additive assert)
      (parse-op-infix parse-multiplicative
        (list pls-symbol mns-symbol) assert))

    (define (parse-multiplicative assert)
      (parse-op-infix parse-exponentiation
        (list mul-symbol div-symbol div-keyword mod-keyword) assert))

    (define (parse-exponentiation assert)
      (parse-op-infix parse-unary (list xop-symbol) assert))

    (define (parse-unary assert)
      (parse-unary-op parse-assignment unary-keywords assert))

    (define (parse-assignment assert)
      (define ref (parse-reference assert #t))
      (cond ((check-and-get-token ceq-symbol)
              (make-ag-stmt-assignment ref (parse-expression #t)))
        ((check-and-get-token from-keyword)
          (make-ag-from ref (lhs #t)))
        ((check-and-get-token fromb-keyword)
          (make-ag-fromb ref (lhs #t)))
        ((check-and-get-token frome-keyword)
          (make-ag-frome ref (lhs #t)))
        (else ref)))

    (define (parse-reference assert lhs-allowed)
      (define tkn token)
      (cond ((check-and-get-token int-symbol)
              (make-ag-integer (token-data tkn)))
        ((check-and-get-token rea-symbol)
          (make-ag-real (token-data tkn)))
        ((check-and-get-token txt-symbol)
          (make-ag-string (token-data tkn)))
        ((check-and-get-token lpr-symbol) (parentheses))
        ((check-token wrd-symbol) (after-idf-reference))
        ((check-and-get-token true-keyword)	(make-ag-boolean #t))
        ((check-and-get-token false-keyword) (make-ag-boolean #f))
        ((check-and-get-token omega-keyword) (make-ag-omega))
        ((check-and-get-token lbc-symbol) (set-former #f))
        ((and lhs-allowed (check-and-get-token mns-symbol))
          (make-ag-skip))
        ((check-and-get-token lbr-symbol) (tuple-former #f))
        ((check-and-get-token exists-keyword)
          (iterlist make-ag-exists))
        ((check-and-get-token notexists-keyword)
          (iterlist make-ag-notexists))
        ((check-and-get-token forall-keyword)
          (iterlist make-ag-forall))
        (else
          (and assert (read-error "expected a reference token" token)))))


    (define (after-idf-reference)
      (let ((idf (identifier #t))
             (exp1 #f)
             (exp2 #f))
        (cond  ((check-and-get-token lpr-symbol)
                 (cond ((check-and-get-token rpr-symbol)
                         (make-ag-stmt-call idf '()))
                   ((begin (set! exp1 (parse-expression #t))
                      (check-and-get-token range-keyword))
                     (cond ((check-and-get-token rpr-symbol)
                             (make-ag-range idf exp1 #f))
                       ((begin (set! exp2 (parse-expression #t))*
                          (check-and-get-token rpr-symbol))
                         (make-ag-range idf exp1 exp2))
                       (else (read-expected-error
                               int-symbol rpr-symbol))))
                   ((check-and-get-token rpr-symbol)
                     (if (check-token ceq-symbol)
                       (make-ag-selection idf (list exp1))
                       (make-ag-simple-call-or-selection
                         idf (list exp1))))
                   ((check-and-get-token com-symbol)
                     (set! exp2 (cons exp1
                                  (read-enumeration parse-with-assert-expression
                                    parse-with-assert-expression
                                    rpr-symbol)))
                     (make-ag-stmt-call idf exp2))))
          ((check-and-get-token lbc-symbol)
            (set! exp1 (parse-expression #t))
            (expect rbc-symbol
              (lambda () (make-ag-mulvalmapref idf exp))))
          (else idf))))


    (define (set-former expect-cst)
      (former parse-without-assert-expression
        parse-without-assert-expression
        rbc-symbol
        make-ag-set
        (if expect-cst
          (lambda (x y)
            (read-error "set iterators aren't constant" token))
          make-ag-setiterator)
        make-ag-setslice))

    (define (tuple-former expect-cst)
      (former parse-without-assert-expression
        parse-without-assert-expression
        rbr-symbol
        make-ag-tuple
        (if expect-cst
          (lambda (x y)
            (read-error "tuple iterators aren't constant" token))
          make-ag-tupleiterator)
        make-ag-tupleslice))

    (define (former firstrule
              restrule
              endsymbol
              constructor
              itconstructor
              sliceconstructor)
      (let ((exp1 (firstrule))
             (exp2 #f)
             (exp3 #f)
             (it #f))
        (cond ((and (not exp1) (check-and-get-token endsymbol))
                (constructor '()))
          ((check-and-get-token endsymbol)
            (constructor (list exp1)))
          ((check-and-get-token col-symbol)
            (set! it (iterator))
            (expect endsymbol (lambda () (itconstructor exp1 it))))
          ((check-and-get-token range-keyword)
            (set! exp2 (firstrule))
            (expect endsymbol
              (lambda () (sliceconstructor #f exp1 exp2))))
          ((check-and-get-token com-symbol)
            (set! exp2 (firstrule))
            (cond ((check-and-get-token endsymbol)
                    (constructor (list exp1 exp2)))
              ((check-and-get-token range-keyword)
                (set! exp3 (firstrule))
                (expect endsymbol
                  (lambda () (sliceconstructor exp1 exp2 exp3))))
              ((check-and-get-token com-symbol)
                (constructor (cons exp1
                               (cons exp2
                                 (read-enumeration firstrule
                                   restrule
                                   endsymbol)))))
              (else (read-error
                      "not a valid container former" token))))
          (else (read-error "not a valid container former" token)))))

    (define (iterator)
      (define lst (iter-elements))
      (if (check-and-get-token sch-symbol)
        (make-ag-iterator lst (parse-expression #t))
        (make-ag-iterator lst (make-ag-boolean #t))))

    (define (iter-elements)
      (read-list (lambda () (iter-element #t))
        (lambda (itel) (and itel (check-and-get-token com-symbol)))
        (lambda (itel) (if itel (list itel) '()))))

    (define (iter-element assert)
      (define lefthand (lhs assert))
      (cond ((check-and-get-token in-keyword)
              (make-ag-iterelement-in lefthand (parse-expression #t)))
        ((check-and-get-token eql-symbol)
          (let ((idf (identifier #t)))
            (cond ((check-and-get-token lpr-symbol)
                    (make-ag-iterelement-selection
                      lefthand idf (read-enumeration
                                     (lambda () (lhs #t))
                                     (lambda () (lhs #f))
                                     rpr-symbol)))
              ((check-and-get-token lbc-symbol)
                (make-ag-iterelement-sselection
                  lefthand idf (read-enumeration (lambda () (lhs #t))
                                 (lambda () (lhs #f))
                                 rbc-symbol)))
              (else (read-expected-error lpr-symbol lbc-symbol)))))
        (assert (read-error "expected an iteration element" token))
        (else #f)))

    (define (lefthand rule empty-arglist)
      (define lh (rule))
      (cond ((check-and-get-token lpr-symbol)
              (if (check-and-get-token rpr-symbol)
                (empty-arglist lh)
                (former (lambda () (parse-reference #t #t))
                  (lambda () (parse-reference #f #t))
                  lpr-symbol
                  (lambda (s) (make-ag-selection lh s))
                  (lambda (e i)
                    (read-error "iterator construct not allowed" token))
                  (lambda (first bgn end)
                    (if first
                      (read-error "invalid range" token)
                      (make-ag-range lh bgn end))))))
        ((check-and-get-token lbc-symbol)
          (make-ag-sselection lh
            (read-enumeration parse-with-assert-expression
              parse-without-assert-expression
              rbc-symbol)))
        (else lh)))

    (define (lhs assert)
      (cond ((check-token wrd-symbol)
              (lefthand (lambda () (identifier #t))
                (lambda (x)
                  (read-error
                    "selection should contain at least one expression" token))))
        ((check-and-get-token lbr-symbol)
          (lefthand (lambda ()
                      (make-ag-tuple (read-enumeration (lambda ()
                                                         (lhst #t))
                                       (lambda ()
                                         (lhst #f))
                                       rbr-symbol)))
            (lambda (x)
              (read-error "selection should contain at least one expression" token))))
        (assert (read-error "expected a left hand side" token))
        (else #f)))

    (define (lhst assert)
      (define lefthand (lhs #f))
      (cond (lefthand lefthand)
        ((check-and-get-token mns-symbol) (make-ag-skip))
        (assert (read-error "expected a left hand side or -" token))
        (else #f)))

    (define (parse-op-infix rule tokenlist assert)
      (define args (rule assert))
      (define argscopy args)
      (define (loop)
        (if (member (token-symbol token) tokenlist)
          (let* ((operator (token-symbol (next-token token)))
                  (assign (check-and-get-token ceq-symbol))
                  (arg (list args (rule assert))))
            (if assign
              (set! args
                (make-ag-stmt-assignment argscopy (make-ag-application operator arg)))
              (set! args (make-ag-application operator arg)))
            (loop))))
      (loop)
      args)

    (define (parse-unary-op rule tokenlist assert)
      (define (loop)
        (if (member (token-symbol token) tokenlist)
          (make-ag-application (token-symbol (next-token token))
            (list (loop)))
          (rule assert)))
      (loop))

    (define (parentheses)
      (let ((exp (parse-expression #t)))
        (expect rpr-symbol (lambda () exp))))

    (define (end-of-statement)
      (expect smc-symbol 'void))


    (define (elseif)
      (and (check-and-get-token elseif-keyword)
        (make-ag-elseif (parse-expression #t)
          (expect then-keyword statements))))


    (define (elseif-list)
      (read-list elseif (lambda (x) (and x (check-token elseif-keyword)))
        (lambda (x) (if x (list x) '()))))

    (define (if-statement)
      (let ((if-cond #f)
             (if-then '())
             (if-elseif '())
             (if-else '())
             (rec #f))

        (define (parse-elseif)
          (when (check-token elseif-keyword)
            (set! if-elseif (elseif-list)))
          (parse-else))

        (define (parse-else)
          (when (check-and-get-token else-keyword)
            (set! if-else (statements)))
          (parse-end))

        (define (parse-end)
          (end rec)
          (make-ag-stmt-if if-cond if-then if-elseif if-else))

        (start-record)
        (get-token)
        (set! if-cond (parse-expression #t))
        (set! rec (stop-record))
        (expect then-keyword (lambda ()
                               (set! if-then (statements))
                               (parse-elseif)))))

    (define (loop-statement)
      (let ((iter #f)
             (stmts #f)
             (rec #f))
        (start-record)
        (get-token)
        (set! iter (loopiter))
        (set! rec (stop-record))
        (expect do-keyword (lambda ()
                             (set! stmts (statements))
                             (end rec)
                             (make-ag-stmt-loop iter stmts)))))

    (define (loopiter)
      (if (check-and-get-token for-keyword)
        (make-ag-loopiter-for (iterator))
        (make-ag-loopiter
          (if (check-and-get-token init-keyword)
            (make-ag-init (statements)) #f)
          (if (check-and-get-token doing-keyword)
            (make-ag-doing (statements)) #f)
          (if (check-and-get-token while-keyword)
            (make-ag-while (parse-expression #t)) #f)
          (if (check-and-get-token step-keyword)
            (make-ag-step (statements)) #f)
          (if (check-and-get-token until-keyword)
            (make-ag-until (parse-expression #t)) #f)
          (if (check-and-get-token term-keyword)
            (make-ag-term (statements)) #f))))

    (define (return-statement)
      (define exp (parse-expression #f))
      (make-ag-stmt-return exp))


    (define (proclist)
      (read-list proc
        (lambda (proc) (and proc (check-and-get-token smc-symbol)))
        (lambda (proc)
          (if proc
            (list proc)
              '()))))

    (define (proc)
      (start-record)
      (if (not (check-and-get-token proc-keyword))
        (begin (stop-record) #f)
        (let ((procname (identifier #t))
               (arglist '())
               (bdy '())
               (rec #f))
          (when (check-and-get-token lpr-symbol)
            (set! arglist (read-enumeration formal formal rpr-symbol)))
          (set! rec (stop-record))
          (expect smc-symbol (lambda ()
                               ; (set! rec (stop-record))
                               (set! bdy (body))
                               (end rec)
                               (make-ag-proc procname arglist bdy))))))

    (define (formal)
      (define type
        (cond ((check-token rd-keyword) (next-token (token-symbol token)))
          ((check-token rw-keyword) (next-token (token-symbol token)))
          ((check-token wr-keyword) (next-token (token-symbol token)))
          (else #f)))
      (make-ag-formal type (identifier #t)))

    (define (refine)
      (define idf (identifier #f))
      (if (not idf)
        #f
        (expect dcl-symbol (lambda ()
                             (make-ag-refine idf (statements))))))

    (define (refine-list)
      (read-list refine
        (lambda (rf)
          (and rf (check-and-get-token smc-symbol)))
        (lambda (rf)
          (if rf
            (list rf)
              '()))))

    (define (var-decl)
      (define var-decl-list (read-list (lambda ()
                                         (identifier #t))
                              (lambda (x)
                                (check-and-get-token com-symbol))
                              list))
      (end-of-statement)
      var-decl-list)

    (define (const-decl-item)
      (define idf (identifier #t))
      (if (check-and-get-token eql-symbol)
        (let ((cst (constant)))
          (make-ag-constdeclaration idf cst))
        idf))

    (define (init-decl)
      (define init-decl-list (read-list init-decl-item
                               (lambda (x)
                                 (check-and-get-token com-symbol))
                               list))
      (end-of-statement)
      init-decl-list)

    (define (init-decl-item)
      (define idf (identifier #t))
      (expect ceq-symbol (lambda ()
                           (define cst (constant))
                           (make-ag-initdeclaration idf cst))))

    (define (read-enumeration firstrule restrule last-token)
      (define first (firstrule))
      (if first
        (if (check-and-get-token last-token)
          (list first)
          (expect com-symbol (lambda ()
                               (define rest
                                 (read-list restrule
                                   (lambda (x)
                                     (check-and-get-token com-symbol))
                                   list))
                               (expect last-token (lambda () (cons first rest))))))
        (expect last-token (lambda () '()))))

    (define (signed-number mns)
      (define (make-signed-int)
        (if mns
          (make-ag-integer (- (token-data token)))
          (make-ag-integer (token-data token))))
      (define (make-signed-real)
        (if mns
          (make-ag-real (- (token-data token)))
          (make-ag-real (token-data token))))
      (cond ((check-and-get-token int-symbol) (make-signed-int))
        ((check-and-get-token rea-symbol) (make-signed-real))
        (else (read-expected-error int-symbol rea-symbol))))


    (define (cst)
      (define tkn token)
      (cond ((check-and-get-token pls-symbol) (signed-number #f))
        ((check-and-get-token mns-symbol) (signed-number #t))
        ((check-and-get-token int-symbol)
          (make-ag-integer (token-data tkn)))
        ((check-and-get-token rea-symbol)
          (make-ag-real (token-data tkn)))
        ((check-and-get-token txt-symbol)
          (make-ag-string (token-data tkn)))
        ((check-and-get-token lbc-symbol) (set-former #t))
        ((check-and-get-token lbr-symbol) (tuple-former #t))
        (else #f)))

    (define (constant)
      (or (cst) (read-error "not a constant" token)))

    (define (const-decl)
      (define const-decl-list
        (read-list const-decl-item
          (lambda (x)
            (check-and-get-token com-symbol))
          list))
      (end-of-statement)
      const-decl-list)

    (define (declarations var const init)
      (cond ((check-and-get-token var-keyword)
              (if (null? var)
                (declarations (var-decl) const init)
                (read-error
                  "duplicate global variable declarations" token)))
        ((check-and-get-token const-keyword)
          (if (null? const)
            (declarations var (const-decl) init)
            (read-error
              "duplicate global constant declarations" token)))
        ((check-and-get-token init-keyword)
          (if (null? init)
            (declarations var const (init-decl))
            (read-error "duplicate global init declarations" token)))
        (else (make-ag-declarations var const init))))

    (define (end rec)
      (expect end-keyword (lambda ()
                            (match-recorded-tokens rec))))

    (define (body)
      (let ((decl (declarations '() '() '()))
             (stmt (statements)))
        (make-ag-body decl stmt #f)))

    (define (program-body)
      (let ((decl (declarations '() '() '()))
             (stmt (statements))
             (procs (proclist))
             (refs (refine-list)))
        (make-ag-programbody decl stmt procs refs)))

    (define (program)
      (let ((rec #f))
        (define (program-name)
          (define idf (identifier #t))
          (set! rec (stop-record))
          (end-of-statement)
          idf)

        (start-record)
        (expect program-keyword (lambda ()
                                  (define name (program-name))
                                  (define bdy (program-body))
                                  (end rec)
                                  (check-and-get-token smc-symbol)
                                  (make-ag-program name bdy)))))

    (define (dispatch message)
      (case message
        ((read) read)
        (else (error 'parser "unknown message"))))

    dispatch))

;; Environments
;; ============


;; Creating

(define make-environment make-hash-table)

;; Printing environments, useful for debugging
(define (print-environment env)
  (hash-table-for-each env (lambda (v k)
                             (out v " - " k)
                             (newline))))


;; Predicate to see if a variable is defined in the given environment
(define (defined-in-env? env idf)
  (define name (ag-identifier-name idf))
  (hash-table-get env name (lambda () #f)))

;; Add a variable binding to the environment
(define (add-to-environment! env idf val cst onduplicate)
  (define name (ag-identifier-name idf))
  (if (defined-in-env? env idf)
    (onduplicate name)
    (hash-table-put! env name (cons val cst))))

;; Returns the environment in which the variable is defined
(define (find-env env glob idf)
  (cond ((defined-in-env? env idf) env)
    ((defined-in-env? glob idf) glob)
    (else #f)))

;; Alter the value of a variable in the environment
(define (set-variable-value! env glob idf val onconstant)
  (let ((found (find-env glob env idf))
         (name (ag-identifier-name idf)))
    (cond ((not found) (hash-table-put! env name (cons val #f)) val)
      ((cdr (defined-in-env? found idf)) (onconstant name))
      (else (hash-table-put! found name (cons val #f)) val))))

;; Returns the value of a variable
(define (get-variable-value env glob idf onunknown)
  (let ((found (find-env env glob idf))
         (name (ag-identifier-name idf)))
    (if (not found)
      (onunknown name)
      (hash-table-get found name))))



(define (make-natives type-check-function report-eval-error)
  (let ((operators (make-hash-table))
         (scheme-procs (make-hash-table))
         (type type-check-function)
         (eval-error report-eval-error)
         (setl #f))


    ;; Procedures

    (define (scheme-proc? idf)
      (hash-table-get scheme-procs
        (ag-identifier-name idf) (lambda () #f)))

    (define (add-scheme-proc name function)
      (hash-table-put! scheme-procs name function))

    (define (install-scheme-procs)
      (add-scheme-proc 'print
        (lambda (exp)
          (send-message setl 'print (print-ag exp)))))

    (define (apply-scheme-proc idf actual)
      (let ((name (ag-identifier-name idf)))
        (apply (hash-table-get scheme-procs
                 name
                 (lambda ()
                   (eval-error
                     (format "unimplemented native procedure: ~a" name))))
          actual)))

    ;; Operators

    (define (add-operator keyword function)
      (hash-table-put! operators keyword function))

    (define (install-operators)
      (add-operator pls-symbol plus)
      (add-operator ceil-keyword ceil)
      (add-operator div-keyword div)
      (add-operator mns-symbol minus)
      (add-operator mul-symbol mul)
      (add-operator div-symbol division)
      (add-operator xop-symbol exponentiation)
      (add-operator eql-symbol equal)
      (add-operator dif-symbol different)
      (add-operator lss-symbol less)
      (add-operator and-keyword logand)
      (add-operator or-keyword logor)
      (add-operator not-keyword lognot)
      (add-operator els-symbol equal-or-less)
      (add-operator grt-symbol greater)
      (add-operator egr-symbol equal-or-greater)
      (add-operator impl-keyword implication)
      (add-operator in-keyword in)
      (add-operator notin-keyword notin)
      (add-operator max-keyword smax)
      (add-operator min-keyword smin)
      (add-operator mod-keyword mod)
      (add-operator amt-symbol amount)
      (add-operator is_tuple-keyword is_tuple)
      (add-operator is_boolean-keyword is_boolean)
      (add-operator is_integer-keyword is_integer)
      (add-operator is_set-keyword is_set)
      (add-operator is_real-keyword is_real)
      (add-operator is_string-keyword is_string))



    (define (apply-operator keyword args env)
      (apply (hash-table-get operators
               keyword
               (lambda ()
                 (eval-error
                   (format "unimplemented native: ~a" keyword))))
        args))

    ;; Operator tools

    (define (assure-length lst len)
      (unless (= (length lst) len)
        (eval-error
          (format "non-matching argument count: ~a ~a" len lst))))

    (define (assure-pos-int int)
      (type ag-integer? int)
      (unless (>= (ag-integer-value int) 0)
        (eval-error (format "non-positive integer ~a" int))))

    (define (assure-pos-real int)
      (type ag-real? int)
      (unless (>= (ag-real-value int) 0)
        (eval-error (format "non-positive real ~a" int))))

    (define (assure-non-zero-int int)
      (type ag-integer? int)
      (when (= (ag-integer-value int) 0)
        (eval-error
          (format "integer must be different from zero ~a" int))))

    (define (assure-non-zero-real real)
      (type ag-real? real)
      (when (= (ag-real-value real) 0)
        (eval-error (format "real must be different from zero ~a" int))))

    (define (operator-unary args check constructor accessor function)
      (assure-length args 1)
      (let ((arg (car args)))
        (type check arg)
        (constructor (function (accessor arg)))))

    (define (operator-binary args
              check1
              check2
              constructor
              accessor1
              accessor2
              function)
      (assure-length args 2)
      (let ((first (car args))
             (second (cadr args)))
        (type check1 first)
        (type check2 second)
        (constructor (function (accessor1 first)
                       (accessor2 second)))))


    ;; Operator definitions

    (define (mod . args)
      (operator-binary args
        ag-integer?
        ag-integer?
        make-ag-integer
        ag-integer-value
        ag-integer-value
        (lambda (x y) (modulo x y))))

    (define (logand . args)
      (operator-binary args
        ag-boolean?
        ag-boolean?
        make-ag-boolean
        ag-boolean-value
        ag-boolean-value
        (lambda (x y) (and x y))))

    (define (logor . args)
      (operator-binary args
        ag-boolean?
        ag-boolean?
        make-ag-boolean
        ag-boolean-value
        ag-boolean-value
        (lambda (x y) (or x y))))

    (define (different . args)
      (operator-binary args
        identity
        identity
        make-ag-boolean
        identity
        identity
        (lambda (x y)
          (not (equal? x y)))))

    (define (equal . args)
      (assure-length args 2)
      (let ((first (car args))
             (second (cadr args)))
        (cond ((ag-integer? first)
                (make-ag-boolean (and (ag-integer? second)
                                   (= (ag-integer-value first)
                                     (ag-integer-value second)))))
          ((ag-real? first)
            (make-ag-boolean (and (ag-real? second)
                               (= (ag-real-value first)
                                 (ag-real-value second)))))
          ((ag-string? first)
            (make-ag-boolean (and (ag-string? second)
                               (equal? (ag-string-value first)
                                 (ag-string-value second)))))
          ((ag-boolean? first)
            (make-ag-boolean (and (ag-boolean? second)
                               (equal? (ag-boolean-value first)
                                 (ag-boolean-value second)))))
          ((ag-omega? first)
            (make-ag-boolean (ag-omega? second)))
          (else (make-ag-boolean #f)))))


    (define (less . args)
      (assure-length args 2)
      (let ((first (car args))
             (second (cadr args)))
        (cond ((ag-integer? first)
                (type ag-integer? second)
                (make-ag-boolean (< (ag-integer-value first)
                                   (ag-integer-value second))))
          ((ag-real? first)
            (type ag-real? second)
            (make-ag-boolean (< (ag-real-value first)
                               (ag-real-value second))))
          ((ag-string? first)
            (type ag-string? second)
            (make-ag-boolean (string<? (ag-string-value first)
                               (ag-string-value second))))
          (else
            (eval-error (format "invalid argument types for <: ~a" args))))))

    (define (amount . args)
      (assure-length args 1)
      (let ((arg (car args)))
        (make-ag-integer
          (cond ((ed-set? arg) (ed-set-size arg))
            ((ag-string? arg) (string-length (ag-string-value arg)))
            ((ag-tuple? arg) (ag-tuple-highest arg))
            (else
              (eval-error
                (format "invalid argument types for #: ~a" args)))))))

    (define (lognot . args)
      (operator-unary args
        ag-boolean?
        make-ag-boolean
        ag-boolean-value
        (lambda (x) (not x))))

    (define (is_tuple . args)
      (operator-unary args
        identity
        make-ag-boolean
        identity
        (lambda (x) (ag-tuple? x))))

    (define (is_boolean . args)
      (operator-unary args
        identity
        make-ag-boolean
        identity
        (lambda (x) (ag-boolean? x))))

    (define (is_integer . args)
      (operator-unary args
        identity
        make-ag-boolean
        identity
        (lambda (x) (ag-integer? x))))

    (define (is_set . args)
      (operator-unary args
        identity
        make-ag-boolean
        identity
        (lambda (x) (ed-set? x))))

    (define (is_real . args)
      (operator-unary args
        identity
        make-ag-boolean
        identity
        (lambda (x) (ag-real? x))))

    (define (is_string . args)
      (operator-unary args
        identity
        make-ag-boolean
        identity
        (lambda (x) (ag-string? x))))


    (define (equal-or-less . args)
      (logor (apply equal args)
        (apply less args)))

    (define (greater . args)
      (lognot (apply equal-or-less args)))

    (define (equal-or-greater . args)
      (lognot (apply less args)))

    (define (implication . args)
      (operator-binary args
        ag-boolean?
        ag-boolean?
        make-ag-boolean
        ag-boolean-value
        ag-boolean-value
        (lambda (x y) (or (not x) y))))

    (define (ceil . args)
      (operator-unary args ag-real? make-ag-real ag-real-value ceiling))

    (define (div . args)
      (operator-binary args
        (lambda (x) (type ag-number? x))
        (lambda (x)
          (type ag-number? x)
          (unless (not (= 0 (ag-number-value x)))
            (eval-error "division by zero")))
        make-ag-number
        ag-number-value
        ag-number-value
        quotient))


    (define (minus . args)
      (assure-length args 2)
      (let ((first (car args))
             (second (cadr args)))
        (cond ((ag-number? first)
                (type ag-number? second)
                (make-ag-number (- (ag-number-value first)
                                  (ag-number-value second))))
          ((ed-set? first)
            (type ed-set? second)
            (ed-set-difference first second))
          (else
            (eval-error (format "wrong argument types for -: ~a" args))))))

    (define (plus . args)
      (assure-length args 2)
      (let ((first (car args))
             (second (cadr args)))
        (cond ((ag-number? first)
                (type ag-number? second)
                (make-ag-number (+ (ag-number-value first)
                                  (ag-number-value second))))
          ((ag-string? first)
            (when (ag-number? second)
              (set! second
                (make-ag-string (number->string (ag-number-value second)))))
            (type ag-string? second)
            (make-ag-string (string-append (ag-string-value first)
                              (ag-string-value second))))
          ((ag-tuple? first)
            (type ag-tuple? second)
            (make-ag-tuple (append (ag-tuple->list first)
                             (ag-tuple->list second))))
          ((ed-set? first)
            (type ed-set? second)
            (ed-set-union first second))

          (else
            (eval-error (format "wrong argument types for +: ~a" args))))))

    (define (mul . args)
      (define (mul-string s i)
        (assure-pos-int i)
        (make-ag-string  (duplicate-string (ag-string-value s)
                           (ag-integer-value i))))
      (define (mul-tuple t i)
        (assure-pos-int i)
        (make-ag-tuple (duplicate-list (ag-tuple-contents t)
                         (ag-integer-value i))))
      (assure-length args 2)
      (let ((first (car args))
             (second (cadr args)))
        (cond ((ag-integer? first)
                (cond ((ag-number? second)
                        (make-ag-number (* (ag-number-value first)
                                          (ag-number-value second))))
                  ((ag-string? second) (mul-string second first))
                  ((ag-tuple? second) (mul-tuple second first))
                  (else
                    (eval-error (format "wrong argument types for *: ~a" args)))))
          ((ag-string? first) (mul-string first second))
          ((ag-tuple? first) (mul-tuple first second))
          ((ed-set? first)
            (type ed-set? second)
            (ed-set-intersection first second))
          (else
            (eval-error (format "wrong argument types for *: ~a" args))))))

    (define ( division . args)
      (assure-length args 2)
      (let ((first (car args))
             (second (cadr args)))
        (cond ((ag-integer? first)
                (assure-non-zero-int second)
                (make-ag-real (/ (ag-integer-value first)
                                (ag-integer-value second)))))))

    (define (exponentiation  . args)
      (assure-length args 2)
      (let ((first (car args))
             (second (cadr args)))
        (cond ((ag-integer? first)
                (assure-pos-int second)
                (make-ag-integer (expt (ag-integer-value first)
                                   (ag-integer-value second))))
          ((ag-real? first)
            (cond ((ag-real? second)
                    (assure-pos-real second)
                    (make-ag-real (expt (ag-real-value first)
                                    (ag-real-value second))))
              ((ag-integer? second)
                (assure-pos-int second)
                (make-ag-real (expt (ag-real-value first)
                                (ag-integer-value second))))
              (else
                (eval-error (format "wrong argument types for /: ~a" args)))))
          (else
            (eval-error (format "wrong argument types for /: ~a" args))))))

    (define (in . args)
      (assure-length args 2)
      (let ((first (car args))
             (second (cadr args)))
        (cond ((and (ag-string? first) (ag-string? second))
                (make-ag-boolean
                  (sublist (string->list (ag-string-value first))
                    (string->list (ag-string-value second)))))
          ((ag-tuple? second)
            (make-ag-boolean
              (if (member first (ag-tuple-contents second)) #t #f)))
          (else
            (eval-error (format "wrong argument types for in: ~a" args))))))

    (define (notin . args)
      (lognot (apply in args)))

    (define (smax . args)
      (assure-length args 2)
      (let ((first (car args))
             (second (cadr args)))
        (cond ((ag-integer? first)
                (type ag-integer? second)
                (make-ag-integer (max (ag-integer-value first)
                                   (ag-integer-value second))))
          ((ag-real? first)
            (type ag-real? second)
            (make-ag-real (max (ag-real-value first)
                            (ag-real-value second))))
          (else
            (eval-error (format "wrong argument types for max: ~a" args))))))


    (define (smin . args)
      (assure-length args 2)
      (let ((first (car args))
             (second (cadr args)))
        (cond ((ag-integer? first)
                (type ag-integer? second)
                (make-ag-integer (min (ag-integer-value first)
                                   (ag-integer-value second))))
          ((ag-real? first)
            (type ag-real? second)
            (make-ag-real (min (ag-real-value first)
                            (ag-real-value second))))
          (else
            (eval-error (format "wrong argument types for min: ~a" args))))))


    (define (initialize s)
      (set! setl s)
      (install-scheme-procs)
      (install-operators))

    (define (dispatch message)
      (case message
        ((initialize) initialize)
        ((scheme-proc?) scheme-proc?)
        ((apply-scheme-proc) apply-scheme-proc)
        ((apply-operator) apply-operator)
        (else (error 'make-natives "unknown message ~a" message))))

    dispatch))


(define (print-tuple t)
  (let ((high (ag-tuple-highest t)))
    (define (ag-tuple-print t)
      (let loop ((pos 1))
        (if (< pos high)
          (string-append (print-ag (ag-tuple-val t pos))
            " "
            (loop (+ pos 1)))
          (print-ag (ag-tuple-val t pos)))))
    (string-append "["
      (if (ag-tuple-empty? t)
        " "
        (ag-tuple-print t))
      "]")))

(define (print-set s)
  (define str "")
  (ed-set-for-each (lambda (x)
                     (set! str (string-append (print-ag x) " " str)))
    s)
  (string-append "{ "
    str
    "}"))

(define (print-omega o)
  "om")


(define (print-ag ag)
  (cond ((list? ag) (for-each print-ag ag))
    ((ag-integer? ag) (number->string (ag-integer-value ag)))
    ((ag-real? ag) (number->string (ag-real-value ag)))
    ((ag-string? ag) (ag-string-value ag))
    ((ag-tuple? ag) (print-tuple ag))
    ((ed-set? ag) (print-set ag))
    ((ag-omega? ag) (print-omega ag))
    (else (format "<~a>" ag))))

;; Main evaluator

(define (make-evaluator)
  (let ((setl #f) ;; shared SETL session
         ;; ag-program properties
         (name #f)
         (body #f)
         (declarations #f)
         (statements #f)
         (refines #f)
         (routines #f)
         (decvar #f)
         (decconst #f)
         (decinit #f)
         ;; Environment & natives
         (natives #f)
         (global #f))


    ;; Auxiliary functions
    (define (type t exp)
      (unless (t exp)
        (eval-error (format "wrong type of expression: ~a" exp))))

    (define (err-duplicate name)
      (eval-error (format "duplicate variable: ~a" name)))

    (define (err-constant name)
      (eval-error (format "assignment to constant variable: ~a" name)))

    (define (err-unknown name)
      (eval-error (format "unknown variable: ~a" name)))


    (define (eval-error message)
      (send-message setl 'eval-error message))

    ;; initialize
    (define (init program s)
      (set! setl s)
      (set! name (ag-program-name program))
      (set! body (ag-program-body program))
      (set! declarations (ag-programbody-declarations body))
      (set! statements (ag-programbody-statements body))
      (set! refines (ag-programbody-refines body))
      (set! routines (ag-programbody-routines body))
      (set! decvar (ag-declarations-var declarations))
      (set! decconst (ag-declarations-const declarations))
      (set! decinit (ag-declarations-init declarations))
      (set! global (make-environment))
      (set! natives (make-natives type eval-error))
      (send-message natives 'initialize setl))

    (define (process-declarations var const init env)
      ;; add the global variables to the environment
      (for-each
        (lambda (idf)
          (add-to-environment! env
            idf
            (make-ag-omega)
            #f
            err-duplicate))
        var)
      ;; add all constants
      (for-each
        (lambda (constvar)
          (add-to-environment! env
            (ag-constdeclaration-var constvar)
            (ag-constdeclaration-constant constvar)
            #t
            err-duplicate))
        const)
      ;; add all init variables
      (for-each
        (lambda (initvar)
          (add-to-environment! env
            (ag-initdeclaration-var initvar)
            (evaluate (ag-initdeclaration-init initvar) env)
            #f
            err-duplicate))
        init))
    ;; procedures
    (define (process-routines procs)
      (for-each
        (lambda (proc)
          (add-to-environment!
            global (ag-proc-name proc) proc  #t err-duplicate))
        procs))

    (define (eval-program program setl)
      ;; initialize
      (init program setl)
      (process-declarations decvar decconst decinit global)
      (process-routines routines)
      ;; evaluate each statement in the global environment
      (for-each
        (lambda (statement)
          (evaluate (ag-statement-body statement) global))
        statements))

    (define (self-evaluating? exp)
      (or (ag-integer? exp)
        (ag-real? exp)
        (ag-string? exp)
        (ag-omega? exp)
        (ag-proc? exp)
        (ag-stmt-return? exp)
        (ag-boolean? exp)
        (ed-set? exp)))

    (define (evaluate exp env)
      (cond ((self-evaluating? exp) exp)
        ((ag-stmt-assignment? exp) (eval-stmt-assignment exp env))
        ((ag-identifier? exp)
          (car (get-variable-value env global exp err-unknown)))
        ((ag-tupleslice? exp) (eval-simple-tuple-former exp env))
        ((ag-setiterator? exp) (eval-set-former exp env))
        ((ag-tupleiterator? exp) (eval-tuple-former exp env))
        ((ag-setslice? exp) (eval-simple-set-former exp env))
        ((ag-range? exp) (eval-range (ag-range-var exp)
                           (ag-range-begin exp)
                           (ag-range-end exp)
                           env))
        ((ag-stmt-call? exp) (eval-call exp env))
        ((ag-simple-call-or-selection? exp)
          (eval-simple-call-or-selection exp env))
        ((ag-application? exp) (eval-application exp env))
        ((ag-stmt-if? exp) (eval-if exp env))
        ((ag-exists? exp) (eval-exists exp env))
        ((ag-notexists? exp) (eval-notexists exp env))
        ((ag-forall? exp) (eval-forall exp env))
        ((ag-tuple? exp)
          (ag-tuple-map (lambda (exp) (evaluate exp env)) exp))
        ((ag-set? exp)
          (make-ed-set (map (lambda (exp)
                              (evaluate exp env))
                         (ag-set-contents exp))))
        ((ag-fromb? exp) (eval-ag-fromb exp env))
        ((ag-frome? exp) (eval-ag-frome exp env))
        ((ag-from? exp) (eval-ag-from exp env))
        ((ag-stmt-loop? exp) (eval-loop exp env))
        (else   (eval-error
                  (format "unimplemented statement or expression: ~a" exp)))))

    (define (true? exp)
      (equal? (ag-boolean-value exp) #t))

    ;; Evalueert een sequence
    ;; Wanneer deze leeg is, wordt er omega teruggegeven.
    ;; Als eval-return #t is, wordt een return statement ge�valueerd (procs)
    (define (eval-sequence exp env eval-return)
      (if (null? exp)
        (make-ag-omega)
        (let ((rslt (evaluate (ag-statement-body (car exp)) env)))
          (cond ((and eval-return (ag-stmt-return? rslt))
                  (evaluate (ag-stmt-return-val rslt) env))
            ((ag-stmt-return? rslt) rslt)
            ((null? (cdr exp)) (make-ag-omega))
            (else (eval-sequence (cdr exp) env eval-return))))))

    (define (eval-if exp env)
      (if (true? (evaluate (ag-stmt-if-condition exp) env))
        (eval-sequence (ag-stmt-if-then exp) env #f)
        (eval-sequence  (ag-stmt-if-else exp) env #f)))

    ;; Operators
    (define (eval-application exp env)
      (let* ((op (ag-application-operator exp))
              (operands (ag-application-operands exp))
              (args (map (lambda (operand)
                           (evaluate operand env))
                      operands)))
        (send-message natives
            'apply-operator
          op
          args
          env)))
    ;; Applications (user-defined or SETL-supplied like print)
    (define (eval-call exp env)
      (let ((name (ag-stmt-call-var exp)))
        (if (not (send-message natives 'scheme-proc? name))
          (apply-proc exp env)
          (send-message natives
              'apply-scheme-proc
            name
            (map (lambda (arg)
                   (evaluate arg env))
              (ag-stmt-call-args exp))))))

    ;; User-defined procedure
    (define (apply-proc exp env)
      (let* ((proc (evaluate (ag-stmt-call-var exp) env))
              (formal (ag-proc-args proc))
              (body (ag-proc-body proc))
              (declarations (ag-body-declarations body))
              (vardec (ag-declarations-var declarations))
              (constdec (ag-declarations-const declarations))
              (initdec (ag-declarations-init declarations))
              (stmts (ag-body-statements body))
              (refines (ag-body-refines body))
              (actual (ag-stmt-call-args exp))
              (procenv (make-environment)))
        (if (not (= (length formal) (length actual)))
          (eval-error
            (format "non-matching argument list: ~a ~a" formal actual))
          (for-each (lambda (f a)
                      (type ag-formal? f)
                      (set-variable-value! procenv
                        global
                        (ag-formal-var f)
                        (evaluate a env)
                        err-constant))
            formal
            actual))
        (process-declarations vardec constdec initdec procenv)
        (eval-sequence stmts procenv #t)))


    ;; The reader is unable to decide about the nature of a(2)
    (define (eval-simple-call-or-selection exp env)
      (let ((val (evaluate (ag-simple-call-or-selection-var exp) env)))
        (if (not (ag-proc? val))
          (evaluate
            (make-ag-range (ag-simple-call-or-selection-var exp)
              (car (ag-simple-call-or-selection-el exp))
              (car (ag-simple-call-or-selection-el exp)))
            env)
          (eval-call
            (make-ag-stmt-call (ag-simple-call-or-selection-var exp)
              (ag-simple-call-or-selection-el exp))
            env))))

    ;; a(2..3)
    (define (eval-range lhs begn end env)
      (let ((var (evaluate lhs env))
             (bgn (evaluate begn env))
             (end (evaluate end env)))
        (type ag-integer? bgn)
        (type ag-integer? end)
        (cond ((ag-tuple? var)
                (if (= (ag-integer-value bgn) (ag-integer-value end))
                  (ag-tuple-val var (ag-integer-value bgn))
                  (tuple-range var
                    (ag-integer-value bgn)
                    (ag-integer-value end))))
          ((ag-string? var)
            (make-ag-string
              (list->string (list-range (string->list (ag-string-value var))
                              (ag-integer-value bgn)
                              (ag-integer-value end)))))
          (else (eval-error
                  (format "invalid lhs type for range: ~a" var))))))
    ;; [2, 4 ..8]
    (define (eval-simple-tuple-former exp env)
      (let* ((exp1 (ag-tupleslice-exp1 exp))
              (exp2 (ag-tupleslice-exp2 exp))
              (exp3 (ag-tupleslice-exp3 exp))
              (first (if exp1
                       (evaluate exp1 env)
                       (evaluate exp2 env)))
              (exp2eval (evaluate exp2 env))
              (end (evaluate exp3 env))
              (intermediate-check (begin (type ag-integer? first)
                                    (type ag-integer? end)))
              (step (if exp1
                      (begin
                        (type ag-integer? first)
                        (type ag-integer? exp2eval)
                        (make-ag-integer (- (ag-integer-value exp2eval)
                                           (ag-integer-value first))))
                      (make-ag-integer (if (> (ag-integer-value first)
                                             (ag-integer-value end))
                                         -1 1)))))

        (type ag-integer? step)
        (make-ag-tuple (form-list (ag-integer-value first)
                         (ag-integer-value step)
                         (ag-integer-value end)
                         make-ag-integer))))
    ;; {2, 4 .. 8}
    (define (eval-simple-set-former exp env)
      (let* ((exp1 (ag-setslice-exp1 exp))
              (exp2 (ag-setslice-exp2 exp))
              (exp3 (ag-setslice-exp3 exp))
              (first (if exp1 (evaluate exp1 env) (evaluate exp2 env)))
              (exp2eval (evaluate exp2 env))
              (end (evaluate exp3 env))
              (intermediate-check (begin (type ag-integer? first)
                                    (type ag-integer? end)))
              (step (if exp1
                      (begin
                        (type ag-integer? first)
                        (type ag-integer? exp2eval)
                        (make-ag-integer (- (ag-integer-value exp2eval)
                                           (ag-integer-value first))))
                      (make-ag-integer (if (> (ag-integer-value first)
                                             (ag-integer-value end))
                                         -1 1)))))

        (type ag-integer? step)
        (make-ed-set (form-list (ag-integer-value first)
                       (ag-integer-value step)
                       (ag-integer-value end)
                       make-ag-integer))))



    (define (eval-stmt-assignment stmt env)
      (let ((lhs (ag-stmt-assignment-lhs stmt))
             (val (evaluate (ag-stmt-assignment-value stmt) env)))
        (cond ((ag-identifier? lhs)
                (set-variable-value! env global lhs val err-constant))
          ((ag-range? lhs)
            (eval-assign-range (ag-range-var lhs)
              (ag-range-begin lhs)
              (ag-range-end lhs)
              val env))
          ((ag-selection? lhs)
            (eval-assign-selection (ag-selection-lhs lhs)
              (ag-selection-selection lhs)
              val env))
          ((ag-tuple? lhs)
            (eval-assign-tuple lhs val env))
          (else (eval-error
                  (format "unimplemented assignment: ~a" lhs))))))

    ;; [a,b,c] := [1,2,3]
    (define (eval-assign-tuple lhs val env)
      (define (sequential-assign t1 t2)
        (let ((t1end (+ 1 (ag-tuple-highest t1)))
               (t2end (+ 1 (ag-tuple-highest t2))))
          (define (loop pos1 pos2)
            (cond ((and (= pos2 t2end) (= pos1 t1end)) val)
              ((= pos1 t1end) val)
              ((= pos2 t2end)
                (set-variable-value!
                  env
                  global
                  (ag-tuple-val t1 pos1)
                  (make-ag-omega) err-constant)
                (loop (+ pos1 1) pos2))
              ((ag-skip? (ag-tuple-val t1 pos1))
                (loop (+ pos1 1) (+ pos2 1)))
              (else (set-variable-value! env
                      global
                      (ag-tuple-val t1 pos1)
                      (ag-tuple-val t2 pos2)
                      err-constant)
                (loop (+ pos1 1) (+ pos2 1)))))
          (loop 1 1)))
      (if (not (ag-tuple? val))
        (eval-error (format "rhs must be a tuple: ~a" val))
        (begin
          (ag-tuple-for-each (lambda (x) (type lhst? x)) lhs)
          (sequential-assign lhs val))))


    ;; a(2) := 4
    (define (eval-assign-selection lhs selection val env)
      (let ((var (evaluate lhs env))
             (val (evaluate val env))
             (pos (if (= 1 (length selection))
                    (evaluate (car selection) env)
                    (eval-error
                      (format "multiple element selections aren't implemented: ~a"
                        selection)))))
        (cond ((ag-string? var) (assign-range lhs var pos pos val env))
          ((ag-tuple? var) (assign-selection lhs var pos val env))
          ((single-valued-map-reference? var)
            (assign-single-valued-map lhs var pos val env))
          (else (format "invalid lhs type for selection: ~a" lhs)))))

    (define (assign-single-valued-map lhs var dom val env)
      (type ag-integer? dom)
      (set-variable-value! env
        global
        lhs
        (make-ag-set
          (alter-svmap (ag-set-contents var) dom val))
        err-constant))


    (define (assign-selection lhs var pos val env)
      (type ag-integer? pos)
      (ag-tuple-insert var (ag-integer-value pos) val)
      (set-variable-value! env
        global
        lhs
        var
        err-constant))

    ;; a(2..4) := [2,3];
    (define (assign-range lhs var bgn end val env)
      (define (sassign lst vallst)
        (cond ((and (ag-integer? bgn) (ag-integer? end))
                (insert-in-list lst (ag-integer-value bgn)
                  (ag-integer-value end) vallst))
          ((ag-integer? bgn)
            (insert-in-list lst
              (ag-integer-value bgn)
              (length lst) vallst))
          (else
            (eval-error
              (format "range arguments must be integers: ~a, ~a" bgn end)))))
      (define (tassign t1 t2)
        (cond ((and (ag-integer? bgn) (ag-integer? end))
                (ag-tuple-insert-in t1
                  (ag-integer-value bgn)
                  (ag-integer-value end) t2))
          ((ag-integer? bgn)
            (ag-tuple-insert-in
              t1
              (ag-integer-value bgn)
              (ag-tuple-highest t1) t2))
          (else
            (eval-error
              (format "range arguments must be integers: ~a, ~a" bgn end)))))
      (cond ((and (ag-string? var) (ag-string? val))
              (set-variable-value! env
                global
                lhs
                (make-ag-string
                  (list->string
                    (sassign (string->list (ag-string-value var))
                      (string->list (ag-string-value val)))))
                err-constant))
        ((and (ag-tuple? var) (ag-tuple? val))
          (set-variable-value! env global lhs
            (tassign var val)
            err-constant))
        (else
          (eval-error
            (format "invalid variable or value type for range assignment: ~a - ~a"
              var val)))))

    (define (eval-assign-range lhs start finish val env)
      (let ((var (evaluate lhs env))
             (bgn (evaluate start env))
             (end (if finish (evaluate finish env) #f)))
        (assign-range lhs var bgn end val env)))


    (define (eval-loop exp env)
      (let ((loopiter (ag-stmt-loop-loopiter exp))
             (stmts (ag-stmt-loop-stmts exp)))
        (cond ((ag-loopiter-for? loopiter)
                (eval-loop-for loopiter stmts env))
          ((ag-loopiter? loopiter)
            (if (not (ag-loopiter-while loopiter))
              (eval-error
                (format "only while and for loops are implemented: ~" loopiter))
              (eval-loop-while loopiter stmts env))))))

    (define (eval-loop-while loopiter stmts env)
      (let* ((whi (ag-loopiter-while loopiter))
              (cnd (ag-while-condition whi)))
        (define (loop)
          (define pred (evaluate cnd env))
          (type ag-boolean? pred)
          (if (ag-boolean-value pred)
            (begin
              (for-each (lambda (stmt)
                          (evaluate (ag-statement-body stmt) env))
                stmts)
              (loop))
            (make-ag-omega)))
        (loop)))

    (define (eval-loop-for loopiter stmts env)
      (eval-quantified ag-iterator-itlist
        ag-iterator-suchthat
        (ag-loopiter-for-iterator loopiter)
        env
        (lambda (pred return lenv)
          (if (not (ag-boolean-value pred))
            (return (make-ag-omega))
            (for-each (lambda (stmt)
                        (evaluate (ag-statement-body stmt) lenv)) stmts)))))

    (define (eval-exists exp env)
      (eval-quantified ag-exists-iterators
        ag-exists-exp
        exp
        env
        (lambda (pred return lenv)
          (if (ag-boolean-value pred)
            (return pred)
            (make-ag-boolean #f)))))

    (define (eval-notexists exp env)
      (eval-quantified ag-notexists-iterators
        ag-notexists-exp
        exp
        env
        (lambda (pred return lenv)
          (if (ag-boolean-value pred)
            (make-ag-boolean #f)
            (return pred)))))

    (define (eval-forall exp env)
      (eval-quantified ag-forall-iterators
        ag-forall-exp
        exp
        env
        (lambda (pred return lenv)
          (if (not (ag-boolean-value pred))
            (return (make-ag-boolean #f))
            (make-ag-boolean #t)))))

    (define (eval-quantified accessor-iterators
              accessor-such
              exp
              env
              test)
      (let* ((iter-in-els (accessor-iterators exp))
              (first (car iter-in-els))
              (sch (accessor-such exp)))
        (if (not (ag-iterelement-in? first))
          (eval-error (format "unimplemented iterator type: ~a" first))
          (let* ((idf (ag-iterelement-in-lhs first))
                  (e (evaluate (ag-iterelement-in-exp first) env))
                  (fe (cond ((ag-tuple? e) ag-tuple-for-each)
                        ((ed-set? e) ed-set-for-each)
                        (else (eval-error
                                (format "unimplemented iter-element-in type: ~a" e))))))
            (call-with-current-continuation
              (lambda (return)
                (fe (lambda (x)
                      (set-variable-value! env global idf x err-constant)
                      (let ((pred (evaluate sch env)))
                        (type ag-boolean? pred)
                        (test pred return env)
                        (set-variable-value! env
                          global
                          idf
                          (make-ag-omega) err-constant)))
                  e)))))))





    (define (eval-former iterator-expr-accessor
              iterator-iterator-accessor
              empty-constructor
              update
              processing
              exp
              env)
      (let* ((expr (iterator-expr-accessor exp))
              (it (iterator-iterator-accessor exp))
              (itlist (ag-iterator-itlist it))
              (first (car itlist))
              (sch (ag-iterator-suchthat it)))
        (if (not (ag-iterelement-in? first))
          (eval-error (format "unimplemented iterator type: ~a" first))
          (let* ((idf (ag-iterelement-in-lhs first))
                  (e (evaluate (ag-iterelement-in-exp first) env))
                  (fe (cond ((ag-tuple? e) ag-tuple-for-each)
                        ((ed-set? e) ed-set-for-each)
                        (else (eval-error
                                (format "unimplemented iter-element-in type: ~a" e)))))
                  (result (empty-constructor)))
            (type ag-identifier? idf)
            (call-with-current-continuation
              (lambda (return)
                (fe (lambda (x)
                      (set-variable-value! env global idf x err-constant)
                      (let ((pred (evaluate sch env)))
                        (type ag-boolean? pred)
                        (if (ag-boolean-value pred)
                          (set! result
                            (update (evaluate expr env) result)))))
                  e)))
            (processing result)))))

    (define (eval-set-former exp env)
      (eval-former ag-setiterator-expr
        ag-setiterator-iterator
        ed-set-empty
        (lambda (val sofarval)
          (ed-set-insert sofarval val))
        identity
        exp
        env))

    (define (eval-tuple-former exp env)
      (eval-former ag-tupleiterator-expr
        ag-tupleiterator-iterator
        ag-tuple-empty
        (lambda (val sofarval)
          (ag-tuple-insert sofarval
            (+ 1 (ag-tuple-highest sofarval)) val)
          sofarval)
        identity
        exp
        env))


    (define (eval-from dest-accessor
              source-accessor
              position-determinant
              exp
              env)
      (let* ((dest (dest-accessor exp))
              (source (source-accessor exp))
              (sourceval (evaluate source env))
              (pos (position-determinant sourceval))
              (val (ag-tuple-val sourceval pos)))
        (type ag-tuple? sourceval)
        (eval-stmt-assignment (make-ag-stmt-assignment dest val) env)
        (eval-assign-range source
          (make-ag-integer pos)
          (make-ag-integer pos)
          (make-ag-tuple '())
          env)
        val))

    (define (eval-ag-from exp env)
      (eval-ag-fromb exp env))

    (define (eval-ag-fromb exp env)
      (eval-from ag-fromb-dest ag-fromb-source (lambda (x) 1) exp env))

    (define (eval-ag-frome exp env)
      (eval-from ag-frome-dest
        ag-frome-source
        (lambda (t)
          (ag-tuple-highest t)) exp env))

    (define (dispatch message)
      (case message
        ((eval) eval-program)
        ((eval-exp) evaluate)
        (else (error 'parser "unknown message"))))
    dispatch))

(define (make-setl p re ee)
  (let* ((scanner (make-scanner))
          (reader (make-reader scanner))
          (evaluator (make-evaluator))
          (parse-tree #f))

    (define (read port)
      (send-message scanner 'init-scan port)
      (set! parse-tree (send-message reader 'read dispatch))
      parse-tree)

    (define (eval)
      (send-message evaluator 'eval parse-tree dispatch))

    (define (dispatch message)
      (case message
        ((read) read)
        ((eval) eval)
        ((print) p)
        ((read-error) re)
        ((eval-error) ee)
        (else (error 'setl "unknown message ~a" message))))


    dispatch))

(define setl (make-setl display
               (lambda (message token)
                 (display "Read error: ") (display message)
                 (newline)(display "> ")(display token)(display " <")
                 (error "Parser error."))
               (lambda (message)
                 (display "Eval error: ")(display message)
                 (error "Evaluation error."))))

(define (send-message object message . parameters)
  (let ((method (object message)))
    (apply method parameters)))

(define (out . params)
  (for-each (lambda (x) (display x)) params))

(define identity (lambda (x) x))

(define (insert-in-list lst begn end val)
  (define (insert lst pos val)
    (cond ((null? lst) (list val))
      ((< pos begn) (cons (car lst) (insert (cdr lst) (+ pos 1) val)))
      ((<= pos end) (if (null? val)
                      (insert (cdr lst) (+ pos 1) val)
                      (cons (car val) (insert lst pos (cdr val)))))
      ((> pos end) (cons (car lst) (insert (cdr lst) (+ pos 1) val)))))
  (if (or (> begn (length lst)) (> end (length lst)))
    #f
    (insert lst 1 val)))

(define (list-range lst bg end)
  (define (range lst pos)
    (cond ((null? lst) '())
      ((and (>= pos bg) (<= pos end))
        (cons (car lst) (range (cdr lst) (+ pos 1))))
      (else (range (cdr lst) (+ pos 1)))))
  (cond ((or (> bg (length lst)) (> end (length lst))) #f)
    ((> bg end) '())
    (else (range lst 1))))

(define (alter-pos-in-list lst pos val fill)
  (define (loop lst cur)
    (cond ((and (null? lst) (= cur pos)) (cons val (loop lst (+ cur 1))))
      ((and (null? lst) (< cur pos)) (cons fill (loop lst (+ cur 1))))
      ((null? lst) '())
      ((= pos cur) (cons val (loop (cdr lst) (+ cur 1))))
      (else (cons (car lst) (loop (cdr lst) (+ cur 1))))))
  (loop lst 1))

(define (form-list bgn step end constructor)
  (let loop ((i bgn))
    (cond ((= i end) (list (constructor end)))
      ((or (and (positive? step) (> i end))
         (and (not (positive? step)) (< i end)))
          '())
      (else (cons (constructor i) (loop (+ i step)))))))


(define (duplicate-list lst dup)
  (if (= dup 0)
      '()
    (append lst (duplicate-list lst (- dup 1)))))

(define (duplicate-string str dup)
  (if (= dup 0)
    ""
    (string-append str (duplicate-string str (- dup 1)))))

(define (sublist sub lst)
  (define (match lst part)
    (cond ((and (null? part) (null? lst)) #t)
      ((null? lst) #f)
      ((null? part) #t)
      ((equal? (car lst) (car part))
        (match (cdr lst) (cdr part)))
      (else #f)))
  (define (loop lst part)
    (cond ((null? lst) #f)
      ((equal? (car lst) (car part))
        (or (match (cdr lst) (cdr part))
          (loop (cdr lst) part)))
      (else (loop (cdr lst) part))))
  (and (not (null? sub))
    (loop lst sub)))

(define (lower ch)
  (if (eof-object? ch)
    ch
    (char-downcase ch)))


;; Tokens - used for communication between scanner and parser
;; ==========================================================

;; Token symbols

(define lpr-symbol 'left-parenthesis)
(define rpr-symbol 'right-parenthesis)
(define lbc-symbol 'left-brace)
(define rbc-symbol 'right-brace)
(define lbr-symbol 'left-bracket)
(define rbr-symbol 'right-bracket)
(define wrd-symbol 'word)
(define int-symbol 'integer)
(define rea-symbol 'real)
(define smc-symbol 'semicolon)
(define eol-symbol 'end-of-line)
(define end-symbol 'end-of-input)
(define cmt-symbol 'comment)
(define txt-symbol 'text)
(define com-symbol 'comma)
(define col-symbol 'colon)
(define mop-symbol 'multiplicative-operator)
(define aop-symbol 'additive-operator)
(define pls-symbol 'plus)
(define mns-symbol 'minus)
(define eql-symbol 'equality)
(define ceq-symbol 'colon-equality)
(define xop-symbol 'exponentiation-operator)
(define sch-symbol 'such-that)
(define err-symbol 'error)
(define mul-symbol 'multiplication)
(define div-symbol 'division)
(define dcl-symbol 'double-colon)
(define dif-symbol 'difference)
(define lss-symbol 'less)
(define els-symbol 'equal-or-less)
(define grt-symbol 'greater)
(define egr-symbol 'equal-or-greater)
(define amt-symbol 'amount)

;; Token keywords
(define program-keyword 'program-keyword)
(define end-keyword 'end-keyword)
(define range-keyword 'range-keyword)
(define var-keyword 'var-keyword)
(define const-keyword 'const-keyword)
(define init-keyword 'init-keyword)
(define exit-keyword 'exit-keyword)
(define pass-keyword 'pass-keyword)
(define stop-keyword 'stop-keyword)
(define goto-keyword 'goto-keyword)
(define impl-keyword 'implication-keyword)
(define or-keyword 'or-keyword)
(define and-keyword 'and-keyword)
(define not-keyword 'not-keyword)
(define is_tuple-keyword 'is_tuple-keyword)
(define is_set-keyword 'is_set-keyword)
(define is_string-keyword 'is_string-keyword)
(define is_integer-keyword 'is_integer-keyword)
(define is_real-keyword 'is_real-keyword)
(define is_map-keyword 'is_map-keyword)
(define is_atom-keyword 'is_atom-keyword)
(define is_boolean-keyword 'is-boolean_keyword)
(define abs-keyword 'abs-keyword)
(define acos-keyword 'acos-keyword)
(define arb-keyword 'arb-keyword)
(define asin-keyword 'asin-keyword)
(define atan-keyword 'atan-keyword)
(define ceil-keyword 'ceil-keyword)
(define char-keyword 'char-keyword)
(define cos-keyword 'cos-keyword)
(define domain-keyword 'domain-keyword)
(define even-keyword 'even-keyword)
(define expr-keyword 'expr-keyword)
(define fix-keyword 'fix-keyword)
(define float-keyword 'float-keyword)
(define floor-keyword 'floor-keyword)
(define log-keyword 'log-keyword)
(define not-keyword 'not-keyword)
(define odd-keyword 'odd-keyword)
(define pow-keyword 'pow-keyword)
(define random-keyword 'random-keyword)
(define range-keyword 'range-keyword)
(define sign-keyword 'sign-keyword)
(define sin-keyword 'sin-keyword)
(define sqrt-keyword 'sqrt-keyword)
(define str-keyword 'str-keyword)
(define tan-keyword 'tan-keyword)
(define tanh-keyword 'tanh-keyword)
(define type-keyword 'type-keyword)
(define in-keyword 'in-keyword)
(define notin-keyword 'notin-keyword)
(define subset-keyword 'subset-keyword)
(define true-keyword 'true-keyword)
(define false-keyword 'false-keyword)
(define omega-keyword 'omega-keyword)
(define exists-keyword 'exists-keyword)
(define notexists-keyword 'notexists-keyword)
(define forall-keyword 'forall-keyword)
(define from-keyword 'from-keyword)
(define fromb-keyword 'fromb-keyword)
(define frome-keyword 'frome-keyword)
(define max-keyword 'max-keyword)
(define min-keyword 'min-keyword)
(define mod-keyword 'mod-keyword)
(define with-keyword 'with-keyword)
(define if-keyword 'if-keyword)
(define then-keyword 'then-keyword)
(define else-keyword 'else-keyword)
(define elseif-keyword 'elseif-keyword)
(define return-keyword 'return-keyword)
(define loop-keyword 'loop-keyword)
(define do-keyword 'do-keyword)
(define for-keyword 'for-keyword)
(define init-keyword 'init-keyword)
(define doing-keyword 'doing-keyword)
(define while-keyword 'while-keyword)
(define step-keyword 'step-keyword)
(define until-keyword 'until-keyword)
(define term-keyword 'term-keyword)
(define proc-keyword 'proc-keyword)
(define rd-keyword 'rd-keyword)
(define rw-keyword 'rw-keyword)
(define wr-keyword 'wr-keyword)
(define div-keyword 'div-keyword)

(define unary-keywords
  (list abs-keyword acos-keyword arb-keyword asin-keyword
    atan-keyword ceil-keyword char-keyword cos-keyword
    domain-keyword even-keyword expr-keyword fix-keyword
    float-keyword floor-keyword log-keyword
    odd-keyword pow-keyword random-keyword
    sign-keyword sin-keyword sqrt-keyword str-keyword
    tan-keyword tanh-keyword type-keyword amt-symbol))

(define not-is_xx (list not-keyword is_tuple-keyword
                    is_set-keyword is_string-keyword
                    is_integer-keyword is_real-keyword
                    is_map-keyword is_atom-keyword
                    is_boolean-keyword))

(define assigning
  (list impl-keyword or-keyword and-keyword in-keyword notin-keyword
    subset-keyword pls-symbol mns-symbol div-symbol mul-symbol xop-symbol max-keyword
    min-keyword mod-keyword notin-keyword with-keyword eql-symbol dif-symbol lss-symbol
    els-symbol grt-symbol egr-symbol amt-symbol))

(define rop-tokens (list in-keyword notin-keyword subset-keyword eql-symbol
                     dif-symbol lss-symbol els-symbol grt-symbol egr-symbol))

;; Token structure
;(define-struct token (symbol line position data))

(define (make-token . args) (cons 'token args))
(define (token? x)(eq? (car x) 'token))
(define token-symbol cadr)
(define token-line caddr)
(define token-position cadddr)
(define token-data (lambda (x) (car (cddddr x))))
(define (token? x)(eq? (car x 'token)))

(define (equivalent-tokens? x y)
  (and (equal? (token-symbol x)
         (token-symbol y))
    (equal? (token-data x)
      (token-data y))))

;; Scanner
;; =======


;; Scanner
(define (make-scanner)
  (let ((pos 0)
         (line 0)
         (char #f)
         (hold #f)
         (port #f)
         (keywords (list (cons "program" program-keyword)
                     (cons "end" end-keyword)
                     (cons "var" var-keyword)
                     (cons "const" const-keyword)
                     (cons "init" init-keyword)
                     (cons "exit" exit-keyword)
                     (cons "goto" goto-keyword)
                     (cons "pass" pass-keyword)
                     (cons "stop" stop-keyword)
                     (cons "impl" impl-keyword)
                     (cons "or" or-keyword)
                     (cons "and" and-keyword)
                     (cons "not" not-keyword)
                     (cons "is_tuple" is_tuple-keyword)
                     (cons "is_set" is_set-keyword)
                     (cons "is_string" is_string-keyword)
                     (cons "is_integer" is_integer-keyword)
                     (cons "is_real" is_real-keyword)
                     (cons "is_map" is_map-keyword)
                     (cons "is_atom" is_atom-keyword)
                     (cons "is_boolean" is_boolean-keyword)
                     (cons "abs" abs-keyword)
                     (cons "acos" acos-keyword)
                     (cons "arb" arb-keyword)
                     (cons "asin" asin-keyword)
                     (cons "atan" atan-keyword)
                     (cons "ceil" ceil-keyword)
                     (cons "char" char-keyword)
                     (cons "cos" cos-keyword)
                     (cons "domain" domain-keyword)
                     (cons "even" even-keyword)
                     (cons "expr" expr-keyword)
                     (cons "fix" fix-keyword)
                     (cons "float" float-keyword)
                     (cons "floor" floor-keyword)
                     (cons "log" log-keyword)
                     (cons "not" not-keyword)
                     (cons "odd" odd-keyword)
                     (cons "pow" pow-keyword)
                     (cons "random" random-keyword)
                     (cons "range" range-keyword)
                     (cons "sign" sign-keyword)
                     (cons "sin" sin-keyword)
                     (cons "sqrt" sqrt-keyword)
                     (cons "str" str-keyword)
                     (cons "tan" tan-keyword)
                     (cons "tanh" tanh-keyword)
                     (cons "type" type-keyword)
                     (cons "in" in-keyword)
                     (cons "notin" notin-keyword)
                     (cons "subset" subset-keyword)
                     (cons "true" true-keyword)
                     (cons "false" false-keyword)
                     (cons "st" sch-symbol)
                     (cons "om" omega-keyword)
                     (cons "exists" exists-keyword)
                     (cons "notexists" notexists-keyword)
                     (cons "forall" forall-keyword)
                     (cons "from" from-keyword)
                     (cons "fromb" fromb-keyword)
                     (cons "frome" frome-keyword)
                     (cons "max" max-keyword)
                     (cons "min" min-keyword)
                     (cons "mod" mod-keyword)
                     (cons "with" with-keyword)
                     (cons "if" if-keyword)
                     (cons "then" then-keyword)
                     (cons "else" else-keyword)
                     (cons "elseif" elseif-keyword)
                     (cons "return" return-keyword)
                     (cons "loop" loop-keyword)
                     (cons "do" do-keyword)
                     (cons "for" for-keyword)
                     (cons "init" init-keyword)
                     (cons "doing" doing-keyword)
                     (cons "while" while-keyword)
                     (cons "step" step-keyword)
                     (cons "until" until-keyword)
                     (cons "term" term-keyword)
                     (cons "proc" proc-keyword)
                     (cons "rd" rd-keyword)
                     (cons "rw" rw-keyword)
                     (cons "wr" wr-keyword)
                     (cons "div" div-keyword)
                   )))

    (define (keyword? text)
      (define result (assoc text keywords))
      (if result
        (cdr result)
        #f))

    (define (char-type ch)
      (if (eof-object? ch)
        eof
        (vector-ref
          (vector
            wsp wsp wsp wsp wsp wsp wsp wsp wsp wsp eol wsp wsp wsp wsp wsp ;000
            wsp wsp wsp wsp wsp wsp wsp wsp wsp wsp wsp wsp wsp wsp wsp wsp ;016
            wsp ill ill amt cmt ill ill txt lpr rpr mul pls com mns per div ;032
            dgt dgt dgt dgt dgt dgt dgt dgt dgt dgt col smc lss eql grt ill ;048
            ill ltr ltr ltr ltr exp ltr ltr ltr ltr ltr ltr ltr ltr ltr ltr ;064
            ltr ltr ltr ltr ltr ltr ltr ltr ltr ltr ltr lbr ill rbr ill ltr ;080
            ill ltr ltr ltr ltr exp ltr ltr ltr ltr ltr ltr ltr ltr ltr ltr ;096
            ltr ltr ltr ltr ltr ltr ltr ltr ltr ltr ltr lbc sch rbc ill ill ;112
            ill ill ill ill ill ill ill ill ill ill ill ill ill ill ill ill ;128
            ill ill ill ill ill ill ill ill ill ill ill ill ill ill ill ill ;144
            ill ill ill ill ill wsp ill ill ill ill ill ill ill ill ill ill ;160
            ill ill ill ill ill ill ill ill ill ill ill ill ill ill ill ill ;176
            ill ill ill ill ill ill ill ill ill ill ill ill ill ill ill ill ;192
            ill ill ill ill ill ill ill ill ill ill ill ill ill ill ill ill ;208
            ill ill ill ill ill ill ill ill ill ill ill ill ill ill ill ill ;224
            ill ill ill ill ill ill ill ill ill ill ill ill ill ill ill ill);240
          (char->integer ch))))


    (define (check . allowed)
      (member (char-type char) allowed))

    (define (peek-check . allowed)
      (member (char-type (peek)) allowed))

    (define (check-consecutive . allowed)
      (let loop ((number 0))
        (if (apply check allowed)
          (begin
            (next)
            (loop (+ number 1)))
          number)))


    (define (init-scan input-port)
      (set! pos 0)
      (set! line 1)
      (set! port input-port)
      (set! char (read-char port)))


    (define (skip)
      (define current-char char)
      (next)
      current-char)


    (define (next)
      (set! pos (+ pos 1))
      (set! char (lower (read-char port))))

    (define (peek)
      (peek-char port))

    (define (scan)
      ((char-type char)))

    (define (ill)
      (make-token err-symbol line pos "illegal character"))

    (define (lss)
      (next)
      (if (check eql)
        (begin (next) (make-token els-symbol line pos #f))
        (make-token lss-symbol line pos #f)))

    (define (grt)
      (next)
      (if (check eql)
        (begin (next) (make-token egr-symbol line pos #f))
        (make-token grt-symbol line pos #f)))

    (define (eol)
      (cond ((char-ready? port)
              (set! line (+ line 1))
              (set! pos 1)
              (next)
              (scan))
        (else (make-token end-symbol line pos #f))))

    (define (wsp)
      (next)
      (scan))

    (define (eof)
      (make-token end-symbol line pos #f))

    (define (amt)
      (next)
      (make-token amt-symbol line pos #f))

    (define (operator token)
      (next)
      (make-token token line pos #f))

    (define (cmt)
      (next)
      (if (or (check eof) (check eol))
        (scan)
        (cmt)))

    (define (lbc)
      (next)
      (make-token lbc-symbol line pos #f))

    (define (rbc)
      (next)
      (make-token rbc-symbol line pos #f))

    (define (lbr)
      (next)
      (make-token lbr-symbol line pos #f))

    (define (rbr)
      (next)
      (make-token rbr-symbol line pos #f))

    (define (sch)
      (next)
      (make-token sch-symbol line pos #f))

    (define (pls)
      (operator pls-symbol))

    (define (mul)
      (next)
      (if (check mul)
        (begin (next) (make-token xop-symbol line pos #f))
        (make-token mul-symbol line pos #f)))

    (define (div)
      (next)
      (if (check eql)
        (begin (next) (make-token dif-symbol line pos #f))
        (make-token div-symbol line pos #f)))

    (define (mns)
      (operator mns-symbol))

    (define (lpr)
      (next)
      (make-token lpr-symbol line pos #f))

    (define (rpr)
      (next)
      (make-token rpr-symbol line pos #f))

    (define (com)
      (next)
      (make-token com-symbol line pos #f))

    (define (eql)
      (next)
      (make-token eql-symbol line pos #f))

    (define (word-or-keyword word)
      (let ((keyword-symbol (keyword? word)))
        (if keyword-symbol
          (make-token keyword-symbol line pos #f)
          (make-token wrd-symbol line pos word))))

    (define (ltr)
      (define (scan-word)
        (if (check ltr dgt exp)
          (cons (skip) (scan-word))
            '()))
      (word-or-keyword (list->string (scan-word))))

    (define exp ltr)

    (define (convert-number lst)
      (let ((number (string->number (list->string lst))))
        (cond  ((integer? number) (make-token int-symbol line pos number))
          ((real? number) (make-token rea-symbol line pos number))
          (else (make-token err-symbol line pos "not a valid number value")))))

    (define (dgt)
      (define per-allowed #t)
      (define (scan-digits)
        (if (check dgt)
          (cons (skip) (scan-digits))
            '()))
      (define (scan-exponent)
        (if (check pls mns)
          (cons (skip) (scan-digits))
          (scan-digits)))
      (define (scan-number)
        (cond ((check dgt) (cons (skip) (scan-number)))
          ((check exp) (cons (skip) (scan-exponent)))
          ((check per)
            (if (and per-allowed (not (peek-check per)))
              (begin
                (set! per-allowed #f)
                (cons (skip) (scan-number)))
                '()))
          (else '())))
      (convert-number (scan-number)))

    (define (per)
      (let ((number (check-consecutive per)))
        (if (or  (= number 2) (= number 3))
          (make-token range-keyword line pos #f)
          (make-token err-symbol line pos "expected .. or ..."))))

    (define (txt)
      (define (loop scanned-so-far)
        (if (not (check txt))
          (loop (cons (skip) scanned-so-far))
          (if (peek-check txt)
            (begin
              (next)
              (loop (cons (skip) scanned-so-far)))
            (if (check txt)
              (begin
                (next)
                (make-token txt-symbol line pos (list->string (reverse scanned-so-far))))
              (make-token err-symbol line pos "expected '")))))
      (next)
      (loop '()))

    (define (col)
      (next)
      (cond ((check eql) (next) (make-token ceq-symbol line pos #f))
        ((check col) (next) (make-token dcl-symbol line pos #f))
        (else (make-token col-symbol line pos #f))))

    (define (smc)
      (next)
      (make-token smc-symbol line pos #f))


    (define (dispatch message)
      (case message
        ((init-scan) init-scan)
        ((scan) scan)
        (else (error 'scanner "unknown message"))))

    dispatch))


;(define scanner (make-scanner))
;(send-message scanner 'init-scan (open-input-string "3*5+2"))
;(define (test)
;  (let ((x (send-message scanner 'scan)))
;    (display (token-symbol x)) (display " ") (display (token-data x)) (newline)))
;(test)
;(test)
;(test)
;(test)
;(test)
;(test)
;(test)
;(test)
;(test)
;(test)

(define (tree data left right)
  (vector data
    (+ 1 (tree-size left)
      (tree-size right))
    left
    right))

(define empty-tree (vector #f 0 #f #f))

(define (tree-data t)
  (vector-ref t 0))

(define (tree-size t)
  (vector-ref t 1))

(define (tree-left t)
  (vector-ref t 2))

(define (tree-right t)
  (vector-ref t 3))

(define (index t i)
  (if (empty? t)
    #f
    (if (< i (tree-size (tree-left t)))
      (index (tree-left t) i)
      (if (> i (tree-size (tree-left t)))
        (index (tree-right t) (- i (tree-size (tree-left t)) 1))
        (tree-data t)))))


(define (empty? t)
  (= 0 (tree-size t)))

(define (singleton e)
  (tree e empty-tree empty-tree))

(define (tree-manipulations less?)
  (define (rotate-single-left d l r)
    (tree
      (tree-data r)
      (tree d l (tree-left r))
      (tree-right r)))

  (define (rotate-double-left d l r)
    (let ((lr (tree-left r)))
      (tree
        (tree-data lr)
        (tree d l (tree-left lr))
        (tree (tree-data r) (tree-right lr) (tree-right r)))))

  (define (rotate-single-right d l r)
    (tree
      (tree-data l)
      (tree-left l)
      (tree d (tree-right l) r)))

  (define (rotate-double-right d l r)
    (let ((rl (tree-right l)))
      (tree
        (tree-data rl)
        (tree (tree-data l) (tree-left l) (tree-left rl))
        (tree d (tree-right rl) r))))

  (define (balance d l r)
    (let ((sl (tree-size l))
           (sr (tree-size r)))
      (cond ((< (+ sl sr) 2) (tree d l r))
        ((>= sr (* 3 sl))
          (if (< (tree-size (tree-left r)) (tree-size (tree-right r)))
            (rotate-single-left d l r)
            (rotate-double-left d l r)))
        ((>= sl (* 3 sr))
          (if (< (tree-size (tree-right l)) (tree-size (tree-left l)))
            (rotate-single-right d l r)
            (rotate-double-right d l r)))
        (else (tree d l r)))))



  (define (element? e t)
    (cond ((empty? t) #f)
      ((less? e (tree-data t))
        (element? e (tree-left t)))
      ((less? (tree-data t) e)
        (element? e (tree-right t)))
      (else #t)))

  (define (insert e t)
    (cond ((empty? t) (singleton e))
      ((less? e (tree-data t))
        (balance
          (tree-data t)
          (insert e (tree-left t))
          (tree-right t)))
      ((less? (tree-data t) e)
        (balance
          (tree-data t)
          (tree-left t)
          (insert e (tree-right t))))
      (else t)))

  (define (concat3 l d r)
    (cond ((empty? l) (insert d r))
      ((empty? r) (insert d l))
      ((< (* 3 (tree-size l)) (tree-size r))
        (balance
          (tree-data r)
          (concat3 l d (tree-left r))
          (tree-right r)))
      ((< (* 3 (tree-size r)) (tree-size l))
        (balance
          (tree-data l)
          (tree-left l)
          (concat3 (tree-right l) d r)))
      (else (tree d l r))))


  (define (split< e t)
    (cond ((empty? t) t)
      ((less? e (tree-data t))
        (split< e (tree-left t)))
      ((less? (tree-data t) e)
        (concat3 (tree-left t) (tree-data t) (split< e (tree-right t))))
      (else (tree-left t))))

  (define (split> e t)
    (cond ((empty? t) t)
      ((less? (tree-data t) e)
        (split> e (tree-right t)))
      ((less? e (tree-data t))
        (concat3 (split> e (tree-left t)) (tree-data t) (tree-right t)))
      (else (tree-right t))))

  (define (union t1 t2)
    (cond ((empty? t1) t2)
      ((empty? t2) t1)
      (else
        (let ((d (tree-data t1)))
          (concat3 (union (tree-left t1) (split< d t2))
            d
            (union (tree-right t1) (split> d t2)))))))

  (define (delete e t)
    (cond ((empty? t) t)
      ((less? e (tree-data t))
        (balance
          (tree-data t)
          (delete e (tree-left t))
          (tree-right t)))
      ((less? (tree-data t) e)
        (balance
          (tree-data t)
          (tree-left t)
          (delete e (tree-right t))))
      (else (merge (tree-left t) (tree-right t)))))

  (define (merge l r)
    (cond ((empty? l) r)
      ((empty? r) l)
      (else
        (balance (smallest r) l (delete-smallest r)))))

  (define (smallest t)
    (if (empty? (tree-left t))
      (tree-data t)
      (smallest (tree-left t))))

  (define (delete-smallest t)
    (if (empty? (tree-left t))
      (tree-right t)
      (balance
        (tree-data t)
        (delete-smallest (tree-left t))
        (tree-right t))))

  (define (difference t1 t2)
    (cond ((empty? t1) t2)
      ((empty? t2) t1)
      (else
        (let ((d (tree-data t2)))
          (concat (difference (split< d t1) (tree-left t2))
            (difference (split> d t1) (tree-right t2)))))))

  (define (concat l r)
    (cond ((empty? l) r)
      ((empty? r) l)
      ((< (* 3 (tree-size l)) (tree-size r))
        (balance
          (tree-data r)
          (concat l (tree-left r))
          (tree-right r)))
      ((< (* 3 (tree-size r)) (tree-size l))
        (balance
          (tree-data l)
          (tree-left l)
          (concat (tree-right l) r)))
      (else
        (balance (smallest r) l (delete-smallest r)))))

  (define (intersection t1 t2)
    (cond ((empty? t1) t1)
      ((empty? t2) t2)
      (else
        (let ((d (tree-data t2)))
          (let ((l (intersection (split< d t1) (tree-left t2)))
                 (r (intersection (split> d t1) (tree-right t2))))
            (if (element? d t1)
              (concat3 l d r)
              (concat l r)))))))

  (define (adjoin e t)
    (if (element? e t)
      t
      (insert e t)))

  (define (tree-less t1 t2)
    (define (loop i)
      (cond ((>= i (tree-size t1)) #f)
        ((less? (index t1 i)
           (index t2 i))
          #t)
        (else (loop (+ i 1)))))
    (cond ((< (tree-size t1)
             (tree-size t2))
            #t)
      ((= (tree-size t1)
         (tree-size t2))
        (loop 0))
      (else #f)))

  (define (the-empty-tree) empty-tree)

  (define (list->tree l)
    (let loop ((l l)
                (t (the-empty-tree)))
      (if (null? l)
        t
        (loop (cdr l) (adjoin (car l) t)))))

  (define (tree->list t)
    (let loop ((t t)
                (l '()))
      (if (empty? t)
        l
        (loop (tree-left t)
          (loop (tree-right t) (cons (tree-data t) l))))))

  (define (tree-for-each t function)
    (unless (empty? t)
      (tree-for-each (tree-left t) function)
      (function (tree-data t))
      (tree-for-each (tree-right t) function)))

  (define (tree-map t function)
    (define result (the-empty-tree))
    (define (loop t)
      (if (empty? t)
        result
        (begin
          (set! result (adjoin (function (tree-data t))
                         result))
          (loop (tree-left t))
          (loop (tree-right t)))))
    (loop t))

  (define (remove e t)
    (if (element? e t)
      (delete e t)
      t))

  (define (dispatch message)
    (case message
      ((element?) element?)
      ((insert) adjoin)
      ((union) union)
      ((smallest-element) smallest)
      ((remove) remove)
      ((difference) difference)
      ((intersection) intersection)
      ((construct-from-list) list->tree)
      ((convert-to-list) tree->list)
      ((for-each) tree-for-each)
      ((map) tree-map)
      ((less?) tree-less)
      (else (error 'tree "unknown message ~a" message))))

  dispatch)


;; Abstract Grammar
;; ================
;(define-struct ag-program (name body))
(define (make-ag-program . args) (cons 'ag-program args))
(define (ag-program? x)(eq? (car x) 'ag-program))
(define make-ag-program list)
(define ag-program-name cadr)
(define ag-program-body caddr)
;(define-struct ag-programbody (declarations statements routines refines))
(define (make-ag-programbody . args) (cons 'ag-programbody args))
(define (ag-programbody? x)(eq? (car x) 'ag-programbody))
(define ag-programbody-declarations cadr)
(define ag-programbody-statements caddr)
(define ag-programbody-routines cadddr)
(define ag-programbody-refines (lambda (x) (car (cddddr x))))
;(define-struct ag-body (declarations statements refines))
(define (make-ag-body . args) (cons 'ag-body args))
(define (ag-body? x)(eq? (car x) 'ag-body))
(define ag-body-declarations cadr)
(define ag-body-statements caddr)
(define ag-body-refines cadddr)
;(define-struct ag-identifier (name))
(define (make-ag-identifier . args) (cons 'ag-identifier args))
(define (ag-identifier? x)(eq? (car x) 'ag-identifier))
;(define-struct ag-declarations (var const init))
(define (make-ag-declarations . args) (cons 'ag-declarations args))
(define (ag-declarations? x)(eq? (car x) 'ag-declarations))
(define ag-declarations-body cadr)
(define ag-declarations-init caddr)
;(define-struct ag-constdeclaration (var constant))
(define (make-ag-declaration . args) (cons 'ag-declaration args))
(define (ag-declaration? x)(eq? (car x) 'ag-declaration))
(define ag-declaration-var cadr)
(define ag-declaration-constant caddr)
;(define-struct ag-initdeclaration (var init))
(define (make-ag-initdeclaration . args) (cons 'ag-initdeclaration args))
(define (ag-initdeclaration? x)(eq? (car x) 'ag-initdeclaration))
(define ag-initdeclaration-var cadr)
(define ag-initdeclaration-init caddr)
;(define-struct ag-integer (value))
(define (make-ag-integer . args) (cons 'ag-integer args))
(define (ag-integer? x)(eq? (car x) 'ag-integer))
(define ag-integer-value cadr)
;(define-struct ag-real (value))
(define (make-agreal . args) (cons 'agreal args))
(define (ag-real? x)(eq? (car x) 'agreal))
(define ag-real-value cadr)
;(define-struct ag-string (value))
(define (make-ag-string . args) (cons 'ag-string args))
(define (ag-string? x)(eq? (car x) 'ag-string))
(define ag-string-value cadr)

;(define-struct ag-set (contents))
(define (make-ag-set . args) (cons 'ag-set args))
(define (ag-set? x)(eq? (car x) 'ag-set))
(define ag-set-contents cadr)

;(define-struct ag-setslice (exp1 exp2 exp3))
(define (make-ag-setslice . args) (cons 'ag-setslice args))
(define (ag-setslice? x)(eq? (car x) 'ag-setslice))
(define ag-setslice-exp1 cadr)
(define ag-setslice-exp2 caddr)
(define ag-setslice-exp3 cadddr)
;(define-struct ag-tupleslice (exp1 exp2 exp3))
(define (make-ag-tupleslice . args) (cons 'ag-tupleslice args))
(define (ag-tupleslice? x)(eq? (car x) 'ag-tupleslice))
(define ag-tupleslice-exp1 cadr)
(define ag-tupleslice-exp2 caddr)
(define ag-tupleslice-exp3 cadddr)
;(define-struct ag-range (var begin end))
(define (make-ag-range . args) (cons 'ag-range args))
(define (ag-range? x)(eq? (car x) 'ag-range))
(define ag-range-var cadr)
(define ag-range-begin caddr)
(define ag-range-end cadddr)

;(define-struct ag-statement (label body))
(define (make-ag-statement . args) (cons 'ag-statement args))
(define (ag-statement? x)(eq? (car x) 'ag-statement))
(define ag-statement-label cadr)
(define ag-statement-body caddr)
;(define-struct ag-stmt-exit ())
(define (make-ag-stmt-exit . args) (cons 'ag-stmt-exit args))
(define (ag-stmt-exit? x)(eq? (car x) 'ag-stmt-exit))
;(define-struct ag-stmt-goto (label))
(define (make-ag-stmt-goto . args) (cons 'ag-stmt-goto args))
(define (ag-stmt-goto? x)(eq? (car x) 'ag-stmt-goto))
;(define-struct ag-stmt-pass ())
(define (make-ag-stmt-pass . args) (cons 'ag-stmt-pass args))
(define (ag-stmt-pass? x)(eq? (car x) 'ag-stmt-pass))
;(define-struct ag-stmt-stop ())
(define (make-ag-stmt-stop . args) (cons 'ag-stmt-stop args))
(define (ag-stmt-stop? x)(eq? (car x) 'ag-stmt-stop))
;(define-struct ag-stmt-assignment (lhs value))
(define (make-ag-stmt-assignment . args) (cons 'ag-stmt-assignment args))
(define (ag-stmt-assignment? x)(eq? (car x) 'ag-stmt-assignment))
(define ag-stmt-assignment-lhs cadr)
(define ag-stmt-assignment-value caddr)
;(define-struct ag-application
; (operator operands)) ;
(define (make-ag-application . args) (cons 'ag-application args))
(define (ag-application? x)(eq? (car x) 'ag-application))
(define ag-application-operator cadr)
(define ag-application-operands caddr)
;(define-struct ag-stmt-call (var args))
(define (make-ag-stmt-call . args) (cons 'ag-stmt-call args))
(define (ag-stmt-call? x)(eq? (car x) 'ag-stmt-call))
(define ag-stmt-call-var cadr)
(define ag-stmt-call-args caddr)
;(define-struct ag-stmt-return (val))
(define (make-ag-stmt-return . args) (cons 'ag-stmt-return args))
(define (ag-stmt-return? x)(eq? (car x) 'ag-stmt-return))
(define ag-stmt-return-val cadr)

;(define-struct ag-boolean (value))
(define (make-ag-boolean . args) (cons 'ag-boolean args))
(define (ag-boolean? x)(eq? (car x) 'ag-boolean))
(define ag-boolean-value cadr)
;(define-struct ag-omega ())
(define (make-ag-omega . args) (cons 'ag-omega args))
(define (ag-omega? x)(eq? (car x) 'ag-omega))
;(define-struct ag-selection (lhs selection))
(define (make-ag-selection . args) (cons 'ag-selection args))
(define (ag-selection? x)(eq? (car x) 'ag-selection))
(define ag-selection-lhs cadr)
(define ag-selection-selection caddr)
;(define-struct ag-sselection (lhs selection))
(define (make-ag-sselection . args) (cons 'ag-sselection args))
(define (ag-sselection? x)(eq? (car x) 'ag-sselection))
(define ag-sselection-lhs cadr)
(define ag-sselection-selection caddr)
;(define-struct ag-iterelement-in (lhs exp))
(define (make-ag-iterelement-in . args) (cons 'ag-iterelement-in args))
(define (ag-iterelement-in? x)(eq? (car x) 'ag-iterelement-in))
(define ag-iterelement-in-lhs cadr)
(define ag-iterelement-in-exp caddr)
;(define-struct ag-iterelement-selection (lhs idf selection))
(define (make-ag-iterelement-selection . args) (cons 'ag-iterelement-selection args))
(define (ag-iterelement-selection? x)(eq? (car x) 'ag-iterelement-selection))
(define ag-iterelement-selection-lhs cadr)
(define ag-iterelement-selection-idf caddr)
(define ag-iterelement-selection-selection cadddr)
;(define-struct ag-iterelement-sselection (lhs idf selection))
(define (make-ag-iterelement-sselection . args) (cons 'ag-iterelement-sselection args))
(define (ag-iterelement-sselection? x)(eq? (car x) 'ag-iterelement-sselection))
(define ag-iterelement-sselection-lhs cadr)
(define ag-iterelement-sselection-idf caddr)
(define ag-iterelement-sselection-selection cadddr)
;(define-struct ag-iterator (itlist suchthat))
(define (make-ag-iterator . args) (cons 'ag-iterator args))
(define (ag-iterator? x)(eq? (car x) 'ag-iterator))
(define ag-iterator-itlist cadr)
(define ag-iterator-suchthat caddr)
;(define-struct ag-setiterator (expr iterator))
(define (make-ag-setiterator . args) (cons 'ag-setiterator args))
(define (ag-setiterator? x)(eq? (car x) 'ag-setiterator))
(define ag-setiterator-expr cadr)
(define ag-setiterator-iterator caddr)
;(define-struct ag-tupleiterator (expr iterator))
(define (make-ag-tupleiterator . args) (cons 'ag-tupleiterator args))
(define (ag-tupleiterator? x)(eq? (car x) 'ag-tupleiterator))
(define ag-tupleiterator-expr cadr)
(define ag-tupleiterator-iterator caddr)
;(define-struct ag-exists (iterators exp))
(define (make-ag-exists . args) (cons 'ag-exists args))
(define (ag-exists? x)(eq? (car x) 'ag-exists))
(define ag-exists-exp cadr)
;(define-struct ag-notexists (iterators exp))
(define (make-ag-notexists . args) (cons 'ag-notexists args))
(define (ag-notexists? x)(eq? (car x) 'ag-notexists))
(define ag-notexists-iterators cadr)
(define ag-notexists-exp caddr)
;(define-struct ag-forall (iterators exp))
(define (make-ag-forall . args) (cons 'ag-forall args))
(define (ag-forall? x)(eq? (car x) 'ag-forall))
(define ag-forall-iterators cadr)
(define ag-forall-exp caddr)
;(define-struct ag-skip ())
(define (make-ag-skip . args) (cons 'ag-skip args))
(define (ag-skip? x)(eq? (car x) 'ag-skip))
;(define-struct ag-from (dest source))
(define (make-ag-from . args) (cons 'ag-from args))
(define (ag-from? x)(eq? (car x) 'ag-from))
(define ag-from-dest cadr)
(define ag-from-source caddr)
;(define-struct ag-fromb (dest source))
(define (make-ag-fromb . args) (cons 'ag-fromb args))
(define (ag-fromb? x)(eq? (car x) 'ag-fromb))
(define ag-fromb-dest cadr)
(define ag-fromb-source caddr)
;(define-struct ag-frome (dest source))
(define (make-ag-frome . args) (cons 'ag-frome args))
(define (ag-frome? x)(eq? (car x) 'ag-frome))
(define ag-frome-dest cadr)
(define ag-frome-source caddr)
;(define-struct ag-mulvalmapref (var el))
(define (make-ag-mulvalmapref . args) (cons 'ag-mulvalmapref args))
(define (ag-mulvalmapref? x)(eq? (car x) 'ag-mulvalmapref))
(define ag-mulvalmapref-var cadr)
(define ag-mulvalmapref-el caddr)
;(define-struct ag-simple-call-or-selection (var el))
(define (make-ag-simple-call-or-selection . args) (cons 'ag-simple-call-or-selection args))
(define (ag-simple-call-or-selection? x)(eq? (car x) 'ag-simple-call-or-selection))
(define ag-simple-call-or-selection-val cadr)
(define ag-simple-call-or-selection-el caddr)
;(define-struct ag-stmt-if (condition then elseif else))
(define (make-ag-stmt-if . args) (cons 'ag-stmt-if args))
(define (ag-stmt-if? x)(eq? (car x) 'ag-stmt-if))
(define ag-stmt-if-condition cadr)
(define ag-stmt-if-then caddr)
(define ag-stmt-if-elseif caddr)
(define ag-stmt-if-else caddr)
;(define-struct ag-elseif (condition then))
(define (make-ag-stmt-elseif . args) (cons 'ag-stmt-elseif args))
(define (ag-stmt-elseif? x)(eq? (car x) 'ag-stmt-elseif))
(define ag-stmt-elseif-condition cadr)
(define ag-stmt-elseif-then caddr)
;(define-struct ag-stmt-loop (loopiter stmts))
(define (make-ag-stmt-loop . args) (cons 'ag-stmt-loop args))
(define (ag-stmt-loop? x)(eq? (car x) 'ag-stmt-loop))
(define ag-stmt-loop-loopiter cadr)
(define ag-stmt-loop-stmts caddr)
;(define-struct ag-loopiter-for (iterator))
(define (make-ag-loopiter-for . args) (cons 'ag-loopiter-for args))
(define (ag-loopiter-for? x)(eq? (car x) 'ag-loopiter-for))
(define ag-loopiter-for-iterator cadr)
;(define-struct ag-loopiter (init doing while step until term))
(define (make-ag-loopiter . args) (cons 'ag-loopiter args))
(define (ag-loopiter? x)(eq? (car x) 'ag-loopiter))
(define ag-loopiter-init cadr)
(define ag-loopiter-doing caddr)
(define ag-loopiter-while cadddr)
(define ag-loopiter-step (lambda (x) (car (cddddr x))))
(define ag-loopiter-until (lambda (x) (cadr (cddddr x))))
(define ag-loopiter-term (lambda (x) (caddr (cddddr x))))
;(define-struct ag-doing (stmts))
(define (make-ag-doing . args) (cons 'ag-doing args))
(define (ag-doing? x)(eq? (car x) 'ag-doing))
(define ag-doing-stmts cadr)
;(define-struct ag-while (condition))
(define (make-ag-while . args) (cons 'ag-while args))
(define (ag-while? x)(eq? (car x) 'ag-while))
(define ag-while-condition cadr)
;(define-struct ag-step (stmts))
(define (make-ag-step . args) (cons 'ag-step args))
(define (ag-step? x)(eq? (car x) 'ag-step))
(define ag-step-stmts cadr)
;(define-struct ag-until (condition))
(define (make-ag-until. args) (cons 'ag-until args))
(define (ag-until? x)(eq? (car x) 'ag-until))
(define ag-until-condition cadr)
;(define-struct ag-term (stmts))
(define (make-ag-term . args) (cons 'ag-term args))
(define (ag-term? x)(eq? (car x) 'ag-term))
(define ag-term-stmts cadr)
;(define-struct ag-formal (type var))
(define (make-ag-formal . args) (cons 'ag-formal args))
(define (ag-formal? x)(eq? (car x) 'ag-formal))
(define ag-proc-type cadr)
(define ag-proc-var caddr)
;(define-struct ag-proc (name args body))
(define (make-ag-proc . args) (cons 'ag-proc args))
(define (ag-proc? x)(eq? (car x) 'ag-proc))
(define ag-proc-name cadr)
(define ag-proc-args caddr)
(define ag-proc-body cadddr)
;(define-struct ag-refine (var stmts))
(define (make-ag-refine . args) (cons 'ag-refine args))
(define (ag-refine? x)(eq? (car x) 'ag-refine))
(define ag-refine-var cadr)
(define ag-refine-stmts caddr)

(define (lhst? x)
  (or (ag-identifier? x) (ag-skip? x)))


(define (ag-number? x)
  (or (ag-real? x)
    (ag-integer? x)))

(define (make-ag-number x)
  (cond ((integer? x) (make-ag-integer x))
    ((real? x) (make-ag-real x))
    (else (error "expects argument of number type: ~a" x))))

(define (ag-number-value x)
  (cond ((ag-integer? x) (ag-integer-value x))
    ((ag-real? x) (ag-real-value x))
    (else (error "expects argument of number type: ~a" x))))

(define (make-ag-tuple lst)
  (define tuple (make-hash-table))
  (define pos 0)
  (set-ag-tuple-highest! tuple pos)
  (for-each (lambda (val)
              (set! pos (+ pos 1))
              (ag-tuple-insert tuple pos val))
    lst)
  tuple)

(define (ag-tuple? x)
  (if  (and (hash-table? x) (ag-tuple-highest x))
    #t
    #f))

(define (ag-tuple-highest tuple)
  (hash-table-get tuple
      'highest
    (lambda ()
      #f)))

(define (set-ag-tuple-highest! tuple pos)
  (hash-table-put! tuple 'highest pos))

(define (ag-tuple-find-new-highest tuple oldhigh)
  (cond ((= oldhigh 0) 0)
    ((ag-omega? (ag-tuple-val tuple (- oldhigh 1)))
      (hash-table-remove! tuple oldhigh)
      (set-ag-tuple-highest! tuple (- oldhigh 1))
      (ag-tuple-find-new-highest tuple (- oldhigh 1)))
    (else
      (hash-table-remove! tuple oldhigh)
      (set-ag-tuple-highest! tuple (- oldhigh 1))
      (- oldhigh 1))))

(define (ag-tuple-delete tuple pos)
  (let ((high (ag-tuple-highest tuple)))
    (cond ((= high 0) #f)
      ((< pos high)
        (hash-table-remove! tuple pos))
      ((= pos high)
        (hash-table-remove! tuple pos)
        (ag-tuple-find-new-highest tuple high)))))

(define (ag-tuple-insert tuple pos val)
  (let ((high (ag-tuple-highest tuple)))
    (cond ((< pos 1) #f)
      ((<= pos high)
        (if (ag-omega? val)
          (ag-tuple-delete tuple pos)
          (hash-table-put! tuple pos val)))
      ((and (> pos high)
         (not (ag-omega? val)))
        (hash-table-put! tuple pos val)
        (set-ag-tuple-highest! tuple pos)))))

(define (ag-tuple-back-insert tuple val)
  (let ((high (ag-tuple-highest tuple)))
    (hash-table-put! tuple (+ high 1) val)
    (set-ag-tuple-highest! tuple (+ high 1))))



(define (ag-tuple-val tuple pos)
  (if (< pos 1)
    (make-ag-omega)
    (hash-table-get tuple
      pos
      (lambda ()
        (make-ag-omega)))))

(define (ag-tuple-empty? tuple)
  (= 0 (ag-tuple-highest tuple)))

(define (ag-tuple-empty)
  (define tuple (make-ag-tuple '()))
  (set-ag-tuple-highest! tuple 0)
  tuple)

(define (tuple-range t bg end)
  (let ((high (ag-tuple-highest t))
         (result (ag-tuple-empty)))
    (define (range pos rsltpos)
      (cond ((> pos high) result)
        ((and (>= pos bg) (<= pos end))
          (ag-tuple-insert result rsltpos (ag-tuple-val t pos))
          (range (+ pos 1) (+ rsltpos 1)))
        (else (range (+ pos 1) rsltpos))))
    (cond  ((ag-tuple-empty? t) t)
      ((> bg end) result)
      ((or (> bg high) (> end high)) #f)
      (else (range 1 1)))))

(define (ag-tuple->list t)
  (let ((high (ag-tuple-highest t)))
    (define (loop pos)
      (if (= pos high)
        (list (ag-tuple-val t pos))
        (cons (ag-tuple-val t pos)
          (loop (+ pos 1)))))
    (if (not (ag-tuple-empty? t))
      (loop 1)
        '())))


(define (ag-tuple-for-each f t)
  (let ((high (ag-tuple-highest t)))
    (define (loop pos)
      (if (= pos high)
        (f (ag-tuple-val t pos))
        (begin
          (f (ag-tuple-val t pos))
          (loop (+ pos 1)))))
    (if (not (ag-tuple-empty? t))
      (loop 1)
      #f)))

(define (ag-tuple-map f t)
  (make-ag-tuple (map f (ag-tuple->list t))))

(define (ag-tuple-insert-in t1 bgn end t2)
  (let ((end1 (+ 1 (ag-tuple-highest t1)))
         (end2 (+ 1 (ag-tuple-highest t2)))
         (rslt (ag-tuple-empty)))

    (define (insert-rest-of-tuple pos pos2)
      (when (< pos2 end2)
        (ag-tuple-insert rslt pos (ag-tuple-val t2 pos2))
        (insert-rest-of-tuple (+ pos 1) (+ pos2 1))))


    (define (insert pos1 pos2 pos)
      (cond ((= pos1 end1) (insert-rest-of-tuple pos pos2))
        ((< pos1 bgn)
          (ag-tuple-insert rslt
            pos
            (ag-tuple-val t1 pos1))
          (insert (+ pos1 1) pos2 (+ pos 1)))
        ((<= pos1 end)
          (if (>= pos2 end2)
            (insert (+ pos1 1) pos2 pos)
            (begin
              (ag-tuple-insert rslt
                pos
                (ag-tuple-val t2 pos2))
              (insert pos1 (+ pos2 1) (+ pos 1)))))
        ((> pos1 end)
          (ag-tuple-insert rslt
            pos
            (ag-tuple-val t1 pos1))
          (insert (+ pos1 1) pos2 (+ pos 1)))))

    (unless (or (> bgn end1 1)
              (> end end1 1))
      (insert 1 1 1))
    rslt))

;;;;;;;;;;;;;;;;;;; SETS

(define (determine-precedence x)
  (define precedences  (list ag-omega?
                         ag-boolean?
                         ag-integer?
                         ag-real?
                         ag-string?
                         ag-tuple?
                         ed-set?))
  (define (loop lst prec)
    (cond ((null? lst)
            (error 'determine-precedence "non-allowed set-element type: ~a" x))
      (((car lst) x) prec)
      (else (loop (cdr lst) (+ prec 1)))))
  (loop precedences 0))

(define (set-element-less? x y)
  (let ((px (determine-precedence x))
         (py (determine-precedence y)))
    (or (< px py)
      (and (= px py)
        (compare-types px x y)))))

(define (compare-types typenbr x y)
  (define types (vector
                  ;; om = om
                  (lambda (x y) #f)
                  ;; true < false
                  (lambda (x y) (and (ag-boolean-value x)
                                  (not (ag-boolean-value y))))
                  (lambda (x y) (< (ag-integer-value x)
                                  (ag-integer-value y)))
                  (lambda (x y) (< (ag-real-value x)
                                  (ag-real-value y)))
                  (lambda (x y) (string<? (ag-string-value x)
                                  (ag-string-value y)))
                  (lambda (x y) (ag-tuple-less? x y))
                  (lambda (x y) (ed-set-less? x y))))
  ((vector-ref types typenbr) x y))


(define (ag-tuple-less? x y)
  (let ((hx (ag-tuple-highest x))
         (hy (ag-tuple-highest y)))

    (define (compare-same-size pos)
      (cond ((= pos 1)
              (set-element-less? (ag-tuple-val x pos)
                (ag-tuple-val y pos)))
        ((set-element-less? (ag-tuple-val x pos)
           (ag-tuple-val y pos)) #t)
        (else (compare-same-size (- pos 1)))))

    (cond ((< hx hy) #t)
      ((= hx hy 0) #f)
      ((= hx hy)
        (compare-same-size hx))
      (else #f))))

(define (ed-set-less? x y)
  (send-message ed-set-manipulations 'less? x y))

(define ed-set-manipulations (tree-manipulations set-element-less?))

(define (make-ed-set contents)
  (send-message ed-set-manipulations 'construct-from-list contents))

(define (ed-set? x)
  (and (vector? x)
    (=  4 (vector-length x))))

(define (ed-set-map function x)
  (send-message ed-set-manipulations 'map x function))

(define (ed-set-for-each function x)
  (send-message ed-set-manipulations 'for-each x function))

(define (ed-set->list s)
  (send-message ed-set-manipulations 'convert-to-list s))

(define (list->ed-set l)
  (send-message ed-set-manipulations 'construct-from-list l))

(define (ed-set-size s)
  (tree-size s))

(define (ed-set-empty)
  (make-ed-set '()))

(define (ed-set-insert s e)
  (send-message ed-set-manipulations 'insert e s))

(define (ed-set-union s1 s2)
  (send-message ed-set-manipulations 'union s1 s2))

(define (ed-set-difference s1 s2)
  (send-message ed-set-manipulations 'difference s1 s2))

(define (ed-set-intersection s1 s2)
  (send-message ed-set-manipulations 'intersection s1 s2))

(define (make-reader scanner)
  (let ((setl #f)
         (token #f)
         (record-tokens #f)
         (recorded-tokens '()))

    (define (read-error message token)
      (send-message setl 'read-error message token))


    (define (read-expected-error . tokens)
      (read-error (format "expected ~a - given ~a"  tokens token) token))

    (define (parse-with-assert-expression) (parse-expression #t))
    (define (parse-without-assert-expression) (parse-expression #f))

    (define (get-token)
      (set! token (send-message scanner 'scan))
      (when (equal? (token-symbol token) err-symbol)
        (read-error (token-data token) token))
      (when record-tokens
        (set! recorded-tokens (cons token recorded-tokens))))

    (define (check-token symbol)
      (equal? (token-symbol token) symbol))

    (define (next-token token)
      (get-token)
      token)

    (define (check-and-get-token symbol)
      (cond ((check-token symbol)
              (get-token)
              #t)
        (else #f)))

    (define (check-and-get-tokens symbollist)
      (if (null? symbollist)
        #f
        (or (check-and-get-token (car symbollist))
          (check-and-get-tokens (cdr symbollist)))))

    (define (expect tkn ifok)
      (if (check-and-get-token tkn)
        (ifok)
        (read-expected-error tkn)))

    (define (start-record)
      (set! record-tokens #t)
      (set! recorded-tokens (list token)))

    (define (stop-record)
      (set! record-tokens #f)
      (reverse recorded-tokens))

    (define (match-recorded-tokens recorded)
      (define (match recorded)
        (cond ((null? recorded)
                (check-token end-symbol))
          ((check-token smc-symbol) #t)
          ((equivalent-tokens? token (car recorded))
            (get-token)
            (match (cdr recorded)))
          (else #f)))
      (unless (match recorded)
        (read-error "non-matching end tokens" token)))

    (define (read s)
      (set! setl s)
      (set! token (send-message scanner 'scan))
      (let ((prg (program)))
        (if (equal? (token-symbol token) end-symbol)
          prg
          (read-error "excess tokens" token))))

    (define (identifier assert)
      (define word (token-data token))
      (if (check-token wrd-symbol)
        (begin
          (get-token)
          (make-ag-identifier (string->symbol word)))
        (if assert
          (read-error "expected identifier" token)
          #f)))

    (define (read-list rule assert defaultconstructor)
      (define nt (rule))
      (if (assert nt)
        (cons nt (read-list rule assert defaultconstructor))
        (defaultconstructor nt)))

    (define (check-for-assignment ifok)
      (if (check-and-get-token ceq-symbol)
        (ifok)
        (read-error "expected assignment token :=" token)))

    (define (after-idf-stmt read-idf)
      (let* ((tkn token)
              (exp1 #f)
              (exp2 #f)
              (check-and-make-range
                (lambda ()
                  (check-for-assignment (lambda ()
                                          (make-ag-stmt-assignment
                                            (make-ag-range read-idf exp1 exp2)
                                            (parse-expression #t)))))))
        (cond ((check-and-get-token ceq-symbol)
                (make-ag-stmt-assignment read-idf (parse-expression #t)))
          ((check-and-get-token lpr-symbol)
            (cond ((check-and-get-token rpr-symbol)
                    (make-ag-stmt-call read-idf '()))
              ((begin
                 (set! exp1 (parse-expression #t))
                 (check-and-get-token range-keyword))
                (cond ((check-and-get-token rpr-symbol)
                        (check-and-make-range))
                  ((begin
                     (set! exp2 (parse-expression #t))
                     (check-and-get-token rpr-symbol))
                    (check-and-make-range))
                  (else
                    (read-error "expected .. or integer" token))))
              ((check-and-get-token rpr-symbol)
                (if (check-and-get-token ceq-symbol)
                  (make-ag-stmt-assignment
                    (make-ag-selection read-idf (list exp1)) (parse-expression #t))
                  (make-ag-stmt-call read-idf (list exp1))))
              ((check-and-get-token com-symbol)
                (let ((lst (cons exp1
                             (read-enumeration
                               parse-with-assert-expression
                               parse-with-assert-expression
                               rpr-symbol))))
                  (if (check-and-get-token ceq-symbol)
                    (make-ag-stmt-assignment (make-ag-selection read-idf lst)
                      (parse-expression #t))
                    (make-ag-stmt-call read-idf lst))))
              (else
                (read-error "expected a range, selection or application" token))))
          ((check-and-get-token lbc-symbol)
            (let ((exp (parse-expression #t)))
              (if (check-and-get-token rbc-symbol)
                (check-for-assignment
                  (lambda ()
                    (make-ag-stmt-assignment (make-ag-mulvalmapref read-idf exp)
                      (parse-expression #t))))
                (read-error
                  "expected end of multi-valued map reference" token))))
          ((check-token smc-symbol)
            (make-ag-stmt-call read-idf '()))
          ((check-and-get-tokens assigning)
            (expect ceq-symbol (lambda ()
                                 (make-ag-stmt-assignment
                                   read-idf
                                   (make-ag-application (token-symbol tkn)
                                     (list read-idf (parse-expression #t)))))))
          ((check-and-get-token from-keyword)
            (make-ag-from read-idf (lhs #t)))
          ((check-and-get-token fromb-keyword)
            (make-ag-fromb read-idf (lhs #t)))
          ((check-and-get-token frome-keyword)
            (make-ag-frome read-idf (lhs #t)))
          (else
            (read-error "expected application or assignment" token)))))

    (define (statements)
      (read-list statement
        (lambda (st)
          (and st (check-and-get-token smc-symbol)))
        (lambda (st)
          (if st
            (list st)
              '()))))

    (define (statement)
      (let ((idf (identifier #f)))
        (if (and idf (check-and-get-token col-symbol))
          (make-ag-statement idf (statement-body #f))
          (let ((body (statement-body idf)))
            (and body
              (make-ag-statement #f body))))))

    (define (statement-body read-idf)
      (define lh #f)
      (cond (read-idf (after-idf-stmt read-idf))
        ((check-token wrd-symbol) (after-idf-stmt (identifier #t)))
        ((begin (set! lh (lhs #f)) lh)
          (let ((tkn token))
            (cond ((check-and-get-token ceq-symbol)
                    (make-ag-stmt-assignment lh (parse-expression #t)))
              ((check-and-get-tokens assigning)
                (expect ceq-symbol
                  (lambda ()
                    (make-ag-stmt-assignment lh
                      (make-ag-application (token-symbol tkn)
                        (list lh (parse-expression #t)))))))
              ((check-and-get-token from-keyword) (make-ag-from lh (lhs #t)))
              ((check-and-get-token fromb-keyword)
                (make-ag-fromb lh (lhs #t)))
              ((check-and-get-token frome-keyword)
                (make-ag-frome lh (lhs #t)))
              (else (read-error "expected assignment" token)))))
        ((check-and-get-token exit-keyword) (make-ag-stmt-exit))
        ((check-and-get-token goto-keyword)
          (let ((idf (identifier #t)))
            (make-ag-stmt-goto idf)))
        ((check-and-get-token pass-keyword) (make-ag-stmt-pass))
        ((check-and-get-token stop-keyword) (make-ag-tmt-stop))
        ((check-token if-keyword) (if-statement))
        ((check-and-get-token return-keyword) (return-statement))
        ((check-token loop-keyword) (loop-statement))
        (else #f)))

    (define (iterlist constructor)
      (define lst (iter-elements))
      (expect sch-symbol
        (lambda () (constructor lst (parse-expression #t)))))

    (define (parse-expression assert)
      (cond ((check-and-get-token exists-keyword)
              (iterlist make-ag-exists))
        ((check-and-get-token notexists-keyword)
          (iterlist make-ag-notexists))
        ((check-and-get-token forall-keyword)
          (iterlist make-ag-forall))
        (else (parse-implication assert))))

    (define (parse-implication assert)
      (parse-op-infix parse-or (list impl-keyword) assert))

    (define (parse-or assert)
      (parse-op-infix parse-and (list or-keyword) assert))

    (define (parse-and assert)
      (parse-op-infix parse-not-is_xx (list and-keyword) assert))

    (define (parse-not-is_xx assert)
      (parse-unary-op parse-tests not-is_xx assert))

    (define (parse-tests assert)
      (parse-op-infix parse-maxmin rop-tokens assert))

    (define (parse-maxmin assert)
      (parse-op-infix parse-additive
        (list max-keyword min-keyword) assert))

    (define (parse-additive assert)
      (parse-op-infix parse-multiplicative
        (list pls-symbol mns-symbol) assert))

    (define (parse-multiplicative assert)
      (parse-op-infix parse-exponentiation
        (list mul-symbol div-symbol div-keyword mod-keyword) assert))

    (define (parse-exponentiation assert)
      (parse-op-infix parse-unary (list xop-symbol) assert))

    (define (parse-unary assert)
      (parse-unary-op parse-assignment unary-keywords assert))

    (define (parse-assignment assert)
      (define ref (parse-reference assert #t))
      (cond ((check-and-get-token ceq-symbol)
              (make-ag-stmt-assignment ref (parse-expression #t)))
        ((check-and-get-token from-keyword)
          (make-ag-from ref (lhs #t)))
        ((check-and-get-token fromb-keyword)
          (make-ag-fromb ref (lhs #t)))
        ((check-and-get-token frome-keyword)
          (make-ag-frome ref (lhs #t)))
        (else ref)))

    (define (parse-reference assert lhs-allowed)
      (define tkn token)
      (cond ((check-and-get-token int-symbol)
              (make-ag-integer (token-data tkn)))
        ((check-and-get-token rea-symbol)
          (make-ag-real (token-data tkn)))
        ((check-and-get-token txt-symbol)
          (make-ag-string (token-data tkn)))
        ((check-and-get-token lpr-symbol) (parentheses))
        ((check-token wrd-symbol) (after-idf-reference))
        ((check-and-get-token true-keyword)	(make-ag-boolean #t))
        ((check-and-get-token false-keyword) (make-ag-boolean #f))
        ((check-and-get-token omega-keyword) (make-ag-omega))
        ((check-and-get-token lbc-symbol) (set-former #f))
        ((and lhs-allowed (check-and-get-token mns-symbol))
          (make-ag-skip))
        ((check-and-get-token lbr-symbol) (tuple-former #f))
        ((check-and-get-token exists-keyword)
          (iterlist make-ag-exists))
        ((check-and-get-token notexists-keyword)
          (iterlist make-ag-notexists))
        ((check-and-get-token forall-keyword)
          (iterlist make-ag-forall))
        (else
          (and assert (read-error "expected a reference token" token)))))


    (define (after-idf-reference)
      (let ((idf (identifier #t))
             (exp1 #f)
             (exp2 #f))
        (cond  ((check-and-get-token lpr-symbol)
                 (cond ((check-and-get-token rpr-symbol)
                         (make-ag-stmt-call idf '()))
                   ((begin (set! exp1 (parse-expression #t))
                      (check-and-get-token range-keyword))
                     (cond ((check-and-get-token rpr-symbol)
                             (make-ag-range idf exp1 #f))
                       ((begin (set! exp2 (parse-expression #t))*
                          (check-and-get-token rpr-symbol))
                         (make-ag-range idf exp1 exp2))
                       (else (read-expected-error
                               int-symbol rpr-symbol))))
                   ((check-and-get-token rpr-symbol)
                     (if (check-token ceq-symbol)
                       (make-ag-selection idf (list exp1))
                       (make-ag-simple-call-or-selection
                         idf (list exp1))))
                   ((check-and-get-token com-symbol)
                     (set! exp2 (cons exp1
                                  (read-enumeration parse-with-assert-expression
                                    parse-with-assert-expression
                                    rpr-symbol)))
                     (make-ag-stmt-call idf exp2))))
          ((check-and-get-token lbc-symbol)
            (set! exp1 (parse-expression #t))
            (expect rbc-symbol
              (lambda () (make-ag-mulvalmapref idf exp))))
          (else idf))))


    (define (set-former expect-cst)
      (former parse-without-assert-expression
        parse-without-assert-expression
        rbc-symbol
        make-ag-set
        (if expect-cst
          (lambda (x y)
            (read-error "set iterators aren't constant" token))
          make-ag-setiterator)
        make-ag-setslice))

    (define (tuple-former expect-cst)
      (former parse-without-assert-expression
        parse-without-assert-expression
        rbr-symbol
        make-ag-tuple
        (if expect-cst
          (lambda (x y)
            (read-error "tuple iterators aren't constant" token))
          make-ag-tupleiterator)
        make-ag-tupleslice))

    (define (former firstrule
              restrule
              endsymbol
              constructor
              itconstructor
              sliceconstructor)
      (let ((exp1 (firstrule))
             (exp2 #f)
             (exp3 #f)
             (it #f))
        (cond ((and (not exp1) (check-and-get-token endsymbol))
                (constructor '()))
          ((check-and-get-token endsymbol)
            (constructor (list exp1)))
          ((check-and-get-token col-symbol)
            (set! it (iterator))
            (expect endsymbol (lambda () (itconstructor exp1 it))))
          ((check-and-get-token range-keyword)
            (set! exp2 (firstrule))
            (expect endsymbol
              (lambda () (sliceconstructor #f exp1 exp2))))
          ((check-and-get-token com-symbol)
            (set! exp2 (firstrule))
            (cond ((check-and-get-token endsymbol)
                    (constructor (list exp1 exp2)))
              ((check-and-get-token range-keyword)
                (set! exp3 (firstrule))
                (expect endsymbol
                  (lambda () (sliceconstructor exp1 exp2 exp3))))
              ((check-and-get-token com-symbol)
                (constructor (cons exp1
                               (cons exp2
                                 (read-enumeration firstrule
                                   restrule
                                   endsymbol)))))
              (else (read-error
                      "not a valid container former" token))))
          (else (read-error "not a valid container former" token)))))

    (define (iterator)
      (define lst (iter-elements))
      (if (check-and-get-token sch-symbol)
        (make-ag-iterator lst (parse-expression #t))
        (make-ag-iterator lst (make-ag-boolean #t))))

    (define (iter-elements)
      (read-list (lambda () (iter-element #t))
        (lambda (itel) (and itel (check-and-get-token com-symbol)))
        (lambda (itel) (if itel (list itel) '()))))

    (define (iter-element assert)
      (define lefthand (lhs assert))
      (cond ((check-and-get-token in-keyword)
              (make-ag-iterelement-in lefthand (parse-expression #t)))
        ((check-and-get-token eql-symbol)
          (let ((idf (identifier #t)))
            (cond ((check-and-get-token lpr-symbol)
                    (make-ag-iterelement-selection
                      lefthand idf (read-enumeration
                                     (lambda () (lhs #t))
                                     (lambda () (lhs #f))
                                     rpr-symbol)))
              ((check-and-get-token lbc-symbol)
                (make-ag-iterelement-sselection
                  lefthand idf (read-enumeration (lambda () (lhs #t))
                                 (lambda () (lhs #f))
                                 rbc-symbol)))
              (else (read-expected-error lpr-symbol lbc-symbol)))))
        (assert (read-error "expected an iteration element" token))
        (else #f)))

    (define (lefthand rule empty-arglist)
      (define lh (rule))
      (cond ((check-and-get-token lpr-symbol)
              (if (check-and-get-token rpr-symbol)
                (empty-arglist lh)
                (former (lambda () (parse-reference #t #t))
                  (lambda () (parse-reference #f #t))
                  lpr-symbol
                  (lambda (s) (make-ag-selection lh s))
                  (lambda (e i)
                    (read-error "iterator construct not allowed" token))
                  (lambda (first bgn end)
                    (if first
                      (read-error "invalid range" token)
                      (make-ag-range lh bgn end))))))
        ((check-and-get-token lbc-symbol)
          (make-ag-sselection lh
            (read-enumeration parse-with-assert-expression
              parse-without-assert-expression
              rbc-symbol)))
        (else lh)))

    (define (lhs assert)
      (cond ((check-token wrd-symbol)
              (lefthand (lambda () (identifier #t))
                (lambda (x)
                  (read-error
                    "selection should contain at least one expression" token))))
        ((check-and-get-token lbr-symbol)
          (lefthand (lambda ()
                      (make-ag-tuple (read-enumeration (lambda ()
                                                         (lhst #t))
                                       (lambda ()
                                         (lhst #f))
                                       rbr-symbol)))
            (lambda (x)
              (read-error "selection should contain at least one expression" token))))
        (assert (read-error "expected a left hand side" token))
        (else #f)))

    (define (lhst assert)
      (define lefthand (lhs #f))
      (cond (lefthand lefthand)
        ((check-and-get-token mns-symbol) (make-ag-skip))
        (assert (read-error "expected a left hand side or -" token))
        (else #f)))

    (define (parse-op-infix rule tokenlist assert)
      (define args (rule assert))
      (define argscopy args)
      (define (loop)
        (if (member (token-symbol token) tokenlist)
          (let* ((operator (token-symbol (next-token token)))
                  (assign (check-and-get-token ceq-symbol))
                  (arg (list args (rule assert))))
            (if assign
              (set! args
                (make-ag-stmt-assignment argscopy (make-ag-application operator arg)))
              (set! args (make-ag-application operator arg)))
            (loop))))
      (loop)
      args)

    (define (parse-unary-op rule tokenlist assert)
      (define (loop)
        (if (member (token-symbol token) tokenlist)
          (make-ag-application (token-symbol (next-token token))
            (list (loop)))
          (rule assert)))
      (loop))

    (define (parentheses)
      (let ((exp (parse-expression #t)))
        (expect rpr-symbol (lambda () exp))))

    (define (end-of-statement)
      (expect smc-symbol 'void))


    (define (elseif)
      (and (check-and-get-token elseif-keyword)
        (make-ag-elseif (parse-expression #t)
          (expect then-keyword statements))))


    (define (elseif-list)
      (read-list elseif (lambda (x) (and x (check-token elseif-keyword)))
        (lambda (x) (if x (list x) '()))))

    (define (if-statement)
      (let ((if-cond #f)
             (if-then '())
             (if-elseif '())
             (if-else '())
             (rec #f))

        (define (parse-elseif)
          (when (check-token elseif-keyword)
            (set! if-elseif (elseif-list)))
          (parse-else))

        (define (parse-else)
          (when (check-and-get-token else-keyword)
            (set! if-else (statements)))
          (parse-end))

        (define (parse-end)
          (end rec)
          (make-ag-stmt-if if-cond if-then if-elseif if-else))

        (start-record)
        (get-token)
        (set! if-cond (parse-expression #t))
        (set! rec (stop-record))
        (expect then-keyword (lambda ()
                               (set! if-then (statements))
                               (parse-elseif)))))

    (define (loop-statement)
      (let ((iter #f)
             (stmts #f)
             (rec #f))
        (start-record)
        (get-token)
        (set! iter (loopiter))
        (set! rec (stop-record))
        (expect do-keyword (lambda ()
                             (set! stmts (statements))
                             (end rec)
                             (make-ag-stmt-loop iter stmts)))))

    (define (loopiter)
      (if (check-and-get-token for-keyword)
        (make-ag-loopiter-for (iterator))
        (make-ag-loopiter
          (if (check-and-get-token init-keyword)
            (make-ag-init (statements)) #f)
          (if (check-and-get-token doing-keyword)
            (make-ag-doing (statements)) #f)
          (if (check-and-get-token while-keyword)
            (make-ag-while (parse-expression #t)) #f)
          (if (check-and-get-token step-keyword)
            (make-ag-step (statements)) #f)
          (if (check-and-get-token until-keyword)
            (make-ag-until (parse-expression #t)) #f)
          (if (check-and-get-token term-keyword)
            (make-ag-term (statements)) #f))))

    (define (return-statement)
      (define exp (parse-expression #f))
      (make-ag-stmt-return exp))


    (define (proclist)
      (read-list proc
        (lambda (proc) (and proc (check-and-get-token smc-symbol)))
        (lambda (proc)
          (if proc
            (list proc)
              '()))))

    (define (proc)
      (start-record)
      (if (not (check-and-get-token proc-keyword))
        (begin (stop-record) #f)
        (let ((procname (identifier #t))
               (arglist '())
               (bdy '())
               (rec #f))
          (when (check-and-get-token lpr-symbol)
            (set! arglist (read-enumeration formal formal rpr-symbol)))
          (set! rec (stop-record))
          (expect smc-symbol (lambda ()
                               ; (set! rec (stop-record))
                               (set! bdy (body))
                               (end rec)
                               (make-ag-proc procname arglist bdy))))))

    (define (formal)
      (define type
        (cond ((check-token rd-keyword) (next-token (token-symbol token)))
          ((check-token rw-keyword) (next-token (token-symbol token)))
          ((check-token wr-keyword) (next-token (token-symbol token)))
          (else #f)))
      (make-ag-formal type (identifier #t)))

    (define (refine)
      (define idf (identifier #f))
      (if (not idf)
        #f
        (expect dcl-symbol (lambda ()
                             (make-ag-refine idf (statements))))))

    (define (refine-list)
      (read-list refine
        (lambda (rf)
          (and rf (check-and-get-token smc-symbol)))
        (lambda (rf)
          (if rf
            (list rf)
              '()))))

    (define (var-decl)
      (define var-decl-list (read-list (lambda ()
                                         (identifier #t))
                              (lambda (x)
                                (check-and-get-token com-symbol))
                              list))
      (end-of-statement)
      var-decl-list)

    (define (const-decl-item)
      (define idf (identifier #t))
      (if (check-and-get-token eql-symbol)
        (let ((cst (constant)))
          (make-ag-constdeclaration idf cst))
        idf))

    (define (init-decl)
      (define init-decl-list (read-list init-decl-item
                               (lambda (x)
                                 (check-and-get-token com-symbol))
                               list))
      (end-of-statement)
      init-decl-list)

    (define (init-decl-item)
      (define idf (identifier #t))
      (expect ceq-symbol (lambda ()
                           (define cst (constant))
                           (make-ag-initdeclaration idf cst))))

    (define (read-enumeration firstrule restrule last-token)
      (define first (firstrule))
      (if first
        (if (check-and-get-token last-token)
          (list first)
          (expect com-symbol (lambda ()
                               (define rest
                                 (read-list restrule
                                   (lambda (x)
                                     (check-and-get-token com-symbol))
                                   list))
                               (expect last-token (lambda () (cons first rest))))))
        (expect last-token (lambda () '()))))

    (define (signed-number mns)
      (define (make-signed-int)
        (if mns
          (make-ag-integer (- (token-data token)))
          (make-ag-integer (token-data token))))
      (define (make-signed-real)
        (if mns
          (make-ag-real (- (token-data token)))
          (make-ag-real (token-data token))))
      (cond ((check-and-get-token int-symbol) (make-signed-int))
        ((check-and-get-token rea-symbol) (make-signed-real))
        (else (read-expected-error int-symbol rea-symbol))))


    (define (cst)
      (define tkn token)
      (cond ((check-and-get-token pls-symbol) (signed-number #f))
        ((check-and-get-token mns-symbol) (signed-number #t))
        ((check-and-get-token int-symbol)
          (make-ag-integer (token-data tkn)))
        ((check-and-get-token rea-symbol)
          (make-ag-real (token-data tkn)))
        ((check-and-get-token txt-symbol)
          (make-ag-string (token-data tkn)))
        ((check-and-get-token lbc-symbol) (set-former #t))
        ((check-and-get-token lbr-symbol) (tuple-former #t))
        (else #f)))

    (define (constant)
      (or (cst) (read-error "not a constant" token)))

    (define (const-decl)
      (define const-decl-list
        (read-list const-decl-item
          (lambda (x)
            (check-and-get-token com-symbol))
          list))
      (end-of-statement)
      const-decl-list)

    (define (declarations var const init)
      (cond ((check-and-get-token var-keyword)
              (if (null? var)
                (declarations (var-decl) const init)
                (read-error
                  "duplicate global variable declarations" token)))
        ((check-and-get-token const-keyword)
          (if (null? const)
            (declarations var (const-decl) init)
            (read-error
              "duplicate global constant declarations" token)))
        ((check-and-get-token init-keyword)
          (if (null? init)
            (declarations var const (init-decl))
            (read-error "duplicate global init declarations" token)))
        (else (make-ag-declarations var const init))))

    (define (end rec)
      (expect end-keyword (lambda ()
                            (match-recorded-tokens rec))))

    (define (body)
      (let ((decl (declarations '() '() '()))
             (stmt (statements)))
        (make-ag-body decl stmt #f)))

    (define (program-body)
      (let ((decl (declarations '() '() '()))
             (stmt (statements))
             (procs (proclist))
             (refs (refine-list)))
        (make-ag-programbody decl stmt procs refs)))

    (define (program)
      (let ((rec #f))
        (define (program-name)
          (define idf (identifier #t))
          (set! rec (stop-record))
          (end-of-statement)
          idf)

        (start-record)
        (expect program-keyword (lambda ()
                                  (define name (program-name))
                                  (define bdy (program-body))
                                  (end rec)
                                  (check-and-get-token smc-symbol)
                                  (make-ag-program name bdy)))))

    (define (dispatch message)
      (case message
        ((read) read)
        (else (error 'parser "unknown message"))))

    dispatch))

;; Environments
;; ============


;; Creating

(define make-environment make-hash-table)

;; Printing environments, useful for debugging
(define (print-environment env)
  (hash-table-for-each env (lambda (v k)
                             (out v " - " k)
                             (newline))))


;; Predicate to see if a variable is defined in the given environment
(define (defined-in-env? env idf)
  (define name (ag-identifier-name idf))
  (hash-table-get env name (lambda () #f)))

;; Add a variable binding to the environment
(define (add-to-environment! env idf val cst onduplicate)
  (define name (ag-identifier-name idf))
  (if (defined-in-env? env idf)
    (onduplicate name)
    (hash-table-put! env name (cons val cst))))

;; Returns the environment in which the variable is defined
(define (find-env env glob idf)
  (cond ((defined-in-env? env idf) env)
    ((defined-in-env? glob idf) glob)
    (else #f)))

;; Alter the value of a variable in the environment
(define (set-variable-value! env glob idf val onconstant)
  (let ((found (find-env glob env idf))
         (name (ag-identifier-name idf)))
    (cond ((not found) (hash-table-put! env name (cons val #f)) val)
      ((cdr (defined-in-env? found idf)) (onconstant name))
      (else (hash-table-put! found name (cons val #f)) val))))

;; Returns the value of a variable
(define (get-variable-value env glob idf onunknown)
  (let ((found (find-env env glob idf))
         (name (ag-identifier-name idf)))
    (if (not found)
      (onunknown name)
      (hash-table-get found name))))



(define (make-natives type-check-function report-eval-error)
  (let ((operators (make-hash-table))
         (scheme-procs (make-hash-table))
         (type type-check-function)
         (eval-error report-eval-error)
         (setl #f))


    ;; Procedures

    (define (scheme-proc? idf)
      (hash-table-get scheme-procs
        (ag-identifier-name idf) (lambda () #f)))

    (define (add-scheme-proc name function)
      (hash-table-put! scheme-procs name function))

    (define (install-scheme-procs)
      (add-scheme-proc 'print
        (lambda (exp)
          (send-message setl 'print (print-ag exp)))))

    (define (apply-scheme-proc idf actual)
      (let ((name (ag-identifier-name idf)))
        (apply (hash-table-get scheme-procs
                 name
                 (lambda ()
                   (eval-error
                     (format "unimplemented native procedure: ~a" name))))
          actual)))

    ;; Operators

    (define (add-operator keyword function)
      (hash-table-put! operators keyword function))

    (define (install-operators)
      (add-operator pls-symbol plus)
      (add-operator ceil-keyword ceil)
      (add-operator div-keyword div)
      (add-operator mns-symbol minus)
      (add-operator mul-symbol mul)
      (add-operator div-symbol division)
      (add-operator xop-symbol exponentiation)
      (add-operator eql-symbol equal)
      (add-operator dif-symbol different)
      (add-operator lss-symbol less)
      (add-operator and-keyword logand)
      (add-operator or-keyword logor)
      (add-operator not-keyword lognot)
      (add-operator els-symbol equal-or-less)
      (add-operator grt-symbol greater)
      (add-operator egr-symbol equal-or-greater)
      (add-operator impl-keyword implication)
      (add-operator in-keyword in)
      (add-operator notin-keyword notin)
      (add-operator max-keyword smax)
      (add-operator min-keyword smin)
      (add-operator mod-keyword mod)
      (add-operator amt-symbol amount)
      (add-operator is_tuple-keyword is_tuple)
      (add-operator is_boolean-keyword is_boolean)
      (add-operator is_integer-keyword is_integer)
      (add-operator is_set-keyword is_set)
      (add-operator is_real-keyword is_real)
      (add-operator is_string-keyword is_string))



    (define (apply-operator keyword args env)
      (apply (hash-table-get operators
               keyword
               (lambda ()
                 (eval-error
                   (format "unimplemented native: ~a" keyword))))
        args))

    ;; Operator tools

    (define (assure-length lst len)
      (unless (= (length lst) len)
        (eval-error
          (format "non-matching argument count: ~a ~a" len lst))))

    (define (assure-pos-int int)
      (type ag-integer? int)
      (unless (>= (ag-integer-value int) 0)
        (eval-error (format "non-positive integer ~a" int))))

    (define (assure-pos-real int)
      (type ag-real? int)
      (unless (>= (ag-real-value int) 0)
        (eval-error (format "non-positive real ~a" int))))

    (define (assure-non-zero-int int)
      (type ag-integer? int)
      (when (= (ag-integer-value int) 0)
        (eval-error
          (format "integer must be different from zero ~a" int))))

    (define (assure-non-zero-real real)
      (type ag-real? real)
      (when (= (ag-real-value real) 0)
        (eval-error (format "real must be different from zero ~a" int))))

    (define (operator-unary args check constructor accessor function)
      (assure-length args 1)
      (let ((arg (car args)))
        (type check arg)
        (constructor (function (accessor arg)))))

    (define (operator-binary args
              check1
              check2
              constructor
              accessor1
              accessor2
              function)
      (assure-length args 2)
      (let ((first (car args))
             (second (cadr args)))
        (type check1 first)
        (type check2 second)
        (constructor (function (accessor1 first)
                       (accessor2 second)))))


    ;; Operator definitions

    (define (mod . args)
      (operator-binary args
        ag-integer?
        ag-integer?
        make-ag-integer
        ag-integer-value
        ag-integer-value
        (lambda (x y) (modulo x y))))

    (define (logand . args)
      (operator-binary args
        ag-boolean?
        ag-boolean?
        make-ag-boolean
        ag-boolean-value
        ag-boolean-value
        (lambda (x y) (and x y))))

    (define (logor . args)
      (operator-binary args
        ag-boolean?
        ag-boolean?
        make-ag-boolean
        ag-boolean-value
        ag-boolean-value
        (lambda (x y) (or x y))))

    (define (different . args)
      (operator-binary args
        identity
        identity
        make-ag-boolean
        identity
        identity
        (lambda (x y)
          (not (equal? x y)))))

    (define (equal . args)
      (assure-length args 2)
      (let ((first (car args))
             (second (cadr args)))
        (cond ((ag-integer? first)
                (make-ag-boolean (and (ag-integer? second)
                                   (= (ag-integer-value first)
                                     (ag-integer-value second)))))
          ((ag-real? first)
            (make-ag-boolean (and (ag-real? second)
                               (= (ag-real-value first)
                                 (ag-real-value second)))))
          ((ag-string? first)
            (make-ag-boolean (and (ag-string? second)
                               (equal? (ag-string-value first)
                                 (ag-string-value second)))))
          ((ag-boolean? first)
            (make-ag-boolean (and (ag-boolean? second)
                               (equal? (ag-boolean-value first)
                                 (ag-boolean-value second)))))
          ((ag-omega? first)
            (make-ag-boolean (ag-omega? second)))
          (else (make-ag-boolean #f)))))


    (define (less . args)
      (assure-length args 2)
      (let ((first (car args))
             (second (cadr args)))
        (cond ((ag-integer? first)
                (type ag-integer? second)
                (make-ag-boolean (< (ag-integer-value first)
                                   (ag-integer-value second))))
          ((ag-real? first)
            (type ag-real? second)
            (make-ag-boolean (< (ag-real-value first)
                               (ag-real-value second))))
          ((ag-string? first)
            (type ag-string? second)
            (make-ag-boolean (string<? (ag-string-value first)
                               (ag-string-value second))))
          (else
            (eval-error (format "invalid argument types for <: ~a" args))))))

    (define (amount . args)
      (assure-length args 1)
      (let ((arg (car args)))
        (make-ag-integer
          (cond ((ed-set? arg) (ed-set-size arg))
            ((ag-string? arg) (string-length (ag-string-value arg)))
            ((ag-tuple? arg) (ag-tuple-highest arg))
            (else
              (eval-error
                (format "invalid argument types for #: ~a" args)))))))

    (define (lognot . args)
      (operator-unary args
        ag-boolean?
        make-ag-boolean
        ag-boolean-value
        (lambda (x) (not x))))

    (define (is_tuple . args)
      (operator-unary args
        identity
        make-ag-boolean
        identity
        (lambda (x) (ag-tuple? x))))

    (define (is_boolean . args)
      (operator-unary args
        identity
        make-ag-boolean
        identity
        (lambda (x) (ag-boolean? x))))

    (define (is_integer . args)
      (operator-unary args
        identity
        make-ag-boolean
        identity
        (lambda (x) (ag-integer? x))))

    (define (is_set . args)
      (operator-unary args
        identity
        make-ag-boolean
        identity
        (lambda (x) (ed-set? x))))

    (define (is_real . args)
      (operator-unary args
        identity
        make-ag-boolean
        identity
        (lambda (x) (ag-real? x))))

    (define (is_string . args)
      (operator-unary args
        identity
        make-ag-boolean
        identity
        (lambda (x) (ag-string? x))))


    (define (equal-or-less . args)
      (logor (apply equal args)
        (apply less args)))

    (define (greater . args)
      (lognot (apply equal-or-less args)))

    (define (equal-or-greater . args)
      (lognot (apply less args)))

    (define (implication . args)
      (operator-binary args
        ag-boolean?
        ag-boolean?
        make-ag-boolean
        ag-boolean-value
        ag-boolean-value
        (lambda (x y) (or (not x) y))))

    (define (ceil . args)
      (operator-unary args ag-real? make-ag-real ag-real-value ceiling))

    (define (div . args)
      (operator-binary args
        (lambda (x) (type ag-number? x))
        (lambda (x)
          (type ag-number? x)
          (unless (not (= 0 (ag-number-value x)))
            (eval-error "division by zero")))
        make-ag-number
        ag-number-value
        ag-number-value
        quotient))


    (define (minus . args)
      (assure-length args 2)
      (let ((first (car args))
             (second (cadr args)))
        (cond ((ag-number? first)
                (type ag-number? second)
                (make-ag-number (- (ag-number-value first)
                                  (ag-number-value second))))
          ((ed-set? first)
            (type ed-set? second)
            (ed-set-difference first second))
          (else
            (eval-error (format "wrong argument types for -: ~a" args))))))

    (define (plus . args)
      (assure-length args 2)
      (let ((first (car args))
             (second (cadr args)))
        (cond ((ag-number? first)
                (type ag-number? second)
                (make-ag-number (+ (ag-number-value first)
                                  (ag-number-value second))))
          ((ag-string? first)
            (when (ag-number? second)
              (set! second
                (make-ag-string (number->string (ag-number-value second)))))
            (type ag-string? second)
            (make-ag-string (string-append (ag-string-value first)
                              (ag-string-value second))))
          ((ag-tuple? first)
            (type ag-tuple? second)
            (make-ag-tuple (append (ag-tuple->list first)
                             (ag-tuple->list second))))
          ((ed-set? first)
            (type ed-set? second)
            (ed-set-union first second))

          (else
            (eval-error (format "wrong argument types for +: ~a" args))))))

    (define (mul . args)
      (define (mul-string s i)
        (assure-pos-int i)
        (make-ag-string  (duplicate-string (ag-string-value s)
                           (ag-integer-value i))))
      (define (mul-tuple t i)
        (assure-pos-int i)
        (make-ag-tuple (duplicate-list (ag-tuple-contents t)
                         (ag-integer-value i))))
      (assure-length args 2)
      (let ((first (car args))
             (second (cadr args)))
        (cond ((ag-integer? first)
                (cond ((ag-number? second)
                        (make-ag-number (* (ag-number-value first)
                                          (ag-number-value second))))
                  ((ag-string? second) (mul-string second first))
                  ((ag-tuple? second) (mul-tuple second first))
                  (else
                    (eval-error (format "wrong argument types for *: ~a" args)))))
          ((ag-string? first) (mul-string first second))
          ((ag-tuple? first) (mul-tuple first second))
          ((ed-set? first)
            (type ed-set? second)
            (ed-set-intersection first second))
          (else
            (eval-error (format "wrong argument types for *: ~a" args))))))

    (define ( division . args)
      (assure-length args 2)
      (let ((first (car args))
             (second (cadr args)))
        (cond ((ag-integer? first)
                (assure-non-zero-int second)
                (make-ag-real (/ (ag-integer-value first)
                                (ag-integer-value second)))))))

    (define (exponentiation  . args)
      (assure-length args 2)
      (let ((first (car args))
             (second (cadr args)))
        (cond ((ag-integer? first)
                (assure-pos-int second)
                (make-ag-integer (expt (ag-integer-value first)
                                   (ag-integer-value second))))
          ((ag-real? first)
            (cond ((ag-real? second)
                    (assure-pos-real second)
                    (make-ag-real (expt (ag-real-value first)
                                    (ag-real-value second))))
              ((ag-integer? second)
                (assure-pos-int second)
                (make-ag-real (expt (ag-real-value first)
                                (ag-integer-value second))))
              (else
                (eval-error (format "wrong argument types for /: ~a" args)))))
          (else
            (eval-error (format "wrong argument types for /: ~a" args))))))

    (define (in . args)
      (assure-length args 2)
      (let ((first (car args))
             (second (cadr args)))
        (cond ((and (ag-string? first) (ag-string? second))
                (make-ag-boolean
                  (sublist (string->list (ag-string-value first))
                    (string->list (ag-string-value second)))))
          ((ag-tuple? second)
            (make-ag-boolean
              (if (member first (ag-tuple-contents second)) #t #f)))
          (else
            (eval-error (format "wrong argument types for in: ~a" args))))))

    (define (notin . args)
      (lognot (apply in args)))

    (define (smax . args)
      (assure-length args 2)
      (let ((first (car args))
             (second (cadr args)))
        (cond ((ag-integer? first)
                (type ag-integer? second)
                (make-ag-integer (max (ag-integer-value first)
                                   (ag-integer-value second))))
          ((ag-real? first)
            (type ag-real? second)
            (make-ag-real (max (ag-real-value first)
                            (ag-real-value second))))
          (else
            (eval-error (format "wrong argument types for max: ~a" args))))))


    (define (smin . args)
      (assure-length args 2)
      (let ((first (car args))
             (second (cadr args)))
        (cond ((ag-integer? first)
                (type ag-integer? second)
                (make-ag-integer (min (ag-integer-value first)
                                   (ag-integer-value second))))
          ((ag-real? first)
            (type ag-real? second)
            (make-ag-real (min (ag-real-value first)
                            (ag-real-value second))))
          (else
            (eval-error (format "wrong argument types for min: ~a" args))))))


    (define (initialize s)
      (set! setl s)
      (install-scheme-procs)
      (install-operators))

    (define (dispatch message)
      (case message
        ((initialize) initialize)
        ((scheme-proc?) scheme-proc?)
        ((apply-scheme-proc) apply-scheme-proc)
        ((apply-operator) apply-operator)
        (else (error 'make-natives "unknown message ~a" message))))

    dispatch))


(define (print-tuple t)
  (let ((high (ag-tuple-highest t)))
    (define (ag-tuple-print t)
      (let loop ((pos 1))
        (if (< pos high)
          (string-append (print-ag (ag-tuple-val t pos))
            " "
            (loop (+ pos 1)))
          (print-ag (ag-tuple-val t pos)))))
    (string-append "["
      (if (ag-tuple-empty? t)
        " "
        (ag-tuple-print t))
      "]")))

(define (print-set s)
  (define str "")
  (ed-set-for-each (lambda (x)
                     (set! str (string-append (print-ag x) " " str)))
    s)
  (string-append "{ "
    str
    "}"))

(define (print-omega o)
  "om")


(define (print-ag ag)
  (cond ((list? ag) (for-each print-ag ag))
    ((ag-integer? ag) (number->string (ag-integer-value ag)))
    ((ag-real? ag) (number->string (ag-real-value ag)))
    ((ag-string? ag) (ag-string-value ag))
    ((ag-tuple? ag) (print-tuple ag))
    ((ed-set? ag) (print-set ag))
    ((ag-omega? ag) (print-omega ag))
    (else (format "<~a>" ag))))

;; Main evaluator

(define (make-evaluator)
  (let ((setl #f) ;; shared SETL session
         ;; ag-program properties
         (name #f)
         (body #f)
         (declarations #f)
         (statements #f)
         (refines #f)
         (routines #f)
         (decvar #f)
         (decconst #f)
         (decinit #f)
         ;; Environment & natives
         (natives #f)
         (global #f))


    ;; Auxiliary functions
    (define (type t exp)
      (unless (t exp)
        (eval-error (format "wrong type of expression: ~a" exp))))

    (define (err-duplicate name)
      (eval-error (format "duplicate variable: ~a" name)))

    (define (err-constant name)
      (eval-error (format "assignment to constant variable: ~a" name)))

    (define (err-unknown name)
      (eval-error (format "unknown variable: ~a" name)))


    (define (eval-error message)
      (send-message setl 'eval-error message))

    ;; initialize
    (define (init program s)
      (set! setl s)
      (set! name (ag-program-name program))
      (set! body (ag-program-body program))
      (set! declarations (ag-programbody-declarations body))
      (set! statements (ag-programbody-statements body))
      (set! refines (ag-programbody-refines body))
      (set! routines (ag-programbody-routines body))
      (set! decvar (ag-declarations-var declarations))
      (set! decconst (ag-declarations-const declarations))
      (set! decinit (ag-declarations-init declarations))
      (set! global (make-environment))
      (set! natives (make-natives type eval-error))
      (send-message natives 'initialize setl))

    (define (process-declarations var const init env)
      ;; add the global variables to the environment
      (for-each
        (lambda (idf)
          (add-to-environment! env
            idf
            (make-ag-omega)
            #f
            err-duplicate))
        var)
      ;; add all constants
      (for-each
        (lambda (constvar)
          (add-to-environment! env
            (ag-constdeclaration-var constvar)
            (ag-constdeclaration-constant constvar)
            #t
            err-duplicate))
        const)
      ;; add all init variables
      (for-each
        (lambda (initvar)
          (add-to-environment! env
            (ag-initdeclaration-var initvar)
            (evaluate (ag-initdeclaration-init initvar) env)
            #f
            err-duplicate))
        init))
    ;; procedures
    (define (process-routines procs)
      (for-each
        (lambda (proc)
          (add-to-environment!
            global (ag-proc-name proc) proc  #t err-duplicate))
        procs))

    (define (eval-program program setl)
      ;; initialize
      (init program setl)
      (process-declarations decvar decconst decinit global)
      (process-routines routines)
      ;; evaluate each statement in the global environment
      (for-each
        (lambda (statement)
          (evaluate (ag-statement-body statement) global))
        statements))

    (define (self-evaluating? exp)
      (or (ag-integer? exp)
        (ag-real? exp)
        (ag-string? exp)
        (ag-omega? exp)
        (ag-proc? exp)
        (ag-stmt-return? exp)
        (ag-boolean? exp)
        (ed-set? exp)))

    (define (evaluate exp env)
      (cond ((self-evaluating? exp) exp)
        ((ag-stmt-assignment? exp) (eval-stmt-assignment exp env))
        ((ag-identifier? exp)
          (car (get-variable-value env global exp err-unknown)))
        ((ag-tupleslice? exp) (eval-simple-tuple-former exp env))
        ((ag-setiterator? exp) (eval-set-former exp env))
        ((ag-tupleiterator? exp) (eval-tuple-former exp env))
        ((ag-setslice? exp) (eval-simple-set-former exp env))
        ((ag-range? exp) (eval-range (ag-range-var exp)
                           (ag-range-begin exp)
                           (ag-range-end exp)
                           env))
        ((ag-stmt-call? exp) (eval-call exp env))
        ((ag-simple-call-or-selection? exp)
          (eval-simple-call-or-selection exp env))
        ((ag-application? exp) (eval-application exp env))
        ((ag-stmt-if? exp) (eval-if exp env))
        ((ag-exists? exp) (eval-exists exp env))
        ((ag-notexists? exp) (eval-notexists exp env))
        ((ag-forall? exp) (eval-forall exp env))
        ((ag-tuple? exp)
          (ag-tuple-map (lambda (exp) (evaluate exp env)) exp))
        ((ag-set? exp)
          (make-ed-set (map (lambda (exp)
                              (evaluate exp env))
                         (ag-set-contents exp))))
        ((ag-fromb? exp) (eval-ag-fromb exp env))
        ((ag-frome? exp) (eval-ag-frome exp env))
        ((ag-from? exp) (eval-ag-from exp env))
        ((ag-stmt-loop? exp) (eval-loop exp env))
        (else   (eval-error
                  (format "unimplemented statement or expression: ~a" exp)))))

    (define (true? exp)
      (equal? (ag-boolean-value exp) #t))

    ;; Evalueert een sequence
    ;; Wanneer deze leeg is, wordt er omega teruggegeven.
    ;; Als eval-return #t is, wordt een return statement ge�valueerd (procs)
    (define (eval-sequence exp env eval-return)
      (if (null? exp)
        (make-ag-omega)
        (let ((rslt (evaluate (ag-statement-body (car exp)) env)))
          (cond ((and eval-return (ag-stmt-return? rslt))
                  (evaluate (ag-stmt-return-val rslt) env))
            ((ag-stmt-return? rslt) rslt)
            ((null? (cdr exp)) (make-ag-omega))
            (else (eval-sequence (cdr exp) env eval-return))))))

    (define (eval-if exp env)
      (if (true? (evaluate (ag-stmt-if-condition exp) env))
        (eval-sequence (ag-stmt-if-then exp) env #f)
        (eval-sequence  (ag-stmt-if-else exp) env #f)))

    ;; Operators
    (define (eval-application exp env)
      (let* ((op (ag-application-operator exp))
              (operands (ag-application-operands exp))
              (args (map (lambda (operand)
                           (evaluate operand env))
                      operands)))
        (send-message natives
            'apply-operator
          op
          args
          env)))
    ;; Applications (user-defined or SETL-supplied like print)
    (define (eval-call exp env)
      (let ((name (ag-stmt-call-var exp)))
        (if (not (send-message natives 'scheme-proc? name))
          (apply-proc exp env)
          (send-message natives
              'apply-scheme-proc
            name
            (map (lambda (arg)
                   (evaluate arg env))
              (ag-stmt-call-args exp))))))

    ;; User-defined procedure
    (define (apply-proc exp env)
      (let* ((proc (evaluate (ag-stmt-call-var exp) env))
              (formal (ag-proc-args proc))
              (body (ag-proc-body proc))
              (declarations (ag-body-declarations body))
              (vardec (ag-declarations-var declarations))
              (constdec (ag-declarations-const declarations))
              (initdec (ag-declarations-init declarations))
              (stmts (ag-body-statements body))
              (refines (ag-body-refines body))
              (actual (ag-stmt-call-args exp))
              (procenv (make-environment)))
        (if (not (= (length formal) (length actual)))
          (eval-error
            (format "non-matching argument list: ~a ~a" formal actual))
          (for-each (lambda (f a)
                      (type ag-formal? f)
                      (set-variable-value! procenv
                        global
                        (ag-formal-var f)
                        (evaluate a env)
                        err-constant))
            formal
            actual))
        (process-declarations vardec constdec initdec procenv)
        (eval-sequence stmts procenv #t)))


    ;; The reader is unable to decide about the nature of a(2)
    (define (eval-simple-call-or-selection exp env)
      (let ((val (evaluate (ag-simple-call-or-selection-var exp) env)))
        (if (not (ag-proc? val))
          (evaluate
            (make-ag-range (ag-simple-call-or-selection-var exp)
              (car (ag-simple-call-or-selection-el exp))
              (car (ag-simple-call-or-selection-el exp)))
            env)
          (eval-call
            (make-ag-stmt-call (ag-simple-call-or-selection-var exp)
              (ag-simple-call-or-selection-el exp))
            env))))

    ;; a(2..3)
    (define (eval-range lhs begn end env)
      (let ((var (evaluate lhs env))
             (bgn (evaluate begn env))
             (end (evaluate end env)))
        (type ag-integer? bgn)
        (type ag-integer? end)
        (cond ((ag-tuple? var)
                (if (= (ag-integer-value bgn) (ag-integer-value end))
                  (ag-tuple-val var (ag-integer-value bgn))
                  (tuple-range var
                    (ag-integer-value bgn)
                    (ag-integer-value end))))
          ((ag-string? var)
            (make-ag-string
              (list->string (list-range (string->list (ag-string-value var))
                              (ag-integer-value bgn)
                              (ag-integer-value end)))))
          (else (eval-error
                  (format "invalid lhs type for range: ~a" var))))))
    ;; [2, 4 ..8]
    (define (eval-simple-tuple-former exp env)
      (let* ((exp1 (ag-tupleslice-exp1 exp))
              (exp2 (ag-tupleslice-exp2 exp))
              (exp3 (ag-tupleslice-exp3 exp))
              (first (if exp1
                       (evaluate exp1 env)
                       (evaluate exp2 env)))
              (exp2eval (evaluate exp2 env))
              (end (evaluate exp3 env))
              (intermediate-check (begin (type ag-integer? first)
                                    (type ag-integer? end)))
              (step (if exp1
                      (begin
                        (type ag-integer? first)
                        (type ag-integer? exp2eval)
                        (make-ag-integer (- (ag-integer-value exp2eval)
                                           (ag-integer-value first))))
                      (make-ag-integer (if (> (ag-integer-value first)
                                             (ag-integer-value end))
                                         -1 1)))))

        (type ag-integer? step)
        (make-ag-tuple (form-list (ag-integer-value first)
                         (ag-integer-value step)
                         (ag-integer-value end)
                         make-ag-integer))))
    ;; {2, 4 .. 8}
    (define (eval-simple-set-former exp env)
      (let* ((exp1 (ag-setslice-exp1 exp))
              (exp2 (ag-setslice-exp2 exp))
              (exp3 (ag-setslice-exp3 exp))
              (first (if exp1 (evaluate exp1 env) (evaluate exp2 env)))
              (exp2eval (evaluate exp2 env))
              (end (evaluate exp3 env))
              (intermediate-check (begin (type ag-integer? first)
                                    (type ag-integer? end)))
              (step (if exp1
                      (begin
                        (type ag-integer? first)
                        (type ag-integer? exp2eval)
                        (make-ag-integer (- (ag-integer-value exp2eval)
                                           (ag-integer-value first))))
                      (make-ag-integer (if (> (ag-integer-value first)
                                             (ag-integer-value end))
                                         -1 1)))))

        (type ag-integer? step)
        (make-ed-set (form-list (ag-integer-value first)
                       (ag-integer-value step)
                       (ag-integer-value end)
                       make-ag-integer))))



    (define (eval-stmt-assignment stmt env)
      (let ((lhs (ag-stmt-assignment-lhs stmt))
             (val (evaluate (ag-stmt-assignment-value stmt) env)))
        (cond ((ag-identifier? lhs)
                (set-variable-value! env global lhs val err-constant))
          ((ag-range? lhs)
            (eval-assign-range (ag-range-var lhs)
              (ag-range-begin lhs)
              (ag-range-end lhs)
              val env))
          ((ag-selection? lhs)
            (eval-assign-selection (ag-selection-lhs lhs)
              (ag-selection-selection lhs)
              val env))
          ((ag-tuple? lhs)
            (eval-assign-tuple lhs val env))
          (else (eval-error
                  (format "unimplemented assignment: ~a" lhs))))))

    ;; [a,b,c] := [1,2,3]
    (define (eval-assign-tuple lhs val env)
      (define (sequential-assign t1 t2)
        (let ((t1end (+ 1 (ag-tuple-highest t1)))
               (t2end (+ 1 (ag-tuple-highest t2))))
          (define (loop pos1 pos2)
            (cond ((and (= pos2 t2end) (= pos1 t1end)) val)
              ((= pos1 t1end) val)
              ((= pos2 t2end)
                (set-variable-value!
                  env
                  global
                  (ag-tuple-val t1 pos1)
                  (make-ag-omega) err-constant)
                (loop (+ pos1 1) pos2))
              ((ag-skip? (ag-tuple-val t1 pos1))
                (loop (+ pos1 1) (+ pos2 1)))
              (else (set-variable-value! env
                      global
                      (ag-tuple-val t1 pos1)
                      (ag-tuple-val t2 pos2)
                      err-constant)
                (loop (+ pos1 1) (+ pos2 1)))))
          (loop 1 1)))
      (if (not (ag-tuple? val))
        (eval-error (format "rhs must be a tuple: ~a" val))
        (begin
          (ag-tuple-for-each (lambda (x) (type lhst? x)) lhs)
          (sequential-assign lhs val))))


    ;; a(2) := 4
    (define (eval-assign-selection lhs selection val env)
      (let ((var (evaluate lhs env))
             (val (evaluate val env))
             (pos (if (= 1 (length selection))
                    (evaluate (car selection) env)
                    (eval-error
                      (format "multiple element selections aren't implemented: ~a"
                        selection)))))
        (cond ((ag-string? var) (assign-range lhs var pos pos val env))
          ((ag-tuple? var) (assign-selection lhs var pos val env))
          ((single-valued-map-reference? var)
            (assign-single-valued-map lhs var pos val env))
          (else (format "invalid lhs type for selection: ~a" lhs)))))

    (define (assign-single-valued-map lhs var dom val env)
      (type ag-integer? dom)
      (set-variable-value! env
        global
        lhs
        (make-ag-set
          (alter-svmap (ag-set-contents var) dom val))
        err-constant))


    (define (assign-selection lhs var pos val env)
      (type ag-integer? pos)
      (ag-tuple-insert var (ag-integer-value pos) val)
      (set-variable-value! env
        global
        lhs
        var
        err-constant))

    ;; a(2..4) := [2,3];
    (define (assign-range lhs var bgn end val env)
      (define (sassign lst vallst)
        (cond ((and (ag-integer? bgn) (ag-integer? end))
                (insert-in-list lst (ag-integer-value bgn)
                  (ag-integer-value end) vallst))
          ((ag-integer? bgn)
            (insert-in-list lst
              (ag-integer-value bgn)
              (length lst) vallst))
          (else
            (eval-error
              (format "range arguments must be integers: ~a, ~a" bgn end)))))
      (define (tassign t1 t2)
        (cond ((and (ag-integer? bgn) (ag-integer? end))
                (ag-tuple-insert-in t1
                  (ag-integer-value bgn)
                  (ag-integer-value end) t2))
          ((ag-integer? bgn)
            (ag-tuple-insert-in
              t1
              (ag-integer-value bgn)
              (ag-tuple-highest t1) t2))
          (else
            (eval-error
              (format "range arguments must be integers: ~a, ~a" bgn end)))))
      (cond ((and (ag-string? var) (ag-string? val))
              (set-variable-value! env
                global
                lhs
                (make-ag-string
                  (list->string
                    (sassign (string->list (ag-string-value var))
                      (string->list (ag-string-value val)))))
                err-constant))
        ((and (ag-tuple? var) (ag-tuple? val))
          (set-variable-value! env global lhs
            (tassign var val)
            err-constant))
        (else
          (eval-error
            (format "invalid variable or value type for range assignment: ~a - ~a"
              var val)))))

    (define (eval-assign-range lhs start finish val env)
      (let ((var (evaluate lhs env))
             (bgn (evaluate start env))
             (end (if finish (evaluate finish env) #f)))
        (assign-range lhs var bgn end val env)))


    (define (eval-loop exp env)
      (let ((loopiter (ag-stmt-loop-loopiter exp))
             (stmts (ag-stmt-loop-stmts exp)))
        (cond ((ag-loopiter-for? loopiter)
                (eval-loop-for loopiter stmts env))
          ((ag-loopiter? loopiter)
            (if (not (ag-loopiter-while loopiter))
              (eval-error
                (format "only while and for loops are implemented: ~" loopiter))
              (eval-loop-while loopiter stmts env))))))

    (define (eval-loop-while loopiter stmts env)
      (let* ((whi (ag-loopiter-while loopiter))
              (cnd (ag-while-condition whi)))
        (define (loop)
          (define pred (evaluate cnd env))
          (type ag-boolean? pred)
          (if (ag-boolean-value pred)
            (begin
              (for-each (lambda (stmt)
                          (evaluate (ag-statement-body stmt) env))
                stmts)
              (loop))
            (make-ag-omega)))
        (loop)))

    (define (eval-loop-for loopiter stmts env)
      (eval-quantified ag-iterator-itlist
        ag-iterator-suchthat
        (ag-loopiter-for-iterator loopiter)
        env
        (lambda (pred return lenv)
          (if (not (ag-boolean-value pred))
            (return (make-ag-omega))
            (for-each (lambda (stmt)
                        (evaluate (ag-statement-body stmt) lenv)) stmts)))))

    (define (eval-exists exp env)
      (eval-quantified ag-exists-iterators
        ag-exists-exp
        exp
        env
        (lambda (pred return lenv)
          (if (ag-boolean-value pred)
            (return pred)
            (make-ag-boolean #f)))))

    (define (eval-notexists exp env)
      (eval-quantified ag-notexists-iterators
        ag-notexists-exp
        exp
        env
        (lambda (pred return lenv)
          (if (ag-boolean-value pred)
            (make-ag-boolean #f)
            (return pred)))))

    (define (eval-forall exp env)
      (eval-quantified ag-forall-iterators
        ag-forall-exp
        exp
        env
        (lambda (pred return lenv)
          (if (not (ag-boolean-value pred))
            (return (make-ag-boolean #f))
            (make-ag-boolean #t)))))

    (define (eval-quantified accessor-iterators
              accessor-such
              exp
              env
              test)
      (let* ((iter-in-els (accessor-iterators exp))
              (first (car iter-in-els))
              (sch (accessor-such exp)))
        (if (not (ag-iterelement-in? first))
          (eval-error (format "unimplemented iterator type: ~a" first))
          (let* ((idf (ag-iterelement-in-lhs first))
                  (e (evaluate (ag-iterelement-in-exp first) env))
                  (fe (cond ((ag-tuple? e) ag-tuple-for-each)
                        ((ed-set? e) ed-set-for-each)
                        (else (eval-error
                                (format "unimplemented iter-element-in type: ~a" e))))))
            (call-with-current-continuation
              (lambda (return)
                (fe (lambda (x)
                      (set-variable-value! env global idf x err-constant)
                      (let ((pred (evaluate sch env)))
                        (type ag-boolean? pred)
                        (test pred return env)
                        (set-variable-value! env
                          global
                          idf
                          (make-ag-omega) err-constant)))
                  e)))))))





    (define (eval-former iterator-expr-accessor
              iterator-iterator-accessor
              empty-constructor
              update
              processing
              exp
              env)
      (let* ((expr (iterator-expr-accessor exp))
              (it (iterator-iterator-accessor exp))
              (itlist (ag-iterator-itlist it))
              (first (car itlist))
              (sch (ag-iterator-suchthat it)))
        (if (not (ag-iterelement-in? first))
          (eval-error (format "unimplemented iterator type: ~a" first))
          (let* ((idf (ag-iterelement-in-lhs first))
                  (e (evaluate (ag-iterelement-in-exp first) env))
                  (fe (cond ((ag-tuple? e) ag-tuple-for-each)
                        ((ed-set? e) ed-set-for-each)
                        (else (eval-error
                                (format "unimplemented iter-element-in type: ~a" e)))))
                  (result (empty-constructor)))
            (type ag-identifier? idf)
            (call-with-current-continuation
              (lambda (return)
                (fe (lambda (x)
                      (set-variable-value! env global idf x err-constant)
                      (let ((pred (evaluate sch env)))
                        (type ag-boolean? pred)
                        (if (ag-boolean-value pred)
                          (set! result
                            (update (evaluate expr env) result)))))
                  e)))
            (processing result)))))

    (define (eval-set-former exp env)
      (eval-former ag-setiterator-expr
        ag-setiterator-iterator
        ed-set-empty
        (lambda (val sofarval)
          (ed-set-insert sofarval val))
        identity
        exp
        env))

    (define (eval-tuple-former exp env)
      (eval-former ag-tupleiterator-expr
        ag-tupleiterator-iterator
        ag-tuple-empty
        (lambda (val sofarval)
          (ag-tuple-insert sofarval
            (+ 1 (ag-tuple-highest sofarval)) val)
          sofarval)
        identity
        exp
        env))


    (define (eval-from dest-accessor
              source-accessor
              position-determinant
              exp
              env)
      (let* ((dest (dest-accessor exp))
              (source (source-accessor exp))
              (sourceval (evaluate source env))
              (pos (position-determinant sourceval))
              (val (ag-tuple-val sourceval pos)))
        (type ag-tuple? sourceval)
        (eval-stmt-assignment (make-ag-stmt-assignment dest val) env)
        (eval-assign-range source
          (make-ag-integer pos)
          (make-ag-integer pos)
          (make-ag-tuple '())
          env)
        val))

    (define (eval-ag-from exp env)
      (eval-ag-fromb exp env))

    (define (eval-ag-fromb exp env)
      (eval-from ag-fromb-dest ag-fromb-source (lambda (x) 1) exp env))

    (define (eval-ag-frome exp env)
      (eval-from ag-frome-dest
        ag-frome-source
        (lambda (t)
          (ag-tuple-highest t)) exp env))

    (define (dispatch message)
      (case message
        ((eval) eval-program)
        ((eval-exp) evaluate)
        (else (error 'parser "unknown message"))))
    dispatch))

(define (make-setl p re ee)
  (let* ((scanner (make-scanner))
          (reader (make-reader scanner))
          (evaluator (make-evaluator))
          (parse-tree #f))

    (define (read port)
      (send-message scanner 'init-scan port)
      (set! parse-tree (send-message reader 'read dispatch))
      parse-tree)

    (define (eval)
      (send-message evaluator 'eval parse-tree dispatch))

    (define (dispatch message)
      (case message
        ((read) read)
        ((eval) eval)
        ((print) p)
        ((read-error) re)
        ((eval-error) ee)
        (else (error 'setl "unknown message ~a" message))))


    dispatch))

(define setl (make-setl display
               (lambda (message token)
                 (display "Read error: ") (display message)
                 (newline)(display "> ")(display token)(display " <")
                 (error "Parser error."))
               (lambda (message)
                 (display "Eval error: ")(display message)
                 (error "Evaluation error."))))

((setl 'read)
  "program assign;

    a := 3;
    a +:= 4;
    b:= a +:= 1;

    print(b);
    print(a);

    a := 3 + b := 5 + 2;

    print(b);
    print(a);

    abc := 'the quick brown fox';
      cde := abc(5 .. 8);
    cde := abc(5);
    edf := abc(5 ..);

    print(cde);
    print(edf);

  end program assign;")
((setl 'eval))
((setl 'print))
((setl 'read)
"program assign;

	a := 3;
	a +:= 4;
	b:= a +:= 1;

	print(b);
	print(a);

	a := 3 + b := 5 + 2;

	print(b);
	print(a);

	abc := 'the quick brown fox';
    cde := abc(5 .. 8);
	cde := abc(5);
	edf := abc(5 ..);

	print(cde);
	print(edf);

end program assign;")
((setl 'eval))
((setl 'print))