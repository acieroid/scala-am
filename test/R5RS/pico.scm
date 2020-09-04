;-----------------------------------;
;           >>>Pico-ag<<<           ;
;            Theo D'Hondt           ;
;   Lab voor Programmeerkunde VUB   ;
;               �1995               ;
;-----------------------------------;

;----------------------------------------------------------------------------;
; <void>                -> [_VOI_TAG_]                                       ;
; <number>              -> [_NBR_TAG_,NbrSet]                                ;
; <symbol>              -> [_SYM_TAG_,SymSet]                                ;
; <function>            -> [_FUN_TAG_,ArgSet,ExpSet,EnvSet]                  ;
; <table>               -> [_TAB_TAG_,TabSet]                                ;
; <native function>     -> [_NAT_TAG_,NatSet]                                ;
; <variable definition> -> [_DFN_TAG_,IdfSet,ExpSet]                         ;
; <variable reference>  -> [_RFN_TAG_,IdfSet]                                ;
; <variable assignment> -> [_STN_TAG_,IdfSet,ExpSet]                         ;
; <function definition> -> [_DFF_TAG_,IdfSet,ArgSet,ExpSet]                  ;
; <function reference>  -> [_RFF_TAG_,IdfSet,ArgSet]                         ;
; <function assignment> -> [_STF_TAG_,IdfSet,ArgSet,ExpSet]                  ;
; <table definition>    -> [_DFT_TAG_,IdfSet,NbrSet,ExpSet]                  ;
; <table reference>     -> [_RFT_TAG_,IdfSet,NbrSet]                         ;
; <table assignment>    -> [_STT_TAG_,IdfSet,NbrSet,ExpSet]                  ;
;                                                                            ;
;  NbrSet       = {#nbr#}                                                    ;
;  SymSet       = {#sym#}                                                    ;
;  NatSet       = {#nat#}                                                    ;
;  TabSet       = {#tab#}                                                    ;
;  EnvSet       = {#env#}                                                    ;
;  IdfSet       = {#idf#}                                                    ;
;  ArgSet       = IdfSet + {<table>}                                         ;
;  ExpSet       = NbrSet + SymSet + {<variable definition>,                  ;
;                                    <variable reference>,                   ;
;                                    <variable assignment>,                  ;
;                                    <function definition>,                  ;
;                                    <function reference>,                   ;
;                                    <function assignment>,                  ;
;                                    <table definition>,                     ;
;                                    <table reference>,                      ;
;                                    <table assignment>}                     ;
;----------------------------------------------------------------------------;

(define _VOI_TAG_  0)
(define _NBR_TAG_  1)
(define _SYM_TAG_  2)
(define _FUN_TAG_  3)
(define _TAB_TAG_  4)
(define _NAT_TAG_  5)
(define _DFN_TAG_  6)
(define _RFN_TAG_  7)
(define _STN_TAG_  8)
(define _DFF_TAG_  9)
(define _RFF_TAG_ 10)
(define _STF_TAG_ 11)
(define _DFT_TAG_ 12)
(define _RFT_TAG_ 13)
(define _STT_TAG_ 14)

(define-macro _AG_ASSERT_
  (lambda (ag . tags)
      `(if (member (car ,ag) ',tags)
         ,ag
         (_error_ "abstract grammar violation"))))

(define-macro _AG_MAKE_NBR_
  (lambda (nbr)
      `(list ,_NBR_TAG_ ,nbr)))

(define-macro _AG_MAKE_VOI_
  (lambda ()
      `(list ,_VOI_TAG_)))

(define-macro _AG_MAKE_SYM_
  (lambda (sym)
      `(list ,_SYM_TAG_ ,sym)))

(define-macro _AG_MAKE_FUN_
  (lambda (par bod env)
      `(list ,_FUN_TAG_ ,par ,bod ,env)))

(define-macro _AG_MAKE_TAB_
  (lambda (siz ini)
      `(list ,_TAB_TAG_ (make-vector ,siz ,ini))))

(define-macro _AG_MAKE_TAB_aux_
  (lambda (lst)
      `(list ,_TAB_TAG_ (list->vector ,lst))))

(define-macro _AG_MAKE_NAT_
  (lambda (nat)
      `(list ,_NAT_TAG_ ,nat)))

(define-macro _AG_MAKE_DFN_
  (lambda (nam val)
      `(list ,_DFN_TAG_ ,nam ,val)))

(define-macro _AG_MAKE_RFN_
  (lambda (nam)
      `(list ,_RFN_TAG_ ,nam)))

(define-macro _AG_MAKE_STN_
  (lambda (nam val)
      `(list ,_STN_TAG_ ,nam ,val)))

(define-macro _AG_MAKE_DFF_
  (lambda (nam arg exp)
      `(list ,_DFF_TAG_ ,nam ,arg ,exp)))

(define-macro _AG_MAKE_RFF_
  (lambda (nam arg)
      `(list ,_RFF_TAG_ ,nam ,arg)))

(define-macro _AG_MAKE_STF_
  (lambda (nam arg exp)
      `(list ,_STF_TAG_ ,nam ,arg ,exp)))

(define-macro _AG_MAKE_DFT_
  (lambda (nam idx cmp)
      `(list ,_DFT_TAG_ ,nam ,idx ,cmp)))

(define-macro _AG_MAKE_RFT_
  (lambda (nam idx)
      `(list ,_RFT_TAG_ ,nam ,idx)))

(define-macro _AG_MAKE_STT_
  (lambda (nam idx cmp)
      `(list ,_STT_TAG_ ,nam ,idx ,cmp)))

(define-macro _AG_TAG_
  (lambda (ag)
      `(car ,ag)))

(define-macro _AG_GET_nbr_
  (lambda (ag)
      `(cadr (_AG_ASSERT_ ,ag ,_NBR_TAG_))))

(define-macro _AG_GET_sym_
  (lambda (ag)
      `(cadr (_AG_ASSERT_ ,ag ,_SYM_TAG_))))

(define-macro _AG_GET_fun_
  (lambda (ag)
      `(cdr (_AG_ASSERT_ ,ag ,_FUN_TAG_))))

(define-macro _AG_GET_par_
  (lambda (ag)
      `(cadr (_AG_ASSERT_ ,ag ,_FUN_TAG_))))

(define-macro _AG_GET_bod_
  (lambda (ag)
      `(caddr (_AG_ASSERT_ ,ag ,_FUN_TAG_))))

(define-macro _AG_GET_env_
  (lambda (ag)
      `(cadddr (_AG_ASSERT_ ,ag ,_FUN_TAG_))))

(define-macro _AG_GET_siz_
  (lambda (ag)
      `(vector-length (cadr (_AG_ASSERT_ ,ag ,_TAB_TAG_)))))

(define-macro _AG_GET_tab_
  (lambda (ag)
      `(cadr (_AG_ASSERT_ ,ag ,_TAB_TAG_))))

(define-macro _AG_GET_elt_
  (lambda (ag idx)
      `(let* ((vec (cadr (_AG_ASSERT_ ,ag ,_TAB_TAG_)))
               (siz (vector-length vec)))
         (if (and (positive? ,idx) (<= ,idx siz) (integer? ,idx))
           (vector-ref vec (- ,idx 1))
           (_error_ "index out of range")))))

(define-macro _AG_SET_elt_
  (lambda (ag idx val)
      `(let* ((vec (cadr (_AG_ASSERT_ ,ag ,_TAB_TAG_)))
               (siz (vector-length vec)))
         (if (and (positive? ,idx) (<= ,idx siz) (integer? ,idx))
           (vector-set! vec (- ,idx 1) ,val)
           (_error_ "index out of range")))))

(define-macro _AG_MAP_TAB_
  (lambda (ag proc)
      `(let* ((vec (cadr (_AG_ASSERT_ ,ag ,_TAB_TAG_)))
               (siz (vector-length vec))
               (new-vec (make-vector siz)))
         (do ((idx 0 (+ idx 1)))
           ((= idx siz))
           (vector-set! new-vec idx (,proc (vector-ref vec idx))))
         (list ,_TAB_TAG_ new-vec))))

(define-macro _AG_FOR_EACH_TAB_
  (lambda (ag proc default)
      `(let* ((vec (cadr (_AG_ASSERT_ ,ag ,_TAB_TAG_)))
               (siz (vector-length vec)))
         (do ((result ,default)
               (idx 0 (+ idx 1)))
           ((= idx siz) result)
           (set! result (,proc (vector-ref vec idx)))))))

(define-macro _AG_GET_nat_
  (lambda (ag)
      `(cadr (_AG_ASSERT_ ,ag ,_NAT_TAG_))))

(define-macro _AG_GET_nam_
  (lambda (ag)
      `(cadr (_AG_ASSERT_ ,ag ,_DFN_TAG_ ,_RFN_TAG_ ,_STN_TAG_
               ,_DFF_TAG_ ,_RFF_TAG_ ,_STF_TAG_
               ,_DFT_TAG_ ,_RFT_TAG_ ,_STT_TAG_))))

(define-macro _AG_GET_val_
  (lambda (ag)
      `(caddr (_AG_ASSERT_ ,ag ,_DFN_TAG_ ,_STN_TAG_))))

(define-macro _AG_GET_arg_
  (lambda (ag)
      `(caddr (_AG_ASSERT_ ,ag ,_DFF_TAG_ ,_RFF_TAG_ ,_STF_TAG_))))

(define-macro _AG_GET_exp_
  (lambda (ag)
      `(cadddr (_AG_ASSERT_ ,ag ,_DFF_TAG_ ,_STF_TAG_))))

(define-macro _AG_GET_idx_
  (lambda (ag)
      `(caddr (_AG_ASSERT_ ,ag ,_DFT_TAG_ ,_RFT_TAG_ ,_STT_TAG_))))

(define-macro _AG_GET_cmp_
  (lambda (ag)
      `(cadddr (_AG_ASSERT_ ,ag ,_DFT_TAG_ ,_STT_TAG_))))

;-----------------------------------;
;          >>>Pico-scan<<<          ;
;            Theo D'Hondt           ;
;   Lab voor Programmeerkunde VUB   ;
;               �1995               ;
;-----------------------------------;

;-----------------------------------;
; ( 'number            . <number>   )
; ( 'symbol            . <string>   )
; ( 'name              . <string>   )
; ( 'relop             . <string>   )
; ( 'mulop             . <string>   )
; ( 'addop             . <string>   )
; ( 'left-parenthesis  . <null>     )
; ( 'right-parenthesis . <null>     )
; ( 'left-bracket      . <null>     )
; ( 'right-bracket     . <null>     )
; ( 'colon             . <null>     )
; ( 'assign            . <null>     )
; ( 'comma             . <null>     )
; ( 'commercial-at     . <null>     )
; ( 'end-of-entry      . <null>     )
; ( 'error             . <string>   )
;-----------------------------------;

(define _NBR_TOKEN_  0)
(define _SYM_TOKEN_  1)
(define _NAM_TOKEN_  2)
(define _ROP_TOKEN_  3)
(define _MOP_TOKEN_  4)
(define _AOP_TOKEN_  5)
(define _LPR_TOKEN_  6)
(define _RPR_TOKEN_  7)
(define _LBR_TOKEN_  8)
(define _RBR_TOKEN_  9)
(define _COL_TOKEN_ 10)
(define _ASS_TOKEN_ 11)
(define _COM_TOKEN_ 12)
(define _CAT_TOKEN_ 13)
(define _END_TOKEN_ 14)
(define _ERR_TOKEN_ 15)

(define _ch_ '())

(define (_flush_)
  (cond
    ((char-ready? _in_)
      (read-char _in_)
      (_flush_))))

(define (_init_)
  (set! _ch_ (read-char _in_))
  (_scan_))

(define (_scan_)

  (define (advance)
    (define this-ch _ch_)
    (set! _ch_ (read-char _in_))
    this-ch)

  (define (check . allowed-list)
    (member (handler) allowed-list))

  (define (operator)
    (define (scan-operator position)
      (define ch (advance))
      (define str ((if (check Rop Eql Mop Aop Pls Mns)
                     scan-operator
                     make-string) (+ 1 position)))
      (string-set! str position ch)
      str)
    (scan-operator 0))

  (define (name)
    (define (scan-name position)
      (define ch (advance))
      (define str ((if (check Ltr Exp Dgt)
                     scan-name
                     make-string) (+ 1 position)))
      (string-set! str position ch)
      str)
    (scan-name 0))

  (define (symbol)
    (define (scan-symbol position)
      (define ch (advance))
      (define str ((if (check Apo Eol)
                     make-string
                     scan-symbol) (+ 1 position)))
      (string-set! str position ch)
      str)
    (scan-symbol 0))

  (define (number)
    (define fraction-allowed (not (check Per)))
    (define (scan-exponent-1 position)
      (define ch (advance))
      (define str ((if (check Dgt)
                     scan-exponent-1
                     make-string)
                    (+ 1 position)))
      (string-set! str position ch)
      str)
    (define (scan-exponent position)
      (define ch (advance))
      (define str ((cond
                     ((check Dgt Pls Mns) scan-exponent-1)
                     (else make-string))
                    (+ 1 position)))
      (string-set!
        str
        position
        ch))
    (define (scan-fraction position)
      (define ch (advance))
      (define str ((cond
                     ((check Dgt) scan-fraction)
                     ((check Exp) scan-exponent)
                     (else make-string))
                    (+ 1 position)))
      (string-set! str position ch)
      str)
    (define (scan-number position)
      (define ch (advance))
      (define str ((cond
                     ((check Dgt) scan-number)
                     ((and fraction-allowed (check Per)) scan-fraction)
                     ((check Exp) scan-exponent)
                     (else make-string))
                    (+ 1 position)))
      (string-set! str position ch)
      str)
    (string->number (scan-number 0)))

  (define (Ill)
    (cons _ERR_TOKEN_ "invalid character"))

  (define (Eol)
    (cond
      ((char-ready? _in_)
        (advance)
        (handle))
      (else
        (list _END_TOKEN_))))

  (define (Wsp)
    (advance)
    (handle))

  (define (Eof)
    (list _END_TOKEN_))

  (define (Rop)
    (cons _ROP_TOKEN_ (operator)))

  (define (Mop)
    (cons _MOP_TOKEN_ (operator)))

  (define (Aop)
    (cons _AOP_TOKEN_ (operator)))

  (define (Pls)
    (cons _AOP_TOKEN_ (operator)))

  (define (Mns)
    (cons _AOP_TOKEN_ (operator)))

  (define (Apo)
    (advance)
    (let ((hold (symbol)))
      (advance)
      (cons _SYM_TOKEN_ hold)))

  (define (Lpr)
    (advance)
    (list _LPR_TOKEN_))

  (define (Rpr)
    (advance)
    (list _RPR_TOKEN_))

  (define (Lbr)
    (advance)
    (list _LBR_TOKEN_))

  (define (Rbr)
    (advance)
    (list _RBR_TOKEN_))

  (define (Dgt)
    (cons _NBR_TOKEN_ (number)))

  (define (Per)
    (cons _NBR_TOKEN_ (number)))

  (define (Col)
    (advance)
    (cond
      ((check Eql)
        (advance)
        (list _ASS_TOKEN_))
      (else
        (list _COL_TOKEN_))))

  (define (Com)
    (advance)
    (list _COM_TOKEN_))

  (define (Cat)
    (advance)
    (list _CAT_TOKEN_))

  (define (Ltr)
    (cons _NAM_TOKEN_ (name)))

  (define (Exp)
    (cons _NAM_TOKEN_ (name)))

  (define (Eql)
    (cons _ROP_TOKEN_ (operator)))

  (define (handler)
    (if (eof-object? _ch_)
      Eof
      (vector-ref
        (vector
          Wsp Wsp Wsp Wsp Wsp Wsp Wsp Wsp Wsp Wsp Eol Wsp Wsp Wsp Wsp Wsp ;000
          Wsp Wsp Wsp Wsp Wsp Wsp Wsp Wsp Wsp Wsp Wsp Wsp Wsp Wsp Wsp Wsp ;016
          Wsp Aop Ill Rop Aop Aop Mop Apo Lpr Rpr Mop Pls Com Mns Per Mop ;032
          Dgt Dgt Dgt Dgt Dgt Dgt Dgt Dgt Dgt Dgt Col Rop Rop Eql Rop Aop ;048
          Cat Ltr Ltr Ltr Ltr Exp Ltr Ltr Ltr Ltr Ltr Ltr Ltr Ltr Ltr Ltr ;064
          Ltr Ltr Ltr Ltr Ltr Ltr Ltr Ltr Ltr Ltr Ltr Lbr Mop Rbr Mop Ltr ;080
          Ill Ltr Ltr Ltr Ltr Exp Ltr Ltr Ltr Ltr Ltr Ltr Ltr Ltr Ltr Ltr ;096
          Ltr Ltr Ltr Ltr Ltr Ltr Ltr Ltr Ltr Ltr Ltr Ill Aop Ill Aop Ill ;112
          Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill ;128
          Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill ;144
          Ill Ill Ill Ill Ill Wsp Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill ;160
          Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill ;176
          Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill ;192
          Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill ;208
          Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill ;224
          Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill Ill);240
        (char->integer _ch_))))

  (define (handle)
    ((handler)))

  (handle))

;-----------------------------------;
;          >>>Pico-read<<<          ;
;            Theo D'Hondt           ;
;   Lab voor Programmeerkunde VUB   ;
;               �1995               ;
;-----------------------------------;

;----------------------------------------------------------------------------;
;SPECIFICATION                                                               ;
;   <spc> ::= <exp>                                                          ;
;   <spc> ::= <idf> ':' <spc>                           => (DFN #nam# <spc>) ;
;   <spc> ::= <tab> ':' <spc>                     => (DFT #nam# <idx> <spc>) ;
;   <spc> ::= <apl> ':' <spc>                     => (DFF #ref# <arg> <spc>) ;
;   <spc> ::= <idf> ':=' <spc>                          => (STN #nam# <spc>) ;
;   <spc> ::= <tab> ':=' <spc>                    => (STT #nam# <idx> <spc>) ;
;   <spc> ::= <apl> ':=' <spc>                    => (STF #ref# <arg> <spc>) ;
;EXPRESSION                                                                  ;
;   <exp> ::= <cmp>                                                          ;
;   <exp> ::= <cmp> #rop# <exp>             => (RFF #rop# (tab <cmp> <exp>)) ;
;COMPARAND                                                                   ;
;   <cmp> ::= <trm>                                                          ;
;   <cmp> ::= <trm> #aop# <cmp>             => (RFF #aop# (tab <trm> <cmp>)) ;
;TERM                                                                        ;
;   <trm> ::= <fct>                                                          ;
;   <trm> ::= <fct> #mop# <trm>             => (RFF #mop# (tab <fct> <trm>)) ;
;FACTOR                                                                      ;
;   <fct> ::= <ref>                                                          ;
;   <fct> ::= '(' <spc> ')'                                                  ;
;REFERENCE                                                                   ;
;   <ref> ::= <opr> <ref>                         => (RFF <opr> (tab <ref>)) ;
;   <ref> ::= <apl>                                                          ;
;   <ref> ::= <tab>                                                          ;
;   <ref> ::= <idf>                                                          ;
;   <ref> ::= <nbr>                                                          ;
;   <ref> ::= <sym>                                                          ;
;OPERATOR                                                                    ;
;   <opr> ::= #rop#                                                          ;
;   <opr> ::= #aop#                                                          ;
;   <opr> ::= #mop#                                                          ;
;IDENTIFIER                                                                  ;
;   <idf> ::= #nam#                                           => (RFN #nam#) ;
;   <idf> ::= #opr#                                           => (RFN #opr#) ;
;APPLICATION                                                                 ;
;   <apl> ::= <idf> '(' ')'                                   => (RFF #ref#) ;
;   <apl> ::= <idf> '(' <arg> ')'                       => (RFF #ref# <arg>) ;
;   <apl> ::= <idf> '@' <nam>                           => (RFF #ref# <nam>) ;
;TABULATION                                                                  ;
;   <tab> ::= <idf> '[' <idx> ']'                       => (RFT #ref# <idx>) ;
;ARGUMENTS                                                                   ;
;   <arg>  ::= <spc>                                                => <spc> ;
;   <arg>  ::= <spc> ',' <arg>                               => (tab <spc>*) ;
;INDEX                                                                       ;
;   <idx>  ::= <spc>                                                => <spc> ;
;                                                                            ;
;----------------------------------------------------------------------------;

;(load "ag.def")

(define (_read_)
  (define token (_init_))

  (define (get-token)
    (set! token (_scan_)))

  (define (value+get-token)
    (define hold (cdr token))
    (get-token)
    hold)

  (define (check+get-token sy)
    (cond
      ((= (car token) sy)
        (get-token)
        #t)
      (else
        #f)))

  (define (argument-table . args)
    (_AG_MAKE_TAB_aux_ args))

  (define (enumerate)
    (define <spc> (specification))
    (if (check+get-token _COM_TOKEN_)
      (cons <spc> (enumerate))
      (list <spc>)))

  (define (application ident)
    (define arguments
      (if (check+get-token _RPR_TOKEN_)
          '()
        (let ((arguments (enumerate)))
          (if (check+get-token _RPR_TOKEN_)
            arguments
            (_error_ "right parenthesis expected")))))
    (_AG_MAKE_RFF_ ident (apply argument-table arguments)))

  (define (application-at ident)
    (if (= (car token) _NAM_TOKEN_)
      (let ((<nam> (value+get-token)))
        (_AG_MAKE_RFF_ ident (_AG_MAKE_RFN_ <nam>)))
      (_error_ "name expected")))

  (define (tabulation name)
    (define <spc> (specification))
    (if (not (check+get-token _RBR_TOKEN_))
      (_error_ "right bracket expected"))
    (_AG_MAKE_RFT_ name <spc>))

  (define (operator)
    (define tk (car token))
    (cond
      ((or (= tk _ROP_TOKEN_)
         (= tk _AOP_TOKEN_)
         (= tk _MOP_TOKEN_))
        (value+get-token))
      (else
        #f)))

  (define (reference)
    (define tk (car token))
    (cond
      ((= tk _NBR_TOKEN_)
        (_AG_MAKE_NBR_ (value+get-token)))
      ((= tk _SYM_TOKEN_)
        (_AG_MAKE_SYM_ (value+get-token)))
      ((or (= tk _ROP_TOKEN_)
         (= tk _AOP_TOKEN_)
         (= tk _MOP_TOKEN_))
        (let ((<opr> (value+get-token)))
          (if (check+get-token _LPR_TOKEN_)
            (application <opr>)
            (if (check+get-token _CAT_TOKEN_)
              (application-at <opr>)
              (let ((<ref> (reference)))
                (if <ref>
                  (_AG_MAKE_RFF_ <opr> (argument-table <ref>))
                  (_AG_MAKE_RFN_ <opr>)))))))
      ((= tk _NAM_TOKEN_)
        (let ((<idf> (value+get-token)))
          (if (check+get-token _LPR_TOKEN_)
            (application <idf>)
            (if (check+get-token _CAT_TOKEN_)
              (application-at <idf>)
              (if (check+get-token _LBR_TOKEN_)
                (tabulation <idf>)
                (_AG_MAKE_RFN_ <idf>))))))
      ((= tk _ERR_TOKEN_)
        (_error_ (cdr token)))
      (else
        #f)))

  (define (factor)
    (cond
      ((check+get-token _LPR_TOKEN_)
        (let ((<spc> (specification)))
          (if (check+get-token _RPR_TOKEN_)
            <spc>
            (_error_ "right parenthesis expected"))))
      (else
        (let ((<ref> (reference)))
          (if <ref>
            <ref>
            (_error_ "factor expected"))))))

  (define (term)
    (let ((<fct> (factor)))
      (cond
        ((= (car token) _MOP_TOKEN_)
          (let ((hold (value+get-token)))
            (_AG_MAKE_RFF_ hold (argument-table <fct> (term)))))
        (else
          <fct>))))

  (define (comparand)
    (let ((<trm> (term)))
      (cond
        ((= (car token) _AOP_TOKEN_)
          (let ((hold (value+get-token)))
            (_AG_MAKE_RFF_ hold (argument-table <trm> (comparand)))))
        (else
          <trm>))))

  (define (expression)
    (define <cmp> (comparand))
    (cond
      ((= (car token) _ROP_TOKEN_)
        (let ((hold (value+get-token)))
          (_AG_MAKE_RFF_ hold (argument-table <cmp> (expression)))))
      (else
        <cmp>)))

  (define (specification)
    (define <exp> (expression))
    (if (check+get-token _COL_TOKEN_)
      (let ((tag (_AG_TAG_ <exp>))
             (val (specification)))
        (cond
          ((= tag _RFN_TAG_)
            (_AG_MAKE_DFN_ (_AG_GET_nam_ <exp>) val))
          ((= tag _RFF_TAG_)
            (_AG_MAKE_DFF_ (_AG_GET_nam_ <exp>) (_AG_GET_arg_ <exp>) val))
          ((= tag _RFT_TAG_)
            (_AG_MAKE_DFT_ (_AG_GET_nam_ <exp>) (_AG_GET_idx_ <exp>) val))
          (else
            (_error_ "illegal definition"))))
      (if (check+get-token _ASS_TOKEN_ )
        (let ((tag (_AG_TAG_ <exp>))
               (val (specification)))
          (cond
            ((= tag _RFN_TAG_)
              (_AG_MAKE_STN_ (_AG_GET_nam_ <exp>) val))
            ((= tag _RFF_TAG_)
              (_AG_MAKE_STF_ (_AG_GET_nam_ <exp>) (_AG_GET_arg_ <exp>) val))
            ((= tag _RFT_TAG_)
              (_AG_MAKE_STT_ (_AG_GET_nam_ <exp>) (_AG_GET_idx_ <exp>) val))
            (else
              (_error_ "illegal assignment"))))
        <exp>)))

  (define spc (specification))
  (if (= (car token) _END_TOKEN_)
    spc
    (_error_ "excess token(s)")))

;-----------------------------------;
;           >>>Pico-env<<<          ;
;            Theo D'Hondt           ;
;   Lab voor Programmeerkunde VUB   ;
;               �1995               ;
;-----------------------------------;

(define (_create_env_)
    '(()))

(define (_reference_env_ ref env)
  (if (null? env)
    (_error_ (string-append "unknown variable: " ref))
    (let ((entry (assoc ref (car env))))
      (if entry
        (cdr entry)
        (_reference_env_ ref (cdr env))))))

(define (_update_env_ ref val env)
  (if (null? env)
    (_error_ (string-append "unknown variable: " ref))
    (let ((entry (assoc ref (car env))))
      (cond
        (entry
          (set-cdr! entry val)
          val)
        (else
          (_update_env_ ref val (cdr env)))))))

(define (_define_env_ ref val env)
  (define entry (assoc ref (car env)))
  (if entry
    (set-cdr! entry val)
    (set-car! env (cons (cons ref val) (car env))))
  val)

(define (_extend_env_ env)
  (cons '() env))

;-----------------------------------;
;           >>>Pico-nat<<<          ;
;            Theo D'Hondt           ;
;   Lab voor Programmeerkunde VUB   ;
;               �1995               ;
;-----------------------------------;

;(load "ag.def")

(define-macro _ARITY=1_
  (lambda (argtab)
      `(if (not (= (_AG_GET_siz_ ,argtab) 1))
         (_error_ "exactly 1 argument required"))))

(define-macro _ARITY=2_
  (lambda (argtab)
      `(if (not (= (_AG_GET_siz_ ,argtab) 2))
         (_error_ "exactly 2 arguments required"))))

(define-macro _ARITY<=2_
  (lambda (argtab)
      `(if (not (member (_AG_GET_siz_ ,argtab) '(1 2)))
         (_error_ "1 or 2 arguments required"))))

(define-macro _ARG=ANY_
  (lambda (argtab env idx)
      `(_eval_ (_AG_GET_elt_ ,argtab ,idx) ,env)))

(define-macro _ARG=VAL_
  (lambda (argtab env idx)
      `(let ((arg (_ARG=ANY_ ,argtab ,env ,idx)))
         (if (= (_AG_TAG_ arg) _NBR_TAG_)
           (_AG_GET_nbr_ arg)
           (if (= (_AG_TAG_ arg) _SYM_TAG_)
             (_AG_GET_sym_ arg)
             (if (= (_AG_TAG_ arg) _FUN_TAG_)
               (_AG_GET_fun_ arg)
               (if (= (_AG_TAG_ arg) _TAB_TAG_)
                 (_AG_GET_tab_ arg)
                 (_error_ "value argument required"))))))))

(define-macro _ARG=TAB_
  (lambda (argtab env idx)
      `(let ((arg (_ARG=ANY_ ,argtab ,env ,idx)))
         (if (= (_AG_TAG_ arg) _TAB_TAG_)
           (_AG_GET_siz_ arg)
           (_error_ "table argument required")))))

(define-macro _ARG=NBR_
  (lambda (argtab env idx)
      `(let ((arg (_ARG=ANY_ ,argtab ,env ,idx)))
         (if (= (_AG_TAG_ arg) _NBR_TAG_)
           (_AG_GET_nbr_ arg)
           (_error_ "numerical argument required")))))

(define-macro _ARG=NBR0_
  (lambda (argtab env idx)
      `(let ((nbr (_ARG=NBR_ ,argtab ,env ,idx)))
         (if (zero? nbr)
           (_error_ "non-zero argument required")
           nbr))))

(define-macro _ARG=INT_
  (lambda (argtab env idx)
      `(let ((nbr (_ARG=NBR_ ,argtab ,env ,idx)))
         (if (integer? nbr)
           nbr
           (_error_ "integer argument required")))))

(define-macro _ARG=INT0_
  (lambda (argtab env idx)
      `(let ((nbr (_ARG=INT_ ,argtab ,env ,idx)))
         (if (zero? nbr)
           (_error_ "non-zero argument required")
           nbr))))

(define _nat_add_
  (_AG_MAKE_NAT_ (lambda (argtab env)
                   (_ARITY<=2_ argtab)
                   (case (_AG_GET_siz_ argtab)
                     ((1) (_AG_MAKE_NBR_ (_ARG=NBR_ argtab env 1)))
                     ((2) (_AG_MAKE_NBR_ (+ (_ARG=NBR_ argtab env 1)
                                           (_ARG=NBR_ argtab env 2))))))))
(define _nat_sub_
  (_AG_MAKE_NAT_ (lambda (argtab env)
                   (_ARITY<=2_ argtab)
                   (case (_AG_GET_siz_ argtab)
                     ((1) (_AG_MAKE_NBR_ (- (_ARG=NBR_ argtab env 1))))
                     ((2) (_AG_MAKE_NBR_ (- (_ARG=NBR_ argtab env 1)
                                           (_ARG=NBR_ argtab env 2))))))))
(define _nat_mul_
  (_AG_MAKE_NAT_ (lambda (argtab env)
                   (_ARITY=2_ argtab)
                   (_AG_MAKE_NBR_ (* (_ARG=NBR_ argtab env 1)
                                    (_ARG=NBR_ argtab env 2))))))
(define _nat_div_
  (_AG_MAKE_NAT_ (lambda (argtab env)
                   (_ARITY=2_ argtab)
                   (_AG_MAKE_NBR_ (/ (_ARG=NBR_ argtab env 1)
                                    (_ARG=NBR0_ argtab env 2))))))
(define _nat_quo_
  (_AG_MAKE_NAT_ (lambda (argtab env)
                   (_ARITY=2_ argtab)
                   (_AG_MAKE_NBR_ (quotient (_ARG=INT_ argtab env 1)
                                    (_ARG=INT0_ argtab env 2))))))
(define _nat_mod_
  (_AG_MAKE_NAT_ (lambda (argtab env)
                   (_ARITY=2_ argtab)
                   (_AG_MAKE_NBR_ (modulo (_ARG=INT_ argtab env 1)
                                    (_ARG=INT0_ argtab env 2))))))
(define _nat_exp_
  (_AG_MAKE_NAT_ (lambda (argtab env)
                   (_ARITY=2_ argtab)
                   (_AG_MAKE_NBR_ (expt (_ARG=NBR_ argtab env 1)
                                    (_ARG=NBR_ argtab env 2))))))
(define _nat_sqt_
  (_AG_MAKE_NAT_ (lambda (argtab env)
                   (_ARITY=1_ argtab)
                   (_AG_MAKE_NBR_ (sqrt (_ARG=NBR_ argtab env 1))))))
(define _nat_les_
  (_AG_MAKE_NAT_ (lambda (argtab env)
                   (_ARITY=2_ argtab)
                   (if (< (_ARG=NBR_ argtab env 1)
                         (_ARG=NBR_ argtab env 2))
                     _true_ _false_))))
(define _nat_eql_
  (_AG_MAKE_NAT_ (lambda (argtab env)
                   (_ARITY=2_ argtab)
                   (if (equal? (_ARG=VAL_ argtab env 1)
                         (_ARG=VAL_ argtab env 2))
                     _true_ _false_))))
(define _nat_grt_
  (_AG_MAKE_NAT_ (lambda (argtab env)
                   (_ARITY=2_ argtab)
                   (if (> (_ARG=NBR_ argtab env 1)
                         (_ARG=NBR_ argtab env 2))
                     _true_ _false_))))
(define _nat_trn_
  (_AG_MAKE_NAT_ (lambda (argtab env)
                   (_ARITY=1_ argtab)
                   (_AG_MAKE_NBR_ (truncate (_ARG=NBR_ argtab env 1))))))
(define _nat_nbr_
  (_AG_MAKE_NAT_ (lambda (argtab env)
                   (_ARITY=1_ argtab)
                   (if (= (_AG_TAG_ (_ARG=ANY_ argtab env 1)) _NBR_TAG_)
                     _true_ _false_))))
(define _nat_sym_
  (_AG_MAKE_NAT_ (lambda (argtab env)
                   (_ARITY=1_ argtab)
                   (if (= (_AG_TAG_ (_ARG=ANY_ argtab env 1)) _SYM_TAG_)
                     _true_ _false_))))
(define _nat_fun_
  (_AG_MAKE_NAT_ (lambda (argtab env)
                   (_ARITY=1_ argtab)
                   (if (= (_AG_TAG_ (_ARG=ANY_ argtab env 1)) _FUN_TAG_)
                     _true_ _false_))))
(define _nat_tab_
  (_AG_MAKE_NAT_ (lambda (argtab env)
                   (_ARITY=1_ argtab)
                   (if (= (_AG_TAG_ (_ARG=ANY_ argtab env 1)) _TAB_TAG_)
                     _true_ _false_))))
(define _nat_siz_
  (_AG_MAKE_NAT_ (lambda (argtab env)
                   (_ARITY=1_ argtab)
                   (_AG_MAKE_NBR_ (_ARG=TAB_ argtab env 1)))))
(define _nat_prn_
  (_AG_MAKE_NAT_ (lambda (argtab env)
                   (_AG_FOR_EACH_TAB_ argtab
                     (lambda (arg)
                       (_print_ (_eval_ arg env))
                       _void_)
                     _void_))))
(define _nat_seq_
  (_AG_MAKE_NAT_ (lambda (argtab env)
                   (_AG_FOR_EACH_TAB_ argtab
                     (lambda (arg)
                       (_eval_ arg env))
                     _void_))))
(define _nat_exi_
  (_AG_MAKE_NAT_ (lambda (argtab env)
                   (_error_ #f))))

;-----------------------------------;
;          >>>Pico-eval<<<          ;
;            Theo D'Hondt           ;
;   Lab voor Programmeerkunde VUB   ;
;               �1995               ;
;-----------------------------------;

;----------------------------------------------------------------------------;
;EVAL(Exp,Env)                                                               ;
;                                                                            ;
;Exp=[_VOI_TAG_]                                                      => Exp ;
;                                                                            ;
;Exp=[_NBR_TAG_,Nbr]                                                  => Exp ;
;                                                                            ;
;Exp=[_SYM_TAG_,Sym]                                                  => Exp ;
;                                                                            ;
;Exp=[_FUN_TAG_,Arg,Exp,Env]                                          => Exp ;
;                                                                            ;
;Exp=[_TAB_TAG_,Tab]                                                  => Exp ;
;                                                                            ;
;Exp=[_NAT_TAG_,Nat]                                                  => Exp ;
;                                                                            ;
;Exp=[_DFN_TAG_,Idf,Exp]                    => DEFINE(Idf EVAL(Exp,Env) Env) ;
;                                                                            ;
;Exp=[_RFN_TAG_,Idf]                                  => (REFERENCE Idf Env) ;
;                                                                            ;
;Exp=[_STN_TAG_,Idf,Exp]                    => UPDATE(Idf EVAL(Exp,Env) Env) ;
;                                                                            ;
;Exp=[_DFF_TAG_,Idf,Arg,Exp]             => DEFINE(Idf FUN(Arg Exp Env) Env) ;
;                                                                            ;
;Exp=[_RFF_TAG_,Idf,Arg]        => EVAL(BODY(Idf),EXTEND(Idf BIND(Idf Arg))) ;
;                                                                            ;
;Exp=[_STF_TAG_,Idf,Arg,Exp]             => UPDATE(Idf FUN(Arg Exp Env) Env) ;
;                                                                            ;
;Exp=[_DFT_TAG_,Idf,Nbr,Exp] => DEFINE(Idf TAB(EVAL(Nbr,Env) EVAL(Exp,Env))) ;
;                                                                            ;
;Exp=[_RFT_TAG_,Idf,Nbr]            => GET(REFERENCE(Idf Env) EVAL(Nbr,Env)) ;
;                                                                            ;
;Exp=[_STT_TAG_,Idf,Nbr,Exp]  => SET(REFERENCE(Idf Env) EVAL(Nbr,Env)        ;
;                                                            EVAL(Exp,Env))) ;
;----------------------------------------------------------------------------;

;(load "ag.def")

(define (_eval_ exp env)
  (define tag (_AG_TAG_ exp))

  (define (verify-par par)
    (define tag (_AG_TAG_ par))
    (cond
      ((= tag _RFN_TAG_)
        #t)
      ((= tag _TAB_TAG_)
        (let ((siz (_AG_GET_siz_ par)))
          (do ((idx 1 (+ idx 1)))
            ((> idx siz))
            (let* ((exp (_AG_GET_elt_ par idx))
                    (tag (_AG_TAG_ exp)))
              (cond
                ((= tag _RFN_TAG_)
                  #t)
                ((= tag _RFF_TAG_)
                  (verify-par (_AG_GET_arg_ exp)))
                (else
                  (_error_ "invalid parameter")))))))
      (else
        (_error_ "invalid parameter list"))))

  (define (bind arg par fun-env)
    (define par-tag (_AG_TAG_ par))
    (if (not (= (_AG_TAG_ arg) _TAB_TAG_))
      (set! arg (_eval_ arg env)))
    (if (= (_AG_TAG_ arg) _TAB_TAG_)
      (if (= par-tag _RFN_TAG_)
        (let* ((siz (_AG_GET_siz_ arg))
                (eval-arg (_AG_MAKE_TAB_ siz '())))
          (do ((idx 1 (+ idx 1)))
            ((> idx siz))
            (_AG_SET_elt_ eval-arg idx (_eval_ (_AG_GET_elt_ arg idx) env)))
          (_define_env_ (_AG_GET_nam_ par) eval-arg fun-env))
        (let ((siz (_AG_GET_siz_ par)))
          (if (= siz (_AG_GET_siz_ arg))
            (do ((idx 1 (+ idx 1)))
              ((> idx siz))
              (let* ((par-exp (_AG_GET_elt_ par idx))
                      (arg-exp (_AG_GET_elt_ arg idx))
                      (tag (_AG_TAG_ par-exp))
                      (nam (_AG_GET_nam_ par-exp)))
                (_define_env_ nam
                  (cond
                    ((= tag _RFN_TAG_)
                      (_eval_ arg-exp env))
                    ((= tag _RFF_TAG_)
                      (_AG_MAKE_FUN_ (_AG_GET_arg_ par-exp) arg-exp env)))
                  fun-env)))
            (_error_ "non-matching argument list"))))
      (_error_ "invalid argument")))

  (define (VOIeval)
    exp)

  (define (NBReval)
    exp)

  (define (SYMeval)
    exp)

  (define (FUNeval)
    exp)

  (define (TABeval)
    exp)

  (define (NATeval)
    exp)

  (define (DFNeval)
    (define nam (_AG_GET_nam_ exp))
    (define val (_AG_GET_val_ exp))
    (_define_env_ nam (_eval_ val env) env))

  (define (RFNeval)
    (define nam (_AG_GET_nam_ exp))
    (_reference_env_ nam env))

  (define (STNeval)
    (define nam (_AG_GET_nam_ exp))
    (define val (_AG_GET_val_ exp))
    (_update_env_ nam (_eval_ val env) env))

  (define (DFFeval)
    (define nam (_AG_GET_nam_ exp))
    (define par (_AG_GET_arg_ exp))
    (define bod (_AG_GET_exp_ exp))
    (verify-par par)
    (_define_env_ nam (_AG_MAKE_FUN_ par bod env) env))

  (define (RFFeval)
    (define nam (_AG_GET_nam_ exp))
    (define arg (_AG_GET_arg_ exp))
    (define fun (_reference_env_ nam env))
    (define tag (_AG_TAG_ fun))
    (cond
      ((= tag _FUN_TAG_)
        (let ((par (_AG_GET_par_ fun))
               (bod (_AG_GET_bod_ fun))
               (fun-env (_extend_env_ (_AG_GET_env_ fun))))
          (bind arg par fun-env)
          (_eval_ bod fun-env)))
      ((= tag _NAT_TAG_)
        (let ((nat (_AG_GET_nat_ fun)))
          (nat arg env)))
      (else
        (_error_ "not a function"))))

  (define (STFeval)
    (define nam (_AG_GET_nam_ exp))
    (define par (_AG_GET_arg_ exp))
    (define bod (_AG_GET_exp_ exp))
    (verify-par par)
    (_update_env_ nam (_AG_MAKE_FUN_ par bod env) env))

  (define (DFTeval)
    (define nam (_AG_GET_nam_ exp))
    (define idx (_eval_ (_AG_GET_idx_ exp) env))
    (define ini (_eval_ (_AG_GET_cmp_ exp) env))
    (define siz
      (let ((tag (_AG_TAG_ idx)))
        (if (= tag _NBR_TAG_)
          (let ((nbr (_AG_GET_nbr_ idx)))
            (if (positive? nbr)
              nbr
              (_error_ "non-positive size")))
          (_error_ "invalid size"))))
    (_define_env_ nam (_AG_MAKE_TAB_ siz ini) env))

  (define (RFTeval)
    (define nam (_AG_GET_nam_ exp))
    (define idx (_eval_ (_AG_GET_idx_ exp) env))
    (define tab (_reference_env_ nam env))
    (define tag (_AG_TAG_ tab))
    (if (= tag _TAB_TAG_)
      (let ((tag (_AG_TAG_ idx)))
        (if (= tag _NBR_TAG_)
          (_AG_GET_elt_ tab (_AG_GET_nbr_ idx))
          (_error_ "invalid index")))
      (_error_ "not a table")))

  (define (STTeval)
    (define nam (_AG_GET_nam_ exp))
    (define idx (_eval_ (_AG_GET_idx_ exp) env))
    (define val (_eval_ (_AG_GET_cmp_ exp) env))
    (define tab (_reference_env_ nam env))
    (define tag (_AG_TAG_ tab))
    (if (= tag _TAB_TAG_)
      (let ((tag (_AG_TAG_ idx)))
        (if (= tag _NBR_TAG_)
          (_AG_SET_elt_ tab (_AG_GET_nbr_ idx) val)
          (_error_ "invalid index")))
      (_error_ "not a table"))
    tab)

  ((vector-ref (vector VOIeval
                 NBReval
                 SYMeval
                 FUNeval
                 TABeval
                 NATeval
                 DFNeval
                 RFNeval
                 STNeval
                 DFFeval
                 RFFeval
                 STFeval
                 DFTeval
                 RFTeval
                 STTeval) tag)))

;-----------------------------------;
;          >>>Pico-print<<<         ;
;            Theo D'Hondt           ;
;   Lab voor Programmeerkunde VUB   ;
;               �1995               ;
;-----------------------------------;

;(load "ag.def")

(define (_print_ exp)
  (define tag (_AG_TAG_ exp))

  (define (show str)
    (define nam (_AG_GET_nam exp))
    (display "[" _out_)
    (display str _out_)
    (display ": " _out_)
    (display nam _out_)
    (display "]" _out_))

  (define (VOIprint)
    #f)

  (define (NBRprint)
    (define nbr (_AG_GET_nbr_ exp))
    (display nbr _out_))

  (define (SYMprint)
    (define sym (_AG_GET_sym_ exp))
    (display sym _out_))

  (define (FUNprint)
    (display "[function]" _out_))

  (define (TABprint)
    (define ctr (_AG_GET_siz_ exp))
    (display "[" _out_)
    (_AG_FOR_EACH_TAB_ exp
      (lambda (elt)
        (_print_ elt)
        (set! ctr (- ctr 1))
        (display (if (zero? ctr) "]" ",") _out_))
      _false_))

  (define (NATprint)
    (display "[native]" _out_))

  (define (DFNprint)
    (show "variable definition"))

  (define (RFNprint)
    (show "variable reference"))

  (define (STNprint)
    (show "variable assignment"))

  (define (DFFprint)
    (show "function definition"))

  (define (RFFprint)
    (show "function reference"))

  (define (STFprint)
    (show "function assignment"))

  (define (DFTprint)
    (show "table definition"))

  (define (RFTprint)
    (show "table reference"))

  (define (STTprint)
    (show "table assignment"))

  (define (RFPprint)
    (show "primitive reference"))

  ((vector-ref (vector VOIprint
                 NBRprint
                 SYMprint
                 FUNprint
                 TABprint
                 NATprint
                 DFNprint
                 RFNprint
                 STNprint
                 DFFprint
                 RFFprint
                 STFprint
                 DFTprint
                 RFTprint
                 STTprint
                 RFPprint) tag)))

;-----------------------------------;
;          >>>Pico-loop<<<          ;
;            Theo D'Hondt           ;
;   Lab voor Programmeerkunde VUB   ;
;               �1995               ;
;-----------------------------------;

;(load "ag.def")

(define _in_ (open-input-file "Pico.ini"))
(define _out_ (current-output-port))
(define _global_ (_create_env_))
(define _void_  (_AG_MAKE_VOI_))
(define _true_  (_AG_MAKE_SYM_ "true"))
(define _false_ (_AG_MAKE_SYM_ "false"))

(define (_loop_ flag)
  (error "initialisation file corrupt"))

(define (_error_ msg)
  (cond
    (msg
      (display "***error: " _out_)
      (display msg _out_)
      (newline _out_)
      (_flush_)
      (_loop_ #t))
    (else
      (display 'exiting... _out_)
      (newline  _out_)
      (close-input-port _in_)
      (_loop_ #f))))

(_define_env_ "void"     _void_    _global_)
(_define_env_ "+"        _nat_add_ _global_)
(_define_env_ "-"        _nat_sub_ _global_)
(_define_env_ "*"        _nat_mul_ _global_)
(_define_env_ "/"        _nat_div_ _global_)
(_define_env_ "//"       _nat_quo_ _global_)
(_define_env_ "\\\\"     _nat_mod_ _global_)
(_define_env_ "^"        _nat_exp_ _global_)
(_define_env_ "<"        _nat_les_ _global_)
(_define_env_ "="        _nat_eql_ _global_)
(_define_env_ ">"        _nat_grt_ _global_)
(_define_env_ "trunc"    _nat_trn_ _global_)
(_define_env_ "sqrt"     _nat_sqt_ _global_)
(_define_env_ "number"   _nat_nbr_ _global_)
(_define_env_ "symbol"   _nat_sym_ _global_)
(_define_env_ "function" _nat_fun_ _global_)
(_define_env_ "table"    _nat_tab_ _global_)
(_define_env_ "size"     _nat_siz_ _global_)
(_define_env_ "print"    _nat_prn_ _global_)
(_define_env_ "seq"      _nat_seq_ _global_)
(_define_env_ "exit"     _nat_exi_ _global_)

(cond
  ((call-with-current-continuation
     (lambda (exit)
       (_eval_ (_read_) _global_)
       (close-input-port _in_)
       (set! _in_ (current-input-port))
       (set! _out_ (current-output-port))
       (set! _true_  (_eval_ (_AG_MAKE_RFN_ "true") _global_))
       (set! _false_ (_eval_ (_AG_MAKE_RFN_ "false") _global_))
       (display "Pico (v. 6-6-95)" _out_)
       (newline  _out_)
       (newline  _out_)
       (set! _loop_ exit)
       #t))
    (display "� " _out_)
    (_print_ (_eval_ (_read_) _global_))
    (newline  _out_)
    (_loop_ #t))
  (else
      'Pico))

;; TODO: make this file do something + use the pico.ini file.