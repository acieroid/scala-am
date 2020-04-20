(begin 
  (define (require p)
    (if (not p) (amb)))
  
  (define N '(naamwoord huis kat muis))
  (define W '(werkwoord eet jaagt))
  (define L '(lidwoord de een het))
  (define V '(voorzetsel in op))
  
  (define (ontleed-zin)
    (list 'zin (ontleed-naamwoord-vorm) (ontleed-werkwoord-vorm)))
  
  (define (ontleed-eenvoudige-naamwoord-vorm)
    (list 'eenvoudige-naamwoord-vorm (ontleed-woord L) (ontleed-woord N)))
  
  (define (ontleed-naamwoord-vorm)
    (define (misschien naamwoord-vorm)
      (amb naamwoord-vorm
           (misschien 
            (list 'naamwoord-vorm naamwoord-vorm (ontleed-voorzetsel-vorm)))))
    (misschien (ontleed-eenvoudige-naamwoord-vorm)))
  
  (define (ontleed-werkwoord-vorm)
    (define (misschien werkwoord-vorm)
      (amb werkwoord-vorm
           (misschien 
            (list 'werkwoord-vorm werkwoord-vorm
                  (ontleed-voorzetsel-vorm)))))
    (misschien (ontleed-woord W)))
  
  (define (ontleed-woord woord-lijst)
    (require (not (null? *te-doen*)))
    (require (memq (car *te-doen*) (cdr woord-lijst)))
    (let ((gevonden (car *te-doen*)))
      (set! *te-doen* (cdr *te-doen*))
      (list (car woord-lijst) gevonden)))
  
  (define (ontleed-voorzetsel-vorm)
    (list 'voorzetsel-vorm (ontleed-woord V) (ontleed-naamwoord-vorm)))
  
  (define *te-doen* '())
  
  (define (ontleed invoer)
    (set! *te-doen* invoer)
    (let ((gedaan (ontleed-zin)))
      (require (null? *te-doen*))
      gedaan))

  (ontleed '(de kat jaagt op de muis in het huis)))