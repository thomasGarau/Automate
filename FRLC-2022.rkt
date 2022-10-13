(require (lib "trace.ss"))

(require (lib "compat.ss"))

(require (lib "list.ss"))
(define *frames* '())
(define *frame '())
(define *slot '())
(define *facet '())
(define *value '())
(define b '())

(define (fget frame slot facet)
  (map car 
       (mycdr (myassoc  facet 
                        (mycdr (myassoc slot 
                                        (mycdr (mygetprop frame 'frame))))))))

(define (mycdr l)
  (cond ((null? l) '())
        (#t (cdr l))))

(define (mygetprop s p) 
  (cond ((equal? #f (getprop s p)) '())
        (#t (getprop s p))))

(define (fassoc-slot frame cle aliste)
  (cond ((assoc  cle (cdr aliste)))
        (#t    (putprop frame 'frame (ajoute-slot cle aliste (getprop frame 'frame))) (myassoc cle (cdr (getprop frame 'frame)))
               )))


(define (fassoc-facet frame slot facet cle aliste)
  
  
  
  (cond ((null? aliste) '())
        ((assoc  cle (cdr aliste)))
        ;((
        (#t    (putprop frame 'frame (ajoute-facet  facet slot cle aliste (getprop frame 'frame))) (myassoc cle (cdr (myassoc slot (cdr (getprop frame 'frame)))))
               )))

(define (fassoc-value  frame slot facet  value cle aliste)
  
  (cond ((null? aliste) '())
        ((assoc  cle (cdr aliste)))
        (#t (putprop frame 'frame (ajoute-value  value facet  slot 
                                                 cle aliste (getprop frame 'frame)))
            ))) 

(define (rplacd l l1)
  (cons (car l) l1))

(define (rplacd2 lambda l l1)
  (set-rest! l l1) (cons (car l) l1))


(define (ajoute-slot cle aliste  l)
  (cond ((null? (cdr l)) (list (car l) (cons cle (cdr l))))
        (#t (cons (car l) (cons (cons cle (cdr aliste)) (cdr l))))))

(define (removemy s l)
  (cond ((null? l) '())
        ((equal? s (car l))(removemy s (cdr l)))
        (else (cons (car l) (removemy s (cdr l)))))) 


(define (ajoute-facet  facet slot cle aliste  l) 
  
  (cons (car l) (cons (cons  slot  (cons (list facet) (cdr aliste))) (remove (myassoc slot (cdr l)) (cdr l)))))

(define (ajoute-value value facet  slot cle aliste  l)
  ;(cons (car l)(cons (cons slot (cons (cons facet (cons (list value) (cdr aliste))) (myassoc facet (cdr aliste)))) (remove (myassoc slot (cdr l)) (cdr l))))) 
  (cons (car l)(cons (append (list slot) (cons (cons facet (cons (list value) (cdr aliste))) (myassoc facet (cdr aliste))) (liste-facets (car l)  facet slot l)  ) (remove (myassoc slot (cdr l)) (cdr l)))))  

(define (liste-facets frame facet slot l)
  
  (cond ((not (null? (mycdr (mycdr (myassoc slot (cdr l)))))) (cond ((not (member (caar (mycdr (mycdr (myassoc slot (cdr l)))))  (list-slots l)))   (mycdr (cdr (assoc slot (cdr l)))))
                                                                    (#t '()))) 
        
        (#t '())))


(define (list-slots l)
  (map (lambda ( ls)(car ls)  )  (cdr  (fgetframe (car l))))) 

(define (myassoc cle aliste)
  (cond ((equal? #f (assoc cle aliste)) '())
        ((assoc cle  aliste))))

(define (fput frame slot facet valeur)
  (cond ((equal? valeur (mycar (fget frame slot facet))) #f)
        (#t  (fassoc-value frame slot facet valeur valeur
                           (fassoc-facet frame slot  facet facet
                                         (fassoc-slot frame slot
                                                      (fgetframe frame)))) valeur)  ))  

(define (mycar l)(cond ((null? l) '())
                       (#t (car l))))

(define fgetframe (lambda (frame)
                    (cond ((getprop frame 'frame))
                          (#t (set! *frames* (append *frames* (list frame))) 
                              (putprop frame  'frame (list frame))))
                    (getprop frame 'frame) )) 


(define last (lambda (l)
               (cond ( (not (null? l)) (list (car (reverse l))))
                     (#t '()))))


(define (myfget frame slot facet)
  (map car 
       (mycdr (myassoc  facet 
                        (mycdr (myassoc slot 
                                        (mycdr b)))))))

(define (myffacet frame slot)
(map car (mycdr (myassoc slot (mycdr b)))))

(define (myval frame slot facet)
(map car (mycdr (myassoc facet (mycdr (myassoc slot (mycdr b)))))))

(define (fremove frame slot facet valeur)
    (define taille_slot (fslot frame))
    (define r #f)
    (set! b (fgetframe frame))
    (putprop frame 'frame (list frame))
    (map(lambda(e)
      (map (lambda(fac)
        (map(lambda(val)
            (cond((not(equal? valeur val))
              (fput frame e fac val)) 
              (#t (set! r 'T))))
              (myval frame e fac)))(myffacet frame e)))taille_slot)r)


(define (fcreate frame name)
  (fput name 'ako 'valeur frame)
  (fput name 'classification 'valeur 'instance))

(define (finst frame name)
  (fcreate frame name) 
  (set! *frame name)
  (map(lambda(e)
        (map (lambda (slot)
        (cond((not (null?(fget e slot 'valeur))))
             (#t (cond((not (null? (fget e slot 'defaut))))
                      (#t (set! *slot slot)(apply(eval (mycar (fget e slot 'ifneeded))) '()))))))
               (fslot e)))
      (fgetclasses frame)))

(define (fgetclasses frame)
  (cond((null? frame) '())
       ((equal? frame 'objet) '(objet))
      (#t (cons frame (fgetclasses(mycar(fget frame 'ako 'valeur)))))))  

(define(fslot frame)
  (map car(mycdr (mygetprop frame 'frame))))

(define (ffacet frame slot)
(map car (mycdr (myassoc slot (mycdr (mygetprop frame 'frame))))))


(define (fget-I frame slot)
  (define liste (fgetclasses frame))
  (cond((equal? 1 (length liste)) (fget  (car liste) slot 'valeur))
       ((not(equal? '() (fget  (car liste) slot 'valeur)))(fget  (car liste) slot 'valeur))
  (#t(fget-I (car (cdr liste)) slot))))
    
(define (fget-Ibis frame slot cle)
  (define liste (fgetclasses frame))
  (cond((equal? 1 (length liste)) (fget  (car liste) slot cle))
       ((not(equal? '() (fget  (car liste) slot cle)))(fget  (car liste) slot cle))
  (#t(fget-Ibis (car (cdr liste)) slot cle))))

(define (fget-N frame slot)
  (cond((not(equal? '() (fget-Ibis frame slot 'valeur))) (fget-Ibis frame slot 'valeur))
       ((not(equal? '() (fget-Ibis frame slot 'defaut))) (fget-Ibis frame slot 'defaut))
       ((not(equal? '() (fget-Ibis frame slot 'ifneeded))) (fget-Ibis frame slot 'ifneeded))))

(define (fget-Z frame slot)
  (define liste (fgetclasses frame))
  (cond((equal? 1 (length liste)) (cond((not(equal? '() (fget  (car liste) slot 'valeur))) (fget  (car liste) slot 'valeur))
                                       ((not(equal? '() (fget  (car liste) slot 'defaut))) (fget  (car liste) slot 'defaut))
                                       ((not(equal? '() (fget  (car liste) slot 'ifneeded))) (fget  (car liste) slot 'ifneeded))))
       ((cond((not(equal? '() (fget  (car liste) slot 'valeur))) (fget  (car liste) slot 'valeur))
             ((not(equal? '() (fget  (car liste) slot 'defaut))) (fget  (car liste) slot 'defaut))
             ((not(equal? '() (fget  (car liste) slot 'ifneeded))) (fget  (car liste) slot 'ifneeded))))
  (#t(fget-Z (car (cdr liste)) slot))))

(define (fgename frame))
