(require racket/string)
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

(define (fput+ frame slot facet valeur)
  (fput frame slot facet (apply(eval(mycar(fget (mycar(cdr(fgetclasses frame))) slot 'if-added))) (list valeur))))

(define (calcul-taille valeur)
  ( + valeur '1))


(define (fako? frame1 frame2 e)
  (define a '())
  (cond ((equal? 1 (length e)) #f)
  ((null? e)(set! a (fgetclasses frame1)))
   (#t (set! a e)))
  (cond ((not(member (car a)) (fgetclasses frame2)) (fako? frame1 frame2 (cdr a)))
    (#t e)))

(define (fako? frame1 frame2)
  (cond ((member frame1 (fgetclasses frame2)) #t)
        ( (member frame2 (fgetclasses frame1)) #t)
        (#t #f)))

(define (flink? frame1 frame2 slot)
  (cond ((member (mycar(fget frame1 slot 'valeur)) (fgetslotsvalue frame2 slot)) #t)
        ((member (mycar(fget frame2 slot 'valeur)) (fgetslotsvalue frame1 slot)) #t)
        (#t #f)))
        

(define (salut)
  (display "salut"))
(define (fremove+ frame slot facet valeur)
  (fremove frame slot facet valeur)
  (cond ((equal? facet 'if-removed) (apply(eval (mycar (fget frame slot 'if-removed))) '()))))

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


(define (frames? frame)
  (define liste *frames*)
  (is-in-list liste frame))
  
(define (fname nom)
  (define liste *frames*)
  (cond ((not (equal? #t (is-in-list liste nom)))#f)
          (#t (print nom))))
  

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

(define (fgetslotsvalue frame slot)
  (define liste (fslot frame))
  (cond((null? frame) '())
       ((equal? frame 'objet) '(objet))
       (#t (cons frame (fgetslotsvalue(mycar(fget frame slot 'valeur)) slot)))))  

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


(define (fgename frame)
  (cond((getprop frame 'number))
       (#t(putprop frame 'number 1))
  )
  (define char (symbol->string frame))
  (define num (number->string (getprop frame 'number)))
  (define res (string->symbol (string-append char "_" num)))
  (putprop frame 'number (+ 1 (getprop frame 'number)))res)

(define (fchildren frame slot)
  (define liste (cadr (fgetclasses frame)))liste)

(define (Frame frame)
  (cond((member frame *frames*) (fgetframe frame)) (#t '())))

;retourne toute les key du dico sans les value
(define (fkey dico)
  (map(lambda(e)
        (car e))dico))

(define (fmenu)
  (define dico '(
                (fget 3)
                (fget-I 2)
                (fget-Z 2)
                (fget-N 2)
                (fput 3)
                (fput+ 4)
                (fremove 3)
                (fRemove+ 4)
                (fcreate 2)
                (finst 2)
                (fgetclasses 1)
                (fgename 1)
                (fchildren 2)
                (Fframe 1)
                (Fframe? 2)
                (flink 2)
                (fako? 2)
                (fname 1)
                (fname? 2)
                (finstance? 1)
                (fgeneric? 1)
                (fcheck 2)
               ))
  (define listeKey (fkey dico))
  (print "vous pouvez utiliser une fonction parmis :")
  (print listeKey)
  (define input (string->symbol (read-line (current-input-port))))
  (cond ((not(member input listeKey)) "saisie incorect"))
  (define nbArg (myassoc input dico))
  (
  )


(define (Fwriteframe frame)
  (define ret "" )
  (set! ret (string-append ret (symbol->string (car(fgetframe frame))) ":" " \n "))
  (define listslot (fslot frame))
  (map(lambda(e)
    (set! ret (string-append ret (symbol->string e) ":" " \n " ))
    (set! ret (string-append ret "   " (symbol->string (car(ffacet frame e))) "\n" "      "(cond ((number? (car (fget frame e (car(ffacet frame e))))) (number->string (car (fget frame e (car(ffacet frame e))))))
                                                                                           ((symbol? (car (fget frame e (car(ffacet frame e))))) (symbol->string (car (fget frame e (car(ffacet frame e))))))
                                                                                           ((string? (car (fget frame e (car(ffacet frame e))))) (car (fget frame e (car(ffacet frame e))))) )  " \n "))
    )listslot) ret)


(define (Fimprim frame)
  (define out (open-output-file "sauvegarde.txt" #:exists 'append))
  (println (Fwriteframe frame)out)
  (close-output-port out))


(define (Fsave)
  (define out (open-output-file "sauvegarde.txt" #:exists 'truncate))
  (define a *frames*)
  (map(lambda(e)
        (Fimprim e))a))

(define (Fload)
  (define file "sauvegarde.txt")
  (define (f file)
    (let ((line (read-line file 'any)))
      (unless (eof-object? line)
        (fputSharp line)
        (f file))))
  (call-with-input-file file f))

(define (fputSharp var)
  (define l (string-split var ":"))
  (set! l (string-join l ""))
  (set! l (string->list l))
  (set! l (remove* '(#\space) l))
  (set! l (list->string l))
  (set! l (string-split l "\\n"))
  (display l)
  (define len (/ (- (length l) 2) 3))
  (set! len (build-list len values))
  (set! len (map (lambda (n) (* 3 n)) len))
  (define nom (substring (car l) 1))
  (map(lambda(e)
        (fput (string->symbol nom) (string->symbol (list-ref l (+ e 1) )) (string->symbol (list-ref l (+ e 2))) (cond( (not(equal? #f (string->number (list-ref l (+ e 3))))) (string->number (list-ref l(+ e 3))))
                                                                                                                (#t (string->symbol (list-ref l (+ e 3)))))
  ))len))  
  
  (define (Refresh frame panel)
    (define text(new text%))
    (send panel set-editor text)
    (send text auto-wrap #t)
    (send text set-padding 10 10 10 10)
    (define f (symbol->string (car(fgetframe frame))))
    (define listslot (fslot frame))
    (send text insert (make-object string-snip% f))
    (send text insert (make-object string-snip% f)))

  (send fenetre show #t))


(define (Fwriteframe frame)
  (define ret "|" )
  (set! ret (string-append ret (symbol->string (car(fgetframe frame))) ":" " \n "))
  (define listslot (fslot frame))
  (map(lambda(e)
    (set! ret (string-append ret (symbol->string e) ":" " \n " ))
    (set! ret (string-append ret (symbol->string (car(ffacet frame e))) "->" (symbol->string (car (fget frame e (car(ffacet frame e))))) " \n "))
    )listslot) ret)

(define (Fimprim frame)
  (define out (open-output-file "sauvegarde.txt" #:exists 'truncate))
  (println (Fwriteframe frame) out)
  (close-output-port out))

(fput 'homme 'vie 'defaut 'vivant)
(fput 'homme 'classification 'valeur 'prototype)
(fput 'homme 'ako 'valeur 'objet)
(fput 'homme 'age 'if-added 'calcul-taille)
(fput 'homme 'age 'if-removed 'del)
(fput 'homme 'travail 'ifneeded 'ask)
(fput 'homme 'marié 'defaut 'non)
(fput 'homme 'mere 'defaut 'inconnue)
(fput 'femme 'ako 'valeur 'objet)
(fput 'femme 'vie 'defaut 'vivant)
(fput 'femme 'travail 'defaut 'aucun)
(fput 'femme 'marié 'defaut 'non)
(fput 'femme 'enfant 'defaut 0)
(fput 'femme 'mere 'defaut 'inconnue)
(fput 'canari 'couleur 'valeur 'jaune)
(fput 'pioupiou 'ako 'valeur 'canari)
(fput 'canari 'ako 'valeur 'oiseau)
(fput 'henry 'ako 'valeur 'homme)
(fput 'marc 'ako 'valeur 'homme)
(fput 'marc 'classification 'valeur 'instance)
(fput 'henry 'classification 'valeur 'instance)
(fput 'marie 'ako 'valeur 'femme)
(fput+ 'henry 'age 'age '21)
(fremove+ 'henry 'age 'age '21)
(fako?  'cannari 'pioupiou)
(fmenu)
(flink? 'canari 'pioupiou 'couleur)
(flink? 'henry 'marc 'classification)
(flink? 'homme 'henry 'ako)
(flink? 'oiseau 'pioupiou 'ako)
(flink? 'marie 'henry 'ako)

