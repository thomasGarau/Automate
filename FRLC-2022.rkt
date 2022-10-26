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
  ; putprop ajoute une valeur dans un prop((une sorte d'objet avec clé value)) 
  ; pour cela appel ajoute-slot qui va grace à getprop récup le prop((la liste des slot du frame)) et le concatener avec le slot à ajouté 
  ; putprop ajouteras donc la liste de tout les slots avec le nouveau slot

  ;; puisque le slot est creer pour placer la facette fassoc-facette à besoin d'une valeur de retour contenant le nouveau slot 
  (cond ((assoc  cle (cdr aliste)))
  ;place dans la frame la propriété frame avec une liste dans laquelle on place à l'index 1 la propriété
        (#t    (putprop frame 'frame (ajoute-slot cle aliste (getprop frame 'frame))) (myassoc cle (cdr (getprop frame 'frame)))
               )))

(define (fassoc-facet frame slot facet cle aliste)
    
  (cond ((null? aliste) '())
        ((assoc  cle (cdr aliste)))
        (#t    (putprop frame 'frame (ajoute-facet  facet slot cle aliste (getprop frame 'frame))) (myassoc cle (cdr (myassoc slot (cdr (getprop frame 'frame)))))
               )))

(define (fassoc-value  frame slot facet  value cle aliste)
  ;test si la liste est null;
  ;cdr retourne les élement de la liste ((qui est en faite une facet)) - le premier element
  (cond ((null? aliste) '())
        ;assoc retourne le premier element de la liste correspondant à la clé 
        ((assoc  cle (cdr aliste)))
        ;si la liste (facet) est vide alors jaoute  
        (#t (putprop frame 'frame (ajoute-value  value facet  slot 
                                                 cle aliste (getprop frame 'frame)))
            ))) 

(define (rplacd l l1)
  (cons (car l) l1))

(define (rplacd2 lambda l l1)
  (set-rest! l l1) (cons (car l) l1))


(define (ajoute-slot cle aliste  l)
                        ;si;
  (cond ((null? (cdr l)) (list (car l) (cons cle (cdr l))))
  ;si non;
        (#t (cons (car l) (cons (cons cle (cdr aliste)) (cdr l))))))

(define (removemy s l)
  (cond ((null? l) '())
        ((equal? s (car l))(removemy s (cdr l)))
        (else (cons (car l) (removemy s (cdr l)))))) 


(define (ajoute-facet  facet slot cle aliste  l) 
  
  (cons (car l) (cons (cons  slot  (cons (list facet) (cdr aliste))) (remove (myassoc slot (cdr l)) (cdr l)))))

(define (ajoute-value value facet  slot cle aliste  l)
  ;(cons (car l)(cons (cons slot (cons (cons facet (cons (list value) (cdr aliste))) (myassoc facet (cdr aliste)))) (remove (myassoc slot (cdr l)) (cdr l))))) 
  ;cons creer une liste à partir de deux élements; 
  ;append fusionne deux liste;
  ;myassoc retourne le premier élement du cdr correspondant à la valeur passer en param
  ;list slot permet de cast slot en une liste afin de pouvoir utilisé le append ((en gros ca permet de fussioner une liste avec un element seule))
  (cons(car l) (cons(append(list slot)(cons (cons facet (cons (list value) (cdr aliste)))(myassoc facet (cdr aliste)))(liste-facets (car l)  facet slot l)  )(remove (myassoc slot (cdr l)) (cdr l)))))  

(define (liste-facets frame facet slot l)
  
  (cond ((not (null? (mycdr (mycdr (myassoc slot (cdr l)))))) (cond ((not (member (caar (mycdr (mycdr (myassoc slot (cdr l)))))  (list-slots l)))   (mycdr (cdr (assoc slot (cdr l)))))
                                                                    (#t '()))) 
        
        (#t '())))


(define (list-slots l)
  (map (lambda ( ls)(car ls)  )  (cdr  (fgetframe (car l))))) 

(define (myassoc cle aliste)
  (cond ((equal? #f (assoc cle aliste)) '())
        ((assoc cle  aliste))))

;setter de slot;
;frame slot facet valeur = param;
(define (fput frame slot facet valeur)
;vérifie d'abord que la valeur n'est pas égale à celle déja présente; 
;#f = false en gros;
;fget retourne car (donc le première élement en gros)
;mycar retourne valeur de la facette du slot de la frame passer en param
;#f marche comme un returne;

  (cond ((equal? valeur (mycar (fget frame slot facet))) #f)
        ;#t => si non returne true + ... ;
        (#t  (fassoc-value frame slot facet valeur valeur
                            ;creer une facet
                           (fassoc-facet frame slot  facet facet
                                        ;on place la facet dans un slot que l'on creer
                                         (fassoc-slot frame slot
                                        ;recreer la frame avec le nouveau slot contenant la facette que l'on souhaiter ajouté 
                                                      (fgetframe frame)))) valeur)  ))  

(define (fput+ frame slot facet valeur)
  (fput frame slot facet (apply(eval(mycar(fget (mycar(cdr(fgetclasses frame))) slot 'if-added))) (list valeur))))

(define (fremove+ frame slot facet valeur)
  (fremove frame slot facet (apply(eval(mycar(fget (mycar(cdr(fgetclasses frame))) slot 'if-removed))) (list valeur))))

(define (del valeur)
  ( + valeur '1))

(define (calcul-taille valeur)
  ( + valeur '1))

(define (fako? frame1 frame2)
  (cond ((member frame1 (fgetclasses frame2)) #t)
        ( (member frame2 (fgetclasses frame1)) #t)
        (#t #f)))
 

(define (salut)
  (display "salut"))

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

(define (is-in-list list value)
 (cond
  [(empty? list) false]
  [(equal? (first list) value) true]
  [else (is-in-list (rest list) value)]))

(define (Fframes? frame)
  (define liste *frames*)
  (is-in-list liste frame))
  
(define (fname nom)
  (define liste *frames*)
  (cond ((not (equal? #t (is-in-list liste nom)))#f)
          (#t (print nom))))
  
(define (finstance? frame)
    (cond ((not(equal? (car(fget-I frame 'classification)) 'instance)) #f)
        (#t)))
  

(define (fgeneric? frame)
  (cond ((not(equal? (car(fget-I frame 'classification)) 'prototype)) #f)
      (#t)))

(define (fcreate frame name)
  ;il faudrait ajouté une verification que le frame existe pas déja avant de le créer ((liste des frames *frame*)); pour ca utilisé la fonctio nmember on peut aussi utilisé print (ca fait partie de scheme)
  (fput name 'ako 'valeur frame)
  (fput name 'classification 'valeur 'instance))

(define (finst frame name)
  (fcreate frame name) 
  (set! *frame name)
  ;map lambda execute une meme action sur chaque element de la liste
  ;e correspond à la frame ((cela est récupérer dans fgetclasses frame))
  (map(lambda(e)
        ;applique l'action lambda(ce qui est définit juste apres) sur slot récup grace à fslot e
        (map (lambda (slot)
        ; verifie pour chaque slot si ca valeur est n'est pas null
        (cond((not (null?(fget e slot 'valeur))))
        ; si elle le slot n'a pas de valeur alors  verifie si ca valeur par defaut n'est pas null
             (#t (cond((not (null? (fget e slot 'defaut))))
                      ; si il n'a ni valeur ni valeur par defaut alors lui attribut une valeur grace a ifneeded
                      ; set attribut au slot slot(celui de l'itération) la clé de la facette ifneeded du slot
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

(define (Fframe frame)
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
(fput 'henry 'classification 'valeur 'instance)
(fput+ 'henry 'age 'age '21)
(fremove+ 'henry 'age 'age '21)
(fako?  'cannari 'pioupiou)
(fmenu)


