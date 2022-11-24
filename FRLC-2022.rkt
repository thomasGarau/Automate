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

(define (is-in-list list value)
 (cond
  [(empty? list) false]
  [(equal? (first list) value) true]
  [else (is-in-list (rest list) value)]))

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


;on n'utilise pas *value car on transmet la valeur en paramètres au demon, ce dernier retourne ensuite la valeur modifié, cette valeur modifié seras celle qui seras ajouté 
(define (fput+ frame slot facet valeur)
  (fput frame slot facet (apply(eval(mycar(fget (mycar(cdr(fgetclasses frame))) slot 'if-added))) (list valeur))))


;demons
;retourne la valeur + 1 (elle est utilisé par fput+ pour changé la valeur de l'age)
(define (calcul-taille valeur)
  ( + valeur '1))

;valeur correspond à la frame
;utilisé par fremove+ lorsque l'age de la frame est supprimé alors on considére la frame comme "morte"
(define (del valeur )
  (fput valeur 'vie 'valeur 'mort))

;utilisé par ifneeded (retourne 1 si absence de valeur)
(define (ask) '1)
;fin demons

; retourne true si un lien de paranté entre les deux frame false si non 
(define (fako? frame1 frame2)
  (cond ((member frame1 (fgetclasses frame2)) #t)
        ( (member frame2 (fgetclasses frame1)) #t)
        (#t #f)))

;retourne #t si les frames ont la même valeur dans le même slot
(define (flink? frame1 frame2 slot)
  (cond ((member (mycar(fget frame1 slot 'valeur)) (fget-I frame2 slot)) #t)
        ((member (mycar(fget frame2 slot 'valeur)) (fget-I frame1 slot)) #t)
        (#t #f)))

;retourne #t si la frame possède une valeur dans le slot passé.
(define (fcheck frame slot)
    (cond ((null? (car(fget-I frame slot))) #f)
    (#t #t)))   

(define (salut)
  (display "salut"))

(define (fremove+ frame slot facet valeur)
  (cond ((member 'if-removed (ffacet frame slot)) (apply(eval(mycar (fget frame slot 'if-removed))) (list frame))))
  (fremove frame slot facet valeur))

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

;efface le frame en question et le reecris sans la valeur demandée
(define (fremove frame slot facet valeur)
    (define taille_slot (fslot frame))
    (define r #f)
    (set! b (fgetframe frame))
    (putprop frame 'frame (list frame))
    (map(lambda(e)
      (map (lambda(fac)fput
        (map(lambda(val)
            (cond((not(equal? valeur val))
              (fput frame e fac val)) 
              (#t (set! r 'T))))
              (myval frame e fac)))(myffacet frame e)))taille_slot)r)

;retourne True si l’argument passé en paramètre est un frame et nil sinon
(define (frames? frame)
  (define liste *frames*)
  (is-in-list liste frame))

;returne le nom du frame passé en paramètre et nil si il n’existe pas  
(define (fname nom)
  (cond ((member nom *frames*) nom)
          (#t #f)))

;retourne T si l’argument passé en paramètre a un nom et nil sinon
(define (fnames? nom)
  (cond ((member nom *frames*)#t)
        (#t ())))
  
;
(define (fcreate frame name age)
  (fput name 'ako 'valeur frame)
  (fput name 'classification 'valeur 'instance)
  (fput name 'age 'valeur age)
  (fput name 'vivant 'valeur 'oui)
  (cond ((equal? 'femme (car(fget name 'ako 'valeur))) (fput name 'enfant 'valeur 0))))


;frame correspond à la classe name au nom de l'instance 
(define (finst frame name age)
  (fcreate frame name age) 
  (set! *frame name)
  (map(lambda(e)
        (map (lambda (slot)
        (cond((not (null?(fget e slot 'valeur))))
             (#t (cond((not (null? (fget e slot 'defaut))))
                      (#t (cond( (not(null? (fget e slot 'ifneeded))) (set! *slot slot)(apply(eval (mycar (fget e slot 'ifneeded))) '()))))))))
               (fslot e)))
      (fgetclasses frame)))

(define (fgetclasses frame)
  (cond((null? frame) '())
       ((equal? frame 'objet) '(objet))
      (#t (cons frame (fgetclasses(mycar(fget frame 'ako 'valeur)))))))

(define (fgetslotsvalue frame slot)
  (define liste (fslot frame))
  (cond((null? frame) '())
       (#t (cond((member slot liste))
        ((equal? frame 'objet) '(objet))
        (#t (cons frame (fgetslotsvalue (mycar(fget frame slot 'valeur)) slot)))))))  

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

;genere un nom unique grace a un numero
(define (fgename frame)
  (cond((getprop frame 'number))
       (#t(putprop frame 'number 1))
  )
  (define char (symbol->string frame))
  (define num (number->string (getprop frame 'number)))
  (define res (string->symbol (string-append char "_" num)))
  (putprop frame 'number (+ 1 (getprop frame 'number)))res)


;retourne le premier parent du frame par rapport au slot
(define (fchildren frame slot)
  (define liste (cadr (fgetclasses frame)))liste)

;retourne la struture du frame passé en paramètre et nil sinon
(define (Frame frame)
  (cond((member frame *frames*) (fgetframe frame)) (#t '())))

;retourne toute les key du dico sans les value
(define (fkey dico)
  (map(lambda(e)
        (car e))dico))

;permet d'ajouter une ou plusieurs value à la facet du slot de la frame de manière guidé (utilisé pour fmenu)
(define (ajouteValue frame slot facet)
  (print "Veuillez saisir la valeur à ajouter")
  (define value (string->symbol (read-line (current-input-port))))
  (fput frame slot facet value)
  (print "Souhaiter vous ajouté une nouvelle value?")
  (define choix1 (string->symbol (read-line (current-input-port))))
  (cond ((equal? choix1 'y) (ajouteValue frame slot facet)))
  (car (fgetframe frame)))

;permet d'ajouter une ou plusieurs facet au slot de la frame de manière guidé (utilisé pour fmenu)
(define (ajouteFacet frame slot)
  (print "Veuillez saisir le nom de la facet à ajouter")
  (define facet (string->symbol (read-line (current-input-port))))
  (print "Veuillez saisir une valeur pour la facet")
  (define value (string->symbol (read-line (current-input-port))))
  (fput frame slot facet value)

  (print "Shouaiter vous ajouter une autre valeur à la nouvelle facet?")
  (define choix1 (string->symbol (read-line (current-input-port))))
  (cond ((equal? choix1 'y) (ajouteValue frame slot facet)))
  
  (print "Souhaiter vous ajouter une nouvelle facet ?")
  (define choix2 (string->symbol (read-line (current-input-port))))
  (cond ((equal? choix2 'y) (ajouteFacet frame slot)))
  
  (car (fgetframe frame)))

;permet d'ajouter un ou plusieurs slot à la frame de manière guidé (pour fmenu)
(define (ajouteSlot frame)
  (print "Veuillez saisir le nom du slot à ajouter")
  (define slot (string->symbol (read-line (current-input-port))))
  (print "Veuillez saisir le nom de la facet à ajouter")
  (define facet (string->symbol (read-line (current-input-port))))
  (print "Veuillez saisir le nom de la value à ajouter")
  (define value (string->symbol (read-line (current-input-port))))
  (fput frame slot facet value)

  (print "Shouaiter vous ajouter une autre valeur à la facet facet du nouveau slot?")
  (define choix1 (string->symbol (read-line (current-input-port))))
  (cond ((equal? choix1 'y) (ajouteValue frame slot facet)))
  
  (print "Souhaiter vous ajouter une nouvelle facet au slot ?")
  (define choix2 (string->symbol (read-line (current-input-port))))
  (cond ((equal? choix2 'y) (ajouteFacet frame slot)))

  (print "Souhaiter vous ajouter un autre slot au au frame ?")
  (define choix3 (string->symbol (read-line (current-input-port))))
  (cond ((equal? choix3 'y) (ajouteslot frame)))

  (car (fgetframe frame)))

(define (fputmenu)
  (print "Saisissez le nom du frame")
  (define framu (string->symbol (read-line (current-input-port))))
  (print "Saisissez le nom du slot")
  (define slotu (string->symbol (read-line (current-input-port))))
  (print "Saisissez le nom de la facet")
  (define facetu (string->symbol (read-line (current-input-port))))
  (print "Saisisser la valeur de la facet")
  (define value (string->symbol (read-line (current-input-port))))
  ;créer la frame avec les entrée de l'utilisateur
  (fput framu slotu facetu value)

  ;propose à l'utilisateur d'ajouter à la facet du slot de la frame créer une nouvlle value
  (print "Souhaiter vous ajouter une nouvelle value ?")
  (define choix1 (string->symbol (read-line (current-input-port))))
  (cond ((equal? choix1 'y) (ajouteValue framu slotu facetu)))

  ;propose à l'utilisateur d'ajouter une nouvelle facet au slot créer
  (print "Souhaiter vous ajouter une nouvelle facet ?")
  (define choix2 (string->symbol (read-line (current-input-port))))
  (cond ((equal? choix2 'y) (ajouteFacet framu slotu)))

  ;propose à l'utilisateur d'ajouter un nouveau slot à la frame créer
  (print "Souhaiter vous ajouter un nouveau slot ?")
  (define choix3 (string->symbol (read-line (current-input-port))))
  (cond ((equal? choix3 'y) (ajouteSlot framu)))
  
  (car(fgetframe framu)))

;permet de créer une instance de manière guidé (utilisé de manière guidé)
(define (fputinst)
  (print "Saisisser le nom du frame")
  (define framu (string->symbol (read-line (current-input-port))))
  (print "Saisissez le nom de l'instance")
  (define name (string->symbol (read-line (current-input-port))))
  (finst framu name)
  (print "Souhaiter vous ajouter un slot à la nouvelle instance ? y pour oui")
  (define choix3 (string->symbol (read-line (current-input-port))))
  (cond ((equal? choix3 'y) (ajouteSlot framu))))


;permet d'utilisé fput et finst de manière guidé pour l'utilisateur
(define (fmenu)
  ;définit la liste des fonction utilisable par l'utilisateur
  (define dico '(
                (fput 3)
                (finst 2)
               ))
  (define listeKey (fkey dico))
  (print "Shouaiter vous charger le fichier sauvegarde ? y pour oui")
  (define inputLoad (string->symbol (read-line (current-input-port))))
  (cond((equal? inputLoad 'y)(Fload)))  
  (print "Quelle fonction souhaiter vous utilisez parmis :")
  (print listeKey)
  (define input (string->symbol (read-line (current-input-port))))
  (cond ((not(member input listeKey)) "saisie incorect")
  (#t (cond ((equal? 'fput input)(fgetframe (fputmenu)))
            ((equal? 'finst input)(fputinst)))))
  (print "Shouaiter vous sauvegarder vos modification")
  (define choix1 (string->symbol (read-line (current-input-port))))
  (cond ((equal? choix1 'y) (Fsave)))
  )

;fonction qui reecris un frame sous une forme plus lisible
(define (Fwriteframe frame)
  (define ret "" )
  (set! ret (string-append ret (symbol->string (car(fgetframe frame))) ":" " \n "))
  (define listslot (fslot frame))
  (map(lambda(e)
    (set! ret (string-append ret (symbol->string e) ":" " \n " ))
    (set! ret (string-append ret "   " (symbol->string (car(ffacet frame e))) "\n" "      "(cond ((number? (car (fget frame e (car(ffacet frame e))))) (number->string (car (fget frame e (car(ffacet frame e))))))
                                                                                           ((symbol? (car (fget frame e (car(ffacet frame e))))) (symbol->string (car (fget frame e (car(ffacet frame e))))))
                                                                                           ((string? (car (fget frame e (car(ffacet frame e))))) (car (fget frame e (car(ffacet frame e))))) )  " \n "))
    )listslot) (display ret))

;ecris un frame dans le txt
(define (Fimprim frame)
  (define out (open-output-file "sauvegarde.txt" #:exists 'append));permet d'ecrire a la ligne suiv
  (println (Fwriteframe frame)out)
  (close-output-port out))

;vérifie si une frame est une instance, retourne #t si vrai, #f si faux et nil si elle n'existe
(define (finstance? frame)
    (cond ((not(member frame *frames*)) '())
          (#t (cond ((equal? (car(fget-I frame 'classification)) 'instance) #t)
                (#t #f)))))

;vérifie si une frame est un prototype, retourne #t si vrai, #f si faux et nil si elle n'existe 
(define (fgeneric? frame)
    (cond ((not(member frame *frames*)) '())
          (#t (cond ((equal? (car(fget-I frame 'classification)) 'prototype) #t)
                (#t #f)))))

;ecris tout les frames au moment de l'execution dans un txt
(define (Fsave)
  (define out (open-output-file "sauvegarde.txt" #:exists 'truncate));suprime avant d'ecrire
  (define a *frames*)
  (map(lambda(e)
        (Fimprim e))a))

;charge les info depuis sauvegarde.txt et les fput dans *frames*
(define (Fload)
  (define file "sauvegarde.txt")
  (define (f file)
    (let ((line (read-line file 'any)))
      (unless (eof-object? line)
        (fputSharp line)
        (f file))))
  (call-with-input-file file f))

;;la fonction enleve les element parasites du string var puis fput ce dernier
(define (fputSharp var)
  (define l (string-split var ":"));;on separe sur les :
  (set! l (string-join l ""));on reforme le string
  (set! l (string->list l))
  (set! l (remove* '(#\space) l));on enleve les espaces
  (set! l (list->string l))
  (set! l (string-split l "\\n"));et on resplit sur les saut de ligne
  (display l)
  (define len (/ (- (length l) 2) 3))
  (set! len (build-list len values))
  (set! len (map (lambda (n) (* 3 n)) len))
  (define nom (substring (car l) 1))
  (map(lambda(e);ici on recast les differente variable du fput
        (fput (string->symbol nom) (string->symbol (list-ref l (+ e 1) )) (string->symbol (list-ref l (+ e 2))) (cond( (not(equal? #f (string->number (list-ref l (+ e 3))))) (string->number (list-ref l(+ e 3))))
                                                                                                                (#t (string->symbol (list-ref l (+ e 3)))))
  ))len))  
  
(define (naissance name sexe mere)
  (cond ((not(member name *frames*))
        (cond((equal?(and (equal? 'femme (cadr(fgetclasses mere)))(equal? 'oui (car(fget mere 'marié 'valeur)))) #t)
            (cond((< (car(fget mere 'enfant 'valeur)) 10) (cigogne name sexe mere))))))))

;permet d'ajouter un métier à une frame si elle est un homme        
(define (ajouteTravail name job)
  (cond ((equal? (cadr(fgetclasses name)) 'homme) (fput name 'travail 'valeur job))))

;vérifie que la frame existe si elle existe et qu'elle est un homme alors print sont travail si non vérifie que la frame existe que c'est une femme et qu'elle est marié si c'est le cas créer un enfant
(define (travail name)
  (cond ((equal?(and (equal? (fname name) name)(equal? (cadr(fgetclasses name)) 'homme))#t) (print(fget name 'travail 'valeur)))
        ((equal? (and (equal? (fname name) name) (equal? (cadr(fgetclasses name)) 'femme) (equal? (car(fget name 'marié 'valeur)) 'oui))#t)(naissance (fgename 'enfant)'homme name))))

;crée une instance qui a un lien de parenté(fille) avec sa mère au sens courant. Ajoute aussi 1 au nombre d'enfant de la mère.
(define (cigogne name sexe mere)
  (fcreate sexe name 0)
  (fput name 'mère 'valeur mere)
  (define temp2 (car(fget mere 'enfant 'valeur)))
  (fremove mere 'enfant 'valeur (car (fget mere 'enfant 'valeur)))
  (fput mere 'enfant 'valeur (+ temp2 1)))

;
(define (marriage namehusband namewife)
  (fput namehusband 'marié 'valeur 'oui)
  (fput namewife 'marié 'valeur 'oui))      

(fput 'homme 'vie 'defaut 'vivant)
(fput 'homme 'classification 'valeur 'prototype)
(fput 'homme 'ako 'valeur 'objet)
(fput 'homme 'age 'if-added 'calcul-taille)
(fput 'henry 'age 'if-removed 'del)
(fput 'homme 'travail 'ifneeded 'ask)
(fput 'homme 'travail 'default 'unemploye)
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
(fput 'henry 'age 'if-removed 'del)
(fput 'marc 'ako 'valeur 'homme)
(fput 'marc 'classification 'valeur 'instance)
(fput 'henry 'classification 'valeur 'instance)
(fput 'marie 'ako 'valeur 'femme)
(fput 'marie 'marié 'valeur 'oui)
(fput+ 'henry 'age 'valeur 21)
(fremove+ 'henry 'age 'valeur 22)
(fako?  'cannari 'pioupiou)
(flink? 'canari 'pioupiou 'couleur)
(flink? 'henry 'marc 'classification)
(flink? 'homme 'henry 'ako)
(flink? 'oiseau 'pioupiou 'ako)
(finst 'femme 'ève 20)
(marriage 'henry 'ève)
(flink? 'marie 'henry 'ako)
(ajouteTravail 'henry 'ahouahou)
(travail 'ève)



