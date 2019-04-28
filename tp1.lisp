(defvar echec 'echec)
(defvar faits nil)
(defvar regles nil)
(defvar questions nil)

;Fonction qui ajoute un fait à la liste de faits
(defun ajouteFait (fait)
	(setq faits (cons fait faits))
)

;Fonction qui ajoute une règle à la liste de règles
;On crée une liste avec deux éléments; la première étant
;la liste de conditions, la seconde étant la conséquence
(defun ajouteRegle (conditions result) 
	(setq regles (cons (list conditions result) regles))
)

;Fonction qui initialise les bases de données
(defun initDBs() 
	(setq faits ())
	(setq regles ())
	(setq questions ())
)

;On enregistre les faits dans la base de données de faits
(defun remplir_faits () 
	(ajouteFait 'BA)
	(ajouteFait 'RA)
	(ajouteFait 'MB)
	(ajouteFait 'AP)
	(ajouteFait 'IN)
)

;On enregistre les règles dans la base de données de règles
(defun remplir_regles()
	(ajouteRegle '(MB) 'CO)
	(ajouteRegle '(AP) 'CO)
	(ajouteRegle '(RA) 'RE)
	(ajouteRegle '(IN) 'PA)
	(ajouteRegle '(BA RE) 'OK)
	(ajouteRegle '(CO PA RE) 'OK)
)

;Cette function nous permet d'initialiser les bases de données
;puis d'y insérer des informations de départ
(defun remplir()
	(initDBs)
	(remplir_faits)
	(remplir_regles)
)

;Cette fonction imprime un fait à l'écran
(defun imprimeFait (fait)
	(print fait)
)

;Cette fonction retourne les conditions d'une règle
;Elle se contente donc de retourner le premier élément
;de la liste
(defun conditionsRegle (regle)
	(nth 0 regle)
)

;Cette function retoune la conséquence d'une règle 
;(second élément de la liste)
(defun consequenceRegle (regle)
	(nth 1 regle)
)

;Cette fonction permet de vérifier si un fait satisfait une
;des conditions d'une règle. Il s'agit donc de vérifier si le
;fait fait partie de la liste des conditions d'une règle. Pour ce
;faire un utilise la fonction member et on vérifie que le résultat
;est différent de nil
(defun satisfaitUneCondition (regle fait)
	(not (null (member fait (conditionsRegle regle))))
)

;Cette fonction permet de vérifier si une liste est incluse dans
;une autre.  
(defun inclusion (L1 L2)
	(cond 
		((null L1) t)
		((member (car L1) L2) (inclusion (cdr L1) L2))
		(t nil)
	)
)

;cette fonction permet de vérifier si toutes les conditions d'une
;règle peuvent être satisfaites par les faits de la base de faits
;elle prend en paramètre une regle et retourne t ou nil
(defun satisfaitConditions (regle)
	(inclusion (conditionsRegle regle) faits)
)

;cette fonction permet de vérifier si un atom est contenue dans une liste
;elle pend en paramètre un atom et une liste
;elle retourne t ou nil
(defun estDans (item l)
	(cond
		((null item) nil)
		((null l) nil)
		((eql (car l) item) t)
		(t (estDans item (cdr l)))
	)
)

;cette fonction permet de vérifier si un fait est une solution
;elle retourne nil si c'est le cas, t sinon
;elle prend en paramètre un fait
(defun estUneSolution (fait)
	(not (estDans fait faits))
)

;fonction de chainage avant simple 
;elle prend en paramètre la liste des règles et 
;la liste des faits initiaux
;toutes les règles vont être essayées pour inférer de nouveaux faits
;ceux-ci seront ajoutés à la base de faits et seront affichés
;le processus s'arrête lorsqu'aucun nouveau fait ne peut être inféré
(defun chainageAvantSimple (faitsInitiaux regles)
	(let ((Q faitsInitiaux))
		(loop
			(setq p (car Q))
			(setq Q (cdr Q))
			(if (estUneSolution p)
				(ajouteFait p)
				(imprimeFait p)
			)
			(dolist (r regles)
				(if (and 
						(satisfaitUneCondition r p) 
						(and 
							(satisfaitConditions r) 
							(estUneSolution (consequenceRegle r))))
					(setq Q (cons (consequenceRegle r) Q))
				)
			)
			(when (= (length  Q) 0) (return nil))
		)
	)
)

;fonction qui permet d’insérer une nouvelle question dans 
;la base de questions questions
(defun ajouterQuestion(question)
	(setq questions (cons question questions))
)


;fonction qui permet de remplir la bases questions 
;avec plusieurs appels de la fonction ajouterQuestion
(defun remplirQuestions()
	(ajouterQuestion "Question 1: ")
	(ajouterQuestion "Question 2: ")
	(ajouterQuestion "Question 3: ")
	(ajouterQuestion "Question 4: ")
)

;permet d’afficher une question et ses options de réponse 
;et d’ajouter le fait correspondant 
;au choix tapé par l’utilisateur
(defun poserQuestion (question)
	(print question)
	(ajouterQuestion (read))
)

;permet de parcourir la base de questions 
;et d’appeller poserQuestion pour chaque question
(defun poserToutesQuestion()
	(dolist (question questions)
		(poserQuestion question)
	)
)

(remplir)
;(remplirQuestions)
;(poserToutesQuestion)
(chainageAvantSimple faits regles)
(print faits)