(ql:quickload :fact-base)

(defpackage olivier.compta
  (:use #:cl #:fact-base)
  (:export #:init #:enregistrer #:fermer #:ajout-compte #:achat-jeu #:gain-argent #:afficher-comptes #:afficher-solde))

(in-package olivier.compta)

(defconstant +type-transaction+ '(:transaction :ouverture :fermeture))
(defconstant +type-compte+ '(:revenu :depense :asset))

(defparameter *compta-db* nil
  "Objet comptabilité.")

(defun escape-double-quote (text)
  "Returns TEXT surrounded by \"."
  (format nil "\"~a\"" text))


(defun make-compta-error-message (subject what? context)
  "Return a string formatted as an error message for the compta package."
  (escape-double-quote (format nil "~a: ~a. ~a." subject what? context)))

(defun make-log-record (log-level error-type message &rest args)
  "Renvoie un list des information concernant en enregistrement dans le journal."
  `(log
    (log-level ,log-level)
    (type ,error-type)
    (record-date ,(get-universal-time))
    (message ,message)
    ,@args))

(defun make-log-record-unknown-account-type (compte faux-type)
  "Retourne une structure décrivant le type d'erreur type de compte inconnu."
  (let ((message-to-join (make-compta-error-message faux-type
                                                    "type de compte inconnu"
                                                    (format nil "Erreur lors de l'ajout du compte ~a" compte))))
    (make-log-record 'error
                     'unknown-account-type-error
                     message-to-join
                     (list 'account-type-received faux-type)
                     (list 'account (escape-double-quote compte)))))

(defun make-log-record-unknown-transaction-type (compte-destination faux-type transaction-date)
  "Retourne une structure décrivant le type d'erreur type de compte inconnu."
  (let ((message-to-join (make-compta-error-message faux-type
                                                    "type de transaction inconnu"
                                                    (format nil "Erreur lors de l'ajout d'une transaction. Destination: ~a - Date: ~a" compte-destination transaction-date))))
    (make-log-record 'error
                     'unknown-transaction-type-error
                     message-to-join
                     (list 'transaction-type-received faux-type)
                     (list 'destination-account (escape-double-quote compte-destination))
                     (list 'transaction-date (escape-double-quote transaction-date)))))

(defun make-log-record-database-not-set (operation)
  "Retourne une structure décrivant le type d'erreur type de compte inconnu."
  (make-log-record 'error
                   'database-not-set-error
                   (escape-double-quote "Aucune base de donnée ouverte.")
                   (list 'requested-operation operation)))

(defun fichier-compta-p (fichier)
  "Est-ce que le fichier de comptabilité existe?"
  (probe-file (pathname fichier)))

(defun compta-initialisee-p ()
  "Est-ce que le système de gestion de comptabilité a été initialisé?"
  (typep *compta-db* 'FACT-BASE))

(defun init (fichier)
  "Initialisation du système de gestion de comptabilité."
  (unless (compta-initialisee-p)
    ;; (if (fichier-compta-p fichier)
    ;;     (setq *compta-db* (fact-base:load! fichier))
    (setq *compta-db* (fact-base:make-fact-base :file-name fichier))));)

(defun enregistrer ()
  "Enregitrer les modification dans la comptabilité sur disque."
  (if (compta-initialisee-p)
      (write! *compta-db*)
      (error "~a~%" (make-log-record-database-not-set 'enregistrer))))

(defun fermer (&optional (enregistre! t))
  "Ferme le fichier de comptabilité et s'assure que tout a été enregistré."
  (if (compta-initialisee-p)
      (progn
        (when enregistre! (enregistrer))
        (setq *compta-db* nil))
      (error "~a~%" (make-log-record-database-not-set 'fermer))))

(defun ajout-compte (compte type description)
  "Ajout d'un compte dans la db de la comptabilité."
  (if (compta-initialisee-p)
      (if (find type +type-compte+)
          (fact-base:multi-insert! *compta-db* `((:compte ,compte) (:type ,type) (:description ,description)))
          (error "~a~%" (make-log-record-unknown-account-type compte type)))
      (error "~a~%" (make-log-record-database-not-set 'ajout-compte))))

(defun ajout-transaction (date destination montant &key (devise "EUR") (type :transaction) (source "argent:poche") beneficiaire annotation)
  "Ajout d'une transaction."
  (if (compta-initialisee-p)
      (if (find type +type-transaction+)
          (let ((id-transaction (fact-base:multi-insert! *compta-db* `((:date ,date)
                                                                       (:destination ,destination)
                                                                       (:montant ,montant)
                                                                       (:devise ,devise)
                                                                       (:type ,type)))))
            (when (eq type :transaction) (fact-base:insert! *compta-db* (list id-transaction :source source)))
            (when beneficiaire (fact-base:insert! *compta-db* (list id-transaction :beneficiaire beneficiaire)))
            (when annotation (fact-base:insert! *compta-db* (list id-transaction :annotation annotation))))
          (error (make-log-record-unknown-transaction-type destination type date)))
      (error "~a~%" (make-log-record-database-not-set 'ajout-transaction))))

(defun achat-jeu (date montant beneficiaire annotation)
  "Ajout d'une transaction d'achat de jeu vidéo."
  (if (compta-initialisee-p)
      (ajout-transaction date "jeux:steam" montant :source "argent:poche" :annotation annotation :beneficiaire beneficiaire)
      (error "~a~%" (make-log-record-database-not-set 'achat-jeu))))

(defun gain-argent (date montant source)
  "Ajout d'une transaction de gain d'argent."
  (if (compta-initialisee-p)
      (ajout-transaction date "argent:poche" montant :source source)
      (error "~a~%" (make-log-record-database-not-set 'gain-argent))))

(defun afficher-liste-comptes ()
  "Afficher tout les comptes existant."
  (if (compta-initialisee-p)
      (let ((list-comptes (for-all (and (?id :type ?type)
                                        (?id :compte ?compte)
                                        (?id :description ?desc))
                                   :in *compta-db*
                                   :collect (list ?type ?compte ?desc))))
        (dolist (info-compte list-comptes)
          (format t
                  "Compte: ~a~%- type de compte: ~a~%- description: ~a~%"
                  (second info-compte)
                  (first info-compte)
                  (third info-compte))))
      (error "~a~%" (make-log-record-database-not-set 'afficher-liste-comptes))))

(defun afficher-solde ()
  "Affiche le solde du compte argent:poche."
  (if (compta-initialisee-p)
      (let ((list-trn (for-all (and (?id :destination ?dest)
                                    (?id :type :transaction)
                                    (?id :source ?src)
                                    (?id :montant ?mnt))
                               :in *compta-db*
                               :collect (list ?src ?dest ?mnt)))
            (solde (car (for-all (and (?id :type :ouverture)
                                      (?id :src "argent:poche")
                                      (?id :montant ?mnt))
                                 :in *compta-db*
                                 :collect ?mnt))))
        (dolist (transaction list-trn)
          (if (string= (third transaction) "argent:poche")
              (setq solde (- solde (fourth transaction)))
              (setq solde (+ solde (fourth transaction)))))
        (format t "Solde pour le compte argent:poche: ~a~%" solde))
      (error "~a~%" (make-log-record-database-not-set 'afficher-solde))))



;; (for-all (and (?id :type :revenu) (?id :compte ?compte) (?id :description ?desc)) :in *compta-db* :collect (list ?compte ?desc))
;; (write! *compta-db* :file-name "~/org/private/data/compta-olivier.fbdb")
