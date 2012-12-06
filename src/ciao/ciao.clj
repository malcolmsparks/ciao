(ns ciao.ciao
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic)
  )

;; Ch1: Verbs

;; parlare
;; abitare
;; aspettare
;; volare
;; lavorare

;; In grammar, inflection or inflexion is the modification of a word to express different grammatical categories such as tense, grammatical mood, grammatical voice, aspect, person, number, gender and case. Conjugation is the inflection of verbs; declension is the inflection of nouns, adjectives and pronouns.

(defrel verb infinitive meaning)

;; First conjugation - o, i, a, iamo, ate, ano
(fact verb (vec "parlare") "speak")
(fact verb (vec "abitare") "live")
(fact verb (vec "aspettare") "wait")
(fact verb (vec "volare") "fly")
(fact verb (vec "lavorare") "work")

;; Second conjugation - o, i, e, iamo, ete, ono

(fact verb (vec "conoscere") "know")
(fact verb (vec "credere") "believe")
(fact verb (vec "godere") "enjoy")

;; lemma is the canonical form of a lexeme

;; a word has a single stem, namely the part of the word that is common to all its inflected variants

;; Conjugation may be affected by person, number, gender, tense, aspect, mood, voice, or other grammatical categories.

;; lexeme: [:verb {:infinitive "speak" :person [:first :singular] :tense [:present :indicative]}]

;; A lexeme ( pronunciation (help·info)) is an abstract unit of morphological analysis in linguistics, 

;; Conjugation may be affected by person, number, gender, tense, aspect, mood, voice, or other grammatical categories.

;; Person and tense are called 'grammatical categories'.

(defne regular-verb-table [class person tense suffix]
  ([_ [:first :singular] [:present :indicative] (vec "o")])
  ([_ [:second :singular] [:present :indicative] (vec "i")])
  ([(vec "are") [:third :singular] [:present :indicative] (vec "a")])
  ([(vec "ere") [:third :singular] [:present :indicative] (vec "e")])
  ([_ [:first :plural] [:present :indicative] (vec "iamo")])
  ([(vec "are") [:second :plural] [:present :indicative] (vec "ate")])
  ([(vec "ere") [:second :plural] [:present :indicative] (vec "ete")])
  ([(vec "are") [:third :plural] [:present :indicative] (vec "ano")])
  ([(vec "ere") [:third :plural] [:present :indicative] (vec "ono")]))

(run* [q p t]
      (regular-verb-table q p t (vec "i")))

(defn conjugateo [infinitive person tense word meaning]
  (fresh [stem suffix verbtype]
         (appendo stem verbtype infinitive)
         (appendo stem suffix word)
         (membero verbtype [(vec "are") (vec "ere") (vec "ire")])
         (verb infinitive meaning)
         (regular-verb-table verbtype person tense suffix)))

(run 1 [q]
     (conjugateo (vec "parlare") [:first :plural] (lvar) q (lvar)))

(run 1 [q person]
     (conjugateo q person (lvar) (vec "parliamo") (lvar)))

;; We'll now be moving on to sentences, here's some helper functions.

(defn tokenize [s] (map vec (clojure.string/split s #"\s+")))
(defn untokenize [ws] (apply str (flatten (interpose " " ws))))

;; In Italian, nouns  are also inflected
;; Inflection of nouns (and adjectives) is called declension

(defne declenso [stem gender number word]
  ([_ :feminine :singular _] (appendo stem (vec "a") word))
  ([_ :feminine :plural _] (appendo stem (vec "e") word))
  ([_ :masculine :singular _] (appendo stem (vec "o") word))
  ([_ :masculine :plural _] (appendo stem (vec "i") word))
  )

;; Ch2: More work needed on nouns

(defrel noun word meaning)
(fact noun (vec "capello") "hair")
(fact noun (vec "gatta") "cat")
(fact noun (vec "gatto") "cat")
(fact noun (vec "gatti") "cats")
(fact noun (vec "gatte") "cats")
(fact noun (vec "cane") "dog")
(fact noun (vec "zio") "uncle")
(fact noun (vec "zia") "aunt")
(fact noun (vec "zii") "uncles")
(fact noun (vec "televisione") "television")

(defne nouno [word gender number meaning]
  ([(vec "italiano") _ _ "Italian"])

  ;; Nouns ending in -ione are generally feminine
  ([_ :feminine :singular _] (fresh [stem]
                                  (appendo stem (vec "ione") word)
                                  (noun word meaning)))

  ;; Words can be nouns subject to declensions
  ([_ _ _ _] (fresh [stem]
                  (noun word meaning)
                  (declenso stem gender number word)
                  )))

(run* [gender number m] (nouno (vec "zio") gender number m))
(run* [gender number m] (nouno (vec "gatti") gender number m))
(run* [gender number m] (nouno (vec "cane") gender number m))

;; Interesting, this can be both singular and plural, so we keep the 'superposition' until we meet the article.
(run* [gender number m] (nouno (vec "televisione") gender number m))

;; Ch3: Adjectives

(defrel adjective word meaning)
(fact adjective (vec "alto") "tall")
(fact adjective (vec "bello") "beautiful")
(fact adjective (vec "grande") "big")
(fact adjective (vec "magro") "skinny")
(fact adjective (vec "nuovo") "new")
(fact adjective (vec "ricco") "rich")
(fact adjective (vec "basso") "short")
(fact adjective (vec "brutto") "ugly")
(fact adjective (vec "piccolo") "small")
(fact adjective (vec "grasso") "fat")
(fact adjective (vec "vecchio") "old")
(fact adjective (vec "povero") "poor")
(fact adjective (vec "azzurro") "blue")
(fact adjective (vec "bianco") "white")
(fact adjective (vec "celeste") "sky")
(fact adjective (vec "giallo") "yello")
(fact adjective (vec "grigio") "gray")
(fact adjective (vec "nero") "black")
(fact adjective (vec "rosso") "red")
(fact adjective (vec "verde") "green")

;; arancione -- orange
;; blu -- dark blue
;; marrone -- brown
;; rosa -- pink
;; viola -- purple

(defrel invariable-adjective word meaning)
(fact invariable-adjective (vec "arancione") "orange")
(fact invariable-adjective (vec "marrone") "brown")

;; 2 main types of adjective: those that end in an 'o' eg. alto, those that end in an 'e', eg. intelligente

;; I'm using the $ (sentence) suffix to mean 'in-place'
;; Use the & to mean 'and meaning'

;; page 39 Complete Italian Grammar
;; Masculine plurals of both -o and -e end in i,
;; Feminine singulars end in -a and -e, plurals and in -e and -i


(defn adjectivo [word gender number meaning]
  (conda
   [(invariable-adjective word meaning)]
   [(matche [gender number]
            ([:masculine :singular] (adjective word meaning))
            ([:masculine :plural] (fresh [stem adj]
                                         (appendo stem (vec "i") word)
                                         (adjective adj meaning)
                                         (conde [(appendo stem (vec "o") adj)]
                                                [(appendo stem (vec "e") adj)])))
            
            ([:feminine :singular] (fresh [stem adj]
                                          (adjective adj meaning)
                                          (conde
                                           [(appendo stem (vec "o") adj)
                                            (appendo stem (vec "a") word)]
                                           [(appendo stem (vec "e") adj)
                                            (appendo stem (vec "e") word)])))
            
            ([:feminine :singular] (fresh [stem adj]
                                          (adjective adj meaning)
                                          (conde
                                           [(appendo stem (vec "o") adj)
                                            (appendo stem (vec "e") word)]
                                           [(appendo stem (vec "e") adj)
                                            (appendo stem (vec "i") word)]))))]))


(run* [gender number m] (adjectivo (vec "nera") gender number m))

(run* [gender number m] (adjectivo (vec "grandi") gender number m))


;; TODO: Noun-phrases - "il capello", "la gatta", "l'atleta", "lo zio"
;; TODO: articles
;; Now let's do an article

;; NB: Articles cannot be defined independently of the nouns the denote

(defn consonanto [q]
  (membero q (map vec ["b" "c" "d" "f" "g" "l" "m" "n" "p" "r" "s" "t"])))

;; Adjectives either end in o or e.

;; 'Invariable' adjectives are not subject to inflection
;; arancione -- orange
;; blu -- dark blue
;; marrone -- brown
;; rosa -- pink
;; viola -- purple

;; Introduce DCGs - what are noun phrases? Use LaTeX support for linguistics!
;; Codification of http://en.wikipedia.org/wiki/Italian_grammar#Inflection_of_nouns_and_adjectives
;; TODO: Consonant clusters, sonorants and elisions

;; eg.

;; l'uomo simpatico - the nice man
;; la camicia bianca - the white shirt
;; il bambino stanco - the tired child
;; la gonna lunga - the long skirt
;; il vestito grigio - the gray suit

;; gli uomini simpatici - the nice men
;; le camicie bianche - the white shirts
;; i bambini stanchi - the tired children
;; le gonne lunghe - the long skirts
;; i vestiti grigi - the gray suits

(defn noun-phrase [words definiteness gender number meaning]
  (fresh [adj-meaning noun-meaning]

         (== meaning [:noun-phrase {:noun noun-meaning
                                    :definiteness definiteness
                                    :gender gender
                                    :number number
                                    :adjective adj-meaning}])

         (matche [words definiteness gender number]
                 
                 ;; Feminine cases are a little easier
                 ([[?article . [?word]] :definite :feminine :singular]
                    (nouno ?word gender number noun-meaning)
                    (== ?article (vec "la"))
                    )

                 ([[?article . [?word . [?adjective]]] :definite :feminine :singular]
                    (nouno ?word gender number noun-meaning)
                    (== ?article (vec "la"))     
                    (adjectivo ?adjective gender number adj-meaning)
                    )

                 ([[?article . [?word]] :definite :feminine :plural]
                    (nouno ?word gender number noun-meaning)
                    (== ?article (vec "le")))
                 
                 ;; Masculine cases are somewhat more tricky, due to lo/il gli/i
                 ([[?article . [?word]] :definite :masculine :singular]
                    ;; In Italian, the gender and the number of the article and noun have to match.
                    (nouno ?word gender number noun-meaning)
                    (conda
                     [(conde
                       [(fresh [suffix consonant r]
                               (appendo (vec "s") suffix ?word)
                               (appendo consonant r suffix)
                               (consonanto consonant))]
                       [(fresh [prefix r]
                               (appendo prefix r ?word)
                               (membero prefix (map vec ["z" "gn" "ps" "pn" "x" "y"])))])
                      (== ?article (vec "lo"))]
                     ;; Otherwise it's 'il'
                     [(== ?article (vec "il"))]))

                 ([[?article . [?word]] :definite :masculine :plural]
                    (nouno ?word gender number noun-meaning)
                    (conda
                     [(fresh [prefix r]
                             (appendo prefix r ?word)
                             (membero prefix (map vec ["z" "sc" "gn"])))
                      (== ?article (vec "gli"))
                      ]
                     ;; Otherwise it's 'i'
                     [(== ?article (vec "i"))]))

                 ;; Feminine indefinites
                 ([[?article . [?word]] :indefinite :feminine :singular]
                    (nouno ?word gender number noun-meaning)
                    (== ?article (vec "una"))     
                    )

                 ;; Masculine indefinites
                 ([[?article . [?word]] :indefinite :masculine :singular]
                    (nouno ?word gender number noun-meaning)
                    (conda
                     [(fresh [prefix r]
                             (appendo prefix r ?word)
                             (conde [(== prefix (vec "z"))]
                                    [(== prefix (vec "sc"))]
                                    [(== prefix (vec "gn"))]))
                      (== ?article (vec "uno"))]
                     [(== ?article (vec "un"))]) 
                    ))))


;; Fail
(run* [d gender number m] (noun-phrase (tokenize "il zio") d gender number m))

;; Pass
(run* [d gender number m] (noun-phrase (tokenize "la gatta") d gender number m))

;; Pass
(run* [d gender number m] (noun-phrase (tokenize "la gatta bianca") d gender number m))

;; Pass
(run* [d gender number m] (noun-phrase (tokenize "uno zio") d gender number m))

;; Tests - all should pass
(run* [d gender number m] (noun-phrase (tokenize "una gatta") d gender number m))
(run* [d gender number m] (noun-phrase (tokenize "le gatte") d gender number m))
(run* [d gender number m] (noun-phrase (tokenize "il gatto") d gender number m))
(run* [d gender number m] (noun-phrase (tokenize "un gatto") d gender number m))
(run* [d gender number m] (noun-phrase (tokenize "i gatti") d gender number m))
(run* [d gender number m] (noun-phrase (tokenize "lo zio") d gender number m))
(run* [d gender number m] (noun-phrase (tokenize "uno zio") d gender number m))
(run* [d gender number m] (noun-phrase (tokenize "gli zii") d gender number m))

;; Should fail
(run* [d gender number m] (noun-phrase (tokenize "un zio") d gender number m))
;; because it should be...
(run* [d gender number m] (noun-phrase (tokenize "uno zio") d gender number m))

;; ================================================================================

;; TODO: Now add adjectives (they are subject to declension too!)

;; TODO: Noun-phrases - "il capello nero"

(run* [d g n m]
      (noun-phrase (tokenize "il capello") d g n m))

;; Ok, now a verb phrase to join our verb conjugation with our noun phrase

(defn verb-phrase [words meaning]
  (fresh [verb-word verb-meaning np-words np-definiteness np-gender np-number infinitive person tense np-meaning]
         (== meaning [:verb-phrase {:infinitive infinitive :verb-meaning verb-meaning :person person :tense tense :noun-phrase np-meaning}])
         (conso verb-word np-words words)
         (noun-phrase np-words np-definiteness np-gender np-number np-meaning)
         (conjugateo infinitive person tense verb-word verb-meaning)))

(run 1 [q]
      (verb-phrase (tokenize "parlo il gatto") q))

(defn sentence [words meaning]
  (fresh [np-words vp-words d g n np-meaning vp-meaning]
         (== meaning [:sentence {:verb-phrase vp-meaning :noun-phrase np-meaning}])
         (appendo np-words vp-words words)
         (noun-phrase np-words d g n np-meaning)
         (verb-phrase vp-words vp-meaning)
         ))


#_(map untokenize (run 3 [q]
                     (sentence
                      q
                      [:sentence {:verb-phrase [:verb-phrase {:infinitive (vec "parlare"), :verb-meaning "speak", :person [:first :singular], :tense [:present :indicative], :noun-phrase [:noun-phrase {:noun "cat", :definiteness :definite, :gender :feminine, :number :singular, :adjective "white"}]}], :noun-phrase [:noun-phrase {:noun "hair", :definiteness :definite, :gender :masculine, :number :singular, :adjective (lvar)}]}])))






(run 1 [q]
     (sentence (tokenize "il capello parlo la gatta bianca") q))

(run 1 [q]
     (verb-phrase (tokenize "parlo la italiano") q))

(map untokenize (run 1 [q]
                     (verb-phrase
                      q
                      [:verb-phrase {:infinitive (vec "parlare"), :verb-meaning "speak", :person [:first :plural], :tense [:present :indicative], :noun-phrase [:noun-phrase {:noun "Italian", :definiteness :definite, :gender :feminine, :number :singular, :adjective (lvar)}]}])))

