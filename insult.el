(require 'list)

(defun insult:feedback-text(correct)
  "Returns the text for stick and carrot"
    (if correct
      (format "CORRECT! %s" (list:pick-random insult:praise))
    (format "INCORRECT ! %s" (insult:shakespearen))
  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq insult:praise 
  (list 
   "You are the son zod!"
   "May you find the cure for cancer!"
   "May you find hapiness in Another life!"
   "You are one step closer to enlightenment!" 
   "You are an ubermench!"
   "You are too good for this world!"
   "People will sing praises of you for centuries to come!"
   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun insult:shakespearen-person(name)
  (concat  name ", you are  a" (substring (insult:shakespearen) 4) ))

(defun insult:shakespearen()
  (concat "Thou "
          (format "%s %s %s"
                  (list:pick-random shakepeare-adj-1)
                  (list:pick-random shakepeare-adj-2)
                  (list:pick-random shakepeare-adj-3))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useless date
(defvar shakepeare-adj-1 
  (list "artless"
        "bawdy"
        "beslubbering"
        "bootless"
        "churlish"
        "cockered"
        "clouted"
        "craven"
        "currish"
        "dankish"
        "dissembling"
        "droning"
        
        "errant"
        "fawning"
        "fobbing"
        "froward"
        "frothy"
        "gleeking"
        "goatish"
        "gorbellied"
        "impertinent"
        "infectious"
        "jarring"
        "loggerheaded"
        "lumpish"
        "mammering"
        
        "mangled"
        "mewling"
        "paunchy"
        "pribbling"
        "puking"
        "puny"
        "qualling"
        "rank"
        "reeky"
        "roguish"
        "ruttish"
        "saucy"
        "spleeny"
        "spongy"
        "surly"
        "tottering"
        "unmuzzled"        
        "vain"
        "venomed"
        "villainous"
        "warped"
        "wayward"
        "weedy"
        "yeasty"
        ))


(setq shakepeare-adj-2 
      (list "court"
            "fowling"
            "witted "
            "headed"
            "brained"
            "clawed"
            "brained"
            "kissing"
            "pated"
            "dreaming"
            "eyed"                             
            "doghearted"
            "bolted"
            "vexing"
            "skinned"
            "kidneyed"
            "sucked"
            "mouthed"
            "bitten"
            "fallen"
            "born"
            "gorged"
            "griping"
            "faced"
            "witted"
            "born"
            "hated"
            "headed"
            "breeding"
            "nurtured"
            "pated"
            "livered"
            "minded"
            "eyed"
            "plucked"
            "deep"
            "marked"
            "ripe"
            "hewn"
            "growing"
            "fed"
            "borne"
            "biting"
            "galled"
            "bellied"
            "gaited"
            "brained"
            "spotted"
            "snouted"
            "bitten"
            ))

(setq shakepeare-adj-3
      (list
       "john" "baggage" "barnacle" "bladder" "pig  "
       "bugbear" "bailey" "blossom" "dish"  "clotpole" "coxcomb"
       "codpiece" "token" "dewberry" "dragon"  "wench" "gill"
       "licker" "fustilarian" "giglet" "gudgeon"  "haggard" "harpy"
       "pig" "beast" "mugger" "joithead"  "lewdster" "lout"
       "pie" "worm" "mammet" "measle"  "minnow" "miscreant"
       "moldwarp" "news" "hook" "egg"  "pignut" "puttock"
       "pumpion" "ratsbane" "scut" "skainsmate"  "strumpet" "varlet"
       "vassal" "face" "wagtail"  ))

(provide 'insult)
