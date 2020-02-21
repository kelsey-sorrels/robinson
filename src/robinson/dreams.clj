(ns robinson.dreams
  (:require [clojure.core.strint :as i]))

; https://www.medicalnewstoday.com/articles/284378#interpretations
;
;    school, teachers, and studying
;    being chased or pursued
;    sexual experiences
;    falling
;    arriving too late
;    a living person being dead
;    a person now dead being alive
;    flying or soaring through the air
;    failing an examination
;    being on the verge of falling
;    being frozen with fright
;    being physically attacked
;    being nude
;    eating delicious food
;    swimming
;    being locked up
;    insects or spiders
;    being killed
;    losing teeth
;    being tied up, restrained, or unable to move
;    being inappropriately dressed
;    being a child again
;    trying to complete a task successfully
;    being unable to find toilet, or embarrassment about losing one
;    discovering a new room at home
;    having superior knowledge or mental ability
;    losing control of a vehicle
;    fire
;    wild, violent beasts
;    seeing a face very close to you
;    snakes
;    having magical powers
;    vividly sensing, but not necessarily seeing or hearing, a presence in the room
;    finding money
;    floods or tidal waves
;    killing someone
;    seeing yourself as dead
;    being half-awake and paralyzed in bed
;    people behaving in a menacing way
;    seeing yourself in a mirror
;    being a member of the opposite sex
;    being smothered, unable to breathe
;    encountering God in some form
;    seeing a flying object crash
;    earthquakes
;    seeing an angel
;    part animal, part human creatures
;    tornadoes or strong winds
;    being at the movie
;    seeing extra-terrestrials
;    traveling to another planet
;    being an animal
;    seeing a UFO
;    someone having an abortion
;    being an object

(defn school
  []
  "You dream of when you were a child in school.")

(defn chased-attacked
  []
  (let [adj (rand-nth ["angry" "violent" "exhausted" "mischevious" "deadly" "cuddly"])
        animal (rand-nth ["a tiger" "rats" "rabbits" "chipmunks" "tortoises" "crabs" "frogs" "parrots" "dogs" "cats"])
        action (rand-nth ["chased" "attacked" "mauled" "ripped to shreds"])]
    (i/<< "You dream of being ~{action} by ~{adj} ~{animal}.")))

(defn falling
  []
  "Are woken up by a sudden sense of falling. Just as you are about to hit the ground the dream ends.")

(defn arriving-too-late
  []
  (let [place (rand-nth ["a dinner part" "the dentist" "your own wedding" "a birthday party" "the first day of school" "a test"])]
    (i/<< "You dream of being late to ~{place}.")))

(defn a-living-person-being-dead
  []
  "You dream that you are dead - wandering around without any one else in sight.")

(defn a-person-now-dead-being-alive
  []
  (let [person (rand-nth ["parent" "cousin" "mother" "father" "aunt" "uncle" "teacher"])]
    (i/<< "You dream that you are your dead ~{person}.")))

(defn flying-or-soaring-through-the-air
  []
  "You dream you are flying through the air.")

(defn failing-an-examination
  []
  (let [class (rand-nth ["math" "writing" "history"])]
    (i/<< "You dream you are taking a ~{class} test and forgot to study.")))

(defn being-frozen-with-fright
  []
  "You dream of a monster causing you to be frozen in fright.")

(defn being-nude
  []
  "You dream of being caught naked.")

(defn eating-delicious-food
  []
  "You dream of eating a whole pie.")

(defn swimming
  []
  "You dream of swimming in the deep ocean so far from land that you forget which direction you need to swim back.")

(defn insects-or-spiders
  []
  (let [name (rand-nth ["spiders" "ants" "bugs" "mosquitoes"])]
    (i/<< "You dream of ~{name} crawing all over your body.")))

(defn being-killed
  []
  "You dream of being killed.")

(defn losing-teeth
  []
  "Your dreams are filled with images of you losing your teeth.")

(defn being-tied-up
  []
  "You dream of struggling with a rope which binds you tighter and tighter.")

(defn being-a-child-again
  [])

(defn trying-to-complete-a-task-successfully
  [])

(defn being-unable-to-find-toilet
  []
  "In your dreams you try to find a toilet, but cannot.")

(defn having-superior-knowledge-or-mental-ability
  [])

(defn fire
  [])

(defn snakes
  []
  "You dream of an unending writhin mass of snakes.")

(defn vividly-sensing
  [])

(defn finding-money
  []
  "You dream of finding a thick wad of cash. When you wake up you're sad, but honestly what use would you have with it here?")

(defn floods
  [])

(defn killing-someone
  []
  (let [target (rand-nth ["mother" "fater" "sister" "brother" "teacher" "lover"])]
    (i/<< "You dream of killing your ~{target}. You wakeup horrified and miss them.")))

(defn seeing-yourself-as-dead
  [])

(defn being-half-awake-and-paralyzed-in-bed
  []
  "In the night you wake up and find yourself paralyzed. After some time you manage to get back to sleep.")

(defn people-behaving-in-a-menacing-way
  [])

(defn seeing-yourself-in-a-mirror
  [])

(defn being-smothered
  [])

(defn encountering-god
  []
  "You dream you encounter God. You ask \"why?\" and then you wake up.")

(defn earthquakes
  [])

(defn part-animal-part-human-creatures
  [])

(defn tornadoes-or-strong-winds
  [])

(defn being-an-animal
  [])

(defn being-an-object
  []
  (let [obj (rand-nth ["coconut" "tree" "ship"])]
    (i/<< "You dream you are a ~{obj}. You live your ~{obj} life and then slowly fade away.")))

(defn description
  []
  ((rand-nth [school
              chased-attacked
              falling
              arriving-too-late
              a-living-person-being-dead
              a-person-now-dead-being-alive
              flying-or-soaring-through-the-air
              failing-an-examination
              being-frozen-with-fright
              being-nude
              eating-delicious-food
              swimming
              insects-or-spiders
              being-killed
              losing-teeth
              being-tied-up
              ; being-a-child-again
              ; trying-to-complete-a-task-successfully
              being-unable-to-find-toilet
              ; having-superior-knowledge-or-mental-ability
              ; fire
               snakes
              ; vividly-sensing
              finding-money
              ; floods
               killing-someone
              ; seeing-yourself-as-dead
               being-half-awake-and-paralyzed-in-bed
              ; people-behaving-in-a-menacing-way
              ; seeing-yourself-in-a-mirror
              ; being-smothered
               encountering-god
              ; earthquakes
              ; part-animal-part-human-creatures
              ; tornadoes-or-strong-winds
              ; being-an-animal
               being-an-object
])))
