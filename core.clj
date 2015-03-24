(load-game-file "entities.clj")

(declare main-screen status-screen spider-screen)

;; one of :main, :spider
(def current-screen-k (atom :main))

(defn narrate
  [text]
  (screen! status-screen :on-narration :say text))

(def start-story "You come out of the candy house into the bright daylight. Right! Come on, be brave!")

;; main screen - the map

(defn camera-follow-player!
  [screen entities]
  (when-let [player (find-by-id :player entities)]
    (position! screen (:x player) (:y player)))
  entities)

(defn check-health!
  [health]
  (when (<= health 0)
    (screen! status-screen :on-game-over)
    (screen! main-screen :on-game-over)))

(defn burn
  [player attack]
  (if (:on-fire? player)
    player
    (let [health (max 0 (- (:health player) 10))]
      (check-health! health)
      (narrate "I'M BURNING!!!!")
      (particle-effect! attack :start)
      (screen! status-screen :on-update-health :health health)
      (assoc player :on-fire? true
             :health health))))

(defn shark-bite
  [player attack]
  (if (:bleeding? player)
    player
    (let [health (max 0 (- (:health player) 10))]
      (check-health! health)
      (narrate "HELP!!!!!!")
      (particle-effect! attack :start)
      (screen! status-screen :on-update-health :health health)
      (assoc player :bleeding? true
             :health health))))

(defn poisoning
  [player attack]
  (if (:poisoning? player)
    player
    (let [health (max 0 (- (:health player) 10))]
      (check-health! health)
      (narrate "Wait a second... is this poison...? It is!")
      (screen! status-screen :on-update-health :health health)
      (assoc player :poisoning? true
             :health health))))

(defn interact
  [entities]
  (when-let [player (find-by-id :player entities)]
    (->>
     entities
     (reduce (fn [entities e]
               ;; something we can touch?
               (if (or (:player? e)
                       (not (near-entity? player e 1))
                       (and (not (:attack? e))
                            (not= (:in-pits? e) (:in-pits? player))))
                 entities
                 ;; can touch
                 (cond
                  ;; pick up items
                  (:item? e)
                  (do (screen! status-screen :on-pick-up-item :which-entity e)
                      ;; remove from screen
                      (remove #(= (:id %) (:id e)) entities))
                  ;; meet spiders
                  (:spider? e)
                  (cond
                   ;; attack
                   (:chasing? e)
                   entities
                   ;; already done this spider's riddle, ignore it
                   (get (:riddles-done player) (:id e))
                   entities
                   ;; new riddle
                   :else
                   (do (screen! spider-screen :on-enter
                                :which-entities {:player player :spider e})
                       (reset! current-screen-k :spider)
                       entities))
                  ;; meet sharks
                  (:shark? e)
                  (-> entities
                    (update-by-id :player shark-bite (find-by-id :blood entities))
                    (update-by-id (:id e) assoc :biting? true))
                  ;; meet poison
                  (= :poison (:id e))
                  (update-by-id entities :player poisoning (find-by-id :poison entities))
                  ;; meet volcanos
                  (:volcano? e)
                  (if (particle-effect! e :is-complete)
                    ;; not currently erupting, pass through
                    entities
                    ;; hit by eruption
                    (update-by-id entities :player burn (find-by-id :burn entities)))
                  ;; all others
                  :else
                  entities)))
             entities))))

(defn act
  [screen entities entity]
  (cond
   (:player? entity)
   (if (:dead? entity)
     entity
     (cond-> (merge entity (get-player-velocities entity))
            ;; if not touching water any more
            (and (:floating? entity)
                 (not (some #(touching-layer? screen entity %) float-layers)))
            (assoc :floating? false
                   :walk-layers (apply disj (:walk-layers entity) float-layers))
            ;; if not on fire any more
            (and (:on-fire? entity)
                 (particle-effect! (find-by-id :burn entities) :is-complete))
            (assoc :on-fire? false)
            ;; if not bleeding any more
            (and (:bleeding? entity)
                 (particle-effect! (find-by-id :blood entities) :is-complete))
            (assoc :bleeding? false)
            ;; if touching lava
            (and (not (:jumping? entity))
                 (on-layer-ok? screen entity "lava")
                 (not (touching-layer? screen entity "stepping")))
            (burn (find-by-id :burn entities))
            ))
   ;; spiders
   (:spider? entity)
   (cond
    (and (:chasing? entity) (not (:spitting? entity)))
    (do (add-timer! screen :poison-spit 0.1)
      (assoc entity :spitting? true))
    (:chasing? entity)
    (chase entity (find-by-id :player entities))
    :else
    entity)
   ;; sharks
   (:shark? entity)
   (let [player (find-by-id :player entities)]
     (cond
       (and (:biting? entity)
            (not (:bleeding? player)))
       (assoc entity :biting? false)
       (and (:floating? player)
            (not (:invisible? player)))
       (chase entity player)
       ;; otherwise - go round in circles
       :else
       (go-round-and-round entity)))
   ;; volcanos
   (:volcano? entity)
   (doto entity
     (particle-effect! :update (graphics! :get-delta-time)))
   ;; poison - not sticky
   (= :poison (:id entity))
   entity
   ;; sticky attacks
   (:attack? entity)
   (let [player (find-by-id :player entities)]
     (particle-effect! entity :update (graphics! :get-delta-time))
     (merge entity (select-keys player [:x :y :width :height])))
   :else
   entity))

(defn render-with-overlaps
  [screen entities]
  (render-map! screen :without "bridges")
  (draw! screen (filter :in-pits? entities))
  (render-map! screen :with "bridges")
  (draw! screen (remove :in-pits? entities))
  entities)

(defn try-jump
  [player]
  (if (:in-pits? player)
    (narrate "Bbbbaaaadddd!!!"))
  (start-jump player))

(def lava-step-cells
  (atom ()))

(defn extract-lava-step-cells!
  [screen]
  (let [layer (tiled-map-layer screen "stepping")
        place (rectangle-from-object-layer screen "lava-step-place")]
    (->> (for [tile-x (range (int (:x place))
                             (+ (:x place) (:width place)))
               tile-y (range (int (:y place))
                             (+ (:y place) (:height place)))]
           (let [cell (tiled-map-cell layer tile-x tile-y)]
             (tiled-map-layer! layer :set-cell tile-x tile-y nil)
             [tile-x tile-y cell]))
         (swap! lava-step-cells into))))

(defn place-lava-step-cells!
  [screen]
  (let [layer (tiled-map-layer screen "stepping")]
    (doseq [[tile-x tile-y cell] @lava-step-cells]
      (tiled-map-layer! layer :set-cell tile-x tile-y cell))
    (swap! lava-step-cells empty)))

(defscreen main-screen
  :on-show
  (fn [screen _]
    ;(reset! current-screen-k :main)
    (let [renderer (orthogonal-tiled-map "level.tmx" (/ 1 pixels-per-tile))
          screen (update! screen :camera (orthographic) :renderer renderer)
          player (assoc (create-player)
                        :x 5 :y (- (dec map-height) 24))
          spiders [(assoc (create-spider screen "spider-1")
                          :walk-layers #{"pits"}
                          :in-pits? true
                          :riddle (str "First tell me where you battle and fight, "
                                       "then what is in the middle of battle, "
                                       "and what sounds like the search for a hard to find word. "
                                       "Put them together to make something hard to pick up.")
                          :answer "water")
                   (assoc (create-spider screen "spider-2")
                          :riddle (str "First, what do you say when you want quiet, "
                                       "then what was build when the flood came; "
                                       "these things I want to know, so tell me quick. "
                                       "Put them together and what do you get?")
                          :answer "shark")
                   (assoc (create-spider screen "spider-3")
                          :riddle (str "Help me and my sisters find the thing we lost. " 
                                       "We think we've misplaced our \"padless brick\", "
                                       "but maybe it's just us.")
                          :answer "black spiders")
                   (assoc (create-spider screen "spider-4")
                          :riddle (str "Decode and answer this: hatw anc ouy od ithw a andw?")
                          :answer "magic")]
          rope (assoc (create-entity-from-object-layer screen "rope")
                      :item? true
                      :in-pits? true)
          sword (assoc (create-entity-from-object-layer screen "sword")
                       :item? true)
          floaty (assoc (create-entity-from-object-layer screen "floaty")
                       :item? true)
          wand (assoc (create-entity-from-object-layer screen "wand")
                       :item? true)
          lava-step (assoc (create-entity-from-object-layer screen "lava-step")
                       :item? true)
          pool-shark (assoc (create-shark screen "pool-shark")
                       :focus-point [(:x lava-step) (:y lava-step)])
          sharks [(create-shark screen "shark-1")
                  (create-shark screen "shark-2")
                  (create-shark screen "shark-3")
                  (create-shark screen "shark-4")
                  (create-shark screen "shark-5")
                  (create-shark screen "shark-6")
                  ]
          volcs [(create-volcano screen "volcano-1" "fire.p")
                 (create-volcano screen "volcano-2" "fire2.p")
                 ]
          attacks [(assoc (particle-effect "burn.p" :scale-effect 0.02)
                     :id :burn
                     :attack? true
                     :x 0 :y 0 :width 1 :height 1)
                   (assoc (particle-effect "blood.p" :scale-effect 0.01)
                     :id :blood
                     :attack? true
                     :x 0 :y 0 :width 1 :height 1)
                   (assoc (particle-effect "poison.p" :scale-effect 0.02)
                     :id :poison
                     :attack? true
                     :x 0 :y 0 :width 1 :height 1)
                   ]
          house (assoc (texture "candy-house.png")
                       :id :candy-house
                       :x 0 :y (- (dec map-height) 25)
                       :width 5 :height 6)
          ]
      (extract-lava-step-cells! screen)
      (add-timer! screen :eruption-1 5 5)
      (add-timer! screen :eruption-2 6 5)
      (concat [player rope sword floaty wand lava-step pool-shark house]
              spiders
              sharks
              volcs
              attacks)))
  
  :on-timer
  (fn [screen entities]
    (case (:id screen)
      :eruption-1 (update-by-id entities :volcano-1
                                #(doto % (particle-effect! :start)))
      :eruption-2 (update-by-id entities :volcano-2
                                #(doto % (particle-effect! :start)))
      :visible-again
      (update-by-id entities :player assoc :invisible? false :color nil)
      :poison-spit
      (when-let [spider (find-first #(and (:spider? %) (:spitting? %)) entities)]
        (let [player (find-by-id :player entities)]
          (add-timer! screen :poison-spit-done 4)
          (update-by-id entities :poison (fn [e]
                                           (particle-effect! e :start)
                                           (assoc e :x (:x spider)
                                                  :y (:y spider)
                                                  :x-velocity (* 5.0 (- (:x player) (:x spider)))
                                                  :y-velocity (* 5.0 (- (:y player) (:y spider))))))))
      :poison-spit-done
      (for [e entities]
        (cond
          (:spider? e) (assoc e :spitting? false)
          (:player? e) (assoc e :poisoning? false)
          :else e))
      ))
  
  :on-render
  (fn [screen entities]
    (when (= :main @current-screen-k)
      (clear!)
      (->> entities
        (map (fn [entity]
               (->> entity
                    (act screen entities)
                    (move screen)
                    (animate screen)
                    (prevent-move screen entities))))
        (interact)
        (play-sounds!)
        (sort-by :y >)
        (render-with-overlaps screen)
        (camera-follow-player! screen))))
  
  :on-resize
  (fn [screen entities]
    (height! screen vertical-tiles))
  
  :on-use-item
  (fn [screen entities]
    (when-let [player (find-by-id :player entities)]
      (case (:item-id screen)
        :sword (if (and (not (:in-pits? player))
                        (on-layer-ok? screen player "bridges"))
                 (do (narrate "You slash the rope bridge and tumble into the pit below.")
                     (update-by-id entities :player
                                   assoc :walk-layers #{"pits"}
                                   :in-pits? true))
                   (do (narrate "You slash something unnecessarily and look around foolishly.")
                     nil))
        :rope (if (and (:in-pits? player)
                       (on-layer-ok? screen player "bridges"))
                (do (narrate "Ah, this rope is so useful!!!")
                    (update-by-id entities :player
                                  assoc :walk-layers player-layers
                                  :in-pits? nil))
                (do (narrate "Oops, not there!!!")
                  nil))
        :floaty (if (and (some #(touching-layer? screen player %) float-layers)
                         (not (:floating? player)))
                  (do (narrate "keep up the floating!!!")
                      (update-by-id entities :player
                                    assoc :walk-layers (into (:walk-layers player) float-layers)
                                    :floating? true))
                  (do (narrate "Don't have time to play with toys now!!!")
                    nil
                    ))
        :wand (if (:invisible? player)
                player
                (do (narrate "Abracadabra! It's working!")
                    (add-timer! screen :visible-again 30)
                    (update-by-id entities :player
                                  assoc :invisible? true :color [0 0 0 0.5])))
        :lava-step (if (touching-layer? screen player "lava")
                     (do (narrate "Hop, skip and jump!!!")
                         (place-lava-step-cells! screen)
                         (screen! status-screen :on-lose-item :item-id :lava-step)
                         nil
                       )
                     (do (narrate "Be careful! Don't drop it on your foot!!!")
                          ))
        )))
  
  :on-solve-riddle
  (fn [screen entities]
    (when-let [player (find-by-id :player entities)]
      (case (count (:keys-won player))
        0 (narrate "Your first triumph has come!")
        1 (narrate "Two's the one!")
        2 (narrate "tut tut! 3333!")
        3 (narrate "You've done it!!!")
        )
      (let [health (min 100 (+ (:health player) 10))
            num-keys (+ 1 (count (:keys-won player)))]
        (screen! status-screen :on-update-health :health health)
        (screen! status-screen :on-update-keys :num-key num-keys)
        (update-by-id entities :player
                      (fn [e]
                        (-> e
                            (update-in [:riddles-done] conj (:spider-id screen))
                            (update-in [:keys-won] conj (:spider-id screen))
                            (assoc :health health)))))
      )
    )
  
  :on-give-up-riddle
  (fn [screen entities]
    (when-let [player (find-by-id :player entities)]
      (narrate "This is bad...Ahhhhhhhh!!!!!!!")
      (-> entities
          (update-by-id :player
                        update-in [:riddles-done] conj (:spider-id screen))
          (update-by-id (:spider-id screen)
                        assoc :chasing? true))))

  :on-game-over
  (fn [screen entities]
    (update-by-id entities :player assoc :color [0 0 0 0] :dead? true))

  :on-key-down
  (fn [screen entities]
    (when (= :main @current-screen-k)
      (when-let [player (find-by-id :player entities)]
        (if  (= (:key screen) (key-code :space))
          (update-by-id entities :player try-jump)
          ))))
  
  :on-touch-down
  (fn [screen entities]
    (when (= :main @current-screen-k)
      (when-let [player (find-by-id :player entities)]
        (let [x (game :x)
              y (game :y)
              width (game :width)
              height (game :height)]
          (if (and (< (* width 0.45) x (* width 0.55))
                   (< (* height 0.45) y (* width 0.55)))
            (update-by-id entities :player try-jump)
            )))))
  )

;;;; status screen - items, health

(def button->item-id (atom {}))
(def item-id->table-obj (atom {}))

(defscreen status-screen
  :on-show
  (fn [screen _]
    (let [screen (update! screen :camera (orthographic) :renderer (stage))]
      (height! screen 500)
      [(assoc (vertical [] :left :reverse)
              :id :item-table
              :y 0)
       (assoc (label "0" (color :white))
              :id :key-text
              :x (/ (width screen) 2 ) :y 10)
       (assoc (texture "key.png")
                       :id :key
                       :x (- (/ (width screen) 2) 35)
                       :y -3
                       :width 36 :height 36)
       (assoc (label "health 100" (color :yellow))
              :id :health-text
              :x (- (width screen) 100) :y 10)
       (assoc (shape :filled
                     :set-color (color :white)
                     :rect 0 0 10 102
                     :set-color (color :black)
                     :rect 1 1 8 100)
         :id :health-bar-bg
         :x (- (width screen) 20)
         :y 10)
       (assoc (shape :filled
                     :set-color (color :green)
                     :rect 1 1 8 100)
         :id :health-bar
         :x (- (width screen) 20)
         :y 10
         :scale-y 1.0)
       (assoc (label start-story (color :white))
              :id :narration
              :x 10
              :y (- (height screen) 100)
              :height 50)]
    ))
  
  :on-update-health
  (fn [screen entities]
    (for [e entities]
      (case (:id e)
        :health-bar (assoc e :scale-y (/ (:health screen) 100))
        :health-text (doto e (label! :set-text (str "health "
                                                    (:health screen))))
        e)))
  
  :on-update-keys  
  (fn [screen entities]
    (for [e entities]
      (case (:id e)
        :key-text (doto e (label! :set-text (str (:num-key screen))))
        e)))
  
  ;; when calling this, give :which-entity with the entity
  :on-pick-up-item
  (fn [screen entities]
    (let [e (-> (:which-entity screen)
              (update-in [:width] * pixels-per-tile)
              (update-in [:height] * pixels-per-tile))
          ui-skin (skin "uiskin.json")
          button (text-button " Use " ui-skin)
          item-table (find-by-id :item-table entities)
          held-e (horizontal [(image e) button])
          ]
      (case (:id e)
        :sword (narrate "Your holely sword has been brought to you!")
        :rope (narrate "You find a small rope in a pit.")
        :floaty (narrate "What a surprise! A floaty!")
        :wand (narrate "A wand! Do some magic!")
        :lava-step (narrate "You find a big stepping stone!"))
      (swap! button->item-id assoc (:object button) (:id e))
      (swap! item-id->table-obj assoc (:id e) (:object held-e))
      (add! item-table held-e)
      entities))
  
  :on-lose-item
  (fn [screen entities]
    (let [table-obj (get @item-id->table-obj (:item-id screen))
          item-table (find-by-id :item-table entities)]
      (vertical! item-table :remove-actor table-obj)
      entities))
  
  :on-ui-changed
  (fn [screen entities]
    (let [item-id (get @button->item-id (:actor screen))]
      (screen! main-screen :on-use-item :item-id item-id)
      entities))
  
  :on-narration
  (fn [screen entities]
    (update-by-id entities :narration
                  (fn [e]
                    (label! e :set-text (:say screen))
                    (assoc e :x (width screen) :counter 0))))
  
  :on-game-over
  (fn [screen entities]
    (conj entities
          (assoc (label (str "GAME" \newline "OVER") (color :white))
            :id :game-over-text
            :x (/ (width screen) 2)
            :y (/ (height screen) 2)
            :set-font-scale 10
            :set-alignment (align :center))))

  :on-render
  (fn [screen entities]
    (->> (for [e entities]
           (case (:id e)
             :narration (cond
                          (nil? (:counter e))
                          e
                          (> (:counter e 0) (* 60 5))
                          (do (label! e :set-text "")
                            (dissoc e :counter))
                          :else
                          (-> e
                            (update-in [:x] #(int (* % 0.9)))
                            (update-in [:counter] inc)))
             e))
         (render! screen)))
  
  :on-resize
  (fn [screen entities]
    (height! screen 500)
    (for [e entities]
      (case (:id e)
        :health-bar-bg (assoc e :x (- (width screen) 20))
        :health-bar (assoc e :x (- (width screen) 20))
        :health-text (assoc e :x (- (width screen) 100))
        e)))
  )


;;;; spider screen - sub-game for riddles, fighting

(defn make-speech-bubbles
  [screen]
  (let [spider-bubble (assoc (shape :filled
                                    :set-color (color :white)
                                    :ellipse 0 0 400 160
                                    :triangle -10 -10 0 80 60 50)
                             :id :spider-bubble
                             :x 150
                             :y 300)
        spider-speech (assoc (label "" (color :black)
                                    :set-alignment (align :top)
                                    :set-wrap true)
                             :id :spider-speech
                             :x (+ (:x spider-bubble) 50)
                             :y (+ (:y spider-bubble) 10)
                             :width 300
                             :height 120)
        ui-skin (skin "uiskin.json")
        response (assoc (text-field "" ui-skin
                                    :set-alignment (align :left))
                        :id :response-field
                        :x (- (width screen) 220)
                        :y 400
                        :width 200)
        response-but (assoc (text-button " Reply " ui-skin)
                            :id :response-button
                            :x (- (width screen) 170)
                            :y (- (:y response) 30))
        give-up-but (assoc (text-button "give up" ui-skin)
                            :id :give-up-button
                            :x (- (width screen) 100)
                            :y (- (:y response) 30))]
    [spider-bubble spider-speech response response-but give-up-but]))
  
(defn spider-say!
  [entities text]
  (label! (find-by-id :spider-speech entities)
          :set-text text))

(defscreen spider-screen
  :on-show
  (fn [screen _]
    (let [screen (update! screen :camera (orthographic) :renderer (stage))]
      (height! screen 600)
      (make-speech-bubbles screen)))
  
  :on-render
  (fn [screen entities]
    (when (= :spider @current-screen-k)
      (clear! 0.2 0.2 0.2 1)
      (render! screen)
      (->> entities
        (draw! screen))))
  
  :on-enter
  (fn [screen entities]
    (let [in-ents (:which-entities screen)
          spider (assoc (:spider in-ents)
                        :width 200 :height 200
                        :x 100 :y 100)
          player (assoc (:player in-ents)
                        :width 200 :height 200
                        :x (- (width screen) 200)
                        :y 100)]
      (spider-say! entities (:riddle spider))
      (into entities
            [spider player])))
  
  :on-timer
  (fn [screen entities]
    (case (:id screen)
      :say-riddle (let [spider (find-first :spider? entities)]
                    (spider-say! entities (:riddle spider))
                    entities)
      :solved-riddle (let [spider (find-first :spider? entities)]
                       (screen! main-screen :on-solve-riddle :spider-id (:id spider))
                       (reset! current-screen-k :main)
                       ;; remove actors that came in
                       (remove #(or (:player? %) (:spider? %)) entities))
      :give-up (let [spider (find-first :spider? entities)]
                 (screen! main-screen :on-give-up-riddle :spider-id (:id spider))
                 (reset! current-screen-k :main)
                 ;; remove actors that came in
                 (remove #(or (:player? %) (:spider? %)) entities))
      ))
  
  :on-ui-changed
  (fn [screen entities]
    (stage! screen :set-keyboard-focus nil)
    (when-let [player (find-by-id :player entities)]
      (if (= (:actor screen) (:object (find-by-id :response-button entities)))
          (let [field (find-by-id :response-field entities)
                wrote (-> (text-field! field :get-text)
                        (clojure.string/lower-case)
                        (clojure.string/trim))
                spider (find-first :spider? entities)]
            (if (= wrote (:answer spider))
              (do
                (spider-say! entities "Correct.")
                (add-timer! screen :solved-riddle 2)
                entities)
              (do
                (spider-say! entities "Wrong.")
                (add-timer! screen :say-riddle 2)
                entities)
              ))
          ;; gave up
          (do
            (spider-say! entities "You... I want you...!")
            (add-timer! screen :give-up 2)
            entities)
          )))
  
  :on-resize
  (fn [screen entities]
    (height! screen 600)
    (for [e entities]
      (case (:id e)
         :response-field (assoc e :x (- (width screen) 220))
         :response-button ( assoc e :x (- (width screen) 170))
         :give-up-button (assoc e :x (- (width screen) 100))
        e)))
  )

(set-game-screen! main-screen spider-screen status-screen)
